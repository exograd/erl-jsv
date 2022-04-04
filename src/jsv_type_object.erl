%% Copyright (c) 2020-2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(jsv_type_object).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/2, validate_constraint/4, canonicalize/3,
         generate/2]).

-export_type([constraint/0]).

-type constraint() :: {value, jsv:definition()}
                    | {min_size, non_neg_integer()}
                    | {max_size, non_neg_integer()}
                    | {required, Names :: [atom()]}
                    | {members, #{atom() := jsv:definition()}}.

verify_constraint({value, Definition}, State) ->
  jsv_verifier:verify(Definition, State);

verify_constraint({min_size, Min}, _) when is_integer(Min), Min >= 0 ->
  ok;
verify_constraint({min_size, _}, _) ->
  invalid;

verify_constraint({max_size, Max}, _) when is_integer(Max), Max >= 0 ->
  ok;
verify_constraint({max_size, _}, _) ->
  invalid;

verify_constraint({required, Names}, _) when is_list(Names) ->
  case lists:all(fun is_atom/1, Names) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint({required, _}, _) ->
  invalid;

verify_constraint({members, Definitions}, State) when is_map(Definitions) ->
  case lists:all(fun is_atom/1, maps:keys(Definitions)) of
    true ->
      F = fun (_, Definition, {Es, S}) ->
              case jsv_verifier:verify(Definition, S) of
                {ok, S2} ->
                  {Es, S2};
                {error, Es2} ->
                  {Es2 ++ Es, S}
              end
          end,
      case maps:fold(F, {[], State}, Definitions) of
        {[], State2} ->
          {ok, State2};
        {Errors, _} ->
          {error, Errors}
      end;
    false ->
      invalid
  end;
verify_constraint({members, _}, _) ->
  invalid;

verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min_size, 1}, _) ->
  "must contain at least 1 member";
format_constraint_violation({min_size, Min}, _) ->
  {"must contain at least ~0tp members", [Min]};

format_constraint_violation({max_size, Max}, _) ->
  {"must contain at most ~0tp members", [Max]};

format_constraint_violation({required, _}, {missing_names, [Name]}) ->
  {"must contain the following member: ~ts", [Name]};
format_constraint_violation({required, _}, {missing_names, Names}) ->
  Data = lists:join(<<", ">>, lists:map(fun atom_to_binary/1, Names)),
  {"must contain the following members: ~ts", [iolist_to_binary(Data)]};
format_constraint_violation({members, _}, {invalid_names, Names}) ->
  Data = lists:join(<<", ">>, Names),
  {"must not contain the following members: ~ts",
   [iolist_to_binary(Data)]}.

validate_type(Value, State) when is_map(Value) ->
  Options = jsv_validator:state_options(State),
  Value2 = maybe_remove_null_members(Value, Options),
  {ok, Value2, Value2};
validate_type(_, _) ->
  error.

validate_constraint(Value, {value, ValueType}, CData, State) ->
  F = fun (MemberName, MemberValue, {Errors, CData2}) ->
          case
            jsv_validator:validate_child(MemberValue, ValueType, MemberName,
                                         State)
          of
            {ok, ValueCData} ->
              {Errors, CData2#{MemberName => ValueCData}};
            {error, Errors2} ->
              {Errors2 ++ Errors, CData2}
          end
      end,
  case maps:fold(F, {[], CData}, Value) of
    {[], CData3} ->
      {ok, CData3};
    {Errors, _} ->
      Errors
  end;

validate_constraint(Value, {min_size, Min}, _, _) ->
  map_size(Value) >= Min;

validate_constraint(Value, {max_size, Max}, _, _) ->
  map_size(Value) =< Max;

validate_constraint(Value, {required, Names}, _, _) ->
  F = fun (Name0, MissingNames) ->
          Name = atom_to_binary(Name0),
          case maps:is_key(Name, Value) of
            true ->
              MissingNames;
            false ->
              [Name0 | MissingNames]
          end
      end,
  case lists:foldl(F, [], Names) of
    [] ->
      ok;
    MissingNames ->
      {invalid, {missing_names, lists:reverse(MissingNames)}}
  end;

validate_constraint(Value, {members, Definitions}, CData, State) ->
  F = fun (MemberName0, Definition, {Errors, CData2}) ->
          MemberName = atom_to_binary(MemberName0),
          case maps:find(MemberName, Value) of
            {ok, MemberValue} ->
              case
                jsv_validator:validate_child(MemberValue, Definition,
                                             MemberName, State)
              of
                {ok, ValueCData} ->
                  CData3 = maps:remove(MemberName, CData2),
                  {Errors, CData3#{binary_to_atom(MemberName) => ValueCData}};
                {error, Errors2} ->
                  {Errors2 ++ Errors, CData2}
              end;
            error ->
              {Errors, CData2}
          end
      end,
  case maps:fold(F, {[], CData}, Definitions) of
    {[], CData4} ->
      ValidNames = maps:keys(Definitions),
      case maps:keys(maps:without(ValidNames, CData4)) of
        [] ->
          {ok, CData4};
        InvalidNames ->
          Options = maps:get(options, State),
          case maps:get(unknown_member_handling, Options, error) of
            error ->
              {invalid, {invalid_names, InvalidNames}};
            keep ->
              {ok, CData4};
            remove ->
              {ok, maps:with(ValidNames, CData4)}
          end
      end;
    {Errors, _} ->
      Errors
  end.

canonicalize(_, CData, _) ->
  CData.

generate(Term0, State) when is_map(Term0) ->
  Options = jsv_generator:state_options(State),
  Term = maybe_remove_null_members(Term0, Options),
  Definition = jsv_generator:state_definition(State),
  F = fun (MemberName, V, Acc) ->
          case member_definition(MemberName, Definition) of
            {ok, MemberDefinition} ->
              case jsv_generator:generate_child(V, MemberDefinition, State) of
                {ok, MemberValue} ->
                  Acc#{MemberName => MemberValue};
                {error, Reason} ->
                  throw({error, Reason})
              end;
            error ->
              Acc#{MemberName => V}
          end
      end,
  try
    Value = maps:fold(F, #{}, Term),
    case Definition of
      {_, Constraints = #{members := _}, _} ->
        generate_check_members(Value, Constraints, Options);
      _ ->
        {ok, Value}
    end
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end;
generate(_, _) ->
  error.

-spec generate_check_members(json:object(), jsv:constraints(),
                             jsv:options()) ->
        {ok, json:object()} | {error, jsv:generation_error_reason()}.
generate_check_members(Value, Constraints = #{members := Members}, Options) ->
  ValidNames = maps:keys(Members),
  case maps:keys(maps:without(ValidNames, Value)) of
    [] ->
      RequiredNames = maps:get(required, Constraints, []),
      case [Name || Name <- RequiredNames, not is_map_key(Name, Value)] of
        [] ->
          {ok, Value};
        MissingNames ->
          {error, {missing_members, MissingNames, Value}}
      end;
    InvalidNames ->
      case maps:get(unknown_member_handling, Options, error) of
        error ->
          {error, {invalid_members, InvalidNames, Value}};
        keep ->
          {ok, Value};
        remove ->
          {ok, maps:with(ValidNames, Value)}
      end
  end.

-spec member_definition(binary(), jsv:definition()) ->
        {ok, jsv:definition()} | error.
member_definition(Name, TypeName) when is_atom(TypeName) ->
  member_definition(Name, {TypeName, #{}, #{}});
member_definition(Name, {TypeName, Constraints}) ->
  member_definition(Name, {TypeName, Constraints, #{}});
member_definition(Name, {_, #{members := Members}, _}) ->
  maps:find(Name, Members);
member_definition(_Name, {_, #{value := Definition}, _}) ->
  {ok, Definition};
member_definition(_Name, _) ->
  error.

-spec maybe_remove_null_members(json:object(), jsv:options()) ->
        json:object().
maybe_remove_null_members(Object, Options) ->
  case maps:get(null_member_handling, Options, keep) of
    keep ->
      Object;
    remove ->
      jsv_utils:remove_null_map_entries(Object)
  end.
