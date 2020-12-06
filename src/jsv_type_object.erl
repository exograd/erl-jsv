%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(jsv_type_object).

-behaviour(jsv_type).

-export([verify_constraint/2, format_constraint_violation/2,
         validate_type/1, validate_constraint/4, canonicalize/3,
         generate/2]).

-export_type([constraint/0]).

-type constraint() :: {value, jsv:definition()}
                    | {min_size, non_neg_integer()}
                    | {max_size, non_neg_integer()}
                    | {required, Names :: [jsv:keyword()]}
                    | {members, #{jsv:keyword() := jsv:definition()}}.

verify_constraint({value, Definition}, State) ->
  jsv_verifier:verify(State#{definition := Definition});

verify_constraint({min_size, Min}, _) when is_integer(Min), Min >= 0 ->
  ok;
verify_constraint({min_size, _}, _) ->
  invalid;

verify_constraint({max_size, Max}, _) when is_integer(Max), Max >= 0 ->
  ok;
verify_constraint({max_size, _}, _) ->
  invalid;

verify_constraint({required, Names}, _) when is_list(Names) ->
  case lists:all(fun jsv:is_keyword/1, Names) of
    true ->
      ok;
    false ->
      invalid
  end;
verify_constraint({required, _}, _) ->
  invalid;

verify_constraint({members, Definitions}, State) when is_map(Definitions) ->
  case lists:all(fun jsv:is_keyword/1, maps:keys(Definitions)) of
    true ->
      F = fun (_, Definition, Errors) ->
              case jsv_verifier:verify(State#{definition := Definition}) of
                ok ->
                  Errors;
                {error, Errors2} ->
                  Errors2 ++ Errors
              end
          end,
      case maps:fold(F, [], Definitions) of
        [] ->
          ok;
        Errors ->
          {error, Errors}
      end;
    false ->
      invalid
  end;
verify_constraint({members, _}, _) ->
  invalid;

verify_constraint(_, _) ->
  unknown.

format_constraint_violation({min_size, Min}, _) ->
  {"value must contain at least ~0tp members", [Min]};

format_constraint_violation({max_size, Max}, _) ->
  {"value must contain at most ~0tp members", [Max]};

format_constraint_violation({required, _}, {missing_names, [Name]}) ->
  {"value must contain the following member: ~ts", [Name]};
format_constraint_violation({required, _}, {missing_names, Names}) ->
  Data = lists:join(<<", ">>, lists:map(fun jsv:keyword_value/1, Names)),
  {"value must contain the following members: ~ts", [iolist_to_binary(Data)]}.

validate_type(Value) when is_map(Value) ->
  {ok, Value};
validate_type(_) ->
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
          Name = jsv:keyword_value(Name0),
          case maps:is_key(Name, Value) of
            true ->
              MissingNames;
            false ->
              [Name | MissingNames]
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
          MemberName = jsv:keyword_value(MemberName0),
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
      {ok, CData4};
    {Errors, _} ->
      Errors
  end.

canonicalize(_, CData, _) ->
  CData.

generate(Term, State) when is_map(Term) ->
  Definition = jsv_generator:state_definition(State),
  F = fun (K, V, Acc) ->
          MemberName = jsv:keyword_value(K),
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
    {ok, maps:fold(F, #{}, Term)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end;
generate(_, _) ->
  error.

-spec member_definition(binary(), jsv:definition()) ->
        {ok, jsv:definition()} | error.
member_definition(Name, Definition) when is_atom(Definition) ->
  member_definition(Name, {Definition, #{}});
member_definition(Name, {_, #{members := Members}}) ->
  %% This is quite bad, but keys of the members constraint can be binaries,
  %% strings or atoms. Not much we can do here, unless we enforce binaries
  %% which would make them annoying to type.
  %%
  %% If definitions had to be put in catalog registries, we could transform
  %% them during registration, but they can also be provided directly.
  Fun = fun F(It) ->
            case maps:next(It) of
              {Key, Definition, It2} ->
                case jsv:keyword_equal(Key, Name) of
                  true ->
                    {ok, Definition};
                  false ->
                    F(It2)
                end;
              none ->
                error
            end
        end,
  Fun(maps:iterator(Members));
member_definition(_Name, {_, #{value := Definition}}) ->
  {ok, Definition};
member_definition(_Name, _) ->
  error.
