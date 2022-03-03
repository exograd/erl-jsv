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

-module(jsv_verifier).

-export([init/1, verify/2]).

-export_type([state/0]).

-type state() :: #{options := jsv:options(),
                   catalog => jsv:catalog_name(),
                   definitions := definition_set()}.

-type definition_set() ::
        #{definition_key() := boolean()}.

-type definition_key() ::
        {jsv:catalog_name(), jsv:definition_name()}.

-spec init(jsv:options()) -> state().
init(Options) ->
  State = #{options => Options,
            definitions => #{}},
  case maps:find(catalog, Options) of
    {ok, Catalog} ->
      State#{catalog => Catalog};
    error ->
      State
  end.

-spec verify(jsv:definition(), state()) ->
        {ok, state()} | {error, [jsv:definition_error_reason()]}.
verify({one_of, []}, _State) ->
  {error, [invalid_empty_definition_list]};
verify({one_of, Definitions}, State) when is_list(Definitions) ->
  case
    lists:foldl(fun (Def, {Es, S}) ->
                    case verify(Def, S) of
                      {ok, S2} -> {Es, S2};
                      {error, Es2} -> {Es ++ Es2, S}
                    end
                end, {[], State}, Definitions)
  of
    {[], State2} ->
      {ok, State2};
    {Errors, _} ->
      {error, Errors}
  end;
verify({ref, DefinitionName}, State) ->
  case maps:find(catalog, State) of
    {ok, Catalog} ->
      verify({ref, Catalog, DefinitionName}, State);
    error ->
      {error, [no_current_catalog]}
  end;
verify({ref, Catalog, DefinitionName},
       State = #{definitions := Definitions}) ->
  Key = {Catalog, DefinitionName},
  case maps:is_key(Key, Definitions) of
    true ->
      {ok, State};
    false ->
      case jsv:find_catalog_definition(Catalog, DefinitionName) of
        {ok, Definition} ->
          verify(Definition, State#{catalog => Catalog,
                                    definitions => Definitions#{Key => true}});
        {error, Reason} ->
          {error, [Reason]}
      end
  end;
verify(TypeName, State) when is_atom(TypeName) ->
  verify({TypeName, #{}, #{}}, State);
verify({TypeName, Constraints}, State) ->
  verify({TypeName, Constraints, #{}}, State);
verify({TypeName, Constraints, Extra}, State = #{options := Options}) when
    is_atom(TypeName), is_map(Constraints), is_map(Extra) ->
  case maps:find(TypeName, jsv:type_map(Options)) of
    {ok, Module} ->
      F = fun (ConstraintName, ConstraintValue, {Es, S}) ->
              Constraint = {ConstraintName, ConstraintValue},
              Result = case
                         jsv_utils:call_if_defined(Module, verify_constraint,
                                                   [Constraint, S])
                       of
                         {ok, Res} -> Res;
                         undefined -> unknown
                       end,
              case Result of
                ok ->
                  {Es, S};
                {ok, S2} ->
                  {Es, S2};
                unknown ->
                  {[{unknown_constraint, TypeName, Constraint} | Es], S};
                invalid ->
                  Error = {invalid_constraint, TypeName, Constraint,
                           invalid_value},
                  {[Error | Es], S};
                {invalid, Reason} ->
                  Error = {invalid_constraint, TypeName, Constraint, Reason},
                  {[Error | Es], S};
                {error, ValidationErrors} ->
                  {ValidationErrors ++ Es, S}
              end
          end,
      case maps:fold(F, {[], State}, Constraints) of
        {[], State2} ->
          {ok, State2};
        {Errors, _} ->
          {error, Errors}
      end;
    error ->
      {error, [{unknown_type, TypeName}]}
  end;
verify(Definition, _State) ->
  {error, [{invalid_format, Definition}]}.
