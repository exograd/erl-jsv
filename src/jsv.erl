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

-module(jsv).

-export([default_type_map/0,
         validate/2, validate/3,
         generate/2, generate/3,
         verify_catalog/1, verify_catalog/2,
         verify_definition/1, verify_definition/2,
         type_map/1,
         catalog_table_name/1, register_catalog/2, unregister_catalog/1,
         find_catalog_definition/2]).

-export_type([definition/0, definition_name/0,
              catalog/0, catalog_name/0, catalog_table_name/0,
              type/0, type_map/0,
              constraints/0, constraint/0,
              constraint_name/0, constraint_value/0,
              extra/0,
              validate_fun/0, validation_result/1, validation_error/0,
              generate_fun/0, generation_result/0,
              options/0, unknown_member_handling/0,
              definition_error_reason/0,
              value_error/0, value_error_reason/0,
              constraint_violation_details/0,
              generation_error_reason/0]).

-type definition() :: type()
                    | {type(), constraints()}
                    | {type(), constraints(), extra()}
                    | {ref, definition_name()}
                    | {ref, catalog_name(), definition_name()}
                    | {one_of, [definition()]}.

-type definition_name() :: atom().

-type catalog() :: #{definition_name() := definition()}.
-type catalog_name() :: atom().
-type catalog_table_name() :: atom().

-type type() :: atom().
-type type_map() :: #{type() := module()}.

-type constraints() :: #{constraint_name() := constraint_value()}.
-type constraint() :: {constraint_name(), constraint_value()}.
-type constraint_name() :: atom().
-type constraint_value() :: term().

-type extra() :: #{validate => validate_fun(),
                   generate => generate_fun()}.

-type validate_fun() ::
        fun((term()) -> validation_result(term()))
      | fun((term(), term()) -> validation_result(term())).

-type validation_result(Value) ::
        {ok, Value}
      | {error, validation_error()}
      | {error, [validation_error()]}.

-type validation_error() ::
        {invalid_value, term(),
         Reason :: term(), ReasonString :: unicode:chardata()}
      | {invalid_value, term(), json_pointer:pointer(),
         Reason :: term(), ReasonString :: unicode:chardata()}
      | {invalid_child, json_pointer:pointer(), [value_error()]}.

-type generate_fun() ::
        fun((term()) -> generation_result())
      | fun((term(), term()) -> generation_result()).

-type generation_result() :: {ok, term()} | {error, term()}.

-type options() :: #{type_map => type_map(),
                     disable_verification => boolean(),
                     catalog => catalog_name(),
                     unknown_member_handling => unknown_member_handling(),
                     null_member_handling => null_member_handling(),
                     validate_arg => term(),
                     generate_arg => term()}.

-type unknown_member_handling() :: error | keep | remove.
-type null_member_handling() :: keep | remove.

-type definition_error_reason() ::
        {invalid_format, term()}
      | {unknown_type, type()}
      | {unknown_constraint, jsv:type(), jsv:constraint()}
      | {invalid_constraint, jsv:type(),
         jsv:constraint(), term()}
      | invalid_empty_definition_list
      | catalog_definition_error_reason().

-type value_error() ::
        #{reason := value_error_reason(),
          reason_string => binary(),
          value := json:value() | term(),
          pointer := json_pointer:pointer(),
          pointer_string => binary()}.

-type value_error_reason() ::
        invalid_type
      | {invalid_type, ExpectedType :: jsv:type()}
      | {invalid_value, term(), unicode:chardata()}
      | {constraint_violation, jsv:type(), constraint()}
      | {constraint_violation, jsv:type(), constraint(),
         constraint_violation_details()}.

-type catalog_definition_error_reason() ::
        {unknown_catalog, catalog_name()}
      | {unknown_definition, catalog_name(), definition_name()}
      | no_current_catalog.

-type constraint_violation_details() :: undefined | term().

-type generation_error_reason() ::
        {invalid_value, term()}
      | {invalid_value, term(), jsv:type()}
      | {missing_members, [atom()], term()}
      | {invalid_members, [atom()], term()}.

-spec validate(json:value(), definition()) ->
        {ok, term()} | {error, [value_error()]}.
validate(Value, Definition) ->
  validate(Value, Definition, #{}).

-spec validate(json:value(), definition(), options()) ->
        {ok, term()} | {error, [value_error()]}.
validate(Value, Definition, Options) ->
  case maps:get(disable_verification, Options, false) of
    true ->
      ok;
    false ->
      case verify_definition(Definition, Options) of
        ok ->
          ok;
        {error, DefinitionErrors} ->
          error({invalid_definition, DefinitionErrors})
      end
  end,
  State = jsv_validator:init(Value, Definition, Options),
  case jsv_validator:validate(State) of
    {ok, CanonicalValue} ->
      {ok, CanonicalValue};
    {error, Errors} ->
      Errors2 = generate_value_error_reason_string(Errors, Options),
      {error, Errors2}
  end.

-spec generate(term(), definition()) ->
        {ok, json:value()} | {error, generation_error_reason()}.
generate(Term, Definition) ->
  generate(Term, Definition, #{}).

-spec generate(term(), definition(), options()) ->
        {ok, json:value()} | {error, generation_error_reason()}.
generate(Term, Definition, Options) ->
  case maps:get(disable_verification, Options, false) of
    true ->
      ok;
    false ->
      case verify_definition(Definition, Options) of
        ok ->
          ok;
        {error, DefinitionErrors} ->
          error({invalid_definition, DefinitionErrors})
      end
  end,
  State = jsv_generator:init(Term, Definition, Options),
  jsv_generator:generate(State).

-spec verify_catalog(catalog_name()) -> ok | {error, Reason} when
    Reason :: [definition_error_reason()]
            | {unknown_catalog, catalog_name()}.
verify_catalog(CatalogName) ->
  verify_catalog(CatalogName, #{}).

-spec verify_catalog(catalog_name(), options()) -> ok | {error, Reason} when
    Reason :: [definition_error_reason()]
            | {unknown_catalog, catalog_name()}.
verify_catalog(CatalogName, Options0) ->
  %% We need to set the current catalog since the definitions of the catalog
  %% may include references to other definitions in the same catalog, and will
  %% therefore use {ref, DefinitionName}.
  Options = Options0#{catalog => CatalogName},
  TableName = catalog_table_name(CatalogName),
  case ets:whereis(TableName) of
    undefined ->
      {error, {unknown_catalog, CatalogName}};
    Table ->
      ets:safe_fixtable(Table, true),
      try
        State = jsv_verifier:init(Options),
        verify_catalog_definition(Table, ets:first(Table), State)
      after
        ets:safe_fixtable(Table, false)
      end
  end.

-spec verify_catalog_definition(ets:tid(), Key :: term(),
                                jsv_verifier:state()) ->
        ok | {error, [definition_error_reason()]}.
verify_catalog_definition(_Table, '$end_of_table', _State) ->
  ok;
verify_catalog_definition(Table, Name, State) ->
  Definition = ets:lookup_element(Table, Name, 2),
  case jsv_verifier:verify(Definition, State) of
    {ok, State2} ->
      verify_catalog_definition(Table, ets:next(Table, Name), State2);
    {error, Errors} ->
      {error, {invalid_definition, Errors, Name}}
  end.

-spec verify_definition(definition()) ->
        ok | {error, [definition_error_reason()]}.
verify_definition(Definition) ->
  verify_definition(Definition, #{}).

-spec verify_definition(definition(), options()) ->
        ok | {error, [definition_error_reason()]}.
verify_definition(Definition, Options) ->
  State = jsv_verifier:init(Options),
  case jsv_verifier:verify(Definition, State) of
    {ok, _} ->
      ok;
    {error, Errors} ->
      {error, Errors}
  end.

-spec generate_value_error_reason_string([value_error()], options())
                                        -> [value_error()];
                                        (value_error(), options())
                                        -> value_error().
generate_value_error_reason_string(Errors, Options) when is_list(Errors) ->
  [generate_value_error_reason_string(E, Options) || E <- Errors];
generate_value_error_reason_string(Error = #{reason := Reason,
                                             pointer := Pointer},
                                   Options) ->
  TypeMap = type_map(Options),
  Msg = case Reason of
          invalid_type ->
            <<"value does not match any expected type">>;
          {invalid_type, ExpectedType} ->
            io_lib:format(<<"value is not of type ~0tp">>, [ExpectedType]);
          {invalid_value, _Reason, ReasonString} ->
            io_lib:format(<<"invalid value: ~ts">>, [ReasonString]);
          {constraint_violation, Type, Constraint} ->
            Module = maps:get(Type, TypeMap),
            case Module:format_constraint_violation(Constraint, undefined) of
              {Format, Args} ->
                iolist_to_binary(io_lib:format(Format, Args));
              Data ->
                unicode:characters_to_binary(Data)
            end;
          {constraint_violation, Type, Constraint, Details} ->
            Module = maps:get(Type, TypeMap),
            case Module:format_constraint_violation(Constraint, Details) of
              {Format, Args} ->
                iolist_to_binary(io_lib:format(Format, Args));
              Data ->
                unicode:characters_to_binary(Data)
            end;
          _ ->
            io_lib:format(<<"invalid value: ~0tp">>, [Reason])
        end,
  Error#{reason_string => iolist_to_binary(Msg),
         pointer_string => json_pointer:serialize(Pointer)}.

-spec default_type_map() -> type_map().
default_type_map() ->
  #{any => jsv_type_any,
    null => jsv_type_null,
    boolean => jsv_type_boolean,
    integer => jsv_type_integer,
    number => jsv_type_number,
    string => jsv_type_string,
    array => jsv_type_array,
    object => jsv_type_object,
    uuid => jsv_type_uuid,
    ksuid => jsv_type_ksuid,
    uri => jsv_type_uri,
    email_address => jsv_type_email_address,
    time => jsv_type_time,
    date => jsv_type_date,
    datetime => jsv_type_datetime}.

-spec type_map(options()) -> type_map().
type_map(Options) ->
  maps:get(type_map, Options, default_type_map()).

-spec catalog_table_name(catalog_name()) -> catalog_table_name().
catalog_table_name(Name) ->
  jsv_catalog_registry:table_name(Name).

-spec register_catalog(catalog_name(), catalog()) -> catalog_table_name().
register_catalog(Name, Catalog) ->
  jsv_catalog_registry:register_catalog(Name, Catalog).

-spec unregister_catalog(catalog_name()) -> ok.
unregister_catalog(Name) ->
  jsv_catalog_registry:unregister_catalog(Name).

-spec find_catalog_definition(catalog_name(), definition_name()) ->
        {ok, definition()} | {error, catalog_definition_error_reason()}.
find_catalog_definition(CatalogName, DefinitionName) ->
  TableName = catalog_table_name(CatalogName),
  case ets:whereis(TableName) of
    undefined ->
      {error, {unknown_catalog, CatalogName}};
    TableRef ->
      case ets:lookup(TableRef, DefinitionName) of
        [{_, Definition}] ->
          {ok, Definition};
        [] ->
          {error, {unknown_definition, CatalogName, DefinitionName}}
      end
  end.
