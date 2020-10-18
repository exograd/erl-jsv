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

-module(jsv_type_array).

-behaviour(jsv_type).

-export([name/0, verify_constraint/2, validate_type/1, validate_constraint/3]).

-export_type([constraint/0]).

-type constraint() :: {element_type, jsv:definition()}
                    | {min_length, number()}
                    | {max_length, number()}.

-spec name() -> jsv:type().
name() ->
  array.

verify_constraint({element_type, Definition}, TypeMap) ->
  jsv:verify_definition(Definition, TypeMap);
verify_constraint({min_length, Value}, _) when is_number(Value) ->
  ok;
verify_constraint({min_length, _}, _) ->
  invalid;
verify_constraint({max_length, Value}, _) when is_number(Value) ->
  ok;
verify_constraint({max_length, _}, _) ->
  invalid;
verify_constraint(_, _) ->
  unknown.

validate_type(Value) when is_list(Value) ->
  ok;
validate_type(_) ->
  error.

validate_constraint(Value, {element_type, ElementType}, State) ->
  F = fun (I, Element, State2) ->
          jsv_validator:validate_child(Element, ElementType,
                                       integer_to_binary(I), State2)
      end,
  jsv_utils:fold_list_with_index(F, State, Value);

validate_constraint(Value, Constraint = {min_length, Min}, State) when
    is_number(Min) ->
  case length(Value) >= Min of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Constraint, State)
  end;

validate_constraint(Value, Constraint = {max_length, Max}, State) when
    is_number(Max) ->
  case length(Value) =< Max of
    true ->
      State;
    false ->
      jsv_validator:add_constraint_violation(Constraint, State)
  end.
