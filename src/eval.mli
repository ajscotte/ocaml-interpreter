(** Implements the big-step environment model semantics. *)

open Ast

type value
(** [value] is the type of JoCalf values *)

type result
(** [result] is the type of JoCalf results, which are either values or
    exceptions *)

type env
(** [env] is an environment, which maps identifiers to values *)

type state
(** [state] is a state, which maps locations to values *)

val initial_env : env
(** [initial_env] is the environment in which evaluation begins. It must
    contain all the external functions defined in the language manual. *)

val initial_state : state
(** [initial_state] is the state in which evaluation begins. It should
    not have any allocated locations. *)

val string_of_value : value -> string
(** [string_of_value v] is a string representing value [v].

    - If v is an int, that string should be [string_of_int v].
    - If v is a string, that string should be
      ["\"" ^ String.escaped v ^ "\""].
    - If v is a boolean, that string should be [string_of_bool b].
    - If v is undefined, that string should be ["undefined"].
    - If v is an object, that string should be ["<object>"].
    - If v is a location, that string should be ["<reference>"].
    - If v is a closure or extern, that string should be ["<function>"].

    In the final four cases above, note that the string
    [string_of_value] returns should not contain the surrounding square
    brackets or quotes; they are there just to delimit the string in
    this comment. *)

val string_of_result : result -> string
(** [string_of_result r] is a string representing result [r].

    - If r is a value [v], that string should be [string_of_value v].
    - If r is an exception carrying value [v], that string should be
      ["Exception: " ^ string_of_value v]. *)

val string_of_env : env -> string
(** [string_of_env env] is a string representing environment [env]. It
    is up to you how to construct that string; it will only be used in
    the REPL, not in any test cases. *)

val string_of_state : state -> string
(** [string_of_state st] is a string representing state [st]. It is up
    to you how to construct that string; it will only be used in the
    REPL, not in any test cases. *)

val eval_expr_init : expr -> result * state
(** [eval_expr_init e] is [(r,st)] if [e] in the initial environment and
    state, evaluates to [(r,st)], that is,
    [<e, initial_env, initial_state>
    ==> <r, st>]. *)

val eval_expr : expr * env * state -> result * state
(** [eval_expr (e, env, st)] is [(r, st')] if
    [<e, env, st> ==> <r, st'>]. *)

val eval_defn : defn * env * state -> result * env * state
(** [eval_defn (d, env, st)] is [(r, env', st')] if
    [<d, env, st> ==> <r,
    env', st'>]. *)

val eval_phrase : phrase * env * state -> result * env * state
(** [eval_phrase (p, env, st)] is [(r, env', st')] if
    [<p, env, st> ==>
    <r, env', st'>]. *)
