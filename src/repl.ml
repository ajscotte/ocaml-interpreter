(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

exception Quit
(** [Quit] is raised to indicate the user wants to quit the REPL. *)

type repl_state = {
  env : Eval.env;
  st : Eval.state;
}
(** [repl_state] is the state of the user's REPL session. That includes
    the current environment [env], which is modified by [let]
    definitions, and the current JoCalf state [st], which is modified by
    [ref] and [:=]. *)

(** [initial_repl_state] is the initial state of the REPL.*)
let initial_repl_state =
  { env = Eval.initial_env; st = Eval.initial_state }

type handler =
  | NoArg of (repl_state -> repl_state * string)
  | StringArg of (string * repl_state -> repl_state * string)

(** Handle the [#use] directive. *)
let rec use_handler (file, state) =
  try
    let ch = open_in file in
    let rec process_file state outputs =
      try
        let line = input_line ch in
        let new_state, output = eval state line in
        process_file new_state (output :: outputs)
      with
      | End_of_file ->
          close_in ch;
          let final_output =
            outputs |> List.rev |> String.concat "\n"
          in
          (state, final_output)
    in
    process_file state []
  with
  | Sys_error errmsg -> (state, errmsg)

and handlers =
  ref
    [
      ("quit", NoArg (fun _ -> raise Quit));
      ("env", NoArg (fun state -> (state, Eval.string_of_env state.env)));
      ( "state",
        NoArg (fun state -> (state, Eval.string_of_state state.st)) );
      ("use", StringArg use_handler);
    ]

(** Matches a directive. *)
and directive_regex =
  Str.regexp {|^#\([a-z]+\)\( +"\(.*\)"\)?\( *;; *\)?$|}

(** Handle a directive.

    @param state The REPL state in which to handle it.
    @param s The directive to be handled. *)
and handle_directive state s =
  if Str.string_match directive_regex s 0 then
    let directive = Str.matched_group 1 s in
    match List.assoc_opt directive !handlers with
    | None -> (state, "Unknown directive")
    | Some (NoArg handler) -> handler state
    | Some (StringArg handler) ->
        let parameter = Str.matched_group 3 s in
        handler (parameter, state)
  else (state, "Illegal directive")

(** [eval] implements the "E" in "REPL", that is, the evaluation of a
    program expression.

    @param state The REPL state in which to begin evaluation.
    @param s
      The string to be evaluated. It could be a program phrase or a REPL
      directive. *)
and eval state s =
  if String.length s = 0 then (state, "")
  else if s.[0] = '#' then handle_directive state s
  else
    let out, env', st' =
      Interp.interp_phrase (s, state.env, state.st)
    in
    let state' = { env = env'; st = st' } in
    (state', out)

(** Print the REPL prompt. *)
let make_prompt () = print_string "# "

(** [make_output] implements the "P" in "REPL", that is, the printing of
    output [out]. *)
let make_output out =
  if out = "" then print_newline () else Printf.printf "%s\n\n" out

(** [safe_read] implements the "R" in "REPL", that is, the reading of
    input. *)
let safe_read () =
  try read_line () with
  | End_of_file -> raise Quit

(** [main] implements the "L" in "REPL", that is, the loop.

    @param state The current state of the REPL. *)
let rec main state =
  make_prompt ();
  let line = safe_read () in
  let state', out = eval state line in
  make_output out;
  main state'

(** The greeting string to print at the beginning of a session. *)
let greeting = ref "JoCalf"

(** Run the REPL. *)
let repl () =
  try
    print_endline !greeting;
    main initial_repl_state
  with
  | Quit -> ()
