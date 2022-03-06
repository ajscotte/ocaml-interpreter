val greeting : string ref
(** The greeting to print at the beginning of a session. *)

type repl_state
(** The internal state of the REPL. *)

type handler =
  | NoArg of (repl_state -> repl_state * string)
  | StringArg of (string * repl_state -> repl_state * string)
      (** A function that handles a directive. It returns the new REPL
          state along with a string output. *)

val handlers : (string * handler) list ref
(** An association list of directive handlers. The keys are the
    directive names. The value bound to a key is the function to be
    called when that directive is issued.*)

val repl : unit -> unit
(** Run the REPL. *)