open Ast

type value =
  | VBool of bool
  | VInt of int
  | VString of string
  | VUndefined
  | VFun of id list * expr * env

and result =
  | RValue of value
  | RExcn of value

and env = (id * value) list

and state = unit

let initial_env = []

let initial_state = ()

let string_of_value = function
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i
  | VString s -> "\"" ^ s ^ "\""
  | VUndefined -> "undefined"
  | VFun _ -> "<function>"

let string_of_result = function
  | RValue v -> string_of_value v
  | RExcn v -> "Exception: " ^ string_of_value v

let string_of_env env =
  raise (Failure "Unimplemented: Eval.string_of_env")

let string_of_state st =
  raise (Failure "Unimplemented: Eval.string_of_state")

let bool_conv = function
  | EBool false
  | EInt 0
  | EString ""
  | Undefined ->
      EBool false
  | EBool true
  | EInt _
  | EString _ ->
      EBool true
  | _ -> Undefined

let int_conv e =
  match e with
  | EInt _ -> e
  | EBool true -> EInt 1
  | EBool false -> EInt 0
  | Undefined -> Undefined
  | EString s -> begin
      try EInt (int_of_string s) with
      | Failure _ -> Undefined
      | _ -> EInt (int_of_string s)
    end
  | _ -> failwith "not a value"

(* let x = int_of_string s in EInt x *)
(*add if a failure*)

let string_conv e =
  match e with
  | EString _ -> e
  | EInt i -> EString (string_of_int i)
  | EBool e -> EString (string_of_bool e)
  | _ -> Undefined

let eval_val st e =
  match e with
  | EBool b -> (RValue (VBool b), st)
  | EInt i -> (RValue (VInt i), st)
  | EString s -> (RValue (VString s), st)
  | Undefined -> (RValue VUndefined, st)
  | _ -> failwith "not values"

let eval_val_conv e =
  match e with
  | EBool b -> RValue (VBool b)
  | EInt i -> RValue (VInt i)
  | EString s -> RValue (VString s)
  | Undefined -> RValue VUndefined
  | _ -> failwith "not values"

let eval_val2_conv e env =
  match e with
  | EBool b -> VBool b
  | EInt i -> VInt i
  | EString s -> VString s
  | Undefined -> VUndefined
  | Fun (xs, e) -> VFun (xs, e, env)
  | _ -> failwith "not values"

let eval_expr_conv e =
  match e with
  | VBool b -> EBool b
  | VInt i -> EInt i
  | VString s -> EString s
  | VUndefined -> Undefined
  | _ -> failwith "not values"

let add_h a b =
  match (a, b) with
  | EInt x, EString y -> EString (string_of_int x ^ y)
  | EString y, EInt x -> EString (y ^ string_of_int x)
  | EString x, EString y -> EString (x ^ y)
  | _, Undefined -> Undefined
  | Undefined, _ -> Undefined
  | x, y -> (
      let v1 = int_conv x in
      let v2 = int_conv y in
      match (v1, v2) with
      | EInt v1', EInt v2' -> EInt (v1' + v2')
      | _, _ -> failwith "not a string or an int")

let comp_h op a b =
  match op with
  | BopLt -> EBool (a < b)
  | BopLeq -> EBool (a <= b)
  | BopGt -> EBool (a > b)
  | BopGeq -> EBool (a >= b)
  | _ -> failwith "not binary operator"

let comp_rules op a b =
  match (a, b) with
  | EString a, EString b -> comp_h op a b
  | x, y -> (
      match (int_conv x, int_conv y) with
      | EInt v1, EInt v2 -> comp_h op v1 v2
      | Undefined, _
      | _, Undefined ->
          EBool false
      | _, _ -> failwith "not good")

let rec eq_rules a b =
  match (a, b) with
  | Undefined, Undefined -> EBool true
  | EBool x, EBool y -> EBool (x = y)
  | EInt x, EInt y -> EBool (x = y)
  | EString x, EString y -> EBool (x = y)
  | EInt x, EString y -> eq_rules (EInt x) (int_conv (EString y))
  | EString x, EInt y -> eq_rules (int_conv (EString x)) (EInt y)
  | EBool x, y -> eq_rules (int_conv (EBool x)) (int_conv y)
  | x, EBool y -> eq_rules (int_conv x) (int_conv (EBool y))
  | _, _ -> EBool false

let eq_exat_rules a b =
  match (a, b) with
  | Undefined, Undefined -> EBool true
  | EInt x, EInt y -> EBool (x == y)
  | EString x, EString y -> EBool (x == y)
  | EBool x, EBool y -> EBool (x == y)
  | _, _ -> EBool false

let add_env env (id : id) v = (id, eval_val2_conv v env) :: env

let rec find_env (env : (id * value) list) id isvar funct =
  match env with
  | [] -> failwith "Exception: \"Error: Unbound variable\""
  | (k, v) :: t ->
      if isvar then
        if k = id then eval_expr_conv v
        else find_env t id true (Fun ([], EInt 1))
      else if k = " " && v = eval_val2_conv funct env then
        eval_expr_conv v
      else find_env t id false funct

let extract_val v =
  match v with
  | RValue v', _ -> v'
  | _ -> failwith "not implemented result"

let rec eval_expr (e, env, st) =
  match e with
  | EBool b -> (RValue (VBool b), st)
  | EInt i -> (RValue (VInt i), st)
  | EString s -> (RValue (VString s), st)
  | Undefined -> (RValue VUndefined, st)
  | Fun (xs, e) -> (RValue (VFun (xs, e, env)), st)
  | Uop (u, e) -> eval_uop u env e [] [] |> eval_val st
  | Bop (b, e1, e2) -> eval_val st (eval_bop b e1 e2 env [] [])
  | And (e1, e2) -> eval_val st (eval_and e1 e2 env [] [])
  | Or (e1, e2) -> eval_or e1 e2 env |> eval_val st
  | If (e1, e2, e3) -> eval_val st (eval_if e1 e2 e3 env [] [])
  | Var e1 -> find_env env e1 true (Fun ([], EInt 1)) |> eval_val st
  | Let (id, e1, e2) -> eval_val st (eval_let id e1 e2 env [] [])
  | App (f, es) ->
      eval_app (eval_expr (f, env, st)) es env [] [] |> eval_val st
  | Letf (f, xs, e1, e2) -> failwith "not implemented"

and eval_expr_h (e : expr) env lsti lste =
  match e with
  | EInt _
  | EBool _
  | EString _
  | Undefined ->
      e
  | Var x -> find x env lsti lste
  | Uop (u, e) -> eval_uop u env e lsti lste
  | Bop (b, e1, e2) -> eval_bop b e1 e2 env lsti lste
  | And (e1, e2) -> eval_and e1 e2 env lsti lste
  | If (e1, e2, e3) -> eval_if e1 e2 e3 env lsti lste
  | Let (x, e1, e2) -> eval_let x e1 e2 env lsti lste
  | Fun (f, x) -> Fun (f, x)
  | _ -> Undefined
(*for objects and everything else*)

and eval_uop uop env e lst1 lst2 =
  match (uop, eval_expr_h e env lst1 lst2) with
  | UopMinus, EInt a -> EInt (-a)
  | UopNot, e when bool_conv e = EBool true -> EBool false
  | UopNot, e when bool_conv e = EBool false -> EBool true
  | UopNot, _ -> failwith "error"
  | UopTypeof, e -> typeof_h e env lst1 lst2
  | _, Undefined -> Undefined
  | _, _ -> Undefined

and typeof_h e env lsti lste =
  match eval_expr_h e env lsti lste with
  | Undefined -> EString "undefined"
  | EBool _ -> EString "bool"
  | EInt _ -> EString "int"
  | EString _ -> EString "string"
  | Fun _ -> EString "function"
  | _ -> failwith "not a type"

and eval_bop bop e1 e2 env lsti lste =
  match
    (bop, eval_expr_h e1 env lsti lste, eval_expr_h e2 env lsti lste)
  with
  | BopPlus, a, b -> add_h a b
  | BopMinus, e1, e2 -> bop_h BopMinus e1 e2 env
  | BopTimes, e1, e2 -> bop_h BopTimes e1 e2 env
  | BopDiv, e1, e2 -> bop_h BopDiv e1 e2 env
  | BopMod, e1, e2 -> bop_h BopMod e1 e2 env
  | BopLt, e1, e2 -> comp_rules BopLt e1 e2
  | BopLeq, e1, e2 -> comp_rules BopLeq e1 e2
  | BopGt, e1, e2 -> comp_rules BopGt e1 e2
  | BopGeq, e1, e2 -> comp_rules BopGeq e1 e2
  | BopEq, e1, e2 -> eq_rules e1 e2
  | BopNeq, e1, e2 -> eval_uop UopNot env (eq_rules e1 e2) [] []
  | BopEqStrict, e1, e2 -> eq_exat_rules e1 e2
  | BopNeqStrict, e1, e2 ->
      eval_uop UopNot env (eq_exat_rules e1 e2) [] []

and bop_h op a b env =
  let v1 = eval_expr_h a env [] [] in
  let v2 = eval_expr_h b env [] [] in
  if v1 = Undefined || v2 = Undefined then Undefined
  else
    match (op, v1, v2) with
    | BopMinus, EInt a, EInt b -> EInt (a - b)
    | BopTimes, EInt a, EInt b -> EInt (a * b)
    (*add mod and divide by 0*)
    | BopDiv, EInt a, EInt b ->
        if b = 0 then failwith "Exception: \"Error: Division by zero\""
        else EInt (a / b)
    | BopMod, EInt a, EInt b ->
        if b = 0 then failwith "Exception: \"Error: Division by zero\""
        else EInt (a mod b)
    | _, Undefined, _ -> Undefined
    | _, _, Undefined -> Undefined
    | _, _, _ -> Undefined

and eval_and e1 e2 env lsti lste =
  let v1 = bool_conv (eval_expr_h e1 env lsti lste) in
  if v1 = EBool false then EBool false
  else bool_conv (eval_expr_h e2 env lsti lste)

and eval_or e1 e2 env =
  let v1 = bool_conv (eval_expr_h e1 env [] []) in
  if v1 = EBool true then EBool true
  else bool_conv (eval_expr_h e2 env [] [])

and eval_if e1 e2 e3 env lsti lste =
  let b = bool_conv (eval_expr_h e1 env lsti lste) in
  if b = EBool true then e2 else e3

and eval_let id e1 e2 env lsti lste =
  let v1 = eval_expr_h e1 env lsti lste in
  let new_env = add_env env id v1 in
  eval_expr_h e2 new_env lsti lste

(* add undefined operator*)

and eval_app f es env lsti lste =
  match f with
  | RValue (VFun (xs, e, env)), st ->
      let lst1 = List.length es in
      let lst2 = List.length xs in
      if lst1 != lst2 then
        failwith "Exception: \"Error: Wrong number of arguments\""
      else
        let vlst = List.map (fun x -> eval_expr_h x env lsti lste) es in
        eval_expr_h e env xs vlst
  | _ -> failwith "Exception: \"Error: Not a function\""

and find id env lst1 lst2 =
  match (lst1, lst2) with
  | [], [] ->
      find_env env id true (Fun ([], EInt 1)) (*maybe more logic here*)
  | h :: t, h2 :: t2 -> if id = h then h2 else find id env t t2
  | _, _ -> failwith "wrong"

let rec eval_defn (d, env, st) =
  match d with
  | Letd (x, e) -> eval_letd x e env st

and eval_letd id e env st =
  let v = eval_expr_h e env [] [] in
  let new_env = add_env env id v in
  (v |> eval_val_conv, new_env, st)

let eval_phrase (p, env, st) =
  match p with
  | Expr c ->
      let r, st' = eval_expr (c, env, st) in
      (r, env, st')
  | Defn d -> eval_defn (d, env, st)

let eval_expr_init e = eval_expr (e, initial_env, initial_state)
