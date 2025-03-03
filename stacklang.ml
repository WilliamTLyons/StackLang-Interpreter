(*
 * StackLang: A stack-based programming language interpreter in OCaml
 * Implements a parser and evaluator for a language with arithmetic, boolean operations,
 * variables, and control flow constructs.
 * Author: William Lyons
 * Note: Starter code for utility functions and parser combinators provided by 
 * Professor Abbas Attarwala. The command language, evaluator, and additional parsing 
 * logic were implemented by William Lyons.
 *)
 
(*  Parsing Utility Functions 
 * Basic character classification and string manipulation functions 
 * used by the parser combinators  *)
let is_lower_case c = 'a' <= c && c <= 'z'
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_alpha c = is_lower_case c || is_upper_case c
let is_digit c = '0' <= c && c <= '9'
let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c
let is_blank c = String.contains " \012\n\r\t" c
let explode s = List.of_seq (String.to_seq s)
let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res
(* end of util functions *)

(* parser combinators *)
type 'a parser = char list -> ('a * char list) option
let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)
let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)
let fail : 'a parser = fun ls -> None
let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None
let ( >>= ) = bind
let ( let* ) = bind

let read : char parser =
  fun ls ->
    match ls with
    | x :: ls -> Some (x, ls)
    | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
    match ls with
    | x :: ls ->
        if f x then
          Some (x, ls)
        else
          None
    | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
    match p1 ls with
    | Some (_, ls) -> p2 ls
    | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
    match p1 ls with
    | Some (x, ls) -> (
        match p2 ls with
        |Some (_, ls) -> Some (x, ls)
        | None -> None)
    | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
    match p1 ls with
    | Some (x, ls) -> Some (x, ls)
    | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> Some (f a, ls)
    | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
    match p ls with
    | Some (x, ls) -> (
        match many p ls with
        | Some (xs, ls) -> Some (x :: xs, ls)
        | None -> Some ([ x ], ls))
    | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
    match p ls with
    | Some (x, ls) -> (
        match many p ls with
        | Some (xs, ls) -> Some (x :: xs, ls)
        | None -> Some ([ x ], ls))
    | None -> None 

let whitespace : unit parser =
  fun ls ->
    match ls with
    | c :: ls ->
        if String.contains " \012\n\r\t" c then
          Some ((), ls)
        else
          None
    | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
    match many1 digit ls with
    | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
    | _ -> None


let literal (s : string) : unit parser =
  fun ls ->
    let cs = explode s in
    let rec loop cs ls =
      match (cs, ls) with
      | [], _ -> Some ((), ls)
      | c :: cs, x :: xs ->
          if x = c then
            loop cs xs
          else
            None
      | _ -> None
    in
    loop cs ls
let keyword (s : string) : unit parser = literal s >> ws >| ()

(* end of parser combinators *) 

type const = Int of int | Bool of bool | Unit | Name of string

type command = Push of const | Pop of int | Trace of int | Add of int | Sub of int | Mul of int | Div of int | And | Or | Not | Equal | Lte | Local | Global | Lookup | BeginEnd of (command list) | IfElse of (command list * command list)

type program = command list

let const_of_string (s : string) : const option =
  match s with
    "()" -> Some Unit
  |
    "True" -> Some (Bool true)
  |
    "False" -> Some (Bool false)
  |
    _ ->(match int_of_string_opt s with
        Some i -> Some (Int i)
      |
        None -> Some (Name s))

let int_parser : int parser =
  many1 (satisfy is_digit) >|= implode >|= int_of_string

let bool_parser : const parser =
  (char 'T' >> char 'r' >> char 'u' >> char 'e' >> pure (Bool true))
  <|> (char 'F' >> char 'a' >> char 'l' >> char 's' >> char 'e' >> pure (Bool false))

let unit_parser : const parser =
  char '(' >> char ')' >> pure Unit


let name_parser : string parser =
  many1 (satisfy (fun c -> is_alphanum c || c = '_' || c = '\'')) >|= implode

let const_parser : const parser =
  bool_parser
  <|> unit_parser
  <|> (int_parser >|= (fun i -> Int i))
  <|> (name_parser >|= (fun i -> Name i))


let return x = fun input -> Some (x, input)

let rec pushParser ()= 
  literal "Push" >>= fun _ ->
  whitespace >>= fun _ ->
  const_parser >>= fun c ->
  whitespace >>= fun _ ->
  let result = (Push c) in
  return result

and popParser () = 
  literal "Pop" >>= fun _->
  whitespace >>= fun _->
  int_parser >>= fun i->
  whitespace >>= fun _->
  let result = (Pop i) in
  return result

and addParser () = 
  literal "Add" >>= fun _->
  whitespace >>= fun _->
  int_parser >>= fun i->
  whitespace >>= fun _->
  let result = (Add i) in
  return result

and subParser () = 
  literal "Sub" >>= fun _->
  whitespace >>= fun _->
  int_parser >>= fun i->
  whitespace >>= fun _->
  let result = (Sub i) in
  return result

and traceParser ()= 
  literal "Trace" >>= fun _->
  whitespace >>= fun _->
  int_parser >>= fun i->
  whitespace >>= fun _->
  let result = (Trace i) in
  return result

and mulParser () = 
  literal "Mul" >>= fun _->
  whitespace >>= fun _->
  int_parser >>= fun i->
  whitespace >>= fun _->
  let result = (Mul i) in
  return result

and divParser () = 
  literal "Div" >>= fun _->
  whitespace >>= fun _->
  int_parser >>= fun i->
  whitespace >>= fun _->
  let result = (Div i) in
  return result

and andParser () = 
  literal "And" >>= fun _ ->
  whitespace >>= fun _->
  let result = And in
  return result

and orParser () = 
  literal "Or" >>= fun _ ->
  whitespace >>= fun _->
  let result = Or in
  return result

and notParser () = 
  literal "Not" >>= fun _ ->
  whitespace >>= fun _->
  let result = Not in
  return result 

and equalParser () = 
  literal "Equal" >>= fun _ ->
  whitespace >>= fun _->
  let result = Equal in
  return result 

and lteParser () = 
  literal "Lte" >>= fun _ ->
  whitespace >>= fun _->
  let result = Lte in
  return result

and localParser () = 
  literal "Local" >>= fun _ ->
  whitespace >>= fun _->
  let result = Local in
  return result

and globalParser () = 
  literal "Global" >>= fun _ ->
  whitespace >>= fun _->
  let result = Global in
  return result

and lookupParser () = 
  literal "Lookup" >>= fun _ ->
  whitespace >>= fun _->
  let result = Lookup in
  return result

and beginParser () = 
  literal "Begin" >>= fun _ ->
  whitespace >>= fun _->
  many (command_parser()) >>= fun ls->
  literal "End" >>= fun _ ->
  whitespace >>= fun _->
  return (BeginEnd ls) 

and ifParser () = 
  literal "If" >>= fun _ ->
  whitespace >>= fun _->
  many (command_parser()) >>= fun ls1->
  literal "Else" >>= fun _->
  whitespace >>= fun _->
  many (command_parser()) >>= fun ls2->
  literal "End" >>= fun _->
  whitespace >>= fun _->
  return (IfElse (ls1, ls2))

and command_parser () : command parser =
  pushParser()  <|> popParser() <|> addParser() <|> subParser() <|> traceParser() <|> mulParser() <|> divParser() <|> andParser() <|> orParser() <|> notParser() <|> equalParser() <|> lteParser() <|> localParser() <|> globalParser() <|> lookupParser() <|> beginParser() <|> ifParser()


let run_parser (p: 'a parser) (input: string) : 'a option =
  match parse p input with
    Some (result, _) -> Some result
  | 
    None -> None


let parse_string (s : string) (ls : program) : program =
  match run_parser (many (command_parser())) (s ^ "\n") with
    Some result -> List.append result ls
  | 
    None -> ls

let eval_const (c : const) : int =
  match c with
    Int n -> n
  | 
    Bool true -> 1
  | 
    Bool false -> 0
  | 
    Unit -> 0
  |
    Name n -> 0

let rec take n l =
  let rlist = List.rev l in
  let rev_result = take_front n (rlist) in
  List.rev rev_result

and take_front n l =
  match n, l with
    0, _ -> []
  |
    _, [] -> []
  |
    n, x::xs -> x :: take_front (n-1) xs

let drop l n =
  if n = 0 then l else 
    let dlist= List.rev l in
    let rec ddrop l n =
      if n = 0 then List.rev l else 
        match l with
          [] -> []
        | 
          x::xs -> (ddrop xs (n-1))
    in ddrop dlist n

let get_front_element (l : const list) : const=
  let gfelist = List.rev l in
  match gfelist with
    [] -> Unit (*Should never occur*)
  |
    h::t -> h

let drop_front (l :const list) : const list=
  match l with
    [] -> []
  |
    h::t -> t

let rec string_of_value = function
    Int n -> string_of_int n
  |
    Bool b -> if b then "True" else "False"
  |
    Unit -> "()"
  |
    Name n -> n

(*Evaluator Function*)
let rec eval_list (cmds : command list) (stack : const list) (ls : string list) (local : (const * const) list) (global : (const * const) list)  =
  match cmds with
    [] -> (stack, ls)
  | 
    cmd :: rest ->
      match cmd with
        Push const -> (eval_list rest (stack @ [const]) ls local global)
      | 
        Pop n ->
          (if n < 0 || n > List.length stack then failwith "Error"
           else if n = 0 then eval_list rest stack ls local global
           else eval_list rest (drop stack n) ls local global)
      | 
        Trace n ->
          (if n < 0 || n > List.length stack then failwith "Error"
           else if n = 0 then eval_list rest (stack) ls local global
           else
             let values = take n stack in
             let log_strings = List.map string_of_value values in
             eval_list rest (drop stack n) (log_strings @ ls) local global)
      |
        Add n ->
          (if n < 0 || n > List.length stack then failwith "Error"
           else if n = 0 then eval_list rest (stack @ [Int 0]) ls local global
           else
             let values = take n stack in
             let all_ints =
               List.for_all (function Int _ -> true | _ -> false) values
             in
             if not all_ints then failwith "Error"
             else
               let result = List.fold_left (+) 0 (List.map eval_const values) in
               eval_list rest ((drop stack n) @ [Int result]) ls local global)
      | 
        Sub n ->
          (if n < 0 || n > List.length stack then failwith "Error"
           else if n = 0 then eval_list rest (stack @ [Int 0]) ls local global
           else
             let values = take n stack in
             let all_ints =
               List.for_all (function Int _ -> true | _ ->  false) values
             in
             if not all_ints then failwith "Error"
             else
               let top_value = get_front_element values in
               let sum_of_rest =
                 List.fold_left (+) 0
                   (List.map (function Int x -> x | _ -> 0) (drop values 1))
               in
               let result =
                 match top_value with
                   Int x -> Int (x - sum_of_rest)
                 |
                   _ -> failwith "Error"
               in
               eval_list rest ((drop stack n) @ [result]) ls local global)
      | 
        Mul n ->
          (if n < 0 || n > List.length stack then failwith "Error"
           else if n = 0 then eval_list rest (stack @ [Int 1]) ls local global
           else
             let values = take n stack in
             let all_ints =
               List.for_all (function Int _ -> true | _ -> false) values
             in
             if not all_ints then failwith "Error"
             else
               let result =
                 List.fold_left ( * ) 1
                   (List.map (function Int x -> x | _ -> failwith "Error") values)
               in
               eval_list rest ((drop stack n) @[Int result]) ls local global)
      | 
        Div n ->
          (if n < 0 || n > List.length stack then failwith "Error"
           else if n = 0 then eval_list rest (stack @ [Int 1]) ls local global
           else
             let values = take n stack in
             let all_ints = List.for_all (function Int _ -> true | _ -> false) (drop_front values) in
             let prod_of_rest = List.fold_left ( * ) 1 
                 (List.map (function Int x -> x | _ -> failwith "Error") (drop values 1)) in
             if not all_ints || prod_of_rest = 0 then failwith "Error"
             else
               let top_value = get_front_element values in
               let result = (match top_value with 
                     Int x -> x 
                   | 
                     _ -> failwith "Error") / prod_of_rest in
               eval_list rest((drop stack n) @ [Int result]) ls local global)
      | 
        And ->
          (match List.rev stack with
             Bool b1 :: Bool b2 :: other -> if b1 && b2 then eval_list rest ((List.rev other)@ [Bool true]) ls local global else eval_list rest ((List.rev other)@ [Bool false]) ls local global
           |
             _ -> failwith "Error")
      | 
        Or ->
          (match List.rev stack with
             Bool b1 :: Bool b2 :: other -> if b1 || b2 then eval_list rest ((List.rev other)@ [Bool true]) ls local global else eval_list rest ((List.rev other)@ [Bool false]) ls local global
           |
             _ -> failwith "Error")
      | 
        Not ->
          (match List.rev stack with
             Bool b1 :: other -> eval_list rest ((List.rev other)@ [Bool (not b1)]) ls local global
           |
             _ -> failwith "Error")
      |
        Equal ->
          (match List.rev stack with
             Int i1 :: Int i2 :: other -> if (i1 = i2) then eval_list rest ((List.rev other)@ [Bool true]) ls local global else eval_list rest ((List.rev other)@ [Bool false]) ls local global
           |
             _ -> failwith "Error")
      | 
        Lte ->
          (match List.rev stack with
             Int i1 :: Int i2  :: other -> if (i1 <= i2) then eval_list rest ((List.rev other)@ [Bool true]) ls local global else eval_list rest ((List .rev other)@ [Bool false]) ls local global
           |
             _ -> failwith "Error")
      | 
        Local ->
          (match List.rev stack with
             c :: Name n :: other -> eval_list rest ((List.rev other)@ [Unit]) ls ((Name n, c) :: local) global
           | _ -> failwith "Error")
      | 
        Global ->
          (match List.rev stack with
             c :: Name n :: other -> eval_list rest ((List.rev other)@ [Unit]) ls local ((Name n, c) :: global)
           | _ -> failwith "Error")
      | 
        Lookup ->
          if ((global = []) && (local = [])) then failwith "Error" else 
            (match List.rev stack with
               (Name name) :: other -> 
                 let rec lsearch (lt: (const * const) list) =
                   (match lt with
                      (Name name2, v2) :: lrest -> (if name = name2 then eval_list rest ((List.rev other)@ [v2]) ls local global else lsearch lrest)
                    |    
                      [] -> let rec gsearch (gt : ((const * const) list)) =
                              (match gt with
                                 [] -> failwith "Error"
                               |
                                 (Name name3, v3) :: grest -> if name = name3 then eval_list rest ((List.rev other)@ [v3]) ls local global else gsearch grest
                               |
                                 _ -> failwith "Error") in gsearch global
                    |
                      _ -> failwith "Error") in lsearch local
             |
               _ -> failwith "Error")
      | 
        BeginEnd l ->
          (match (try (eval_list l [] [] local global) with Failure s -> failwith "Error") with
               ([], nls) -> failwith "Error"
             | (ns, nls) -> (match List.rev ns with
                   (hs::ts) -> eval_list rest (stack @ [hs]) (ls @ nls) local global
                 | _ -> failwith "Error"))
      |
        IfElse l ->
          (match List.rev stack with
             [] -> failwith "Error"
           |
             Bool true :: srest -> (match l with
                 ([], ls2) -> eval_list rest (drop stack 1) ls local global
               |
                 (ls1, ls2) -> eval_list (ls1 @ rest) (drop stack 1) ls local global)
           | 
             Bool false :: srest -> (match l with
                 (ls1, []) -> eval_list rest (drop stack 1) ls local global
               |
                 (ls1, ls2) -> eval_list (ls2 @ rest) (drop stack 1) ls local global)
           |
             _ -> failwith "Error")

let interp (src : string) : string list =
  let parsed_cmds = parse_string src [] in
  let stack = [] in
  let log = try eval_list parsed_cmds stack [] [] [] with Failure s -> ([], [s]) in
  match log with
  | (_, log_strings) -> log_strings