(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

(* characters and strings *)
let tab 	= "\t"
let tabc 	= '\t'

let nl 		= "\n"
let nlc 	= '\n'

let enl		= "\\n"

let op 		= " {" ^ nl

let cl 		= "}"

let ec    = "\""

let ecit  = "\'"


(* Error handling *)
exception TypeError of string
exception CompilerError of string
exception UnMatched

let rec mymap m = function
  | []     -> []
  | e :: l -> try (m e) :: mymap m l with UnMatched -> mymap m l

let rec myconcat s = function
  | []     -> ""
  | "":: l -> myconcat s l
  | e :: l -> e ^ s ^ myconcat s l

let rec mycon s = function
  | []      -> ""
  | e :: [] -> e
  | "" :: l -> mycon s l
  | e :: l  -> e ^ s ^ mycon s l

let myass k m = try List.assoc k m with _ -> raise (CompilerError("lookup of " ^ k ^ " failed!"))

let mycompare s1 s2 = (String.compare s1 s2 == 0)

(* Helper functions *)
let p_stdout x = Printf.fprintf stdout "%s\n%!" x
let p_stderr x = Printf.fprintf stderr "%s\n%!" x
let p_oc oc x = Printf.fprintf oc "%s\n%!" x

let rec range i j = if i > j then [] else i :: (range (i+1) j)

let size = 1024 (* Static sized parsing buffer in Parsing.ml (Standard lib) *)

let submatch s i m =
  let rec subm s i m r =
    try
      match String.get s (i mod size) with
      | c when c == m 	  -> r
      | c when c == tabc 	-> subm s (i + 1) m (r ^ " ")
      | c 				        -> subm s (i + 1) m (r ^ String.make 1 c)
    with
      _ -> r (* outside string, should never happen *)
  in
  subm s i m ""

