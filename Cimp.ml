(* Copyright Per Lindgren 2018, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* local files *)
open Common
open Options
open Error
open Cmd
open Dump
open Env

(* extracted code (/extract) *)
module Compile = Compiler__Compile_com
module Vm_Ex = Vm_ex__Vm_Ex
module Imp_Ex = Imp_ex__Imp_Ex
module State = State__State

let () =

  cmd; (* parse command line options and put into opt *)
  p_stderr (string_of_opt opt);

  let inBuff =
    try Some (open_in opt.infile)
    with _ -> None
  in

  try
    match inBuff with
    | None -> raise (CompilerError("File open error :" ^ opt.infile))
    | Some inBuffer ->
      let lexbuf = Lexing.from_channel inBuffer in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = opt.infile };
      try
        let prog = Parser.prog Lexer.lex lexbuf in
        if opt.d_ast then
          p_stderr ("Raw AST : \n" ^ of_com prog ^ nl);
        if opt.d_past then
          p_stderr ("Pretty AST: \n" ^ pretty_of_com 0 prog ^ nl);

        let code = Compile.compile_program prog in
        if opt.d_code then
          p_stderr ("Raw Code : \n" ^ of_code false code ^ nl);
        if opt.d_pcode then
          p_stderr ("Pretty Code : \n" ^ of_code true code ^ nl);

        let st_0 = State.const (Z.of_int 0) in (* assume all variables 0 *)

        (* imp_ex execution *)
        if opt.imp_ex then (
          try
            p_stdout ("Execute : imp_ex" );
            let st_end = Imp_Ex.ceval_ex st_0 prog in
            p_stdout ("ceval_ex" ^ nl ^ Env.to_string st_end ^ nl);
          with
          | _ -> p_stdout "ceval : Exited with an error\n";
        );

        (* vm_ex execution *)
        if opt.vm_ex then (
          try
            let _ = Vm_Ex.instr_iter_ex code (VMS (Z.of_int 0, [], st_0)) in
            ()
          with
          | Vm_Ex.Err -> p_stderr ("execution error")
          | Vm_Ex.Halt (VMS (pos, stack, st_halt)) ->
            p_stdout ("execution halted");
            p_stdout ("instr_iter_ex" ^ nl ^ Env.to_string st_halt ^ nl);
            ()
        );

        p_stdout ("Done!");

      with
      | Lexer.SyntaxError msg -> raise (CompilerError ("Syntax error. " ^ msg ^ parse_err_msg lexbuf));
      | Parser.Error -> raise (CompilerError ("Parser error." ^ parse_err_msg lexbuf));
  with
  | CompilerError msg ->
          p_stderr msg;
          exit (-1);
  | Failure msg -> p_stderr ("Failure (internal error): " ^ msg);
    exit (-1);;

