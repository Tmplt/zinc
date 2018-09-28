(* Copyright Per Lindgren, Viktor Sonesten 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                                 *)

open Common
open Options

let usage =
  "The zinc v1.0 compiler, Per Lindgren, Viktor Sonesten (c) 2018" ^ nl ^ "Options summary:"

let o_verbose   = ref false
let o_debug     = ref false

let f_infile    = ref ""
let f_outfile   = ref ""

let d_ast       = ref false
let d_past      = ref false
let d_code      = ref false
let d_pcode     = ref false
let imp_ex      = ref false
let imp_exn     = ref false
let vm_ex       = ref false


let speclist =
  [
    ("-i",       Arg.Set_string f_infile,  "\t\t: infile (-i optional)");
    ("-o",       Arg.Set_string f_outfile, "\t\t: outfile (default infile.s)");

    ("-v",       Arg.Set o_verbose,        "\t\t: verbose mode (default disable)");
    ("-D",       Arg.Set o_debug,          "\t\t: debug mode (default disable)");

    ("-d_ast",   Arg.Set d_ast,            "\t: dump AST");
    ("-d_past",  Arg.Set d_past,           "\t: dump pretty AST");
    ("-d_code",  Arg.Set d_code,           "\t: dump code");
    ("-d_pcode", Arg.Set d_pcode,          "\t: dump pretty code");
    ("-imp_ex",  Arg.Set imp_ex,           "\t: imp_ex evaluation");
    ("-imp_exn", Arg.Set imp_exn,          "\t: imp_exn evaluation");
    ("-vm_ex",   Arg.Set vm_ex,            "\t: vm_ex virtual machine execution");
  ]

(* check if e is a file extension of s *)
let ext s e =
  let le = String.length e in
  let ls = String.length s in
  try
    compare (String.sub s (ls - le) le) e == 0
  with
    _ -> false

(* replace file extension e with r *)
let rep_ext s e r =
  let le = String.length e in
  let ls = String.length s in
  String.sub s 0 (ls - le) ^ r

let cmd =
  (* Read the arguments *)
  Arg.parse speclist (fun x -> f_infile := x) usage;
  try
    (* infile *)
    if (String.compare !f_infile "" == 0) then raise (Arg.Bad "No infile selected");
    if not (ext !f_infile ".imp") then
      raise (Arg.Bad "Bad infile extention (.imp/.mlw exptected)");

    opt.infile <- !f_infile;

    (* outfile *)
    opt.outfile <- if (String.compare (!f_outfile) "" == 0) then rep_ext (!f_infile) ".xxx" ".s" else !f_outfile;
    if (not (ext opt.outfile ".s")) then raise (Arg.Bad("Bad outfile extention (.s exptected) " ^ opt.outfile));
    p_stderr ("outfile :" ^ opt.outfile);

    (* general options *)
    opt.debug     <- ! o_debug;
    opt.verbose   <- ! o_verbose;
    opt.d_ast     <- ! d_ast;
    opt.d_past    <- ! d_past;
    opt.d_code    <- ! d_code;
    opt.d_pcode   <- ! d_pcode;
    opt.imp_ex    <- ! imp_ex;
    opt.imp_exn   <- ! imp_exn;
    opt.vm_ex     <- ! vm_ex;
  with
  | Arg.Bad msg -> p_stderr ("Command line error: " ^ msg); exit (-1);

