(*
 * Graduate Programming Languages - Wes Weimer
 *
 * Test Input Generation Project - Instrumentation
 *
 * ___ Students don't want this file. Go to tigen.ml instead! ___
 *
 * Input: Command-line argument specifying a C file.
 * Output: That same C file instrumented with branch coverage, 
 *      printed to stdout. 
 *
 * http://en.wikipedia.org/wiki/Code_coverage
 *
 * That is, for every IF statement in the input file, we put in two
 * printfs() -- one that activates if you take the true branch, and one for
 * the false branch. 
 *)
open Utils
open Cil

(* 
 * This function actually adds "printf(...)" to a block. 
 *)
let counter = ref 0 
let prepend_printf_to_block b =
  let printf_funname = "printf" in
  let lval va = Lval((Var va), NoOffset) in
  let printf_exp = lval (makeVarinfo true printf_funname (TVoid [])) in
  let printf_arg_string = 
    Printf.sprintf "\n________ %d ________\n" !counter in 
  let printf_arg_exp = Const(CStr(printf_arg_string)) in 
  let printf_call = Call(None, printf_exp, [printf_arg_exp], !currentLoc) in 
  let printf_stmtkind = Instr[printf_call] in 
  let printf_stmt = mkStmt printf_stmtkind in
  let label = Label(Printf.sprintf "instrument_%d" !counter,
    !currentLoc,false) in 
  printf_stmt.labels <- label :: printf_stmt.labels ;
  incr counter ; 
  b.bstmts <- printf_stmt :: b.bstmts 

(* 
 * We'll use the Visitor Design Pattern to iterate over all of the
 * statements in the program. When we find an IF statement, we add some
 * printfs.  
 *
 * http://en.wikipedia.org/wiki/Visitor_pattern
 *)
class instrumentVisitor = object
  inherit nopCilVisitor
  method vstmt s = 
    ChangeDoChildrenPost(s,(fun s -> 
      match s.skind with
      | If(exp,thenblock,elseblock,location) -> 
        prepend_printf_to_block thenblock ;
        prepend_printf_to_block elseblock ;
        s 

      | _ -> s 
    )) 
end 

let main () = begin
  if Array.length Sys.argv < 2 then begin 
    debug "instrument: specify a pre-preprocessed C file\n" ;
    exit 1 
  end ; 
  let filename = Sys.argv.(1) in 
  let file = load_c_file filename in 
  let iv = new instrumentVisitor in 
	visitCilFileSameGlobals (iv) file;
  iterGlobals file (dumpGlobal defaultCilPrinter stdout) ;
  Printf.printf "/* %s: \nMAX IS %d\n */\n" filename (!counter) 
end ;;
main () ;; 
