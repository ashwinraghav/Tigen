(*
 * Graduate Programming Languages - Wes Weimer
 *
 * Test Input Generation Project - Global Utilities
 *
 * All of the real action happens in "tigen.ml". 
 *)
open Cil

(* 
 * This "debugging output" function behaves just like printf(), but it also
 * flushes the output buffer so that you'll always see all of your output,
 * even if the program terminates abnormally. 
 *)
let debug fmt = 
  let k result = begin
    output_string stdout result ; 
    flush stdout ; 
  end in
  Printf.kprintf k fmt 

let abort fmt = 
  debug fmt ;
  exit 1 

(* 
 * Load, lex and parse and pre-processed C-language file (typically a .i
 * file obtained by "gcc -E" or "gcc -save-temps") into a Cil.file abstract
 * syntax tree data structure.
 *)
let load_c_file (filename : string) : Cil.file = 
  Frontc.parse filename () 

(*
 * Pretty-print a Cil expression as a string. Handy for debugging. 
 *)
let cil_exp_to_str (e : Cil.exp) : string = 
  Pretty.sprint ~width:80 (d_exp () e) 

(* 
 * returns the first N elements of the given list 
 *) 
let rec first_nth lst n =  
  if n < 1 then [] 
  else match lst with
  | [] -> []
  | hd :: tl -> hd :: (first_nth tl (pred n))

