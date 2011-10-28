(*ASHWIN RAGHAV MOHAN GANESH---- am2qa*)
(*
 * Graduate Programming Languages - Wes Weimer
 *
 * Test Input Generation Project 
 *
 * Summary: Given a C file, produce a suite of test inputs for that file
 *        such that branch coverage is maximized. 
 *
 * Input: A filename for a pre-processed, instrumented C file. This file
 *        is typically obtained via the "make test" Makefile target or
 *        the "instrument" utility. 
 *        example: test/simple-1/simple-1.i
 *
 * Output: Create a series of C files test0001.c, test0002.c, etc., 
 *        in that same directory. Each has main() defined to call
 *        the last function defined in the input file with the 
 *        goal of visiting every labeled statement. 
 *
 * High-Level Approach: 
 *  Step 1 -- generate straight-line paths of statements and "assumes"
 *  Step 2 -- symbolically execute those paths  
 *  Step 3 -- generate constraints from symex states
 *  Step 4 -- solve those constraints (obtaining values for variables)
 *  Step 5 -- convert constraint solutions into test inputs
 *
 * Many places in this code are annotated with "Possible FIXME", indicating
 * an area that might be expanded in a student project. 
 *)
open Utils
open Cil
open Z3
open Printf
(**********************************************************************
 * Path Enumeration
 *
 * In this step we take as input a C function and statically enumerate a
 * set of paths through that function. A path is a list of executed
 * statements (e.g., "x=2;" along the path) intermixed with assumptions
 * (i.e., if the path corresponds to the true branch in "if (x < 5) {...}"
 * then you can assume "x < 5" after that point on the path). 
 *
 * Because a function may have many paths, we use a worklist to keep track
 * of which parts we are currently exploring. 
 *)

type path_exploration = 
  | Exploring_Block of Cil.block 
  | Exploring_Statement of Cil.stmt  
  | Exploring_Done 

type path_step =
  | Statement of Cil.stmt 
  | Assume of Cil.exp 

type path = path_step list 

let path_enumeration
  (target_fundec : Cil.fundec) (* method to enumerate paths in *) 
  : (path list) (* outputs : paths through that method *) 
  = 
  let enumerated_paths = ref [] in (* gather up our final answer *) 
  let note_path (p : path) = enumerated_paths := p :: !enumerated_paths in 

  (*
   * Each worklist element will contain a five-tuple: 
   * (1) the visited path so far,
   * (2) the current place to explore
   * (3) where to go if the current exploration terminates normally
   * (4) where to go if the current exploration is "break;" 
   * (5) where to go if the current exploration is "continue;" 
   *)
  let worklist = Queue.create () in

  let add_to_worklist path where nn nb nc =
    (* Possible FIXME: To avoid infinite loops in our analysis, if
     * we would enqueue a visit to a statement we've already visited _in
     * this path's history_, we instead give up immediately. *) 
    match where with
    | Exploring_Statement(s) when 
      List.exists (fun already_visited -> match already_visited with
        | Statement(visited_s) when visited_s.sid = s.sid -> 
true
        | _ -> false
      ) path -> Queue.add (path,Exploring_Done,[],[],[]) worklist
    | _ -> Queue.add (path,where,nn,nb,nc) worklist 
  in 

  (* We start enumerating at the first line of the function body. *) 
  add_to_worklist [] (Exploring_Block(target_fundec.sbody)) [] [] [] ;

  while not (Queue.is_empty worklist) do
    (* nn = next normal
     * nb = next if we hit a "break;"
     * nc = next if we hit a "continue;" *)
    let path, here, nn, nb, nc = Queue.pop worklist in 
    let give_up () = 
      (* At various times we will stop exploring along a path but we'll
       * still want to report that path. This function handles such cases. *) 
      add_to_worklist (path) (Exploring_Done) [] [] []
    in 

    (* The heart of path enumeration is a giant switch statement on 
     * the structure of the code being explored. *) 
    match here with

    | Exploring_Done -> begin 
        match nn with
        | [] -> note_path path (* we're done with this path! *) 
        | first :: rest -> 
          (* We might be done exploring the inside of a "then-branch",
           * for example, but we should then fall through and explore
           * whatever came after that whole if. *) 
          add_to_worklist path first rest nb nc 
      end 

    | Exploring_Block(b) -> begin
        match b.bstmts with
          | [] -> add_to_worklist path (Exploring_Done) nn nb nc 
          | first :: rest -> 
            (* if we hit a block with statements "S1; S2; S3;", 
             * we'll schedule a visit to S1 right away and put
             * "S2; S3;" on the list of things to visit next. *) 
            let followup = (Exploring_Block { b with bstmts = rest }) in 
            add_to_worklist path (Exploring_Statement(first))
              (followup :: nn) nb nc 
      end 

    | Exploring_Statement(s) -> begin
      match s.skind with

      | Instr _ -> (* e.g., handle "x = 2;" *) 
        add_to_worklist (Statement(s) :: path) (Exploring_Done) nn nb nc

      | Return _ -> 
        (* Possible FIXME: This is not (yet) an interprocedural analysis. *)
        give_up () 

      | Goto(goto_target,_) -> 
        (* Possible FIXME: Handle totally unstructured programs. *) 
        give_up () 

      | Switch _ -> 
        (* Possible FIXME: Handle switch statements. *) 
        give_up () 

      | TryFinally _ (* Microsoft C Extension *) 
      | TryExcept _ (* Microsoft C Extension *) 
      -> give_up () 

      | Break _ -> begin
          match nb, nc with 
          | b_hd :: b_tl , c_hd :: c_tl -> 
            add_to_worklist path (Exploring_Done) b_hd b_tl c_tl 
          | _, _ -> 
            (* break with no enclosing loop structure *)
            give_up () 
        end 

      | Continue _ -> begin 
          match nb, nc with 
          | b_hd :: b_tl , c_hd :: c_tl -> 
            add_to_worklist path (Exploring_Done) c_hd b_tl c_tl 
          | _, _ -> 
            (* continue with no enclosing loop structure *) 
            give_up () 
        end 

      | If(exp,then_branch,else_branch,_) -> 
(* As usual in Axiomatic Semantics, when exploring the Then-Branch 
         * you get to assume the conditional is True, and when exploring
         * the Else-Branch you get to assume that it is false. *) 
        let then_condition = exp in
        let else_condition = UnOp(LNot,exp,(Cil.typeOf exp)) in (* == !exp *)
        add_to_worklist  ((Assume then_condition) :: path) 
          (Exploring_Block(then_branch)) nn nb nc ;
        add_to_worklist  ((Assume else_condition) :: path) 
          (Exploring_Block(else_branch)) nn nb nc 

      | Loop(loop_block,_,break_opt,continue_opt) -> 
        (* In CIL, while (b) { c } becomes
         *
         * while (1) {
         *   if (!b) break; 
         *   c;
         * } 
         *
         * Thus all Loops are the equivalent of "while true". *)  
        add_to_worklist path (Exploring_Block loop_block) 
          (here :: nn) 
          (nn :: nb) 
          ((here :: nn) :: nc) 

      | Block(b) -> 
        add_to_worklist path (Exploring_Block b) nn nb nc 

    end 
  done ;

  (* We prepended statements to the front of paths, so we have to
   * reverse them to get the right history order. *) 
  let paths = List.map List.rev !enumerated_paths in 

  debug "tigen: %s: %d path(s) enumerated\n" 
    target_fundec.svar.vname 
    (List.length paths) ;

  paths 

(**********************************************************************
 * Symbolic Variable State (or Symbolic Register File) 
 *
 * Our state is a simple mapping from variable names to symbolic
 * expressions. We use the existing Cil.exp expression type for
 * symbolic expressions as well.
 *)
module OrderedString =
  struct
    type t = string
    let compare = compare
  end

module StringMap = Map.Make(OrderedString)
module StringSet = Set.Make(OrderedString)
 
let empty_symbolic_variable_state = StringMap.empty 
let empty_symbolic_record_state = StringMap.empty 

module OrderedRecord =
  struct
    type t = string
    let compare = compare
  end

type symbolic_variable_state = Cil.exp StringMap.t 
type symbolic_record_state = (string*Cil.exp)list StringMap.t
type record_list = Cil.exp list

(* The usual state update: sigma[variable_name := new_value] *) 
let symbolic_variable_state_update 
  (sigma : symbolic_variable_state)  
  (variable_name : string)
  (new_value : Cil.exp) 
  : symbolic_variable_state
  =
  StringMap.add variable_name new_value sigma 

let symbolic_record_state_update 
  (sigma : symbolic_record_state)  
  (record_name : string)
  (field_name : string)
  (new_value : Cil.exp) 
  : symbolic_record_state
  =
  if (StringMap.mem record_name sigma) then
    let fields = StringMap.find record_name sigma in 
      let old_list = List.filter(fun field -> (fst(field) <> field_name))fields in
        let new_list = (field_name, new_value) :: old_list in
        StringMap.add record_name new_list sigma
  else
    let new_list = (field_name, new_value) :: [] in
    StringMap.add record_name new_list sigma
(*
 * Look up a variable in the symbolic state. For example, if we know that
 * [x=10] and [y=z+3] and we lookup "y", we expect to get "z+3" out.
 *)
let symbolic_record_lookup (sigma : symbolic_record_state) (record_name:string) (field_name:string) :Cil.exp =
  let record = StringMap.find record_name sigma in
  let field = List.filter(fun field -> (fst(field) == field_name))record in
  snd(List.nth field 0)

let symbolic_variable_state_lookup 
      (sigma : symbolic_variable_state) 
      (sigma2 : symbolic_record_state) 
      (variable : Cil.exp) 
      : Cil.exp =
  (*debug "var lookup %s\n\n\n" (Pretty.sprint 8 (dn_exp () variable));*)
  let found = match variable with
  | Lval(Var(va),NoOffset) -> 
    begin
      try
        Some(StringMap.find va.vname sigma)
      with Not_found -> 
        None
    end 
  | Lval(lhost,Field(f,o)) ->  (* cannot handle field access *) 
      begin
        match lhost with
        |Var(va) ->
        try
          Some(symbolic_record_lookup sigma2 va.vname f.fname)
        with Not_found ->
          None
        |_ -> None
       end
  | _ -> None (* not a variable *) 
  in 
  match found with
  | Some(answer) -> answer
  | None -> variable 

(*
 * Rewrite an expression based on the current symbolic state.  For example,
 * if we know that [x=10] and [y=z+3] and we lookup "sin(x+y)", we expect
 * to get "sin(10+z+3)". 
 *
 * We use Cil's visitor pattern to implement this.
 * http://en.wikipedia.org/wiki/Visitor_pattern
 *)
  class substituteVisitor (sigma : symbolic_variable_state) (sigma2 : symbolic_record_state) = object
    inherit nopCilVisitor
    method vexpr e = 
      ChangeDoChildrenPost(e,(fun e ->
        symbolic_variable_state_lookup sigma sigma2 e
      ))
  end 
  

let symbolic_variable_state_substitute 
      (sigma : symbolic_variable_state)
      (sigma2 : symbolic_record_state) 
      (exp : Cil.exp) 
      : Cil.exp =
  let sv = new substituteVisitor sigma sigma2 in 
  visitCilExpr sv exp 

(**********************************************************************
 * Symbolic Execution
 *
 * We build on the "symbolic register file" code above to implement a more
 * generic symbolic execution. Given a "path" (a sequence of statements and
 * assumptions) we update our symbolic register file when we encounter
 * assignment statements and then record every assumption as we make it. 
 *
 * Later, we'll feed those assumptions as constraints to an automated
 * theorem prover to generate test inputs. 
 *)

type symex_state = {
  register_file : symbolic_variable_state ;
  record_register_file : symbolic_record_state ;
  assumptions : Cil.exp list ;
} 

let empty_symex_state = {
  register_file = empty_symbolic_variable_state ;
  record_register_file = empty_symbolic_record_state ;
  assumptions = [] ; 
} 

  class noteVarVisitor (rec_list : Cil.varinfo list ref) = object
    inherit nopCilVisitor
    method vvrbl v = 
      rec_list := v::!rec_list ;
      DoChildren
  end

  class noteRecordVisitor (rec_list : Cil.exp list ref) = object
    inherit nopCilVisitor
    method vexpr e = 
      match e with
        | Lval(lhost,Field(f,o)) ->
	  rec_list := e::!rec_list ;
          DoChildren
	| _ -> DoChildren
  end 

(* Given a path, produce a final symbolic execution state (a symbolic
 * register file and set of assumptions) associated with the end of that
 * path. *) 
let symbolic_execution
  (path : path) 
  : symex_state 
  =

  if false then begin (* enable this for symex debugging *) 
    debug "\ntigen: symex:\n" ;
    List.iter (fun step -> 
      match step with
      | Statement(s) -> 
        debug "%s\n" (Pretty.sprint ~width:80 (dn_stmt () s)) 
      | Assume(e) -> 
        debug "Assume %s\n" (Pretty.sprint ~width:80 (dn_exp () e)) 
    ) path ;
  end ;

  let state = empty_symex_state in 
  (* For each variable mentioned in the path, assign it a default,
   * arbitrary value. We use "_x" to represent the unknown initial
   * value of variable "x". 
   *
   * Possible FIXME: This may not handle memory (i.e., arrays, pointers)
   * correctly. *) 
  let records = ref  [] in 
  let nrv = new noteRecordVisitor records in 
  List.iter (fun step -> match step with
    | Statement(s) -> ignore (visitCilStmt nrv s) 
    | Assume(e) -> ignore (visitCilExpr nrv e) 
  ) path ;
  
  let variables = ref [] in 
  let nv = new noteVarVisitor variables in 
  List.iter (fun step -> match step with
    | Statement(s) -> ignore (visitCilStmt nv s) 
    | Assume(e) -> ignore (visitCilExpr nv e) 
  ) path ;
 
  let new_register_file = List.fold_right (fun (va:Cil.varinfo) (state : symbolic_variable_state) ->
    let new_value = Lval(Var(makeVarinfo false ("_" ^ va.vname) 
    (va.vtype)), NoOffset) in
    symbolic_variable_state_update state va.vname new_value
  )!variables state.register_file in 
  
  let new_record_register_file = List.fold_right (fun (record : Cil.exp) (state : symbolic_record_state) ->
     match record with
      | Lval(lhost,Field(f,o)) -> 
        match lhost with
	| Var(va) ->
        let new_value = Lval(Var(makeVarinfo false ("_" ^ va.vname ^ "." ^ f.fname) 
        (va.vtype)),Field(f,o)) in
    	symbolic_record_state_update state va.vname f.fname new_value
  )!records state.record_register_file in 
  
  let state = { state with register_file = new_register_file; record_register_file = new_record_register_file} in 

  (*
   * Now we walk down every step in the path, handling assignment
   * statements (which update the symbolic register file) and assumptions
   * (which are evaluated and gathered up). 
   *)
  let final_state = List.fold_left (fun state step ->
    match step with
    | Assume(e) -> (* recall that we get these from "if" statements. *)
      let evaluated_e = symbolic_variable_state_substitute
        state.register_file state.record_register_file e in
      { state with assumptions = evaluated_e :: state.assumptions} 
    | Statement(s) -> begin
      (*debug "\nTHe statements are %s\n" (Pretty.sprint ~width:80 (dn_stmt () s)); *)
      match s.skind with
      | Instr(il) -> 
        List.fold_left (fun state instr ->
          match instr with
          | Set((Var(va),NoOffset),rhs,_) -> 
            let evaluated_rhs = symbolic_variable_state_substitute 
              state.register_file state.record_register_file rhs 
            in 
            let new_register_file = symbolic_variable_state_update 
              state.register_file va.vname evaluated_rhs in
            { state with register_file = new_register_file } 
          | Set((Mem(address),_),rhs,_) ->
            (* Possible FIXME: cannot handle memory accesses like *p *) state 
          | Set((lhost ,Field(f,o)),rhs,_) -> 
          begin
	    match lhost with
            |Var(va) ->
              let evaluated_rhs = symbolic_variable_state_substitute state.register_file state.record_register_file rhs
            in 
            let new_record_register_file = symbolic_record_state_update state.record_register_file va.vname f.fname evaluated_rhs in
            { state with record_register_file = new_record_register_file } 
            end
            (* Possible FIXME: cannot handle field accesses like e.fld *) 
          | Set((_,Index(i,o)),rhs,_) -> 
            (* Possible FIXME: cannot handle array indexing like a[i] *) state 

          | Call _  -> (* Possible FIXME: cannot handle function calls *) state
          | Asm _ -> (* cannot handle inline ASM *) state 
        ) state il 
      | _ -> state 
    end
  ) state path in

  final_state 

(**********************************************************************
 * Constraint Solving
 *
 * Given the final symbolic excution state corresponding to a path,
 * we now want to generate constraints for a theorem prover and solve those
 * constraints. For example, if we know that "x > 10" && "x < 15", we'd
 * like to come up with a concrete assignment like "x == 11". That concrete
 * value is a test input that forces execution down the path in question!
 *)

(* The final constraint solution will be a mapping from variable names to 
 * textual values (i.e., from "x" to "11"). Possible FIXME: This is
 * unlikely to be sufficient for more complicated values (e.g., pointers,
 * arrays).  *) 
type solved_constraints = string StringMap.t 

let solve_constraints
  (target_fundec : Cil.fundec) (* method to generate inputs for *) 
  (state : symex_state) (* final symex state associated with a path *)
  : (solved_constraints) option  (* Some x == path is feasible 
                                  * None   == path is NOT feasible *) 
  =
  (* We use the Z3 automated theorem prover and SMT solver. We need
   * more than a "yes or no" answer: we need a satisfying assignment (also
   * called a "model"). So we tell Z3 that we want such a model. *) 
  let ctx = mk_context_x [| "MODEL", "true" |] in 
  if false then begin (* enable this for Z3 debugging *) 
    Z3.trace_to_stdout ctx ;  
  end ; 
  (* Much of the work here is converting from CIL Abstract Syntax Trees to
   * Z3 Abstract Syntax Trees. *) 
  let real_sort = Z3.mk_real_sort ctx in (* Possible FIXME: reals unhandled *) 
  let int_sort = Z3.mk_int_sort ctx in (* Possible FIXME: reals unhandled *) 
  let zero_ast = Z3.mk_int ctx 0 int_sort in 
  let undefined_ast = zero_ast in 

  (* Every time we encounter the same C variable "foo" we want to map
   * it to the same Z3 node. We use a hash table to track this. *) 
  let symbol_ht = Hashtbl.create 255 in
  let var_to_ast var_name var_type = 
    try
      Hashtbl.find symbol_ht var_name
    with _ -> begin
      let sym = mk_string_symbol ctx var_name in
      match var_type with
       |TFloat(kind, attributes) ->
         let ast = mk_const ctx sym real_sort in 
         Hashtbl.replace symbol_ht var_name ast ;
         ast
       |_->
         let ast = mk_const ctx sym int_sort in 
         Hashtbl.replace symbol_ht var_name ast ;
         ast
      end	
  in 
  (* In Z3, boolean-valued and integer-valued expressions are different
   * (i.e., have different _Sort_s). CIL does not have this issue. *) 
  let is_binop exp = 
    match exp with 
    | UnOp(LNot,_,_) 
    | BinOp(Lt,_,_,_) 
    | BinOp(Le,_,_,_) 
    | BinOp(Gt,_,_,_) 
    | BinOp(Ge,_,_,_) 
    | BinOp(Eq,_,_,_) 
    | BinOp(Ne,_,_,_) -> true
    | _ -> false
  in 

  (* This is the heart of constraint generation. For every CIL expression
   * (e.g., "x > 10"), convert it to an equivalent Z3 expression. *) 
  let rec exp_to_ast (exp : Cil.exp) : Z3.ast = match exp with
    | Const(CInt64(i,_,_)) -> 
      (* Possible FIXME: large numbers are not handled *) 
      let i = Int64.to_int i in 
      Z3.mk_int ctx i int_sort 
    | Const(CReal(value, fkind, string_rep)) ->
        let multiplier  = 10000.0
        in mk_real ctx (int_of_float (value *.multiplier)) (int_of_float multiplier)
    | Const(CChr(c)) -> 
      (* Possible FIXME: characters are justed treated as integers *) 
      let i = Char.code c in
      Z3.mk_int ctx i int_sort

    | Const(_) -> 
      (* Possible FIXME: reals, enums, strings, etc., are not handled *) 
      undefined_ast

    | Lval(Var(va),NoOffset) -> var_to_ast va.vname va.vtype 
    | Lval(Var(va), Field(f,o) ) -> 
      (*let field_names = [|mk_string_symbol ctx f.fname|]in
      let field_sort = mk_int_sort(ctx) in
      let field_sorts = [|field_sort|]in
      let field_accessors = [|1;2;3|]in
      let type_name = mk_string_symbol ctx va.vname in
      let recogniser = mk_string_symbol ctx "b" in
      let con_sym = mk_string_symbol ctx f.fname in
      let constructor = mk_constructor ctx recogniser con_sym field_names field_sorts field_accessors in
      let constructors = [|constructor|] in 
      let typ_con = [|(field_sort, constructor)|] in
*)  
      undefined_ast

    | Lval(_) -> 
       undefined_ast
      (* Possible FIXME: var.field, *p, a[i], etc., are not handled *) 
    | UnOp(Neg,e,_) -> mk_unary_minus ctx (exp_to_ast e) 
    | UnOp(LNot,e,_) when is_binop e -> mk_not ctx (exp_to_ast e) 
    | UnOp(LNot,e,_) -> mk_eq ctx (exp_to_ast e) (zero_ast) 

    | BinOp(PlusA,e1,e2,_) -> mk_add ctx [| exp_to_ast e1; exp_to_ast e2|]
    | BinOp(MinusA,e1,e2,_) -> mk_sub ctx [| exp_to_ast e1; exp_to_ast e2|]
    | BinOp(Mult,e1,e2,_) -> mk_mul ctx [| exp_to_ast e1; exp_to_ast e2|]
    | BinOp(Div,e1,e2,_) -> 
      let ast2 = exp_to_ast e2 in 
      let not_div_by_zero = mk_distinct ctx [| zero_ast ; ast2 |] in 
      Z3.assert_cnstr ctx not_div_by_zero  ; 
      mk_div ctx (exp_to_ast e1) ast2 
    | BinOp(Mod,e1,e2,_) -> mk_mod ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Lt,e1,e2,_) -> mk_lt ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Le,e1,e2,_) -> mk_le ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Gt,e1,e2,_) -> mk_gt ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Ge,e1,e2,_) -> mk_ge ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Eq,e1,e2,_) -> mk_eq ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Ne,e1,e2,_) -> 
      mk_distinct ctx [| (exp_to_ast e1) ; (exp_to_ast e2) |] 
    | CastE(_,e) -> exp_to_ast e (* Possible FIXME: (int)(3.1415) ? *) 
    | _ -> 
      (* addrof, startof, alignof, sizeof, etc., are not handled *) 
      undefined_ast
  in 

  (* For every assumption along the path, convert it to a Z3 expression
   * and tell the theorem prover to assert it as true (i.e., as a
   * constraint). *) 
  List.iter (fun cil_exp -> 
    try
      let z3_ast = exp_to_ast cil_exp in 
      
      (*debug "tigen: asserting %s\n" 
        (Z3.ast_to_string ctx z3_ast) ; 
      *)
 
      Z3.assert_cnstr ctx z3_ast ; 
    with _ -> begin  
    
      debug "tigen: cannot convert %s to Z3\n"
        (Pretty.sprint ~width:80 (dn_exp () cil_exp)) ;
        ()
    end 
  ) state.assumptions ; 

  (* Now that we've put in all of the constraints, query the theorem
   * prover to see if there is a model that can satisfy them all at the
   * same time. *) 
  let made_model, model = Z3.check_and_get_model ctx in 
  let result = if made_model = L_TRUE then begin
    (* If there is a model, we try to extra concrete values from it. Those
    * concrete values become our solution. *) 
    let solution = ref StringMap.empty in 
    List.iter (fun formal_variable ->
      let underscore_name = "_" ^ formal_variable.vname in 
      let z3_ast = var_to_ast underscore_name formal_variable.vtype in 
      let worked, evaluated = Z3.eval ctx model z3_ast in 
      let evaluated = Z3.ast_to_string ctx evaluated in 
      if worked && evaluated <> "" && evaluated.[0] <> '_' then begin
        solution := StringMap.add formal_variable.vname evaluated 
          !solution 
      end ; 
    ) target_fundec.sformals ;
    Some(!solution) 
  end else None in
  Z3.del_context ctx; 
  result 

(**********************************************************************
 * Emit Test Case
 *
 * Given a concrete solution (e.g., "x = 5", "y = 22"), we must
 * actually emit a test case that calls the method in question with
 * those parameters. For this project, we emit every test case as a
 * separate C file so that we can compile them all separately and
 * calculate the total coverage dynamically. 
 *)
let emit_test_case
  (target_fundec : Cil.fundec) (* method to generate inputs for *) 
  (filename : string) (* where to put this test *) 
  (solution : solved_constraints) (* what values to use *) 
  : unit (* outputs results to disk *) 
  = 

  let fout = open_out filename in 
  let extern_decl = GVarDecl(target_fundec.svar,locUnknown) in 
  (* First, if we're a test case for gcd(int, int), add an
   * forward declaration to tell the compiler that function gcd(int, int)
   * exists. *)
  Printf.fprintf fout "#include <stdio.h>\n\nextern %s\n\n" 
    (Pretty.sprint ~width:80 (dn_global () extern_decl)) ; 

  (* We emit our test case as a little main() program. *) 
  Printf.fprintf fout "int main() {\n" ; 

  (* Declare local variables to hold all of the formals. *) 
  List.iter (fun formal ->
    Printf.fprintf fout "\t%s %s;\n" 
      (Pretty.sprint ~width:80 (dn_type () formal.vtype))
      formal.vname ; 
  ) target_fundec.sformals ; 

  (* The subject program may loop forever, but we don't want to. Break it
   * off after a few seconds. *) 
  Printf.fprintf fout "\talarm(2);\n" ; 

  (* If our solution says that "x" maps to "2", add "x = 2;" to the test
   * case. *) 
  List.iter (fun formal -> 
    try 
      let value = StringMap.find formal.vname solution in
      match formal.vtype with 
	|TFloat(kind, attributes) ->
		 Printf.fprintf fout "\t%s = (double)%s;\n" formal.vname value 
     	|_ ->
		 Printf.fprintf fout "\t%s = %s;\n" formal.vname value 
    with _ -> () 
  ) target_fundec.sformals ; 

  (* Now all that's left to do is actually call the function. That will
   * look something like: "gcd(a,b);" *)
  let var_to_exp v = Lval(Var(v),NoOffset) in 
  let actuals = List.map var_to_exp target_fundec.sformals in
  let call_instr = Call(None,
    var_to_exp target_fundec.svar, actuals, locUnknown) in 

  Printf.fprintf fout "\n\n\t%s\n\treturn 0;\n}\n" 
    (Pretty.sprint ~width:80 (dn_instr () call_instr)) ; 

  close_out fout ; 
  () 

(**********************************************************************
 * Test Input Generation
 *
 * Generate test cases for the given function and write them to the given
 * directory. This is a direct implementation of the multi-step algorithm
 * in the top-level comment: enumerate paths, symbolically execute them, 
 * generate and solve constraints corresponding to those symex states,
 * and emit test cases based on those constraint solutions. 
 *)
let test_input_generation 
  (target_fundec : Cil.fundec) (* method to generate inputs for *) 
  (directory : string) (* where to put the tests *) 
  : unit (* outputs results to disk *) 
  = 

  let paths = path_enumeration target_fundec in 

  let paths = first_nth paths 500 in (* don't take too long! *) 

  let symbolic_states = List.map symbolic_execution paths in 

  (* We'll use a hashtbl as a cheap way to gather up unique
   * solutions since I can't be bothered to define a StringMapSet. *)
  let solutions = Hashtbl.create 255 in 
  List.iter (fun state ->
    match solve_constraints target_fundec state with
    | None -> ()
    | Some(answer) -> Hashtbl.replace solutions answer true 
  ) symbolic_states;

  let test_case_counter = ref 0 in (* how many tests generated so far? *) 
  let next_test_case_name () = 
    let local_name = Printf.sprintf "test%04d.c" !test_case_counter in
    incr test_case_counter ; 
    Filename.concat directory local_name
  in 

  Hashtbl.iter (fun solution _->
    emit_test_case 
      target_fundec
      (next_test_case_name ())
      solution 
  ) solutions ; 

  debug "tigen: %s: %d test case(s) emitted\n" 
    target_fundec.svar.vname 
    !test_case_counter ; 

  () 

(**********************************************************************
 * Main Driver
 *
 * We accept the program to test as the only command-line argument. We
 * emit the test cases in the same directory as the program source. Try
 * "make test" and "make eval" to run this automatically on the provided
 * tests. 
 *)
let main () = begin
  if Array.length Sys.argv < 2 then begin 
    debug "tigen: specify a pre-preprocessed C file\n" ;
    exit 1 
  end ; 
  Z3.toggle_warning_messages true ; 
  let filename = Sys.argv.(1) in 
  let directory = Filename.dirname filename in 
  let file = load_c_file filename in 
  (* 
   * We want each statement to have a unique statement ID, so we'll call
   * computeFileCFG. 
   *)
  Cfg.computeFileCFG file ; 

  (* Find the last fundec in the file. *) 
  let rec find_fundec global_list = 
    match global_list with
    | GFun(fd,loc) :: tl -> test_input_generation fd directory 
    | hd :: tl -> find_fundec tl 
    | [] -> debug "tigen: no functions declared in %s\n" filename 
  in
  find_fundec (List.rev file.globals) ;

end ;;
main () ;;

(* tigen.ml: end of file *) 
