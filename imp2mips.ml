open Imp
open Mips

let push reg = subi sp sp 4 @@ sw reg 0 sp

let pop reg = lw reg 0 sp @@ addi sp sp 4

let tr_function fdef =
  let env = Hashtbl.create 16 in
  List.iteri (fun k id -> Hashtbl.add env id (4 * (k + 1))) fdef.params ;
  List.iteri (fun k id -> Hashtbl.add env id (-4 * (k + 2))) fdef.locals ;
  (*
     Function that generate MIPS code for an IMP expression.
     The function takes a parameter an expression [e] and produces a
     sequence of MIPS instructions that evaluates [e] and put the
     obtained value in register $t0.
   *)
  let rec tr_expr = function
    (* Case of a constant: load the value in the target register $t0.
       Boolean [true] is coded by 1 and boolean [false] by 0. *)
    | Cst n ->
        li t0 n
    | Bool b ->
        if b then li t0 1 else li t0 0
    (* Case of a variable. Look up the identifier in the local environement
       [env] to know the offset, then read at the obtained offset from the
       base address $fp. *)
    | Var id -> (
      match Hashtbl.find_opt env id with
      | Some offset ->
          lw t0 offset fp
      | None ->
          la t0 id @@ lw t0 0 t0 )
    (* Binary operation: use the stack to store intermediate values waiting
       to be used. *)
    | Binop (bop, e1, e2) ->
        let op = match bop with Add -> add | Mul -> mul | Lt -> slt in
        (* Evaluate [e2] *)
        tr_expr e2
        (* Store the value of [e2] on the stack (it has been put in $t0). *)
        @@ push t0
        (* Evaluer [e1] *)
        @@ tr_expr e1
        (* Retrieve the value of [e2] that has been waiting on the stack,
           put it in a register other than $t0 (that one contains the value
           of [e1]). *)
        @@ pop t1
        (* Apply the binary operation to $t0 (the value of [e1]) and $t1 (the
           value of [e2]), and put the result in $t0. *)
        @@ op t0 t0 t1
    (* Function call.
       Before jumping to the function itself, evaluate all parameters and put
       their values on the stack, from last to first. *)
    | Call (label, params) ->
        (* Evaluate the arguments and pass them on the stack. *)
        let params_code =
          List.fold_right
            (fun e code -> code @@ tr_expr e @@ push t0)
            params nop
        in
        params_code @@ jal label @@ addi sp sp (4 * List.length params)
        (* STEP 4 *)
    | Deref e ->
        tr_expr e (* pointer in t0 *) @@ lw t0 0 t0
    | Alloc e ->
        (* request e bytes above the heap *)
        tr_expr e @@ move a0 t0 @@ li v0 9
        @@ syscall (* sbrk -> shifts the limit of the heap *)
        @@ move t0 v0 (* v0 contains the first address of the allocated space *)
    | Addr label ->
        la t0 label
    | DCall (adr, args) ->
        let args_code =
          List.fold_right (fun e code -> code @@ tr_expr e @@ push t0) args nop
        in
        (* compute function address *)
        let fun_adr_code = tr_expr adr in
        (* 1. eval arg and push arg;
           2. eval function location, result at $t0;
           3. jump to the location;
           4. clear stack for argument after return;
        *)
        args_code @@ fun_adr_code @@ jalr t0 @@ addi sp sp (4 * List.length args)
  in
  (*
     Auxiliary function for producing unique labels, for use in the
     translation of control structures (if and while).
   *)
  let new_label =
    let cpt = ref (-1) in
    fun () ->
      incr cpt ;
      Printf.sprintf "__%s_%i" fdef.name !cpt
  in
  (*
     Functions that generate MIPS code for an instruction or a sequence.
   *)
  let rec tr_seq = function
    | [] ->
        nop
    | [i] ->
        tr_instr i
    (* If an IMP sequence contains several instructions, concatenate the
       MIPS sequences for each in order. *)
    | i :: s ->
        tr_instr i @@ tr_seq s
  and tr_instr = function
    (* Prints a char. *)
    | Putchar e ->
        (* Evaluate expression [e] *)
        tr_expr e
        (* Move the value of [e] from $t0 (where it has been produced)
           to $a0 (where syscall expects it). *)
        @@ move a0 t0
        (* Syscall number 11: printing an ASCII character. *)
        @@ li v0 11
        @@ syscall
    (* Assignment.
       After evaluation of [e], its value is in $t0.
       Chose the right instruction to update memory depending on the
       local or global nature of the variable [id]. *)
    | Set (id, e) ->
        let set_code =
          match Hashtbl.find_opt env id with
          | Some offset ->
              sw t0 offset fp
          | None ->
              la t1 id @@ sw t0 0 t1
        in
        tr_expr e @@ set_code
    (* Conditional *)
    | If (c, s1, s2) ->
        (* Create two labels that will serve as targets for jumps. *)
        let then_label = new_label () and end_label = new_label () in
        (* Evaluate the condition [c] *)
        tr_expr c
        (* If we got a non-zero value, which is interpreted as [true], jump
           to the code fragment of the "then" branch... *)
        @@ bnez t0 then_label
        (* ... otherwise just fall to the next instruction.
           Hence, we put the code of the "else" branch just here. *)
        @@ tr_seq s2
        (* At the end of the "else" branch, jump to the instruction that follows
           the conditional. *)
        @@ b end_label
        (* Code for the "then" branch. *)
        @@ label then_label
        @@ tr_seq s1
        (* At the end of the "then" branch, there is no need to jump, since we
           are precisely at the end of the conditional. Just put here the
           final label, without any explicitly associated instruction (it will
           be associated to the instruction that immadiately follows the
           conditional). *)
        @@ label end_label
    (* Loop *)
    | While (c, s) ->
        (* Create two labels for jumps. *)
        let test_label = new_label () and code_label = new_label () in
        (* First instruction: jump to the code that evaluates the condition. *)
        b test_label
        (* Code for the loop body, introduced by its label. *)
        @@ label code_label
        @@ tr_seq s
        (* At the end of a pass through the loop, just fall to the evaluation of
           the condition, that determines whether the loop is executed again. *)
        @@ label test_label
        @@ tr_expr c
        (* If the condition is non-zero, jumps back to the beginning of the loop
           body. *)
        @@ bnez t0 code_label
    (* Otherwise, fall to the next instruction, which in this case is the
       instruction that immediately follows the loop. *)
    (* Note: without the instruction [b test_label] at the beginning, we get
       the behaviour of a do-while loop instead of while. *)

    (* Function termination. *)
    | Return e ->
        (* Evaluate the value to be returned, in $t0 *)
        tr_expr e
        (* Deallocate the part of the stack used for local variables *)
        @@ addi sp fp (-4)
        (* Retrieve the return address *)
        @@ pop ra
        (* Restore the base pointer of the caller *)
        @@ pop fp
        (* Jumps back to the caller *)
        @@ jr ra
    (* Expression used as an instruction.
       Note that the produced MIPS code writes a value in $t0, but
       this value will not be used. *)
    | Expr e ->
        tr_expr e
    | Write (e1, e2) ->
        tr_expr e1 (* t0: pointer *)
        @@ push t0
        @@ tr_expr e2 (* t0: value to be written *)
        @@ pop t1 @@ sw t0 0 t1
    | Seq seq ->
        tr_seq seq
  in
  (*
     MIPS code produced for the function.
     Reminder: when this code is executed, the parameters of the call have
     already been evaluated and pushed on the stack. Half of the activation
     frame is already built, and we now build the second hald.
   *)
  (* Save the base address of the activation frame of the caller and the
     return address. *)
  push fp (* STEP 2 *) @@ push ra
  (* optionnally: save callee-saved registers *)
  (* Definition of the base pointer of the new activation frame. *)
  @@ addi fp sp 4
  (* Allocate space on the stack for local variables, by just shifting the
     pointer that indicates where the top is. *)
  @@ addi sp sp (-4 * List.length fdef.locals) (* END STEP 2 *)
  (* After this preamble, we can execute the actual code of the function. *)
  @@ tr_seq fdef.code
  (* If execution reaches the end of this sequence, it means that no [return]
     instruction has been met. We add a MIPS fragment equivalent to [return 0;] *)
  @@ li t0 0 (* STEP 3 *)
  @@ addi sp fp (-4) (* optionnally: restore callee-saved registers *)
  @@ pop ra @@ pop fp @@ jr ra (* END STEP 3 *)


(** Main function for translating a program. *)
let translate_program prog =
  (* MIPS fragment that will be placed at the beginning of the produced
     assembly code, for retrieving one optional integer command-line
     argument, then jumping to the "main" function. *)
  let init =
    (* At the beginning, $a0 contains the number of command-line arguments.
       If this number is zero, skip the two following lines. *)
    beqz a0 "init_end"
    (* Otherwise, $a1 contains the address of an array of character strings
       containing the command-line arguments. Here we assume only one argument,
       copy the address of the corresponding string in $a0 and call an
       auxiliary function "atoi" (defined below) for converting the string
       into an integer. *)
    @@ lw a0 0 a1
    @@ jal "atoi" @@ label "init_end"
    (* At the end, the obtained integer is in $v0. Push it on the stack to
       pass it as an argument to "main". *)
    @@ push v0
    @@ jal "main"
    (* After execution of the "main" function, system call for halting the
       program. *)
    @@ li v0 10
    @@ syscall
  and built_ins =
    (* Conversion function string -> int, that iterates on the characters of
       the string. *)
    comment "built-in atoi" @@ label "atoi" @@ li v0 0 @@ label "atoi_loop"
    @@ lbu t0 0 a0 @@ beqz t0 "atoi_end" @@ addi t0 t0 (-48)
    @@ bltz t0 "atoi_error" @@ bgei t0 10 "atoi_error" @@ muli v0 v0 10
    @@ add v0 v0 t0 @@ addi a0 a0 1 @@ b "atoi_loop" @@ label "atoi_error"
    @@ li v0 10 @@ syscall @@ label "atoi_end" @@ jr ra
  in
  (*
     Main code for producing the MIPS assembly program corresponding to the
     source IMP program.
   *)
  let function_codes =
    List.fold_right
      (fun fdef code -> label fdef.name @@ tr_function fdef @@ code)
      prog.functions nop
  in
  (* Combine the initialization code seen above, we the code produced for each
     function of the source program. *)
  let text = init @@ function_codes @@ built_ins
  (* In the "data" part, introduce a label for each global variable. Here, all
     the variables are initialized at 0. *)
  and data =
    List.fold_right
      (fun id code -> label id @@ dword [0] @@ code)
      prog.globals nop
  in
  {text; data}
