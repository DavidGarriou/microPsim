open Instset;;
open Stockage;;
open Lexuniv;;

let registre = function
  | [< _MC "r"; _Entier nbr >] -> nbr
  | [< _MC "sp" >] -> sp
  | [< _MC "ra" >] -> ra;;

let constante = function
  | [< _Entier nbr >] -> nbr
  | [< _Ident nom_etiq >] -> valeur_etiquette nom_etiq;;

let operande = function
  | [< registre r >] -> Reg r
  | [< constante c >] -> Imm c;;

let rec instruction = function
  | [< operation op; reg_op_reg (r1, o, r2) >] ->
      assemble(Op(op, r1, o, r2))
  | [< test_inverse test; reg_op_reg (r1, o, r2) >] ->
      assemble(Op(test, r1, o, r2));
      assemble(Op(Seq, r2, Reg 0, r2))
  | [< _MC "jmp"; operande o; _MC ","; registre r >] ->
      assemble(Jmp(o, r))
  | [< _MC "braz"; registre r; _MC ","; constante c >] ->
      assemble(Braz(r, c))
  | [< _MC "branz"; registre r; _MC ","; constante c >] ->
      assemble(Branz(r, c))
  | [< _MC "scall"; _Entier n >] -> assemble (Scall n)
  | [< _MC "write" >] -> assemble (Scall 1)
  | [< _MC "read" >] -> assemble (Scall 0)
  | [< _MC "stop" >] -> assemble Stop
and reg_op_reg = function
  | [< registre r1; _MC ","; operande o; _MC ","; registre r2 >] ->
      (r1, o, r2)
and operation = function
  | [< _MC "load" >] -> Load
  | [<_MC"add">] -> Add
  | [<_MC"sub">] -> Sub
  | [<_MC"and">] -> And
  | [<_MC"xor">] -> Xor
  | [<_MC"shr">] -> Shr
  | [<_MC"sle">] -> Sle
and test_inverse = function
  | [< _MC "sgt" >] -> Sle
  | [< _MC "sge" >] -> Slt
  | [< _MC "store" >] -> Store
  | [< _MC "mult" >] -> Mult
  | [<_MC"div">] -> Div
  | [<_MC"or">] -> Or
  | [<_MC"shl">] -> Shl
  | [<_MC"slt">] -> Slt
  | [<_MC"seq">] -> Seq
  | [< _MC "sne" >] -> Seq;;

let definition_d_etiquette = function
  | [< _Ident nom_etiq; _MC ":" >] -> poser_etiquette nom_etiq;;

let rec instruction_etiq = function
  | [< definition_d_etiquette (); instruction_etiq () >] -> ()
  | [< instruction () >] -> ();;

let rec suite_d_instructions flux =
  match flux with
  | [< instruction_etiq () >] -> suite_d_instructions flux
  | [< >] -> ();;

let analyseur_lexical =
  construire_analyseur
    ["r"; "sp"; "ra"; "load"; "store"; "add"; "mult"; "sub"; "div";
     "and"; "or"; "xor"; "shl"; "shr"; "sgt"; "sge"; "sne";
     "slt"; "sle"; "seq"; "jmp"; "braz"; "branz";
     "scall"; "write"; "read"; "stop"; ","; ":"];;

let programme flux =
  initialise ();
  suite_d_instructions (analyseur_lexical flux);
  extraire_code ();;
