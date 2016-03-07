type registre == int;;

type operande =
  Reg of registre
  | Imm if int;;

type instruction =
  Op of operande * registre * operande * registre
  | Jmp of operande * registre
  | Braz of registre * int
  | Branz of registre * int
  | Scall of int
  | Stop
and operation =
  Load | Store | Add | Mult | Sub | Div
  | And | Or | Xor | Shl | Shr
  | Slt | Sle | Seq;;

value nombre_de_registres: int
  and sp: int
  and ra: int
  taille_du_mot: int;;
