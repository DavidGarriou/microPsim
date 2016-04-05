(*type register = Reg of string;;
type const = Const of int;;
*)

type address = int;;

type value = [
  | `Add of string * string * string
  | `Addc of string * string * string
  | `Addi of string * string * int
  | `Addic of string * string * int
  | `Sub of string * string * string
  | `Subc of string * string * string
  | `And of string * string * string
  | `Andc of string * string * string
  | `Andic of string * string * string
  | `Or of string * string * string
  | `Ori of string * string * string
  | `Ld of string * string * int
  | `Ldb of string * string * int
  | `Ldsb of string * string * int
  | `Ldx of string * string * string
  | `Ldbx of string * string * string
  | `Ldsbx of string * string * string
  | `St of string * int * string
  | `Stb of string * int * string
  | `Stx of string * string * string
  | `Stbx of string * string * string
  | `Lsl of string * string * int
  | `Lsr of string * string * int
  | `Rol of string * string * int
  | `Asr of string * string * int
  | `B of address
  | `Ba of address
  | `Bl of address
  | `Bla of address
  | `Beq of address
  | `Bne of address
  | `Bge of address
  | `Bgt of address
  | `Ble of address
  | `Blt of address
  | `Bvs of address
  | `Bvc of address
  | `Blr
  | `Btr of string
  | `Mtlr of string
  | `Mflr of string
  | `Lih of string * int
  | `Null
  ]

(* part 1 *)
(*  | `Const c -> output_string outc "const"*)
(*  | `Reg r   -> output_string outc "register"*)
open Core.Std
let rec output_value outc = function
  | `Add (r1, r2, r3)   -> let chaine = "add " ^ r1 ^ ", " ^ r2 ^ ", " ^ r3 in
                            output_string outc chaine (*print_assoc outc regs*)
  | `Addc (r1, r2, r3)   -> let chaine = "add. " ^ r1 ^ ", " ^ r2 ^ ", " ^ r3 in
                            output_string outc chaine (*print_assoc outc regs*)
  | `Addi (r1, r2, c)   -> let chaine = "addi " ^ r1 ^ ", " ^ r2 ^ ", " ^ string_of_int(c) in
                            output_string outc chaine (*print_assoc outc regs*)
  | `Addic (r1, r2, c)   -> let chaine = "addi. " ^ r1 ^ ", " ^ r2 ^ ", " ^ string_of_int(c) in
                            output_string outc chaine (*print_assoc outc regs*)
  | `Ldx (rd, rs1, rs2)  -> output_string outc "ldx"
  | `Stx (rd, rs1, rs2)  -> output_string outc "stx"
  | `Lih (r, c)  -> output_string outc "lih"
  | `Null       -> output_string outc "null"
  | _ -> output_string outc "autre"

and print_assoc outc obj =
  output_string outc "{ ";
  let sep = ref "" in
  List.iter ~f:(fun (key, value) ->
      printf "%s\"%s\": %a" !sep key output_value value;
      sep := ",\n  ") obj;
  output_string outc " }"

and print_list outc arr =
  output_string outc "[";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        output_string outc ", ";
      output_value outc v) arr;
  output_string outc "]"
