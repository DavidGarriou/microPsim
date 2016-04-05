{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* part 1 *)
let int = '-'? ['0'-'9'] ['0'-'9']*

(* part 2 *)
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

(* part 3 *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* part 4 *)
rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | ','      { COMMA }
  | '+'      { PLUS }
  | "lih"    { LIH }
  | "b"      { B }
  | "ba"     { BA }
  | "bl"     { BL }
  | "bla"    { BLA }
  | "beq"    { BEQ }
  | "bne"    { BNE }
  | "bge"    { BGE }
  | "bgt"    { BGT }
  | "ble"    { BLE }
  | "blt"    { BLT }
  | "bvs"    { BVS }
  | "bvc"    { BVC }
  | "blr"    { BLR }
  | "btr"    { BTR }
  | "mtlr"   { MTLR }
  | "mflr"   { MFLR }
  | "add"    { ADD }
  | "add."   { ADDC }
  | "sub"    { SUB }
  | "sub."   { SUBC }
  | "and."   { ANDC }
  | "or"     { OR }
  | "ldx"    { LDX }
  | "stx"    { STX }
  | "ldbx"   { LDBX }
  | "ldsbx"  { LDSBX }
  | "stbx"   { STBX }
  | "addi"   { ADDI }
  | "addi."  { ADDIC }
  | "andi."  { ANDIC }
  | "ori"    { ORI }
  | "ld"     { LD }
  | "st"     { ST }
  | "ldb"    { LDB }
  | "ldsb"   { LDSB }
  | "stb"    { STB }
  | "lsl"    { LSL }
  | "rol"    { ROL }
  | "lsr"    { LSR }
  | "asr"    { ASR }
  | "r0"     { REG_R0 }
  | "r1"     { REG_R1 }
  | "r2"     { REG_R2 }
  | "r3"     { REG_R3 }
  | "r4"     { REG_R4 }
  | "r5"     { REG_R5 }
  | "r6"     { REG_R6 }
  | "r7"     { REG_R7 }
  | "lr"     { REG_LR }
  | "pc"     { REG_PC }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

(* part 5 *)
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
