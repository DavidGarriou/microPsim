%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token LEFT_BRACK, RIGHT_BRACK
%token LEFT_PAREN, RIGHT_PAREN
%token COMMA
%token PLUS
%token ADD, ADDC, ADDI, ADDIC
%token SUB, SUBC
%token AND, ANDC, ANDIC
%token OR, ORI
%token LD, LDB, LDSB, LDX, LDBX, LDSBX
%token ST, STB, STSB, STX, STBX
%token LSL, LSR, ROL, ASR
%token LIH
%token B, BA, BL, BLA
%token BEQ, BNE, BGE, BGT, BLE, BLT, BVS, BVC
%token BLR, BTR, MTLR, MFLR
%token REG_R0, REG_R1, REG_R2, REG_R3
%token REG_R4, REG_R5, REG_R6, REG_R7
%token REG_LR, REG_PC
%token EOF

%start <Mips.value option> prog

%%
(* part 1 *)
prog:
  | v = value { Some v }
  | EOF       { None   } ;

value:
  | ADD;   v = regle_add          { `Add v }
  | ADDC;  v = regle_add          { `Addc v }
  | ADDI;  v = regle_addi         { `Addi v }
  | ADDIC; v = regle_addi         { `Addic v }
  | SUB;   v = regle_sub          { `Sub v }
  | SUBC;  v = regle_sub          { `Subc v }
  | AND;   v = regle_and          { `And v }
  | ANDC;  v = regle_and          { `Andc v }
  | ANDIC; v = regle_and          { `Andic v }
  | OR;    v = regle_or           { `Or v }
  | ORI;   v = regle_or           { `Ori v }
  | LD;    v = regle_load         { `Ld v }
  | LDB;   v = regle_load         { `Ldb v }
  | LDSB;  v = regle_load         { `Ldsb v }
  | LDX;   v = regle_load_x       { `Ldx v }
  | LDBX;  v = regle_load_x       { `Ldbx v }
  | LDSBX; v = regle_load_x       { `Ldsbx v }
  | ST;    v = regle_store        { `St v }
  | STB;   v = regle_store        { `Stb v }
  | STX;   v = regle_store_x      { `Stx v }
  | STBX;  v = regle_store_x      { `Stbx v }
  | LSL;   v = regle_shift        { `Lsl v }
  | LSR;   v = regle_shift        { `Lsr v }
  | ROL;   v = regle_shift        { `Rol v }
  | ASR;   v = regle_shift        { `Asr v }
  | B;     v = regle_branch_inc   { `B v }
  | BA;    v = regle_branch_inc   { `Ba v }
  | BL;    v = regle_branch_inc   { `Bl v }
  | BLA;   v = regle_branch_inc   { `Bla v }
  | BEQ;   v = regle_branch_cond  { `Beq v }
  | BNE;   v = regle_branch_cond  { `Bne v }
  | BGE;   v = regle_branch_cond  { `Bge v }
  | BGT;   v = regle_branch_cond  { `Bgt v }
  | BLE;   v = regle_branch_cond  { `Ble v }
  | BLT;   v = regle_branch_cond  { `Blt v }
  | BVS;   v = regle_branch_cond  { `Bvs v }
  | BVC;   v = regle_branch_cond  { `Bvc v }
  | BLR;   v = regle_branch_lr    { `Blr }
  | BTR;   v = regle_branch_reg   { `Btr v }
  | MTLR;  v = regle_transfert_lr { `Mtlr v }
  | MFLR;  v = regle_transfert_lr { `Mflr v }
  | LIH;   v = regle_chargement   { `Lih v }

regle_add:
  rd = reg_field; COMMA; rs1 = reg_field; COMMA; rs2 = reg_field    { (rd, rs1, rs2) } ;

regle_addi:
  rd = reg_field; COMMA; rs1 = reg_field; COMMA; simm5 = INT { (rd, rs1, simm5) }

regle_sub:
    rd = reg_field; COMMA; rs1 = reg_field; COMMA; rs2 = reg_field    { (rd, rs1, rs2) } ;

regle_and:
  v = regle_operation_logique { v } ;

regle_or:
  v = regle_operation_logique { v } ;

regle_operation_logique:
  rd = reg_field; COMMA; rs1 = reg_field; COMMA; rs2 = reg_field    { (rd, rs1, rs2) } ;

regle_load:
  rd = reg_field; COMMA; LEFT_PAREN; rs1 = reg_field; PLUS; c = INT; RIGHT_PAREN    { (rd, rs1, c) } ;

regle_load_x:
  rd = reg_field; COMMA; LEFT_PAREN; rs1 = reg_field; PLUS; rs2 = reg_field; RIGHT_PAREN    { (rd, rs1, rs2) } ;

regle_store:
  LEFT_PAREN; rs1 = reg_field; PLUS; simm5 = INT; RIGHT_PAREN; COMMA; rd = reg_field    { (rs1, simm5, rd) } ;

regle_store_x:
  LEFT_PAREN; rs1 = reg_field; PLUS; rs2 = reg_field; RIGHT_PAREN; COMMA; rd = reg_field    { (rs1, rs2, rd) } ;

regle_shift:
  rd = reg_field; COMMA; rs1 = reg_field; COMMA; uimm4 = INT    { (rd, rs1, uimm4) } ;

regle_branch_inc:
  simm11 = INT { simm11 }

regle_branch_cond:
  simm9 = INT { simm9 }

regle_branch_reg:
  rn = reg_field { rn }

regle_branch_lr:
  { "lr" }

regle_transfert_lr:
  rn = reg_field { rn }

regle_chargement:
  reg1 = reg_field; COMMA; v = INT { (reg1, v) } ;

reg_fields:
  regs = separated_list(COMMA, reg_field)    { regs } ;

reg_fields_3:
  reg1 = reg_field; COMMA; reg2 = reg_field; COMMA; reg3 = reg_field    { (reg1, reg2, reg3) } ;

reg_field:
  | REG_R0                                 { "r0" } ;
  | REG_R1                                 { "r1" } ;
  | REG_R2                                 { "r2" } ;
  | REG_R3                                 { "r3" } ;
  | REG_R4                                 { "r4" } ;
  | REG_R5                                 { "r5" } ;
  | REG_R6                                 { "r6" } ;
  | REG_R7                                 { "r7" } ;
  | REG_LR                                 { "lr" } ;
  | REG_PC                                 { "pc" } ;
