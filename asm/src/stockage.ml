open Instset;;

exception Erreur of string;;

type etat_de_l_assembleur =
  {
    mutable pc: int;
    mutable code: instruction array;
    table_etiq: (string, int) Hashtbl.t;
    mutable a_resoudre: (int * string) list
  };;

let asm =
  {
    pc = 0;
    code = [||];
    table_etiq = Hashtbl.create 17;
    a_resoudre = []
  };;

let initialise () =
  asm.pc <- 0;
  asm.code <- Array.make 100 Stop;
  Hashtbl.clear asm.table_etiq;
  asm.a_resoudre <- [];;

let decode_adresse adr = adr / taille_du_mot;;

let assemble instruction =
  if asm.pc >= Array.length asm.code then
    begin
      let nouveau_code = Array.make (2 * Array.length asm.code) Stop in
      Array.blit asm.code 0 nouveau_code 0 (Array.length asm.code);
      asm.code <- nouveau_code
    end;
    asm.code.(decode_adresse asm.pc) <- instruction;
    asm.pc <- asm.pc + taille_du_mot;;

let definir_etiquette nom_etiq val_etiq =
  try
    Hashtbl.find asm.table_etiq nom_etiq;
    raise (Erreur ("etiquette " ^ nom_etiq ^ " redefinie"))
  with Not_found ->
    Hashtbl.add asm.table_etiq nom_etiq val_etiq;;

let poser_etiquette nom_etiq = definir_etiquette nom_etiq asm.pc;;

let valeur_etiquette nom_etiq =
  try
    Hashtbl.find asm.table_etiq nom_etiq
  with Not_found ->
    asm.a_resoudre <- (asm.pc, nom_etiq) :: asm.a_resoudre;
    0;;

let resoudre_etiquette (adresse, nom_etiq) =
  let valeur =
    try
      Hashtbl.find asm.table_etiq nom_etiq
    with Not_found ->
      raise (Erreur ("etiquette " ^ nom_etiq ^ " indefinie")) in
    let nouvelle_instruction =
      match asm.code.(decode_adresse adresse) with
      | Op(operation, reg1, _, reg2) -> Op(operation, reg1, Imm valeur, reg2)
      | Jmp(_, reg) -> Jmp(Imm valeur, reg)
      | Braz(reg, _) -> Braz(reg, valeur)
      | Branz(reg, _) -> Branz(reg, valeur)
      | _ -> raise (Erreur "resoudre_etiquette") in
    asm.code.(decode_adresse adresse) <- nouvelle_instruction;;

let extraire_code () =
  List.iter resoudre_etiquette asm.a_resoudre;
  Array.sub asm.code 0 (decode_adresse asm.pc);;
