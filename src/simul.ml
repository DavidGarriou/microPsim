open Instset;;

exception Erreur of string * int;;

(* Types de donnees *)
type etat_proc =
  { registres: int array;
    mutable pc: int;
    mutable code: instruction array;
    mutable mem: int array };;

(* Variable d'état de la machine *)
let microP =
  { registres = Array.make nombre_de_registres 0;
    pc = 0;
    code = [| |];
    mem = [| |] };;

(* Fonctions d'accès aux registres et à la mémoire *)
let lire_registre reg =
  if reg < 0 || reg > nombre_de_registres then
    raise (Erreur ("registre illégal", reg));
  microP.registres.(reg);;

let ecrire_registre reg valeur =
  if reg < 0 || reg > nombre_de_registres then
    raise (Erreur ("registre illégal", reg));
  if reg <> 0 then microP.registres.(reg) <- valeur;;

let lire_instruction adresse =
  let adr = adresse/taille_du_mot in
  if adr < 0 || adr >= Array.length microP.code then
    raise (Erreur ("acces en dehors de la zone de code", adr));
  if (adr mod taille_du_mot) <> 0 then
    raise (Erreur ("pc non aligné", adr));
  microP.code.(adr);;

let lire_memoire adresse =
  let adr = adresse/taille_du_mot in
  if adr < 0 || adr > Array.length microP.mem then
    raise (Erreur ("acces en dehors de la zone de code", adr));
  if adr mod taille_du_mot <> 0 then
    raise (Erreur ("pc non aligné", adr));
  microP.mem.(adr);;

let ecrire_memoire adresse valeur =
let adr = adresse/taille_du_mot in
if adr < 0 || adr > Array.length microP.mem then
  raise (Erreur ("écriture en dehors de la zone de code", adr));
if adr mod taille_du_mot <> 0 then
  raise (Erreur ("pc non aligné", adr));
microP.mem.(adr) <- valeur;;

let valeur_operande = function
  Reg r -> lire_registre r
  | Imm n -> n;;

(* Appels systèmes *)
let tab_syscalls =
  Array.make 10 ((function x -> x) : int -> int);;

let syscall appel argument =
  if appel < 0 || appel > Array.length tab_syscalls then
    raise (Erreur ("Appel système inexistant", appel))
  else
    tab_syscalls.(appel) argument;;

(* Moteur de simulation *)
exception Arret;;

(*
add(r1,o,r2)   : r2 <- r1 + o
sub(r1,o,r2)   : r2 <- r1 - o
mult(r1,o,r2)  : r2 <- r1 * O
div(r1,o,r2)   : r2 <- r1 / O
and(r1,o,r2)   : r2 <- r1 et O
or(r1,o,r2)    : r2 <- r1 ou O
xor(r1,o,r2)   : r2 <- r1 xor O
shl(r1,o,r2)   : r2 <- r1 décalé à gauche de o bits
shr(r1,o,r2)   : r2 <- r1 décalé à droite de o bits
slt(r1,o,r2)   : r2 <- 1 si r1 < o, 0 sinon
sle(r1,o,r2)   : r2 <- 1 si r1 =< o, 0 sinon
seq(r1,o,r2)   : r2 <- 1 si r1 = o, 0 sinon
load(r1,o,r2)  : r2 <- contenu de @(r1 + o)
store(r1,o,r2) : contenu(r2) est écrit à @(r1 + o)
jmp(o,r)       : saute à @(o) et stocke l'@ de l'instruction suivant le jmp dans r
braz(r,a)      : saute à @(a) si r = 0
branz(r,a)     : saute à @(a) si r <> 0
scall(n)       : appel système, numéro n
stop           : fin du programme
*)
let cycle_horloge () =
  let instruction = lire_instruction microP.pc in
  microP.pc <- microP.pc + taille_du_mot;
  match instruction with
  | Op(operation, reg1, operande, reg2) ->
      let arg1 = lire_registre reg1
      and arg2 = valeur_operande operande in
      begin match operation with
      | Load  -> ecrire_registre reg2 (lire_memoire (arg1 + arg2))
      | Store -> ecrire_memoire (arg1 + arg2) (lire_registre reg2)
      | Add   -> ecrire_registre reg2 (arg1 + arg2)
      | Mult  -> ecrire_registre reg2 (arg1 * arg2)
      | Sub   -> ecrire_registre reg2 (arg1 - arg2)
      | Div   -> ecrire_registre reg2 (arg1 / arg2)
      | And   -> ecrire_registre reg2 (arg1 land arg2)
      | Or    -> ecrire_registre reg2 (arg1 lor arg2)
      | Xor   -> ecrire_registre reg2 (arg1 lxor arg2)
      | Shl   -> ecrire_registre reg2 (arg1 lsl arg2)
      | Shr   -> ecrire_registre reg2 (arg1 lsr arg2)
      | Slt   -> ecrire_registre reg2 (if arg1 < arg2 then 1 else 0)
      | Sle   -> ecrire_registre reg2 (if arg1 <= arg2 then 1 else 0)
      | Seq   -> ecrire_registre reg2 (if arg1 = arg2 then 1 else 0)
      end
  | Jmp(operande, reg) -> ecrire_registre reg microP.pc;
                          microP.pc <- valeur_operande operande
  | Braz(reg, valeur) -> if (lire_registre reg) = 0 then microP.pc <- valeur
  | Branz(reg, valeur) -> if (lire_registre reg) <> 0 then microP.pc <- valeur
  | Scall(appel_systeme) -> ecrire_registre 1 (syscall appel_systeme (lire_registre 1))
  | Stop -> raise Arret;;

let execute_programme prog mem =
  let nb_mots = mem / 4 in
  microP.code <- prog;
  microP.pc <- 0;
  microP.mem <- Array.make (nb_mots / 4) 0;
  microP.registres.(0) <- 0;
  microP.registres.(sp) <- nb_mots * taille_du_mot;
  try while true do cycle_horloge() done
  with Arret -> ();;
