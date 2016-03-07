#open "instset";;

(* Types de donnees *)
type etat_proc =
  { registres: int vect;
    mutable pc: int;
    mutable code: instruction vect;
    mutable mem: int vect };;

(* Variable d'état de la machine *)
let pico =
  { registres = make_vect nombre_de_registres 0;
    pc = 0;
    code = [| |];
    mem = [| |] };;

(* Fonctions d'accès aux registres et à la mémoire *)
let lire_registre reg =
  if reg < 0 or reg > nombre_de_registres then
    raise (Erreur ("registre illégal", reg));
  pico.registres.(reg);;

let ecrire_registre reg valeur =
  if reg < 0 or reg > nombre_de_registres then
    raise (Erreur ("registre illégal", reg));
  if reg <> 0 then pico.registres.(reg) <- valeur;;

let lire_instruction adresse =
  let adr = adresse/taille_du_mot in
  if adr < 0 or adr >= vect_length pico.code then
    raise (Erreur ("acces en dehors de la zone de code", adr))
  if adr mod taille_du_mot <> 0 then
    raise (Erreur ("pc non aligné", adr))
  pico.code.(adr);;

let lire_memoire adresse =
  let adr = adresse/taille_du_mot in
  if adr < 0 or adr > vect_length pico.mem then
    raise (Erreur ("acces en dehors de la zone de code", adr))
  if adr mod taille_du_mot <> 0 then
    raise (Erreur ("pc non aligné", adr))
  pico.mem.(adr);;

let ecrire_memoire adresse valeur =
let adr = adresse/taille_du_mot in
if adr < 0 or adr > vect_length pico.mem then
  raise (Erreur ("écriture en dehors de la zone de code", adr))
if adr mod taille_du_mot <> 0 then
  raise (Erreur ("pc non aligné", adr))
pico.mem.(adr) <- valeur;;

let valeur_operande = function
  Reg r -> lire_registre r
  | Imm n -> n;;

(* Appels systèmes *)
let tab_syscalls =
  make_vect 10 ((function x -> x) : int -> int);;

let syscall appel argument =
  if appel < 0 or appel > vect_length tab_syscalls then
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
  let instruction = lire_instruction pico.pc in
  pico.pc <- pico.pc + taille_du_mot;
  match instruction with
    Op(operation, reg1, operande, reg2) ->
      let arg1 = lire_registre reg1
      and arg2 = lire_registre reg2
      and
    | Jmp(operande, reg) ->
    | Braz(reg, val) ->
    | Branz(reg, val) ->
    | Scall(appel_systeme) ->
    | Stop ->
