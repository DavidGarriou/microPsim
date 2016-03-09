open Instset;;

exception Erreur of string * int;;

val lire_memoire : int -> int;;
val ecrire_memoire : int -> int -> unit;;
val lire_registre : int -> int;;
val ecrire_registre : int -> int -> unit;;
val tab_syscalls : (int -> int) array;;

val execute_programme : instruction array -> int -> unit;;
