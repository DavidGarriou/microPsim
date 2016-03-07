#open "instset";;

exception Erreur of string * int;;

value lire_memoire : int -> int;;
value ecrire_memoire : int -> int -> unit;;
value lire_registre : int -> int;;
value ecrire_registre : int -> int -> unit;;
value tab_syscalls : (int -> int) vect;;

value execute : instruction vect -> int -> unit;;
