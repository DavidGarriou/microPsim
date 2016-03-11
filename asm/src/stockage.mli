open Instset;;

exception Erreur of string

val initialise: unit -> unit
val assemble: instruction -> unit
val poser_etiquette: string -> unit
val valeur_etiquette: string -> int
val extraire_code: unit -> instruction array
