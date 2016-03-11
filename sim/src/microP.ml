open Instset
open Simul

exception Fichier_incorrect;;

let execute_fichier fichier mem =
  let canal = open_in_bin fichier in
  let programme =
    try (input_value canal : instruction array)
    with Failure _ -> raise Fichier_incorrect in
  execute_programme programme mem;;

exception Mauvais_arguments;;

if !Sys.interactive then () else
try
  if Array.length Sys.argv < 2 then raise Mauvais_arguments;
  let taille_memoire =
    if Array.length Sys.argv < 3
    then 1024
    else
      try int_of_string Sys.argv.(2)
      with Failure _ -> raise Mauvais_arguments in
  execute_fichier Sys.argv.(1) (taille_du_mot * taille_memoire);
  exit 0
with Mauvais_arguments ->
      prerr_endline "Usage: pico_run <fichier> [taille memoire]";
      exit 2
  | Fichier_incorrect ->
      prerr_endline "Le fichier ne contient pas du code executable";
      exit 2
  | Erreur(message, param) ->
      prerr_string "Erreur a l’execution: ";
      prerr_string message;
      prerr_string " ("; prerr_int param; prerr_endline ")";
      exit 2
  | Sys_error message ->
      prerr_string "Erreur du systeme: ";
      prerr_endline message;
      exit 2;;
