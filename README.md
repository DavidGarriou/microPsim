# microPsim

microPsim est un simulateur de microcontrôleur simple.
Le jeu d'instruction initial est une simplification du jeu d'instruction MIPS.

Projet destiné à explorer le langage Objective Caml

Ce projet démarre des exemples du livre [Le langage Caml] de Pierre Weis et Xavier Leroy.
Ce livre est dorénavant disponible sous la licence [Creative Commons BY-NC-SA]

[Le langage Caml]: http://caml.inria.fr/distrib/books/llc.pdf
[Creative Commons BY-NC-SA]: http://creativecommons.org/licenses/by-nc-sa/2.0/fr/legalcode

Le projet consiste d'abord à adapter les exemples du livre vers Ocaml.

Le projet n'est pas encore fonctionnel.

## Todo

* Réaliser le parser : travail en cours avec la librairie "menhir"
* Terminer l'assembleur
* Tester le simulateur sur plusieurs exemples sans appel système
* Ajouter les appels systèmes au simulateur

## Simulateur

Le simulateur ne permet pas de réaliser les appels systèmes spécifiés dans le livre.

### Compilation
Dans sim/src:

```ocamlc -c instset.mli```

```ocamlc -c instset.ml```

```ocamlc -c simul.mli```

```ocamlc -c simul.ml```

```ocamlc -c microP.ml```

```ocamlc -o microP_exe instset.cmo simul.cmo microP.cmo```

## Assembleur

L'assembleur n'est pas fonctionnel.
Il manque le parser.

## Parser

