Résolution du probleme de la 2eme journée en prolog.
Le problème est ici : https://adventofcode.com/2023/day/2

La solution est dans le programme exemple10.pl

Pour l'executer, il faut :
consult('exemple10.pl').

parse1('chemin_vers_le_fichier/test_advent_of_code_2023_day2_part1.txt',R).

La solution est 3035

Quelques commandes :
- set_prolog_flag(double_quotes, chars).
permet de convertir une chaine de caractère ("abc") en liste de caractères

- parse0('chemin_vers_le_fichier/test_advent_of_code_2023_day2_part1.txt',R).
permet de tester le parseur

- parse1('chemin_vers_le_fichier/test4.txt',R).
permet de tester sur un jeux de donnée plus petit

