# Intelligence Artificielle - Rubik's Cube 2x2x2

# Auteurs:
- PIPON Romain [ romain.pipon@etu.univ-nantes.fr ]
- LEBRETON Thomas [ thomas.lebreton@etu.univ-nantes.fr ]

## Dépendances :

- Dépend de la version 3.7.1 de la bibliothèque "Batteries included" : `(opam install batteries)`

Après l'installation de Batteries et à chaque ouverture du projet : `eval $(opam env)`

## Commandes Make :

### Lib
`make lib`
Permet de construire les fichiers de bibliothèque utilisés dans le projet.

### rubiks
`make rubiks`
Construit les fichiers nécessaires à l'exécution du projet. Il s'assure que les fichiers de bibliothèque sont à jour.

### run
`make run`
Exécute les tests du projet après avoir construit les fichiers nécessaires au projet.

### doc
`make doc`
Génère la documentation HTML du projet à partir des commentaires du code source. La documentation est placée dans le dossier **'doc'** à la racine du projet.

### clean
`make clean`
Nettoie les fichiers générés lors de la construction, supprime les dépendances et le dossier de documentation.
