# bibliotheque_intelligence_artificielle



**Dependencies :**

- depends on the "Batteries included" library version 3.7.1.
    ( `opam install batteries` )

After installation of batteries : `eval $(opam env)`		 

**First compilation :**

- in the `lib` directory : `touch .depend && make depend && make`

- If you want to proceed with the examples : in the `test` directory : `touch .depend && make depend && make test`

**Build the HTML documentation :**

- in `lib` directory, `mkdir html && make doc`


**Use of the native-code compiler :**
- in the lib directory : `make lib1.cmxa`
- in the test directory : `make xtest`

**OCaml language :**

- We use functors to parameterize some algorithms by a bunch of types and functions. See the documentation or the tutorial for the concepts of module, module signatures and functors :
https://v2.ocaml.org/manual/moduleexamples.html
https://ocaml.org/docs/functors

- We use labeled parameters to improve the documentations. See the documentation or the tutorial:
https://v2.ocaml.org/manual/lablexamples.html
https://ocaml.org/docs/labels



**Problèmes à la compilation (en fonction des versions des outils et bibliothèques utilisés).**

- Si la commande `ocamlfind query batteries` ne trouve pas la bibliothèque batteries alors que vous l'avez installée, faites `eval $(opam env)` dans la console avant de lancer les autres commandes. Après ceci, la commande `ocamlfind query batteries` indique le répertoire dans lequel cette bibliothèque est installée. Vous pouvez ajouter `eval $(opam env)` dans votre `.bashrc` ou équivalent.

- Si le compilateur vous dit qu'il ne trouve pas `nums.cma`, il faut le localiser sur votre installation et mettre le chemin complet à la place du chemin relatif sur la ligne de commande (changer dans le `Makefile`). .

- Idem pour `batteries.cma`.

- Si le compilateur ne trouve pas `batteries_unthreaded.cma` et que ce fichier n'a pas été installé avec la bibliothèque batteries, retirez simplement ce fichier de la ligne de commande. (Explication : vous utilisez une version de la bibliothèque `batteries` antérieure à 3.7.1)

- Si lors de la compilation de l'exemple, le compilateur se plaint de ne pas trouver `nums.cma`, ajouter dans la variable `LIBRARIES` du `Makefile` le chemin complet vers ce fichier, juste avant l'autre bibliothèque.

