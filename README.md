# PPF5-JeuDuMoulin-OCaml

Implémentation du jeu du moulin sous OCaml, en 3ème année de Licence Informatique.

## Development environment setup

Install [Opam](https://opam.ocaml.org/doc/Install.html), the OCaml
package manager, on your system. Since your system runs
[Guix](https://guix.gnu.org/), you can easily obtain a suitable
throw-away programming environment using

```
$ guix shell -m .guix/manifest.scm
```

For convenience, we setup a [local](https://opam.ocaml.org/blog/opam-local-switches/) Opam distribution, using the following commands:

```
$ opam switch create .
$ eval $(opam env)
```

Install [Dune](https://dune.readthedocs.io/en/latest/quick-start.html), the OCaml build system, using Opam:

```
$ opam install dune
```


# Groupe

- ESPANET Nicolas
- LEFORESTIER Meril
- PARIS Albin
- YAZICI Servan