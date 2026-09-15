(lang dune 3.24)

(name bulls-and-cows)
(version 166e085-dirty)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name bulls-and-cows)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  batteries)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
