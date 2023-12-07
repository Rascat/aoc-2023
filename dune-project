(lang dune 3.12)

(name aoc_2023)

(generate_opam_files true)

(source
 (github Rascat/aoc_2023))

(authors "Rascat")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc_2023)
 (synopsis "A short synopsis")
 (description "My solutions to the Advent of Code 2023")
 (depends ocaml dune re base stdio (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
