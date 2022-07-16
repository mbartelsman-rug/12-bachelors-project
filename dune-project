(lang dune 3.1)

(name ActorsAndChannels)

(generate_opam_files true)

(source
 (github mbartelsmanm-rug/skeletal-actors-and-channels))

(authors "Miguel Bartelsman")

(maintainers "Miguel Bartelsman")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ActorsAndChannels)
 (synopsis "Interpreters and translators for an actors- and a channels-based langauge.")
 (description "Interpreters and translators for an actors- and a channels-based langauge, built using skeletal semantics and necro.")
 (depends ocaml dune)
 (tags
  ("skeletal semantics" skel necro language interpreter)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
