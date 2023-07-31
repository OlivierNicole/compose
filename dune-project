(lang dune 3.7)

(name compose)

(generate_opam_files true)

(source
 (github OlivierNicole/compose))

(authors "Olivier Nicole")

(maintainers "Olivier Nicole")

(license GPL-3.0-only)

(documentation https://github.com/OlivierNicole/compose)

(package
 (name compose)
 (synopsis "Automated musical imitation")
 (description "This program takes two musical works in input and outputs a new musical work in
   invention form (a baroque keyboard form with the melody rocking back and forth
   between the upper and lower voice and some degree of imitation between the
   voices), trying to mimic the style of the input works. It uses the techniques
   described by David Cope in his book *Computers and Musical Style*, 1991, chapter 4.
   The general idea is to look for motives that appear in both works (called
   \"signatures\") and to recombine signatures to compose a new piece of music while
   adhering to some constraints.")

 (depends ocaml dune (llama_midi (>= 0.1.0)) fmt ppx_deriving)
 (tags
  ("David Cope" cope music composition)))
