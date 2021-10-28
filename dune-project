(lang dune 2.9)

(name chat_botte)

(generate_opam_files true)

(package
 (name chat_botte)
 (synopsis "A discord bot")
 (description "A discord that is socled")
 (depends
  (dune
   (>= 2.9))))
(using menhir 2.1)
