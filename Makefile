RESULT = ft_ality
SOURCE_DIR = src
SOURCES = $(SOURCE_DIR)/grammar.ml \
          $(SOURCE_DIR)/automaton.mli \
          $(SOURCE_DIR)/automaton.ml \
          $(SOURCE_DIR)/main.ml
PACKS = Unix
OCAMLMAKEFILE = OCamlMakefile

-include OCamlMakefile