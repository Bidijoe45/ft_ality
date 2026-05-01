RESULT = ft_ality
SOURCE_DIR = src
SOURCES = $(SOURCE_DIR)/grammar.mli \
		  $(SOURCE_DIR)/grammar.ml \
          $(SOURCE_DIR)/automaton.mli \
          $(SOURCE_DIR)/automaton.ml \
          $(SOURCE_DIR)/main.ml
LIBS = unix

OCAMLFLAGS += -I +unix
OCAMLCFLAGS += -I +unix
OCAMLOPTFLAGS += -I +unix
OCAMLLDFLAGS += -I +unix

OCAMLMAKEFILE = OCamlMakefile

-include OCamlMakefile
