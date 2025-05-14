NAME = ft_ality

NC_EXECUTABLE = $(NAME).nc
TEST_EXECUTABLE = test_runner

OCAMLOPT = ocamlopt
OCAMLC = ocamlc
OCAMLFIND = ocamlfind

SRC_DIR = src
SOURCES = lexer.ml main.ml

TEST_DIR = test
TEST_SOURCES = test.ml

COBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)
TESTOBJS = $(TEST_SOURCES:.ml=.cmo)

INCLUDES = -I . -I src -I test

all: $(NAME)

$(NAME): nc
	ln -sf $(NC_EXECUTABLE) $(NAME)

nc: $(NC_EXECUTABLE)

$(NC_EXECUTABLE): $(OPTOBJS)
	$(OCAMLFIND) $(OCAMLOPT) $(LIBS:.cma=.cmxa) $(INCLUDES) -linkpkg -o $@ $^

%.cmo: src/%.ml
	$(OCAMLFIND) $(OCAMLC) $(LIBS) $(INCLUDES) -c $<

%.cmi: src/%.mli
	$(OCAMLFIND) $(OCAMLC) $(LIBS) $(INCLUDES) -c $<

%.cmx: src/%.ml
	$(OCAMLFIND) $(OCAMLOPT) $(LIBS:.cma=.cmxa) $(INCLUDES) -c $<

test: $(TEST_EXECUTABLE)

$(TEST_EXECUTABLE): $(filter-out main.cmo,$(COBJS)) $(TESTOBJS)
	$(OCAMLFIND) $(OCAMLC) $(LIBS) $(INCLUDES) -linkpkg -g $^ -o $@

%.cmo: test/%.ml
	$(OCAMLFIND) $(OCAMLC) $(LIBS) $(INCLUDES) -c $<

DEPEND_FILE = .depend

depend: $(DEPEND_FILE)

$(DEPEND_FILE): $(addprefix $(SRC_DIR)/,$(SOURCES)) $(addprefix $(TEST_DIR)/,$(TEST_SOURCES))
	$(OCAMLDEP) $(INCLUDES) $^ > $(DEPEND_FILE)

clean:
	rm -f $(SRC_DIR)/*.cm* $(SRC_DIR)/*.o
	rm -f $(TEST_DIR)/*.cm* $(TEST_DIR)/*.o
	rm -f $(NAME) $(NC_EXECUTABLE) $(TEST_EXECUTABLE)
	rm .depend

re: clean all

-include $(DEPEND_FILE)

.PHONY: all nc clean re depend test
