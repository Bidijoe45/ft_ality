NAME = ft_ality

NC_EXECUTABLE = $(NAME).nc
TEST_EXECUTABLE = test_runner

OCAMLOPT = ocamlopt
OCAMLC = ocamlc
OCAMLDEP = ocamldep
OCAMLFIND = ocamlfind

SOURCE_DIR = src
TEST_DIR = test

REQUIRED_PACKAGES = tsdl
EXTERNAL_LIBS = tsdl
LIBS = $(foreach lib,$(EXTERNAL_LIBS),-package $(lib))

# Main program sources
SOURCES = $(SOURCE_DIR)/main.ml \
          $(SOURCE_DIR)/lexer.ml

# Sources used for testing (exclude main.ml)
TESTABLE_SOURCES = $(SOURCE_DIR)/lexer.ml

# Test-specific sources
TEST_SOURCES = $(TEST_DIR)/test_lexer.ml \
               $(TEST_DIR)/test.ml

INCLUDES = -I $(SOURCE_DIR)

COBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

all: $(NAME)

$(NAME): check-dependencies nc
	ln -sf $(NC_EXECUTABLE) $(NAME)

nc: $(NC_EXECUTABLE)

$(NC_EXECUTABLE): $(OPTOBJS)
	@echo "Building native code..."
	$(OCAMLFIND) $(OCAMLOPT) $(LIBS) $(INCLUDES) -linkpkg -o $@ $^

# --- Test Runner ---
TEST_OBJS = $(TESTABLE_SOURCES:.ml=.cmx) $(TEST_SOURCES:.ml=.cmx)

$(TEST_EXECUTABLE): $(TEST_OBJS)
	@echo "Building test runner..."
	$(OCAMLFIND) $(OCAMLOPT) $(LIBS) $(INCLUDES) -I $(TEST_DIR) -linkpkg -o $@ $^

$(TEST_DIR)/%.cmx: $(TEST_DIR)/%.ml
	$(OCAMLFIND) $(OCAMLOPT) $(LIBS) $(INCLUDES) -I $(TEST_DIR) -c $<

$(TEST_DIR)/%.cmo: $(TEST_DIR)/%.ml
	$(OCAMLFIND) $(OCAMLC) $(LIBS) $(INCLUDES) -I $(TEST_DIR) -c $<

.PHONY: test
test: $(TEST_EXECUTABLE)
	./$(TEST_EXECUTABLE)

# --- Dependency and Build Rules ---
check-dependencies:
	@echo "Checking for required OPAM packages..."
	@for pkg in $(REQUIRED_PACKAGES); do \
		if ! opam list --installed --short | grep -q "^$$pkg$$"; then \
			echo "Installing missing package: $$pkg"; \
			opam install -y $$pkg; \
		else \
			echo "Package $$pkg is already installed."; \
		fi \
	done

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) $(LIBS) $(INCLUDES) -c $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) $(LIBS) $(INCLUDES) -c $<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(LIBS) $(INCLUDES) -c $<

DEPEND_FILE = .depend

depend: $(DEPEND_FILE)

$(DEPEND_FILE): $(SOURCES) $(TESTABLE_SOURCES) $(TEST_SOURCES)
	@echo "Generating dependency file..."
	$(OCAMLDEP) $(INCLUDES) -I $(TEST_DIR) $^ > $(DEPEND_FILE)

clean:
	@echo "Cleaning..."
	rm -f $(SOURCE_DIR)/*.cm[iox] $(SOURCE_DIR)/*.o
	rm -f $(TEST_DIR)/*.cm[iox] $(TEST_DIR)/*.o
	rm -f $(NAME) $(NC_EXECUTABLE) $(TEST_EXECUTABLE) $(DEPEND_FILE)

re: clean all

-include $(DEPEND_FILE)

.PHONY: all check-dependencies nc clean re depend