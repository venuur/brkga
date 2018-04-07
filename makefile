SRC = .
OBJ = obj
BIN = bin
COMPILE = gfortran.exe -J$(OBJ)
LINK = gfortran.exe -I$(OBJ)

# Default compilation rule, override for files with dependencies.
$(OBJ)/%.o: $(SRC)/%.f08
	$(COMPILE) -c $< -o $@

$(OBJ)/%.mod: $(OBJ)/%.o
	echo "Need " $@ " so compile " $<

$(OBJ)/rng.o: $(SRC)/rng.f08 $(OBJ)/const.mod
	$(COMPILE) -c $< -o $@

$(OBJ)/pretty_print.o: $(SRC)/pretty_print.f08 $(OBJ)/const.mod
	$(COMPILE) -c $< -o $@

$(OBJ)/brkga.o: $(SRC)/brkga.f08 $(OBJ)/const.mod $(OBJ)/check_util.mod
	$(COMPILE) -c $< -o $@

$(OBJ)/test_brkga.o: $(SRC)/test_brkga.f08 $(OBJ)/const.mod $(OBJ)/check_util.mod $(OBJ)/pretty_print.mod $(OBJ)/brkga.mod
	$(COMPILE) -c $< -o $@

$(BIN)/test_brkga: $(OBJ)/test_brkga.o $(OBJ)/const.o $(OBJ)/check_util.o $(OBJ)/pretty_print.o $(OBJ)/brkga.o
	$(LINK) $^ -o $@

$(BIN)/test_rng: $(OBJ)/test_rng.o $(OBJ)/const.o
	$(LINK) $^ -o $@

test: $(BIN)/test_brkga $(BIN)/test_rng

clean:
	rm -fv $(OBJ)/*.o
	rm -fv $(BIN)/*.exe

