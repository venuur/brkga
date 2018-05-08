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

$(OBJ)/tsp.o: $(SRC)/tsp.f08 $(OBJ)/const.mod $(OBJ)/check_util.mod $(OBJ)/pretty_print.mod

$(OBJ)/pretty_print.o: $(SRC)/pretty_print.f08 $(OBJ)/const.mod
	$(COMPILE) -c $< -o $@

$(OBJ)/brkga.o: $(SRC)/brkga.f08 $(OBJ)/const.mod $(OBJ)/check_util.mod $(OBJ)/sort.mod
	$(COMPILE) -c $< -o $@

$(OBJ)/brkga_tsp.o: $(SRC)/brkga_tsp.f08 $(OBJ)/const.mod $(OBJ)/check_util.mod $(OBJ)/brkga.mod $(OBJ)/sort.mod $(OBJ)/tsp.mod $(OBJ)/pretty_print.mod
	$(COMPILE) -c $< -o $@

$(OBJ)/test_brkga.o: $(SRC)/test_brkga.f08 $(OBJ)/const.mod $(OBJ)/check_util.mod $(OBJ)/pretty_print.mod $(OBJ)/brkga.mod $(OBJ)/sort.mod $(OBJ)/tsp.mod $(OBJ)/brkga_tsp.mod
	$(COMPILE) -c $< -o $@

$(OBJ)/test_tsplib_read.o: $(SRC)/test_tsplib_read.f08 $(OBJ)/const.mod $(OBJ)/tsp.mod $(OBJ)/pretty_print.mod
	$(COMPILE) -c $< -o $@

$(OBJ)/solve_tsplib.o: $(SRC)/solve_tsplib.f08 $(OBJ)/const.mod $(OBJ)/check_util.mod $(OBJ)/tsp.mod $(OBJ)/pretty_print.mod
	$(COMPILE) -c $< -o $@

$(BIN)/test_brkga: $(OBJ)/test_brkga.o $(OBJ)/const.o $(OBJ)/check_util.o $(OBJ)/pretty_print.o $(OBJ)/brkga.o $(OBJ)/sort.o $(OBJ)/tsp.o $(OBJ)/brkga_tsp.o
	$(LINK) $^ -o $@

$(BIN)/test_rng: $(OBJ)/test_rng.o $(OBJ)/const.o
	$(LINK) $^ -o $@

$(BIN)/test_tsplib_read: $(OBJ)/test_tsplib_read.o $(OBJ)/const.o $(OBJ)/tsp.o $(OBJ)/check_util.o $(OBJ)/pretty_print.o
	$(LINK) $^ -o $@

$(BIN)/solve_tsplib: $(OBJ)/solve_tsplib.o $(OBJ)/const.o $(OBJ)/check_util.o $(OBJ)/tsp.o $(OBJ)/pretty_print.o
	$(LINK) $^ -o $@

test: $(BIN)/test_brkga $(BIN)/test_rng $(BIN)/test_tsplib_read

tsp: $(BIN)/solve_tsplib

clean:
	rm -fv $(OBJ)/*.o
	rm -fv $(OBJ)/*.mod
	rm -fv $(BIN)/*.exe

