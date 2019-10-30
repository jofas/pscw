FC=           gfortran
FFLAGS=       -O3 -fopenmp
EXE=          percolate
INSTALL_PATH= /usr/bin/
MAIN_FILE=    src/percolate.f90
FSRC=         src/uni.f90 src/map_class.f90 src/sorted_clusters_class.f90 \
			        src/color_map_class.f90 src/cli_info.f90 src/io.f90
OBJ=          $(FSRC:src/%.f90=%.o)

FTST=         tests/tests_util.f90 tests/map_tests.f90 \
							tests/sorted_clusters_tests.f90 tests/color_map_tests.f90
OBJ_TST=      $(FTST:tests/%.f90=%.o)

##all:     compile $(EXE). This is the default rule that
##         gets executed when using make.
##
.PHONY: all
all: $(EXE) clean

##install: installs $(EXE) to a PATH directory. Default is
##         /usr/bin/. Can be changed with the
##         variable INSTALL_PATH.
##
.PHONY: install
install:
	@cp $(EXE) $(INSTALL_PATH)

##clean:   remove all object and module files generated
##         during the build process.
##
.PHONY: clean
clean:
	@rm *.mod *.o
	@echo Done with cleanup.

##help:    displays this help message.
##
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<

##test:    generates test executable containing unit tests.
##
.PHONY: test
test: test_aux clean
	@./test

##profile: generates $(EXE) with the -pg flag for use with
##         gprof.
##
.PHONY: profile
profile: profile_aux clean

.PHONY: test_aux
test_aux: $(OBJ) fruit.o $(OBJ_TST)
	@$(FC) $^ tests/fruit_driver.f90 -o test $(FFLAGS)
	@echo Done making test.

.PHONY: profile_aux
profile_aux: $(OBJ)
	@$(FC) $^ $(MAIN_FILE) -o $(EXE) $(FFLAGS) -pg
	@echo Done making profiler version of $(EXE).

$(EXE): $(OBJ)
	@$(FC) $^ $(MAIN_FILE) -o $(EXE) $(FFLAGS)
	@echo Done making $(EXE).

$(OBJ): $(FSRC)
	@$(FC) -c $^ $(FFLAGS)

$(OBJ_TST): $(FTST)
	@$(FC) -c $^ $(FFLAGS)

fruit.o: fruit/fruit.f90
	@$(FC) -c $^ $(FFLAGS)
