FC=           gfortran
FFLAGS=       -O3
EXE=          percolate
INSTALL_PATH= /usr/bin/
FSRC=         src/uni.f90 src/map_class.f90 src/sorted_clusters_class.f90 \
			        src/color_map_class.f90 src/help.f90 src/io.f90 src/percolate.f90
OBJ=          $(FSRC:src/%.f90=%.o)

.PHONY: all
all: $(EXE) clean

.PHONY: install
install:
	@cp $(EXE) $(INSTALL_PATH)

.PHONY: clean
clean:
	@rm *.mod *.o
	@echo Done with cleanup.

.PHONY: help
help:
	@echo "all:     compile $(EXE). This is the default rule"
	@echo "         that gets executed."
	@echo
	@echo "clean:   remove all object and module files generated"
	@echo	"         during the build process."
	@echo
	@echo "install: installs $(EXE) to a PATH directory."
	@echo "         Default is $(INSTALL_PATH). Can be changed"
	@echo "         with the variable INSTALL_PATH."
	@echo
	@echo "help:    displays this help message."
	@echo

$(EXE): $(OBJ)
	@$(FC) $^ -o $(EXE) $(FFLAGS)
	@echo Done making $(EXE).

$(OBJ): $(FSRC)
	@$(FC) -c $^
