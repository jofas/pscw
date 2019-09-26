FC=           gfortran
FFLAGS=       -O3
EXE=          percolate
INSTALL_PATH= /usr/bin/
FSRC=         uni.f90 map_class.f90 sorted_clusters_class.f90 \
			        color_map_class.f90 help.f90 io.f90 percolate.f90
OBJ=          $(FSRC:.f90=.o)


.PHONY: all
all: $(EXE) clean

.PHONY: install
install:
	@cp $(EXE) $(INSTALL_PATH)

.PHONY: clean
clean:
	@rm *.mod *.o
	@echo Done with cleanup.

$(EXE): $(OBJ)
	@$(FC) $^ -o $(EXE) $(FFLAGS)
	@echo Done making $(EXE).

$(OBJ): $(FSRC)
	@$(FC) -c $^
