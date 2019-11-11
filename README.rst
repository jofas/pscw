percolate v0.1.0
================

This program searches clusters in a L x L matrix that
percolate. A cell of such a matrix can be either full or
empty. Every cell has 4 neighbor cells and builds a cluster
with them (and therefore with their neighbors' neighbors,
making it a recursive relationship). A cluster of free
cells percolates, if such a cluster starts from the top and
continues all the way down to the bottom of the matrix.


Build/install
-------------

If you have a copy of this project, open up a terminal and
type the following commands, in order to build percolate:

```
cd path/to/the/project
make
make test
```

Once you have done that, optionally, you can also install
it on your Linux/Unix/Mac operating system:

```
make install
```

Remember that you need certain privileges to install a
program. Try with ```sudo make install```, if installation
fails.

For more options and some help with building run:

```
make help
```


Run percolate
-------------

After percolate is built, you can run it, simply by typing
```./percolate``` in your terminal (```percolate``` if
you installed it system wide)


Command line interface
----------------------

You can see the command line options of percolate if you
run ```./percolate -h```.


Notes for developers
--------------------

If you'd like to contribute or extend percolate, there
is a unit-test test suite for you to use.
Compile and run it with ```make test```.
Afterwards, the root directory of this repository will
contain an executable ```test```, which executes the unit
tests and can be used for later testing.

percolate also comes with continuous integration setup for
gitlab_.
If you fork this repository, just enable gitlab_ pipelines
and everytime you commit, the unit-test test suite is
executed.

.. _gitlab: https://gitlab.com/
