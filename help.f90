module help
  !
  ! Module containing the message which is displayed when
  ! invoking the -h, --help option from the command line.
  !

  character(len=53), dimension(39) :: HELP_MSG = (/ &
  "percolate v0.1.0                                    ", &
  "                                                    ", &
  "Program for computing, whether any cluster in a     ", &
  "randomly generated matrix percolates.               ", &
  "                                                    ", &
  "Usage: percolate [options ...]                      ", &
  "                                                    ", &
  "                                                    ", &
  "Options:                                            ", &
  "                                                    ", &
  "    -h, --help                 Prints this help mes-", &
  "                               sage.                ", &
  "                                                    ", &
  "    -l, --length INT           Sets the dimension of", &
  "                               the matrix.          ", &
  "                               DEFAULT: 20.         ", &
  "                                                    ", &
  "    -d, --density FLOAT        Sets the density of  ", &
  "                               the full cells.      ", &
  "                               DEFAULT: 0.4.        ", &
  "                                                    ", &
  "    -s, --seed INT             Sets the seed for the", &
  "                               pseudo-random number ", &
  "                               generator.           ", &
  "                               DEFAULT: 1564.       ", &
  "                                                    ", &
  "    -p, --print_n_clusters INT Sets the amount of   ", &
  "                               clusters displayed in", &
  "                               the .pgm file.       ", &
  "                               DEFAULT: all.        ", &
  "                                                    ", &
  "        --data_file_path PATH  Set the path for the ", &
  "                               data file.           ", &
  "                               DEFAULT: map.dat     ", &
  "                                                    ", &
  "        --pgm_file_path PATH   Set the path for the ", &
  "                               .pgm file.           ", &
  "                               DEFAULT: map.pgm     ", &
  "                                                    "  &
  /)
end
