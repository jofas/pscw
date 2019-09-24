program percolate
  !
  ! Program percolate.
  !
  ! This program searches clusters in a L x L matrix that
  ! percolate. A cell is either full (0) or empty
  ! (1 ... |empty cells|). A cell has 4 neighbor cells and
  ! builds a cluster with them (and therefore with their
  ! neighbors' neighbors, making it a recursive relation-
  ! ship). A cluster percolates, if such a cluster starts
  ! from the left most column and finds its way to the
  ! right most column.
  !

  use map_class
  use sorted_clusters_class
  use color_map_class
  use output
  use uni

  implicit none

  integer :: matrix_dimension, seed, print_n_clusters
  real :: density_of_filled_cells

  character(:), allocatable :: data_file_path, pgm_file_path

  type(Map) :: m
  type(ColorMap) :: colors
  type(SortedClusters) :: clustlist

  integer, dimension(:), allocatable :: changes_per_iteration
  integer :: cluster_num
  logical :: does_percolate

  ! DEFAULTS FOR CLI
  matrix_dimension = 20
  density_of_filled_cells = 0.40
  seed = 1564
  data_file_path = 'map.dat'
  pgm_file_path  = 'map.pgm'
  print_n_clusters = matrix_dimension ** 2

  call rinit(seed)

  m = Map(matrix_dimension, density_of_filled_cells)

  changes_per_iteration = m%build_clusters()

  does_percolate = m%does_percolate_horizontically(cluster_num)

  clustlist = SortedClusters(m)

  if (print_n_clusters > clustlist%amount_of_clusters) then
    print_n_clusters = clustlist%amount_of_clusters
  end if

  colors = ColorMap(m, clustlist%cluster_ids, print_n_clusters)

  call print_params_and_actual_density( &
    density_of_filled_cells, matrix_dimension, seed, m%true_density &
  )

  call print_iterations(changes_per_iteration)

  call print_percolation_status(does_percolate, cluster_num)

  call write_data_file(data_file_path, m%inner())

  call print_amount_of_clusters_and_size_of_biggest( &
    clustlist%amount_of_clusters, clustlist%cluster_sizes(1) &
  )

  call print_amount_of_displayed_clusters( &
    print_n_clusters, clustlist%amount_of_clusters &
  )

  call write_pgm_file( &
    pgm_file_path, colors%color_map, print_n_clusters &
  )
end
