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
  ! It should be noted here, that, while the percolation is
  ! searched from left to right, the output files genera-
  ! ted, display the matix bottom to top. That means, the
  ! last column is displayed as the first row.
  !

  use map_class
  use sorted_clusters_class
  use color_map_class
  use io
  use uni

  implicit none

  type(CLIResults) :: cli
  type(Map) :: m
  type(ColorMap) :: colors
  type(SortedClusters) :: sorted_clusters

  integer, dimension(:), allocatable :: changes_per_iteration
  integer :: cluster_num
  logical :: does_percolate

  cli = read_from_cli()

  call rinit(cli%seed)

  m = Map(cli%matrix_dimension, cli%density_of_filled_cells)

  changes_per_iteration = m%build_clusters()

  does_percolate = m%does_percolate_horizontically(cluster_num)

  sorted_clusters = SortedClusters(m)

  if (cli%print_n_clusters > sorted_clusters%amount_of_clusters) then
    cli%print_n_clusters = sorted_clusters%amount_of_clusters
  end if

  colors = ColorMap( &
    m, sorted_clusters%cluster_ids, cli%print_n_clusters &
  )

  if (cli%verbose) then
    call print_params_and_actual_density( &
      cli%density_of_filled_cells, cli%matrix_dimension, &
      cli%seed, m%true_density &
    )

    call print_iterations(changes_per_iteration)

    call print_percolation_status(does_percolate, cluster_num)
  end if

  call write_data_file(cli%data_file_path, m%inner())

  if (cli%verbose) then
    call print_amount_of_clusters_and_size_of_biggest( &
      sorted_clusters%amount_of_clusters, &
      sorted_clusters%cluster_sizes(1)    &
    )

    call print_amount_of_displayed_clusters( &
      cli%print_n_clusters, sorted_clusters%amount_of_clusters &
    )
  end if

  call write_pgm_file( &
    cli%pgm_file_path, colors%color_map, cli%print_n_clusters &
  )
end
