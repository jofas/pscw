program percolate
  use sorted_clusters_class
  use map_class
  use color_map_class
  use files
  use uni

  implicit none

  ! dimensions of grid
  ! CLI PARAM
  integer :: L

  ! density of filled cells (cells not part of a cluster)
  ! CLI PARAM
  real :: rho

  ! seed for pseudo random number generator
  ! CLI PARAM
  integer :: seed

  ! amount of clusters shown in the graphics file
  ! CLI PARAM
  integer :: amount_of_clusters

  ! name of data file
  ! CLI PARAM
  character(:), allocatable :: datafile

  ! name of graphics file
  ! CLI PARAM
  character(:), allocatable :: percfile

  type(Map) :: m
  type(ColorMap) :: colors

  integer :: i

  type(SortedClusters) :: clustlist

  integer, dimension(:), allocatable :: changes_per_iteration

  integer :: cluster_num
  logical :: does_percolate

  ! DEFAULTS FOR CLI
  L = 20
  rho = 0.40
  seed = 1564
  datafile = 'map.dat'
  percfile = 'map.pgm'
  amount_of_clusters = L * L

  call rinit(seed)

  m                     = Map(L, rho)
  changes_per_iteration = m%build_clusters()
  does_percolate        = m%does_percolate_horizontically(cluster_num)

  clustlist = SortedClusters(m%inner())

  if (amount_of_clusters > clustlist%clusters_count) then
    amount_of_clusters = clustlist%clusters_count
  end if

  colors = ColorMap(m, clustlist%clusters(2, :), amount_of_clusters)

  write (*, *) 'Parameters are rho=', rho, ', L=', L, ', seed=', seed
  write (*, *) 'rho = ', rho, ', actual density = ', m%true_density

  do i = 1, size(changes_per_iteration)
    write (*, *) 'Number of changes on loop ', i, ' is ', &
      changes_per_iteration(i)
  end do

  if (does_percolate) then
    write (*, *) 'Cluster DOES percolate. Cluster number: ', cluster_num
  else
    write (*, *) 'Cluster DOES NOT percolate'
  end if

  call write_data_file(datafile, m%inner())

  write (*, *) 'Map has ', clustlist%clusters_count, &
    ' clusters, maximum cluster size is ', clustlist%max_cluster_size

  if (amount_of_clusters == 1) then
    write (*, *) 'Displaying the largest cluster'
  else if (amount_of_clusters == clustlist%clusters_count) then
    write (*, *) 'Displaying all clusters'
  else
    write (*, *) 'Displaying largest ', amount_of_clusters, ' clusters'
  end if

  call write_pgm_file(percfile, colors%color_map, amount_of_clusters)
end
