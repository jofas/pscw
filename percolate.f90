program percolate
  use sort
  use util

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

  ! the grid
  integer, dimension(:, :), allocatable :: map

  integer :: i, j

  integer :: ncluster, max_cluster_size
  integer, parameter :: fmtlen = 32
  character(len=fmtlen)  :: fmtstring
  integer, dimension(:), allocatable :: fileline
  integer, dimension(:, :), allocatable :: clustlist
  integer :: iounit = 12
  integer, dimension(:), allocatable :: color

  integer, dimension(:), allocatable :: cluster_rank
  integer :: color_from_rank

  real :: density

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
  call init_map(L, rho, map, density)
  call build_clusters(map, changes_per_iteration)
  call does_percolate_horizontically(map, cluster_num, does_percolate)

  write (*, *) 'Parameters are rho=', rho, ', L=', L, ', seed=', seed

  write (*, *) 'rho = ', rho, ', actual density = ', density

  do i = 1, size(changes_per_iteration)
    write (*, *) 'Number of changes on loop ', i, ' is ', &
      changes_per_iteration(i)
  end do

  if (does_percolate) then
    write (*, *) 'Cluster DOES percolate. Cluster number: ', cluster_num
  else
    write (*, *) 'Cluster DOES NOT percolate'
  end if

  ! this part writes data file {{{

  write (*, *) 'Opening file ', datafile
  open (unit=iounit, file=datafile)
  write (*, *) 'Writing data ...'

  write (fmtstring, fmt='(''('', i3, ''(1x, I4))'')') L

  do j = L, 1, -1
    write (iounit, fmt=fmtstring) map(:, j)
  end do

  write (*, *) '...done'
  close (iounit)
  write (*, *) 'File closed'

  ! }}}

  ! does something with the clusters
  allocate (clustlist(2, L*L))

  clustlist(1, :) = 0
  clustlist(2, :) = (/ (i, i = 1, L * L) /)

  forall (i = 1:L, j = 1:L, map(i, j) > 0)
     clustlist(1, map(i, j)) = clustlist(1, map(i, j)) + 1
  end forall

  call clustlist_sort(clustlist, L * L)

  ncluster = 0
  do while (ncluster < L*L .and. clustlist(1, ncluster + 1) > 0)
    ncluster = ncluster + 1
  end do

  if (amount_of_clusters > ncluster) then
    amount_of_clusters = ncluster
  end if

  allocate(cluster_rank(L * L))
  do i = 1, ncluster
    cluster_rank(clustlist(2, i)) = i
  end do

  max_cluster_size = clustlist(1, 1)

  ! some stdout statistics about clusters {{{
  write (*, *) 'Map has ', ncluster, &
    ' clusters, maximum cluster size is ', max_cluster_size

  if (amount_of_clusters == 1) then
    write (*, *) 'Displaying the largest cluster'
  else if (amount_of_clusters == ncluster) then
    write (*, *) 'Displaying all clusters'
  else
    write (*, *) 'Displaying largest ', amount_of_clusters, ' clusters'
  end if
  ! }}}

  deallocate(clustlist)

  write (*, *) 'Opening file ', percfile
  open (unit=iounit, file=percfile)
  write (*, *) 'Writing data ...'

  ! write .pgm header {{{
  write (fmtstring, fmt='(''('', i3, ''(1x, I4))'')') L

  write (iounit, fmt='(''P2'')')

  write (iounit, *) L, L

  if (amount_of_clusters > 0) then
    write (iounit, *) amount_of_clusters
  else
    write (iounit, *) 1
  end if
  ! }}}

  ! write .pgm data {{{
  allocate(color(L))

  do j = L, 1, -1
    color(:) = amount_of_clusters
    do i = 1, L
      color_from_rank = cluster_rank(map(i, j)) - 1
      if (map(i, j) > 0 .and. color_from_rank < amount_of_clusters) then
        color(i) = cluster_rank(map(i, j)) - 1
      end if
    end do
    write (iounit, fmt=fmtstring) color
  end do

  deallocate(color)
  deallocate (cluster_rank)
  ! }}}

  write (*, *) '...done'
  close (iounit)
  write (*, *) 'File closed'

  deallocate (map)
end

