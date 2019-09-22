module color_map_class
  use map_class

  implicit none

  private

  type, public :: ColorMap
    integer, dimension(:, :), allocatable :: color_map
  end type

  interface ColorMap
    module procedure new
  end interface

contains

  type(ColorMap) function new(m, cluster_sizes, n_biggest_clusters) &
      result(self)
    type(Map), intent(in) :: m
    integer, intent(in)   :: n_biggest_clusters
    integer, dimension(m%inner_size ** 2), intent(in) :: cluster_sizes

    integer, dimension(m%inner_size ** 2) :: cluster_rank
    integer :: i, j

    do i = 1, n_biggest_clusters
      cluster_rank(cluster_sizes(i)) = i
    end do

    allocate(self%color_map(m%inner_size, m%inner_size))

    self%color_map(:, :) = n_biggest_clusters
    forall (j = 1:m%inner_size, i=1:m%inner_size, m%map(i, j) > 0 &
        .and. cluster_rank(m%map(i, j)) - 1 < n_biggest_clusters)
      self%color_map(i, j) = cluster_rank(m%map(i, j)) - 1
    end forall
  end
end
