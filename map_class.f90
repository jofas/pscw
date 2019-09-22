module map_class
  use uni

  implicit none

  private

  type, public :: Map
    integer, dimension(:, :), allocatable :: map
    integer :: inner_size
    real    :: true_density
  contains
    procedure :: build_clusters
    procedure :: does_percolate_horizontically
    procedure :: inner
  end type

  interface Map
    module procedure new
  end interface

contains

  type(Map) function new(inner_size, density) result(self)
    integer, intent(in) :: inner_size
    real,    intent(in) :: density

    integer :: free_cell_counter, i, j

    allocate(self%map(0:inner_size + 1, 0:inner_size + 1))
    self%map(:, :) = 0

    free_cell_counter = 0
    do i = 1, inner_size
      do j = 1, inner_size
        if (random_uniform() > density) then
          free_cell_counter = free_cell_counter + 1
          self%map(i, j) = free_cell_counter
        end if
      end do
    end do

    self%inner_size   = inner_size
    self%true_density = float(free_cell_counter) &
                      / float(inner_size ** 2)
  end

  function build_clusters(self) result(changes_per_iteration)
    class(Map), intent(inout) :: self
    integer, dimension(:), allocatable :: changes_per_iteration

    integer, dimension(:),    allocatable :: changes_per_iteration_temp
    integer, dimension(:, :), allocatable :: left, right, top, bottom, old_map

    integer :: changes, i, max_iter

    allocate(left, right, top, bottom, old_map, mold = self%map)

    max_iter = self%inner_size * 2
    allocate(changes_per_iteration_temp(max_iter))

    do i = 1, max_iter
      changes = 0
      old_map = self%map

      left   = cshift(self%map, shift = -1, dim = 1)
      right  = cshift(self%map, shift = 1,  dim = 1)
      top    = cshift(self%map, shift = -1, dim = 2)
      bottom = cshift(self%map, shift = 1,  dim = 2)

      where (self%map /= 0)
        where (self%map < left)   self%map = left
        where (self%map < right)  self%map = right
        where (self%map < top)    self%map = top
        where (self%map < bottom) self%map = bottom
      end where

      changes = sum(self%map - old_map)
      changes_per_iteration_temp(i) = changes

      if (changes == 0) exit
    end do

    allocate(changes_per_iteration(i))
    changes_per_iteration(:) = changes_per_iteration_temp(:i)

    deallocate(left, right, top, bottom, old_map)
    deallocate(changes_per_iteration_temp)
  end

  logical function does_percolate_horizontically(self, cluster_num) &
      result(does_percolate)

    class(Map), intent(in) :: self
    integer, optional, intent(out) :: cluster_num

    integer :: i, j

    cluster_num    = 0
    does_percolate = .false.

    do i = 1, self%inner_size
      if (self%map(i, 1) > 0) then
        do j = 1, self%inner_size
          if (self%map(i, 1) == self%map(j, self%inner_size)) then
            cluster_num = self%map(i, 1)
            does_percolate = .true.
            exit
          end if
        end do
      end if
    end do
  end

  function inner(self) result(inner_map)
    class(Map), intent(in) :: self
    integer, dimension(self%inner_size, self%inner_size) :: inner_map
    inner_map = self%map(1:self%inner_size, 1:self%inner_size)
  end
end
