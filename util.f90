module util
  use uni

  implicit none

contains

  subroutine init_map(L, rho, map, density)
    integer, intent(in) :: L
    real, intent(in) :: rho
    integer, dimension(:, :), allocatable, intent(out) :: map
    real, intent(out) :: density

    integer :: free_cell_counter, i, j

    allocate(map(0:L + 1, 0:L + 1))
    map(:, :) = 0

    free_cell_counter = 0
    do i = 1, L
      do j = 1, L
        if (random_uniform() > rho) then
          free_cell_counter =  free_cell_counter + 1
          map(i, j) =  free_cell_counter
        end if
      end do
    end do

    density = float(free_cell_counter) / float(L * L)
  end

  subroutine build_clusters(map, changes_per_iteration)
    integer, dimension(:, :), intent(inout) :: map

    integer, dimension(:), allocatable, intent(out) :: changes_per_iteration
    integer, dimension(:), allocatable :: changes_per_iteration_temp

    integer, dimension(:, :), allocatable :: left, right, top, bottom
    integer, dimension(:, :), allocatable :: old_map

    integer :: changes, i, max_iter

    allocate(left, right, top, bottom, old_map, mold = map)

    max_iter = size(map, dim=1) * 2

    allocate(changes_per_iteration_temp(max_iter))

    do i = 1, max_iter
      changes = 0
      old_map = map

      left   = cshift(map, shift = -1, dim = 1)
      right  = cshift(map, shift = 1,  dim = 1)
      top    = cshift(map, shift = -1, dim = 2)
      bottom = cshift(map, shift = 1,  dim = 2)

      where (map /= 0)
        where (map < left)   map = left
        where (map < right)  map = right
        where (map < top)    map = top
        where (map < bottom) map = bottom
      end where

      changes = sum(map - old_map)
      changes_per_iteration_temp(i) = changes

      if (changes == 0) exit
    end do

    allocate(changes_per_iteration(i))
    changes_per_iteration(:) = changes_per_iteration_temp(:i)

    deallocate(left, right, top, bottom, old_map)
    deallocate(changes_per_iteration_temp)
  end

  subroutine does_percolate_horizontically(map, cluster_num, does_percolate)
    integer, dimension(:, :), allocatable :: map

    integer, intent(out) :: cluster_num
    logical, intent(out) :: does_percolate

    integer :: i, j
    integer :: L

    L = size(map, dim=1) - 1

    write (*,*) L

    cluster_num = 0
    does_percolate = .false.

    do i = 1, L
      if (map(i, 1) > 0) then
        do j = 1, L
          if (map(i, 1) == map(j, L)) then
            cluster_num = map(i, 1)
            does_percolate = .true.
            exit
          end if
        end do
      end if
    end do
  end
end
