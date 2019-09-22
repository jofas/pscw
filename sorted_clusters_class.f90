module sorted_clusters_class

  implicit none

  private

  type, public :: SortedClusters
    integer, dimension(:, :), allocatable :: clusters
    integer :: clusters_count = 0
    integer :: max_cluster_size
  end type

  interface SortedClusters
    module procedure new
  end interface

contains

  type(SortedClusters) function new(map) result(self)
    integer, dimension(:, :), intent(in) :: map

    integer :: i, j, L
    L = size(map, dim=1)

    allocate(self%clusters(2, size(map)))

    self%clusters(1, :) = 0
    self%clusters(2, :) = (/ (i, i = 1, L * L) /)

    forall (i = 1:L, j = 1:L, map(i, j) > 0)
      self%clusters(1, map(i, j)) = &
        self%clusters(1, map(i, j)) + 1
    end forall

    call clustlist_sort(self%clusters, size(map))

    do i = 1, L * L
      if (self%clusters(1, i) > 0) then
        self%clusters_count = i
      else
        exit
      end if
    end do

    self%max_cluster_size = self%clusters(1, 1)
  end

  subroutine clustlist_sort(clustlist, n)

    integer,                  intent(in)    :: n
    integer, dimension(2, n), intent(inout) :: clustlist

    integer, parameter  :: intsize = 4

    call sort(clustlist, 1, n, n)

  end

  recursive subroutine sort(clustlist, begin, end, n)

    integer, intent(in) :: begin, end, n
    integer, dimension(2, n), intent(inout) :: clustlist

    integer pivot1, pivot2, left, right, pivotindex

    pivotindex = begin + (end - begin)/2

    if (end > begin) then
      call partition(clustlist, begin, end, pivotindex, n)
      call sort(clustlist, begin, pivotindex - 1, n)
      call sort(clustlist, pivotindex + 1, end, n)
    end if
  end

  subroutine partition(clustlist, begin, end, pivotindex, n)

    integer, intent(in) :: begin, end, n
    integer, intent(inout) :: pivotindex
    integer, dimension(2, n), intent(inout) :: clustlist

    integer pivot1, pivot2, left, right, i, indexpoint

    pivot1 = clustlist(1, pivotindex)
    pivot2 = clustlist(2, pivotindex)
    call swap(clustlist, end, pivotindex, n)

    left = begin
    right = end - 1
    indexpoint = left

    do i = left, right
      if (clustlist(1, i) .ge. pivot1) then
        if (clustlist(1, i) .eq. pivot1 .and. clustlist(2, i) .lt. pivot2) then
        else
          call swap(clustlist, i, indexpoint, n)
          indexpoint = indexpoint + 1
        end if
      end if
    end do

    call swap(clustlist, indexpoint, end, n)

    pivotindex = indexpoint

  end

  subroutine swap(clustlist, firstpoint, secondpoint, n)

    integer, intent(in) :: firstpoint, secondpoint, n
    integer, dimension(2, n), intent(inout) :: clustlist

    integer :: tempdata1, tempdata2

    tempdata1 = clustlist(1, firstpoint)
    tempdata2 = clustlist(2, firstpoint)
    clustlist(1, firstpoint) = clustlist(1, secondpoint)
    clustlist(2, firstpoint) = clustlist(2, secondpoint)
    clustlist(1, secondpoint) = tempdata1
    clustlist(2, secondpoint) = tempdata2

  end
end
