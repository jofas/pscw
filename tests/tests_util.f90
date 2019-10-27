module tests_util
  !
  ! Module containing utility routines and constants for
  ! the use within this test suite.
  !
  ! It contains two matrices (MAP1, MAP2), their
  ! clustered counterparts and some constants describing
  ! their behavior, e.g. MAP1 does percolate, MAP2 not.
  !

  use fruit
  use map_class

  implicit none

  integer, dimension(0:11, 0:11), parameter :: MAP1 = reshape( &
    [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 &
    , 0,  1,  2,  0,  0,  3,  0,  0,  0,  4,  0,  0 &
    , 0,  0,  5,  0,  6,  7,  0,  0,  8,  0,  0,  0 &
    , 0,  9, 10,  0,  0, 11,  0,  0, 12,  0,  0,  0 &
    , 0, 13,  0,  0,  0, 14, 15,  0,  0,  0,  0,  0 &
    , 0, 16, 17, 18,  0,  0, 19, 20,  0,  0,  0,  0 &
    , 0,  0,  0, 21, 22,  0,  0, 23, 24,  0,  0,  0 &
    , 0, 25, 26, 27,  0,  0,  0,  0, 28, 29, 30,  0 &
    , 0,  0,  0, 31, 32, 33,  0, 34, 35,  0,  0,  0 &
    , 0,  0,  0,  0,  0, 36, 37, 38,  0,  0,  0,  0 &
    , 0, 39,  0,  0,  0, 40,  0,  0, 41,  0,  0,  0 &
    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], &
    shape(MAP1), order=[2, 1])

  integer, dimension(0:11, 0:11) :: MAP1_CLUSTERED = reshape( &
    [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 &
    , 0, 40, 40,  0,  0, 40,  0,  0,  0,  4,  0,  0 &
    , 0,  0, 40,  0, 40, 40,  0,  0, 12,  0,  0,  0 &
    , 0, 40, 40,  0,  0, 40,  0,  0, 12,  0,  0,  0 &
    , 0, 40,  0,  0,  0, 40, 40,  0,  0,  0,  0,  0 &
    , 0, 40, 40, 40,  0,  0, 40, 40,  0,  0,  0,  0 &
    , 0,  0,  0, 40, 40,  0,  0, 40, 40,  0,  0,  0 &
    , 0, 40, 40, 40,  0,  0,  0,  0, 40, 40, 40,  0 &
    , 0,  0,  0, 40, 40, 40,  0, 40, 40,  0,  0,  0 &
    , 0,  0,  0,  0,  0, 40, 40, 40,  0,  0,  0,  0 &
    , 0, 39,  0,  0,  0, 40,  0,  0, 41,  0,  0,  0 &
    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], &
    shape(MAP1), order=[2, 1])

  integer, dimension(10, 10) :: MAP1_COLOR_MAP_1_CLUSTER = reshape( &
    [  0,  0,  1,  1,  0,  1,  1,  1,  1,  1 &
    ,  1,  0,  1,  0,  0,  1,  1,  1,  1,  1 &
    ,  0,  0,  1,  1,  0,  1,  1,  1,  1,  1 &
    ,  0,  1,  1,  1,  0,  0,  1,  1,  1,  1 &
    ,  0,  0,  0,  1,  1,  0,  0,  1,  1,  1 &
    ,  1,  1,  0,  0,  1,  1,  0,  0,  1,  1 &
    ,  0,  0,  0,  1,  1,  1,  1,  0,  0,  0 &
    ,  1,  1,  0,  0,  0,  1,  0,  0,  1,  1 &
    ,  1,  1,  1,  1,  0,  0,  0,  1,  1,  1 &
    ,  1,  1,  1,  1,  0,  1,  1,  1,  1,  1 ], &
    [10, 10], order=[2, 1])

  integer, dimension(10, 10) :: MAP1_COLOR_MAP_ALL_CLUSTERS = reshape( &
    [  0,  0,  5,  5,  0,  5,  5,  5,  4,  5 &
    ,  5,  0,  5,  0,  0,  5,  5,  1,  5,  5 &
    ,  0,  0,  5,  5,  0,  5,  5,  1,  5,  5 &
    ,  0,  5,  5,  5,  0,  0,  5,  5,  5,  5 &
    ,  0,  0,  0,  5,  5,  0,  0,  5,  5,  5 &
    ,  5,  5,  0,  0,  5,  5,  0,  0,  5,  5 &
    ,  0,  0,  0,  5,  5,  5,  5,  0,  0,  0 &
    ,  5,  5,  0,  0,  0,  5,  0,  0,  5,  5 &
    ,  5,  5,  5,  5,  0,  0,  0,  5,  5,  5 &
    ,  3,  5,  5,  5,  0,  5,  5,  2,  5,  5 ], &
    [10, 10], order=[2, 1])

  logical :: MAP1_DOES_PERCOLATE = .true.

  integer :: MAP1_CLUSTER_THAT_PERCOLATES = 40

  integer, dimension(10, 10) :: MAP1_INNER = MAP1(1:10, 1:10)

  integer :: MAP1_AMOUNT_OF_FREE_CELLS = 41

  integer :: MAP1_AMOUNT_OF_CLUSTERS = 5

  integer, dimension(5) :: MAP1_SORTED_IDS = &
    [40, 12, 41, 39, 4]

  integer, dimension(5) :: MAP1_SORTED_SIZES = &
    [36, 2, 1, 1, 1]


  integer, dimension(0:11, 0:11), parameter :: MAP2 = reshape( &
    [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 &
    , 0,  1,  2,  0,  0,  3,  0,  0,  0,  4,  0,  0 &
    , 0,  0,  5,  0,  6,  7,  0,  0,  8,  0,  0,  0 &
    , 0,  9, 10,  0,  0, 11,  0,  0, 12,  0,  0,  0 &
    , 0, 13,  0,  0,  0, 14, 15,  0,  0,  0,  0,  0 &
    , 0, 16, 17, 18,  0,  0, 19, 20,  0,  0,  0,  0 &
    , 0,  0,  0, 21, 22,  0,  0, 23, 24,  0, 30,  0 &
    , 0, 25, 26, 27,  0,  0,  0,  0, 28, 29,  0,  0 &
    , 0,  0,  0, 31, 32, 33,  0, 34, 35,  0,  0,  0 &
    , 0,  0,  0,  0,  0, 36, 37, 38,  0,  0,  0,  0 &
    , 0, 39,  0,  0,  0, 40,  0,  0, 41,  0,  0,  0 &
    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], &
    shape(MAP1), order=[2, 1])

  integer, dimension(0:11, 0:11) :: MAP2_CLUSTERED = reshape( &
    [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 &
    , 0, 40, 40,  0,  0, 40,  0,  0,  0,  4,  0,  0 &
    , 0,  0, 40,  0, 40, 40,  0,  0, 12,  0,  0,  0 &
    , 0, 40, 40,  0,  0, 40,  0,  0, 12,  0,  0,  0 &
    , 0, 40,  0,  0,  0, 40, 40,  0,  0,  0,  0,  0 &
    , 0, 40, 40, 40,  0,  0, 40, 40,  0,  0,  0,  0 &
    , 0,  0,  0, 40, 40,  0,  0, 40, 40,  0, 30,  0 &
    , 0, 40, 40, 40,  0,  0,  0,  0, 40, 40,  0,  0 &
    , 0,  0,  0, 40, 40, 40,  0, 40, 40,  0,  0,  0 &
    , 0,  0,  0,  0,  0, 40, 40, 40,  0,  0,  0,  0 &
    , 0, 39,  0,  0,  0, 40,  0,  0, 41,  0,  0,  0 &
    , 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], &
    shape(MAP1), order=[2, 1])

  integer, dimension(10, 10) :: MAP2_COLOR_MAP_1_CLUSTER = reshape( &
    [  0,  0,  1,  1,  0,  1,  1,  1,  1,  1 &
    ,  1,  0,  1,  0,  0,  1,  1,  1,  1,  1 &
    ,  0,  0,  1,  1,  0,  1,  1,  1,  1,  1 &
    ,  0,  1,  1,  1,  0,  0,  1,  1,  1,  1 &
    ,  0,  0,  0,  1,  1,  0,  0,  1,  1,  1 &
    ,  1,  1,  0,  0,  1,  1,  0,  0,  1,  1 &
    ,  0,  0,  0,  1,  1,  1,  1,  0,  0,  1 &
    ,  1,  1,  0,  0,  0,  1,  0,  0,  1,  1 &
    ,  1,  1,  1,  1,  0,  0,  0,  1,  1,  1 &
    ,  1,  1,  1,  1,  0,  1,  1,  1,  1,  1 ], &
    [10, 10], order=[2, 1])

  integer, dimension(10, 10) :: MAP2_COLOR_MAP_ALL_CLUSTERS = reshape( &
    [  0,  0,  6,  6,  0,  6,  6,  6,  5,  6 &
    ,  6,  0,  6,  0,  0,  6,  6,  1,  6,  6 &
    ,  0,  0,  6,  6,  0,  6,  6,  1,  6,  6 &
    ,  0,  6,  6,  6,  0,  0,  6,  6,  6,  6 &
    ,  0,  0,  0,  6,  6,  0,  0,  6,  6,  6 &
    ,  6,  6,  0,  0,  6,  6,  0,  0,  6,  4 &
    ,  0,  0,  0,  6,  6,  6,  6,  0,  0,  6 &
    ,  6,  6,  0,  0,  0,  6,  0,  0,  6,  6 &
    ,  6,  6,  6,  6,  0,  0,  0,  6,  6,  6 &
    ,  3,  6,  6,  6,  0,  6,  6,  2,  6,  6 ], &
    [10, 10], order=[2, 1])

  logical :: MAP2_DOES_PERCOLATE = .false.

  integer, dimension(10, 10) :: MAP2_INNER = MAP2(1:10, 1:10)

  integer :: MAP2_AMOUNT_OF_FREE_CELLS = 41

  integer :: MAP2_AMOUNT_OF_CLUSTERS = 6

  integer, dimension(6) :: MAP2_SORTED_IDS = &
    [40, 12, 41, 39, 30, 4]

  integer, dimension(6) :: MAP2_SORTED_SIZES = &
    [35, 2, 1, 1, 1, 1]

contains

  subroutine matrices_equal(m1, m2)
    integer, dimension(:, :), intent(in) :: m1, m2
    logical, dimension(:, :), allocatable :: l

    allocate(l(size(m1, dim=1), size(m1, dim=2)))

    l(:, :) = m1(:, :) /= m2(:, :)
    call assert_equals(count(l), 0)
  end


  subroutine vectors_equal(v1, v2)
    integer, dimension(:), intent(in) :: v1, v2
    logical, dimension(:), allocatable :: l

    allocate(l(size(v1)))

    l(:) = v1(:) /= v2(:)
    call assert_equals(count(l), 0)
  end


  function init_map1() result(m)
    type(Map) :: m
    m = Map(MAP1, inner_size=10, true_density=41.0 / 100.0)
  end


  function init_map2() result(m)
    type(Map) :: m
    m = Map(MAP2, inner_size=10, true_density=41.0 / 100.0)
  end


  function build_map1() result(m)
    type(Map) :: m
    integer, dimension(:), allocatable :: x_

    m  = init_map1()
    x_ = m%build_clusters()
  end


  function build_map2() result(m)
    type(Map) :: m
    integer, dimension(:), allocatable :: x_

    m  = init_map2()
    x_ = m%build_clusters()
  end
end
