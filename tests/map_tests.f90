module map_tests
  !
  ! Module containing unit tests for the public methods of
  ! the Map object.
  !

  use fruit
  use map_class
  use uni
  use tests_util

  implicit none

  integer, dimension(5, 5) :: GENERATED_MAP = reshape( &
    [  1,  2,  3,  0,  0 &
    ,  4,  5,  0,  0,  0 &
    ,  6,  7,  8,  9,  0 &
    ,  0, 10,  0,  0, 11 &
    ,  0,  0,  0,  0, 12 ], [5, 5], order=[2,1] )

  real :: GENERATED_MAP_TRUE_DENSITY = 12.0 / 25.0

contains

  subroutine test_map_constructor()
    type(Map) :: m

    call rinit(1564)

    m = Map(5, .5)

    call matrices_equal(m%inner(), GENERATED_MAP)

    call assert_equals( &
      m%true_density, GENERATED_MAP_TRUE_DENSITY &
    )
  end


  subroutine test_build_clusters()
    type(Map) :: m
    integer, dimension(:), allocatable :: changes_per_iteration

    m = init_map1()
    changes_per_iteration = m%build_clusters()
    call matrices_equal(m%map, MAP1_CLUSTERED)

    m = init_map2()
    changes_per_iteration = m%build_clusters()
    call matrices_equal(m%map, MAP2_CLUSTERED)
  end


  subroutine test_does_percolate_horizontically()
    type(Map) :: m
    integer :: cluster_num

    m = build_map1()
    call assert_equals( &
      m%does_percolate_horizontically(cluster_num), &
      MAP1_DOES_PERCOLATE &
    )
    call assert_equals(cluster_num, MAP1_CLUSTER_THAT_PERCOLATES)

    m = build_map2()
    call assert_equals( &
      m%does_percolate_horizontically(), &
      MAP2_DOES_PERCOLATE &
    )
  end

  subroutine test_inner()
    type(Map) :: m

    m = init_map1()
    call matrices_equal(m%inner(), MAP1_INNER)

    m = init_map2()
    call matrices_equal(m%inner(), MAP2_INNER)
  end
end
