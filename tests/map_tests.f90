module map_tests
  !
  ! Module containing unit tests for the public methods of
  ! the Map object.
  !

  use fruit
  use map_class
  use tests_util

  implicit none

contains

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
