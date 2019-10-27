module map_tests
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
      m%does_percolate_horizontically(cluster_num), .true. &
    )
    call assert_equals(cluster_num, 40)

    m = build_map2()
    call assert_equals( &
      m%does_percolate_horizontically(), .false. &
    )
  end

  subroutine test_inner()
    type(Map) :: m

    m = init_map1()
    call matrices_equal(m%inner(), MAP1(1:10, 1:10))

    m = init_map2()
    call matrices_equal(m%inner(), MAP2(1:10, 1:10))
  end
end
