module sorted_clusters_tests
  use fruit
  use sorted_clusters_class
  use tests_util

  implicit none

contains

  subroutine test_sorted_clusters_map1()
    type(SortedClusters) :: sc

    sc = SortedClusters(build_map1())

    call assert_equals( &
      sc%amount_of_clusters, &
      MAP1_AMOUNT_OF_CLUSTERS &
    )

    call vectors_equal( &
      sc%cluster_ids(:MAP1_AMOUNT_OF_CLUSTERS), &
      MAP1_SORTED_IDS &
    )

    call vectors_equal( &
      sc%cluster_sizes(:MAP1_AMOUNT_OF_CLUSTERS), &
      MAP1_SORTED_SIZES &
    )
  end


  subroutine test_sorted_clusters_map2()
    type(SortedClusters) :: sc

    sc = SortedClusters(build_map2())

    call assert_equals( &
      sc%amount_of_clusters, &
      MAP2_AMOUNT_OF_CLUSTERS &
    )

    call vectors_equal( &
      sc%cluster_ids(:MAP2_AMOUNT_OF_CLUSTERS), &
      MAP2_SORTED_IDS &
    )

    call vectors_equal( &
      sc%cluster_sizes(:MAP2_AMOUNT_OF_CLUSTERS), &
      MAP2_SORTED_SIZES &
    )
  end


  subroutine test_sorted_clusters_with_unclustered_map()
    type(SortedClusters) :: sc
    integer :: i

    sc = SortedClusters(init_map1())

    call assert_equals( &
      sc%amount_of_clusters, MAP1_AMOUNT_OF_FREE_CELLS &
    )

    call vectors_equal( &
      sc%cluster_ids(:MAP1_AMOUNT_OF_FREE_CELLS), &
      [(i, i=MAP1_AMOUNT_OF_FREE_CELLS,1,-1)] &
    )

    call vectors_equal( &
      sc%cluster_sizes(:MAP1_AMOUNT_OF_FREE_CELLS), &
      [(1, i=1,MAP1_AMOUNT_OF_FREE_CELLS)] &
    )
  end


  subroutine test_sorted_clusters_arrays_over_amount()
    !
    ! Subroutine testing whether the not existing clusters
    ! behave correctly.
    !

    type(SortedClusters) :: sc
    integer :: i

    sc = SortedClusters(init_map1())

    call assert_equals(size(sc%cluster_ids), size(MAP1_INNER))

    call vectors_equal( &
      sc%cluster_ids(MAP1_AMOUNT_OF_FREE_CELLS + 1:), &
      [(i, i=size(MAP1_INNER),MAP1_AMOUNT_OF_FREE_CELLS,-1)] &
    )

    call vectors_equal( &
      sc%cluster_sizes(MAP1_AMOUNT_OF_FREE_CELLS + 1:), &
      [(0, i=MAP1_AMOUNT_OF_FREE_CELLS+1,size(MAP1_INNER))] &
    )
  end
end
