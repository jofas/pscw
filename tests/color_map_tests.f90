module color_map_tests
  !
  ! Module containing unit tests for the constructor (the
  ! only public interface) of the ColorMap object.
  !

  use fruit
  use color_map_class
  use tests_util

  implicit none

contains

  subroutine test_color_map1()
    type(ColorMap) :: cm

    cm = ColorMap(build_map1(), MAP1_SORTED_IDS, 1)
    call matrices_equal(cm%color_map, MAP1_COLOR_MAP_1_CLUSTER)

    cm = ColorMap( &
      build_map1(), MAP1_SORTED_IDS, MAP1_AMOUNT_OF_CLUSTERS &
    )
    call matrices_equal(cm%color_map, MAP1_COLOR_MAP_ALL_CLUSTERS)
  end


  subroutine test_color_map2()
    type(ColorMap) :: cm

    cm = ColorMap(build_map2(), MAP2_SORTED_IDS, 1)
    call matrices_equal(cm%color_map, MAP2_COLOR_MAP_1_CLUSTER)

    cm = ColorMap( &
      build_map2(), MAP2_SORTED_IDS, MAP2_AMOUNT_OF_CLUSTERS &
    )
    call matrices_equal(cm%color_map, MAP2_COLOR_MAP_ALL_CLUSTERS)
  end
end
