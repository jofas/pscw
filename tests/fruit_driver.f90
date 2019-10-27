! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

program fruit_driver
  use fruit
  use map_tests

  call init_fruit()

  call test_build_clusters()
  call test_does_percolate_horizontically()
  call test_inner()

  call fruit_summary()
  call fruit_finalize()

end
