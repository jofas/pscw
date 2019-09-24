module output
  !
  ! Module for printing (stdout) and writing (files)
  ! information.
  !

  implicit none

  public

  integer, private                  :: IOUNIT  = 12
  integer, parameter, private       :: FMT_LEN = 32
  character(len = FMT_LEN), private :: FMT_STRING

  private write_pgm_header

contains

  subroutine write_data_file(path, inner_map)
    !
    ! Subroutine writing the inner_map to file (path).
    !
    ! It writes the matrix column wise, beginning with the
    ! last column.
    !

    character(:), allocatable,     intent(in) :: path
    integer,      dimension(:, :), intent(in) :: inner_map

    integer :: inner_map_size, j

    inner_map_size = size(inner_map, dim=1)

    write (*, *) 'Opening file ', path
    open (unit=IOUNIT, file=path)
    write (*, *) 'Writing data ...'

    write (FMT_STRING, fmt='(''('', i3, ''(1x, I4))'')') &
      inner_map_size

    do j = inner_map_size, 1, -1
      write (IOUNIT, fmt=FMT_STRING) inner_map(:, j)
    end do

    write (*, *) '...done'
    close (IOUNIT)
    write (*, *) 'File closed'
  end


  subroutine write_pgm_file(path, colors, amount_of_clusters)
    !
    ! Subroutine writing the colors matrix to a .pgm file
    ! (path).
    !
    ! Like write_data_file, the colors matrix is written
    ! column wise, beginning with the last column.
    !

    character(:), allocatable,     intent(in) :: path
    integer,      dimension(:, :), intent(in) :: colors
    integer,                       intent(in) :: amount_of_clusters

    integer :: color_size, j

    color_size = size(colors, dim=1)

    write (*, *) 'Opening file ', path
    open (unit=IOUNIT, file=path)
    write (*, *) 'Writing data ...'

    call write_pgm_header(amount_of_clusters, color_size)

    do j = color_size, 1, -1
      write (IOUNIT, fmt=FMT_STRING) colors(:, j)
    end do

    write (*, *) '...done'
    close (IOUNIT)
    write (*, *) 'File closed'
  end


  subroutine write_pgm_header(amount_of_clusters, color_size)
    !
    ! Subroutine writing some header information to the
    ! .pgm file (handled by IOUNIT).
    !

    integer, intent(in) :: amount_of_clusters, color_size

    write (FMT_STRING, fmt='(''('', i3, ''(1x, I4))'')') &
      color_size

    write (IOUNIT, fmt='(''P2'')')

    write (IOUNIT, *) color_size, color_size

    if (amount_of_clusters > 0) then
      write (IOUNIT, *) amount_of_clusters
    else
      write (IOUNIT, *) 1
    end if
  end


  subroutine print_params_and_actual_density( &
    rho, L, seed, true_density &
  )
    real, intent(in)    :: rho, true_density
    integer, intent(in) :: L, seed

    write (*, *) 'Parameters are rho=', rho, ', L=', L, &
                 ', seed=', seed
    write (*, *) 'rho = ', rho, ', actual density = ', true_density
  end


  subroutine print_iterations(changes_per_iteration)
    integer, dimension(:), intent(in) :: changes_per_iteration

    integer :: i

    do i = 1, size(changes_per_iteration)
      write (*, *) 'Number of changes on loop ', i, ' is ', &
                   changes_per_iteration(i)
    end do
  end


  subroutine print_percolation_status(does_percolate, cluster_num)
    logical, intent(in) :: does_percolate
    integer, intent(in) :: cluster_num

    if (does_percolate) then
      write (*, *) 'Cluster DOES percolate. Cluster number: ', &
                   cluster_num
    else
      write (*, *) 'Cluster DOES NOT percolate'
    end if
  end


  subroutine print_amount_of_clusters_and_size_of_biggest( &
    amount, biggest_size &
  )
    integer, intent(in) :: amount, biggest_size

    write (*, *) 'Map has ', amount, &
      ' clusters, maximum cluster size is ', biggest_size
  end


  subroutine print_amount_of_displayed_clusters(print_n, actual_n)
    integer, intent(in) :: print_n, actual_n

    if (print_n == 1) then
      write (*, *) 'Displaying the largest cluster'
    else if (print_n == actual_n) then
      write (*, *) 'Displaying all clusters'
    else
      write (*, *) 'Displaying largest ', print_n, ' clusters'
    end if
  end
end
