module io
  !
  ! Module for printing (stdout), writing (files)
  ! and the command line interface.
  !

  use help

  implicit none

  public

  integer, private                  :: IOUNIT  = 12
  integer, parameter, private       :: STR_LEN = 32
  character(len = STR_LEN), private :: FMT_STRING
  character(len = STR_LEN), private :: CLI_OPT
  character(len = STR_LEN), private :: CLI_IN

  type :: CLIResults
    integer :: matrix_dimension = 20
    integer :: seed = 1564
    integer :: print_n_clusters = 20 ** 2
    real    :: density_of_filled_cells = .4
    character(:), allocatable :: data_file_path
    character(:), allocatable :: pgm_file_path
  end type

  private write_pgm_header
  private parse_command_to_int
  private parse_command_to_real
  private read_command_value

contains

  type(CLIResults) function read_from_cli() result(cli)
    !
    ! Function reading the command line arguments.
    !
    ! Use percolate -h for more infos.
    !

    integer :: i

    logical :: print_n_clusters_set = .false.

    cli%data_file_path = "map.dat"
    cli%pgm_file_path  = "map.pgm"

    i = 1
    do
      call get_command_argument(i, CLI_OPT)

      select case(CLI_OPT)

        case("--length", "-l")
          call read_command_value(i)

          cli%matrix_dimension = parse_command_to_int()

          if (.not. print_n_clusters_set) then
            cli%print_n_clusters = cli%matrix_dimension ** 2
          end if

        case("--density", "-d")
          call read_command_value(i)

          cli%density_of_filled_cells = parse_command_to_real()

        case("--seed", "-s")
          call read_command_value(i)

          cli%seed = parse_command_to_int()

        case("--print_n_clusters", "-p")
          call read_command_value(i)

          cli%print_n_clusters = parse_command_to_int()

          print_n_clusters_set = .true.

        case("--data_file_path")
          call read_command_value(i)
          cli%data_file_path = trim(cli_in)

        case("--pgm_file_path")
          call read_command_value(i)
          cli%pgm_file_path = trim(cli_in)

        case("--help", "-h")
          call write_help_msg()
          stop

        case("")
          exit

        case default
          write (*, *) "Command line arguments are wrong. &
                       See -h, --help for further information."
          stop
      end select

      if (i == command_argument_count()) exit
      i = i + 1
    end do
  end


  subroutine read_command_value(i)
    integer, intent(inout) :: i

    i = i + 1
    call get_command_argument(i, CLI_IN)
  end


  integer function parse_command_to_int() result(res)
    integer :: stat

    read (CLI_IN, *, iostat = stat) res

    if (stat /= 0) then
      write (*, *) "Could not parse: ", trim(CLI_OPT), &
                   ": ", CLI_IN
      stop
    end if
  end


  real function parse_command_to_real() result(res)
    integer :: stat

    read (CLI_IN, *, iostat = stat) res

    if (stat /= 0) then
      write (*, *) "Could not parse: ", trim(CLI_OPT), &
                   ": ", CLI_IN
      stop
    end if
  end


  subroutine write_help_msg()
    integer :: i

    do i = 1, size(HELP_MSG)
      write (*, *) HELP_MSG(i)
    end do
  end


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
