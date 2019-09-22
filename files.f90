module files

  implicit none

  integer                  :: iounit  = 12
  integer, parameter       :: fmt_len = 32
  character(len = fmt_len) :: fmt_string

contains

  subroutine write_data_file(path, map)
    character(:), allocatable,     intent(in) :: path
    integer,      dimension(:, :), intent(in) :: map

    integer :: L, j

    L = size(map, dim=1)

    write (*, *) 'Opening file ', path
    open (unit=iounit, file=path)
    write (*, *) 'Writing data ...'

    write (fmt_string, fmt='(''('', i3, ''(1x, I4))'')') L

    do j = L, 1, -1
      write (iounit, fmt=fmt_string) map(:, j)
    end do

    write (*, *) '...done'
    close (iounit)
    write (*, *) 'File closed'
  end

  subroutine write_pgm_file(path, colors, amount_of_clusters)
    character(:), allocatable,     intent(in) :: path
    integer,      dimension(:, :), intent(in) :: colors
    integer,                       intent(in) :: amount_of_clusters

    integer :: L, j

    L = size(colors, dim=1)

    write (*, *) 'Opening file ', path
    open (unit=iounit, file=path)
    write (*, *) 'Writing data ...'

    ! write .pgm header {{{
    write (fmt_string, fmt='(''('', i3, ''(1x, I4))'')') L

    write (iounit, fmt='(''P2'')')

    write (iounit, *) L, L

    if (amount_of_clusters > 0) then
      write (iounit, *) amount_of_clusters
    else
      write (iounit, *) 1
    end if
    ! }}}

    do j = L, 1, -1
      write (iounit, fmt=fmt_string) colors(:, j)
    end do

    write (*, *) '...done'
    close (iounit)
    write (*, *) 'File closed'
  end
end
