module tsp
  use const, only: dp
  use check_util, only: check_err
  use pretty_print, only: print_matrix
  implicit none
  private
  public tsp_cost, tsp_read_tsplib

contains
  real(dp) function tsp_cost(tour, cost_mat) result(cost)
    integer, intent(in) :: tour(:)
    real(dp), intent(in) :: cost_mat(:,:)

    integer :: n,i

    n = size(tour) - 1
    
    cost = 0.0
    do i = 1, n
       cost = cost + cost_mat(tour(i), tour(i+1))
    end do
    return
  end function tsp_cost

  subroutine tsp_read_tsplib(tsplib_file, cost_mat)
    character(len=*), intent(in) :: tsplib_file
    real(dp), allocatable, intent(out) :: cost_mat(:,:)

    ! Locals
    character(*), parameter :: EOF = "EOF"
    character(*), parameter :: COORDSEC = "NODE_COORD_SECTION"
    character(*), parameter :: SEP = ":"
    character(*), parameter :: SIZE_KEY = "DIMENSION"

    character(len=1024) :: line
    character(len=18) :: key
    character(len=1005) :: field
    integer :: tsplib

    integer :: mat_size
    integer :: err

    integer, allocatable :: coords(:,:)
    integer :: i, j, dummy_i

    open(newunit=tsplib, file=tsplib_file, status="old", action="read")

    do while (.true.)
       read(tsplib, '(a)') line
       if (line(:3) == EOF) return

       i = scan(line, set=SEP)
       print *, "i", i, trim(line)

       if (i == 0) then
          ! No separator, so must be COORDSEC.
          key = line(:len(key))
       else
          key = line(:i-2)
          field = line(i+2:)
       end if
       
       if (trim(key) == COORDSEC) exit
       print *, trim(key), ",", trim(field)

       ! Read DIMENSION
       if (trim(key) == SIZE_KEY) read(field, *) mat_size
    end do

    if (allocated(cost_mat)) deallocate(cost_mat)
    allocate(cost_mat(mat_size, mat_size), stat=err)
    call check_err(err, "Failed to allocate cost_mat.")

    allocate(coords(2,mat_size), stat=err)
    call check_err(err, "Failed to allocate coords.")
        
    do i = 1,mat_size
       read(tsplib,*) dummy_i, coords(1,i), coords(2,i)
    end do

    print *, "DEBUG coords"
    call print_matrix(coords)
    
    ! Calc euclidean distance rounded to nearest integer.
    do j = 1,mat_size
       do i = 1,mat_size
          cost_mat(i, j) = real(nint(norm2(real(coords(:,i) - coords(:,j), dp))), dp)
       end do
    end do
  end subroutine tsp_read_tsplib
end module tsp

