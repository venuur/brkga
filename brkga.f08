module brkga
  use const, only: dp
  use check_util, only: check_err
  implicit none
  private
  public brkga_solve, brkga_sort_key
  
  integer :: pool_size = 10
  real(dp) :: elite_pool_fraction = 0.3_dp
  real(dp) :: mutant_pool_fraction = 0.1_dp

  ! key_pool dims: key, pop_id
  real(dp), allocatable :: key_pool(:,:) ! key X population
  integer :: elite_pool_end
  integer :: mutant_pool_start

  
  abstract interface
     ! Input arg key can be modified by decode to allow local search
     ! in decoded space to translate to key space.
     real(dp) function brkga_decode_key_value(key) result(key_value)
       use const, only: dp
       real(dp), intent(inout) :: key(:)
     end function brkga_decode_key_value
  end interface

contains

  subroutine brkga_solve(solution_key)
    real(dp), intent(out) :: solution_key(:)

    ! Locals
    integer :: key_size
    integer :: err

    key_size = size(solution_key)

    ! Initialize Globals
    if (allocated(key_pool)) deallocate(key_pool)
    allocate(key_pool(key_size, pool_size), stat=err)
    call check_err(err, "Failed to allocate key pool.")    
    
    elite_pool_end = 1 + floor(pool_size / elite_pool_fraction)
    mutant_pool_start = elite_pool_end + floor(pool_size / mutant_pool_fraction)

    write(*,*) "BRKGA Config"
    write(*,*) "pool_size", pool_size
    write(*,*) "elite_pool_fraction", elite_pool_fraction
    write(*,*) "mutant_pool_fraction", mutant_pool_fraction

    write(*,*) "DEBUG elite_pool_end", elite_pool_end
    write(*,*) "DEBUG mutant_pool_start", mutant_pool_start

  end subroutine brkga_solve
  
  
  subroutine brkga_sort_key(key, key_order)
    real(dp), intent(in) :: key(:)
    integer, intent(out) :: key_order(:)

    ! Local Variables
    integer :: i, n, min_idx, tmp

    n = size(key)
    
    do i = 1, n
       key_order(i) = i
    end do

    do i = 1, n
       min_idx = minloc(key(key_order(i:n)), dim=1) + i - 1 !Correct index for slicing.
       write(*,*) "DEBUG key(key_order(i:n))"
       write(*,*) key(key_order(i:n))
       write(*,*) "DEBUG min_idx", min_idx

       tmp = key_order(i)
       key_order(i) = key_order(min_idx)
       key_order(min_idx) = tmp
       write(*,*) "DEBUG key_order after swap i", i, "and min_idx", min_idx
       write(*,*) key_order
    end do
  end subroutine brkga_sort_key
end module brkga
