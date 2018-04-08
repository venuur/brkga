module brkga
  use const, only: dp
  use check_util, only: check_err
  implicit none
  private
  public brkga_solve, brkga_sort_key, brkga_set_seed

  integer, allocatable :: brkga_seed(:)
  integer :: brkga_n_iter = 10
  integer :: pool_size = 10
  real(dp) :: elite_pool_fraction = 0.3_dp
  real(dp) :: mutant_pool_fraction = 0.1_dp

  real(dp), allocatable :: key_pool(:,:)
  ! key_pool dims: key, pop_id
  real(dp), allocatable :: pool_score(:) 
  ! pool_score dims: pop_id
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

  subroutine brkga_solve(decode, solution_key)
    procedure(brkga_decode_key_value), intent(in) :: decode
    real(dp), intent(out) :: solution_key(:)

    ! Locals
    integer, allocatable :: pool_order(:)
    ! pool_order dims = pop_id
    
    integer :: key_size
    integer :: iter, i
    integer :: err

    key_size = size(solution_key)

    ! Initialize Globals
    if (allocated(key_pool)) deallocate(key_pool)
    allocate(key_pool(key_size, pool_size), stat=err)
    call check_err(err, "Failed to allocate key pool.")

    if (allocated(pool_score)) deallocate(pool_score)
    allocate(pool_score(pool_size), stat=err)
    call check_err(err, "Failed to allocate pool score.")
    
    elite_pool_end = floor(pool_size * elite_pool_fraction)
    mutant_pool_start = ceiling(pool_size - pool_size * mutant_pool_fraction)

    ! Initialize Locals
    allocate(pool_order(pool_size), stat=err)
    call check_err(err, "Failed to allocate pool order.")

    write(*,*) "BRKGA Config"
    write(*,*) "pool_size", pool_size
    write(*,*) "elite_pool_fraction", elite_pool_fraction
    write(*,*) "mutant_pool_fraction", mutant_pool_fraction

    write(*,*) "DEBUG elite_pool_end", elite_pool_end
    write(*,*) "DEBUG mutant_pool_start", mutant_pool_start

    ! Initialize pool.
    if allocated(brkga_seed) then
       call random_seed(put=brkga_seed)
    else
       call random_seed()
    end if

    random_number(key_pool)

    do iter = 1, brkga_n_iter
       ! Score pool.
       do i = 1, pool_size
          pool_score(i) = decode(key_pool(:, i))
       end do

       ! Sort to find elite pool.

       ! Perform crossover to generate next generation.

       ! Create mutants.       
    end do

    ! Score pool.
    
    ! Sort to find solution

    ! Return best
    solution_key = key_pool(:, 1)
    return
  end subroutine brkga_solve
  
  subroutine brkga_set_seed(seed)
    integer :: seed(:)

    ! Locals
    integer :: err
    
    if allocated(brkga_seed) deallocate(brkga_seed)
    allocate(brkga_seed(size(seed)), stat=err)
    call check_err(err, "Failed to allocate BRKGA seed")
    brkga_seed = seed
  end subroutine brkga_set_seed
end module brkga
