module brkga
  use const, only: dp
  use check_util, only: check_err
  use sort, only: minloc_sort_order
  use pretty_print, only: print_matrix
  implicit none
  private
  public brkga_solve, brkga_set_seed

  integer, allocatable :: brkga_seed(:)
  integer :: brkga_n_iter = 1
  integer :: pool_size = 10
  real(dp) :: elite_pool_fraction = 0.3_dp
  real(dp) :: mutant_pool_fraction = 0.1_dp
  real(dp) :: elite_crossover_p = 0.7_dp

  real(dp), allocatable :: key_pool(:,:)
  ! key_pool dims: key, pop_id
  real(dp), allocatable :: pool_score(:) 
  ! pool_score dims: pop_id
  
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
    procedure(brkga_decode_key_value) :: decode
    real(dp), intent(out) :: solution_key(:)

    ! Locals
    integer, allocatable :: pool_order(:)
    real(dp), allocatable :: pool_buf(:,:)
    real(dp), allocatable :: crossover_p(:)
    real(dp), allocatable :: parent_unif(:)
    integer(dp), allocatable :: elite_parent_idx(:), regular_parent_idx(:)
    ! pool_order dims = pop_id

    integer :: elite_pool_size
    integer :: regular_pool_size
    integer :: mutant_pool_start
    integer :: mutant_pool_size
    integer :: nonelite_pool_size

    
    integer :: key_size
    integer :: iter, i, j, k
    integer :: err

    key_size = size(solution_key)

    ! Initialize Globals
    if (allocated(key_pool)) deallocate(key_pool)
    allocate(key_pool(key_size, pool_size), stat=err)
    call check_err(err, "Failed to allocate key pool.")

    if (allocated(pool_score)) deallocate(pool_score)
    allocate(pool_score(pool_size), stat=err)
    call check_err(err, "Failed to allocate pool score.")
    
    elite_pool_size = floor(pool_size * elite_pool_fraction)
    mutant_pool_start = ceiling(pool_size - pool_size * mutant_pool_fraction)
    mutant_pool_size = pool_size - mutant_pool_start + 1
    regular_pool_size = (mutant_pool_start-1) - elite_pool_size
    nonelite_pool_size = pool_size - elite_pool_size
    
    ! Initialize Locals
    allocate(pool_order(pool_size), stat=err)
    call check_err(err, "Failed to allocate pool order.")

    allocate(pool_buf(key_size, pool_size), stat=err)
    call check_err(err, "Failed to allocate elite pool buffer.")

    allocate(crossover_p(key_size), stat=err)
    call check_err(err, "Failed to allocate crossover probability.")

    allocate(parent_unif(regular_pool_size), stat=err)
    call check_err(err, "Failed to allocate parent probability.")
    write(*,*) "DEBUG just allocated parent_unif"
    write(*,*) parent_unif

    allocate(elite_parent_idx(regular_pool_size), stat=err)
    call check_err(err, "Failed to allocate elite parent index.")

    allocate(regular_parent_idx(regular_pool_size), stat=err)
    call check_err(err, "Failed to allocate regular parent index.")
    

    write(*,*) "BRKGA Config"
    write(*,*) "pool_size", pool_size
    write(*,*) "elite_pool_fraction", elite_pool_fraction
    write(*,*) "mutant_pool_fraction", mutant_pool_fraction
    write(*,*) "elite_crossover_p", elite_crossover_p

    write(*,*) "DEBUG elite_pool_size", elite_pool_size
    write(*,*) "DEBUG mutant_pool_start", mutant_pool_start
    write(*,*) "DEBUG regular_pool_size", regular_pool_size

    ! Initialize pool.
    if (allocated(brkga_seed)) then
       call random_seed(put=brkga_seed)
    else
       call random_seed()
    end if

    call random_number(key_pool)
    write(*,*) "DEBUG key_pool"
    call print_matrix(key_pool)

    do iter = 1, brkga_n_iter
       ! Score pool.
       do i = 1, pool_size
          pool_score(i) = decode(key_pool(:, i))
       end do

       write(*,*) "DEBUG pool_score", pool_score

       ! Sort to find elite pool.
       call minloc_sort_order(pool_score, pool_order)
       pool_buf = key_pool(:, pool_order)
       write(*,*) "DEBUG key_pool"
       call print_matrix(key_pool)
       write(*,*) "DEBUG pool_buf"
       call print_matrix(pool_buf)
       
       key_pool(:, :elite_pool_size) = pool_buf(:, :elite_pool_size)
       write(*,*) "DEBUG key_pool"
       call print_matrix(key_pool)
       write(*,*) "DEBUG pool_buf"
       call print_matrix(pool_buf)
       

       
       ! Select parents.
       call random_number(parent_unif)
       elite_parent_idx = 1 + floor(elite_pool_size*parent_unif)
       write(*,*) "DEBUG parent_unif", parent_unif
       write(*,*) "DEBUG elite_parent_idx", elite_parent_idx

       call random_number(parent_unif)
       regular_parent_idx = 1 + elite_pool_size + floor(nonelite_pool_size*parent_unif)
       write(*,*) "DEBUG parent_unif", parent_unif
       write(*,*) "DEBUG regular_parent_idx", regular_parent_idx

       ! Perform crossover to generate next generation.
       do i = elite_pool_size+1, mutant_pool_start-1
          call random_number(crossover_p)
          k = i - elite_pool_size
          write(*,*) "DEBUG crossover i", i
          write(*,*) "DEBUG crossover_p", crossover_p
          write(*,*) "DEBUG elite_parent", pool_buf(:, elite_parent_idx(k))
          write(*,*) "DEBUG regular_parent", pool_buf(:, regular_parent_idx(k))
          where (crossover_p >= elite_crossover_p)
             key_pool(:, i) = pool_buf(:, elite_parent_idx(k))
          elsewhere
             key_pool(:, i) = pool_buf(:, regular_parent_idx(k))
          end where
          write(*,*) "DEBUG key_pool(:,i)", key_pool(:,i)
       end do
       
       ! Create mutants.
       call random_number(key_pool(:, mutant_pool_start:))

       write(*, *) "DEBUG key_pool post mutants"
       call print_matrix(key_pool)
    end do

    ! Score pool.
    do i = 1, pool_size
       pool_score(i) = decode(key_pool(:, i))
    end do
    write(*,*) "DEBUG pool_score", pool_score
       

    ! Sort to find elite pool.
    call minloc_sort_order(pool_score, pool_order)
    pool_buf = key_pool(:, pool_order)
    key_pool = pool_buf
    
    ! Return best
    solution_key = key_pool(:, 1)
    return
  end subroutine brkga_solve
  
  subroutine brkga_set_seed(seed)
    integer :: seed(:)

    ! Locals
    integer :: err
    
    if (allocated(brkga_seed)) deallocate(brkga_seed)
    allocate(brkga_seed(size(seed)), stat=err)
    call check_err(err, "Failed to allocate BRKGA seed")
    brkga_seed = seed
  end subroutine brkga_set_seed
end module brkga
