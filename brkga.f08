module brkga
  use const, only: dp
  use check_util, only: check_err
  use sort, only: minloc_sort_order
  use pretty_print, only: print_matrix
  implicit none
  private
  public brkga_solve, brkga_set_seed, brkga_env

  type brkga_env
     ! Contains algorithm configurations and persistent data.
     integer, allocatable :: seed(:)
     integer :: n_iter = 100
     integer :: pool_size = 100
     
     real(dp) :: elite_fraction = 0.3_dp
     real(dp) :: mutant_fraction = 0.1_dp
     real(dp) :: elite_crossover_p = 0.7_dp
     
     real(dp), allocatable :: key_pool(:,:)
     ! key_pool dims: key, pop_id
     real(dp), allocatable :: pool_score(:) 
     ! pool_score dims: pop_id
  end type brkga_env
  
     
  
  abstract interface
     ! Input arg key can be modified by decode to allow local search
     ! in decoded space to translate to key space.
     real(dp) function brkga_decode_key_value(key) result(key_value)
       use const, only: dp
       real(dp), intent(inout) :: key(:)
     end function brkga_decode_key_value
  end interface

contains

  subroutine brkga_solve(env, decode, solution_key)
    type(brkga_env), intent(inout) :: env
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
    if (allocated(env%key_pool)) deallocate(env%key_pool)
    allocate(env%key_pool(key_size, env%pool_size), stat=err)
    call check_err(err, "Failed to allocate key pool.")

    if (allocated(env%pool_score)) deallocate(env%pool_score)
    allocate(env%pool_score(env%pool_size), stat=err)
    call check_err(err, "Failed to allocate pool score.")
    
    elite_pool_size = floor(env%pool_size * env%elite_fraction)
    mutant_pool_start = ceiling(env%pool_size * (1 - env%mutant_fraction))
    mutant_pool_size = env%pool_size - mutant_pool_start + 1
    regular_pool_size = (mutant_pool_start-1) - elite_pool_size
    nonelite_pool_size = env%pool_size - elite_pool_size
    
    ! Initialize Locals
    allocate(pool_order(env%pool_size), stat=err)
    call check_err(err, "Failed to allocate pool order.")

    allocate(pool_buf(key_size, env%pool_size), stat=err)
    call check_err(err, "Failed to allocate elite pool buffer.")

    allocate(crossover_p(key_size), stat=err)
    call check_err(err, "Failed to allocate crossover probability.")

    allocate(parent_unif(regular_pool_size), stat=err)
    call check_err(err, "Failed to allocate parent probability.")
    ! write(*,*) "DEBUG just allocated parent_unif"
    ! write(*,*) parent_unif

    allocate(elite_parent_idx(regular_pool_size), stat=err)
    call check_err(err, "Failed to allocate elite parent index.")

    allocate(regular_parent_idx(regular_pool_size), stat=err)
    call check_err(err, "Failed to allocate regular parent index.")
    

    write(*,*) "BRKGA Config"
    write(*,*) "env%pool_size", env%pool_size
    write(*,*) "env%elite_fraction", env%elite_fraction
    write(*,*) "env%mutant_fraction", env%mutant_fraction
    write(*,*) "env%elite_crossover_p", env%elite_crossover_p

    write(*,*) "DEBUG elite_pool_size", elite_pool_size
    write(*,*) "DEBUG mutant_pool_start", mutant_pool_start
    write(*,*) "DEBUG regular_pool_size", regular_pool_size

    ! Initialize pool.
    if (allocated(env%seed)) then
       call random_seed(put=env%seed)
    else
       call random_seed()
    end if

    call random_number(env%key_pool)
    ! write(*,*) "DEBUG env%key_pool"
    ! call print_matrix(env%key_pool)

    do iter = 1, env%n_iter
       ! Score pool.
       do i = 1, env%pool_size
          env%pool_score(i) = decode(env%key_pool(:, i))
       end do

       ! write(*,*) "DEBUG env%pool_score", env%pool_score

       ! Sort to find elite pool.
       call minloc_sort_order(env%pool_score, pool_order)
       pool_buf = env%key_pool(:, pool_order)
       ! write(*,*) "DEBUG env%key_pool"
       ! call print_matrix(env%key_pool)
       ! write(*,*) "DEBUG pool_buf"
       ! call print_matrix(pool_buf)
       
       env%key_pool(:, :elite_pool_size) = pool_buf(:, :elite_pool_size)
       ! write(*,*) "DEBUG env%key_pool"
       ! call print_matrix(env%key_pool)
       ! write(*,*) "DEBUG pool_buf"
       ! call print_matrix(pool_buf)
       

       
       ! Select parents.
       call random_number(parent_unif)
       elite_parent_idx = 1 + floor(elite_pool_size*parent_unif)
       ! write(*,*) "DEBUG parent_unif", parent_unif
       ! write(*,*) "DEBUG elite_parent_idx", elite_parent_idx

       call random_number(parent_unif)
       regular_parent_idx = 1 + elite_pool_size + floor(nonelite_pool_size*parent_unif)
       ! write(*,*) "DEBUG parent_unif", parent_unif
       ! write(*,*) "DEBUG regular_parent_idx", regular_parent_idx

       ! Perform crossover to generate next generation.
       do i = elite_pool_size+1, mutant_pool_start-1
          call random_number(crossover_p)
          k = i - elite_pool_size
          ! write(*,*) "DEBUG crossover i", i
          ! write(*,*) "DEBUG crossover_p", crossover_p
          ! write(*,*) "DEBUG elite_parent", pool_buf(:, elite_parent_idx(k))
          ! write(*,*) "DEBUG regular_parent", pool_buf(:, regular_parent_idx(k))
          where (crossover_p >= env%elite_crossover_p)
             env%key_pool(:, i) = pool_buf(:, elite_parent_idx(k))
          elsewhere
             env%key_pool(:, i) = pool_buf(:, regular_parent_idx(k))
          end where
          ! write(*,*) "DEBUG env%key_pool(:,i)", env%key_pool(:,i)
       end do
       
       ! Create mutants.
       call random_number(env%key_pool(:, mutant_pool_start:))

       ! write(*, *) "DEBUG env%key_pool post mutants"
       ! call print_matrix(env%key_pool)
    end do

    ! Score pool.
    do i = 1, env%pool_size
       env%pool_score(i) = decode(env%key_pool(:, i))
    end do
    write(*,*) "DEBUG env%pool_score", env%pool_score
       

    ! Sort to find elite pool.
    call minloc_sort_order(env%pool_score, pool_order)
    pool_buf = env%key_pool(:, pool_order)
    env%key_pool = pool_buf
    
    ! Return best
    solution_key = env%key_pool(:, 1)
    return
  end subroutine brkga_solve
  
  subroutine brkga_set_seed(env, seed)
    type(brkga_env), intent(inout) :: env
    integer, intent(in) :: seed(:)

    ! Locals
    integer :: err
    
    if (allocated(env%seed)) deallocate(env%seed)
    allocate(env%seed(size(seed)), stat=err)
    call check_err(err, "Failed to allocate BRKGA seed")
    env%seed = seed
  end subroutine brkga_set_seed

  subroutine brkga_set_pop_params(env, elite_fraction, mutant_fraction, elite_crossover_p)
    type(brkga_env), intent(inout) :: env
    real(dp), intent(in) :: elite_fraction
    real(dp), intent(in) :: mutant_fraction
    real(dp), intent(in) :: elite_crossover_p

    env%elite_fraction = elite_fraction
    env%mutant_fraction = mutant_fraction
    env%elite_crossover_p = elite_crossover_p
  end subroutine brkga_set_pop_params

  subroutine brkga_set_run_params(env, n_iter, pool_size)
    type(brkga_env), intent(inout) :: env
    integer, intent(in) :: n_iter
    integer, intent(in) :: pool_size

    env%n_iter = n_iter
    env%pool_size = pool_size
  end subroutine brkga_set_run_params
  
end module brkga
