module brkga_tsp
  use const, only: dp
  use check_util, only: check_err
  use brkga, only: brkga_solve, brkga_env
  use sort, only: minloc_sort_order
  use tsp, only: tsp_cost
  use pretty_print, only: print_matrix
  implicit none
  private
  public brkga_tsp_solve, brkga_tsp_decode_solution
  
  real(dp) :: test_key(5) = [ &
       0.3_dp, 0.1_dp, 0.5_dp, 0.05_dp, 0.9_dp ]
  
contains
  subroutine brkga_tsp_solve(env, cost_matrix, solution)
    type(brkga_env), intent(inout) :: env
    real(dp), intent(in) :: cost_matrix(:,:)
    integer, intent(out) :: solution(:)

    ! Locals
    real(dp), allocatable :: solution_key(:)
    real(dp) :: key_value
    integer :: key_size
    integer :: err

    key_size = size(solution) - 1

    ! print *, "DEBUG cost_matrix"
    ! call print_matrix(cost_matrix)
    
    key_value = tsp_decode(test_key)
    ! print *, "DEBUG tsp_decode(test_key)", key_value

    allocate(solution_key(key_size), stat=err)
    call check_err(err, "Failed to allocate solution key.")

    call brkga_solve(env, tsp_decode, solution_key)
    call brkga_tsp_decode_solution(solution_key, solution)
    
    return
    
  contains

    real(dp) function tsp_decode(key) result(key_value)
      real(dp), intent(inout) :: key(:)

      ! Local Variables
      integer, allocatable :: key_solution(:)
      integer :: i, n, err

      n = size(key)
      
      allocate(key_solution(n+1), stat=err)
      call check_err(err, "Failed to allocate key solution.")

      call brkga_tsp_decode_solution(key, key_solution)
      ! print *, "DEBUG key", key
      ! print *, "DEBUG key_solution", key_solution

      key_value = tsp_cost(key_solution, cost_matrix)

      return
    end function tsp_decode
  end subroutine brkga_tsp_solve

  subroutine brkga_tsp_decode_solution(key, solution)
    real(dp), intent(in) :: key(:)
    integer, intent(out) :: solution(:)

    call minloc_sort_order(key, solution)
    solution(size(key)+1) = solution(1)
  end subroutine brkga_tsp_decode_solution
end module brkga_tsp
