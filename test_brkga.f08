module brkga_tsp
  use const, only: dp
  use check_util, only: check_err
  use brkga, only: brkga_solve
  use sort, only: minloc_sort_order
  use tsp, only: tsp_cost
  use pretty_print
  implicit none
  private
  public brkga_tsp_solve, brkga_tsp_decode_solution
  
  real(dp) :: test_key(5) = [ &
       0.3_dp, 0.1_dp, 0.5_dp, 0.05_dp, 0.9_dp ]
  
contains
  subroutine brkga_tsp_solve(cost_matrix, solution)
    real(dp), intent(in) :: cost_matrix(:,:)
    integer, intent(out) :: solution(:)

    ! Locals
    real(dp), allocatable :: solution_key(:)
    real(dp) :: key_value
    integer :: key_size
    integer :: err

    key_size = size(solution) - 1

    print *, "DEBUG cost_matrix"
    call print_matrix(cost_matrix)
    
    key_value = tsp_decode(test_key)
    print *, "DEBUG tsp_decode(test_key)", key_value

    allocate(solution_key(key_size), stat=err)
    call check_err(err, "Failed to allocate solution key.")

    call brkga_solve(tsp_decode, solution_key)
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
      print *, "DEBUG key", key
      print *, "DEBUG key_solution", key_solution

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


program test_brkga
  use const, only: dp
  use brkga_tsp, only: brkga_tsp_solve, brkga_tsp_decode_solution
  use tsp, only: tsp_cost
  implicit none
  
  real(dp), parameter :: test_tsp_cost_matrix(5,5) = reshape( [ &
       0.0_dp,1.0_dp,2.0_dp,3.0_dp,4.0_dp,&
       5.0_dp,0.0_dp,6.0_dp,7.0_dp,8.0_dp,&
       9.0_dp,10.0_dp,0.0_dp,11.0_dp,12.0_dp,&
       13.0_dp,14.0_dp,15.0_dp,0.0_dp,16.0_dp,&
       17.0_dp,18.0_dp,19.0_dp,20.0_dp, 0.0_dp ], &
       shape(test_tsp_cost_matrix), order=[2,1])
  integer, parameter :: test_tsp_optimal_solution(6) = [1, 2, 3, 4, 5, 1]
  integer :: test_tsp_optimal_objective
  
  real(dp), parameter :: test_key(5) = [0.3_dp,0.1_dp,0.5_dp,0.05_dp,0.9_dp]
  integer, parameter :: test_key_tsp_solution(6) = [4,2,1,3,5,4]
  integer :: test_tsp_decode_sol(6)

  test_tsp_optimal_objective = tsp_cost(test_tsp_optimal_solution, test_tsp_cost_matrix)

  call brkga_tsp_decode_solution(test_key, test_tsp_decode_sol)
  print *, "test_key", test_key
  print *, "test_tsp_decode_sol", test_tsp_decode_sol
  print *, "test_key_tsp_solution", test_key_tsp_solution

  call brkga_tsp_solve(test_tsp_cost_matrix, test_tsp_decode_sol)

  print *, "test_tsp_optimal_solution", test_tsp_optimal_solution
  print *, "test_tsp_optimal_objective", test_tsp_optimal_objective
  print *, "test_tsp_decode_sol", test_tsp_decode_sol
  print *, "test_tsp_dec_solution", tsp_cost(test_tsp_optimal_solution, test_tsp_cost_matrix)

  print *, "Finished."
end program test_brkga
