program test_brkga
  use const, only: dp
  use brkga, only: brkga_env
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

  type(brkga_env) :: env

  test_tsp_optimal_objective = tsp_cost(test_tsp_optimal_solution, test_tsp_cost_matrix)

  call brkga_tsp_decode_solution(test_key, test_tsp_decode_sol)
  print *, "test_key", test_key
  print *, "test_tsp_decode_sol", test_tsp_decode_sol
  print *, "test_key_tsp_solution", test_key_tsp_solution

  call brkga_tsp_solve(env, test_tsp_cost_matrix, test_tsp_decode_sol)

  print *, "test_tsp_optimal_solution", test_tsp_optimal_solution
  print *, "test_tsp_optimal_objective", test_tsp_optimal_objective
  print *, "test_tsp_decode_sol", test_tsp_decode_sol
  print *, "test_tsp_dec_solution", tsp_cost(test_tsp_optimal_solution, test_tsp_cost_matrix)

  print *, "Finished."
end program test_brkga
