program test_tsplib_read
  use const, only: dp
  use tsp, only: tsp_read_tsplib
  use pretty_print, only: print_matrix
  implicit none

  character(*), parameter :: test_tsplib_file = "data/test_trunc5_xqf131.tsp"
  real(dp), allocatable :: cost_mat(:, :)

  write(*,*) "DEBUG test_tsplib_file", test_tsplib_file

  call tsp_read_tsplib(test_tsplib_file, cost_mat)

  print *, "DEBUG cost_mat"
  call print_matrix(cost_mat)
    
end program test_tsplib_read
