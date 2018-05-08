program solve_tsplib
  use const, only: dp
  use check_util, only: check_err
  use tsp, only: tsp_read_tsplib, tsp_cost
  use brkga_tsp, only: brkga_tsp_solve
  implicit none

  ! CLI
  character(len=1024) :: argbuf
  character(:), allocatable :: argtsplib
  integer :: arglen
  integer :: argstat

  ! TSP data
  real(dp), allocatable :: cost_mat(:,:)
  integer :: n_stops
  integer, allocatable :: tsp_tour(:)

  ! Error handling
  integer :: err
  
  
  call get_command_argument(1, value=argbuf, length=arglen, status=argstat)

  if (argstat > 0) then
     print *, "Failed reading command line argument TSPLIBFILE."
     stop 0
  else if (argstat == -1) then
     print *, "Command line argument TSPLIBFILE must be less than 1024 characters."
     stop 0
  else
     allocate(character(len=arglen) :: argtsplib, stat=err)
     call check_err(err, "Failed to allocate argtsplib.")

     argtsplib = trim(argbuf)
  end if

  call tsp_read_tsplib(argtsplib, cost_mat)

  n_stops = size(cost_mat, dim=1)
  allocate(tsp_tour(n_stops+1), stat=err)
  call check_err(err, "Failed to allocate tsp_tour")

  call brkga_tsp_solve(cost_mat, tsp_tour)

  print *, "TSP Tour"
  print *, "Cost:", tsp_cost(tsp_tour, cost_mat)
  print *, "Stops:"
  print *, tsp_tour  

end program solve_tsplib
