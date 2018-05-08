program solve_tsplib
  use const, only: dp
  use check_util, only: check_err
  use tsp, only: tsp_read_tsplib
  implicit none

  ! CLI
  character(len=1024) :: argbuf
  character(:), allocatable :: argtsplib
  integer :: arglen
  integer :: argstat

  ! TSP data
  real(dp), allocatable :: cost_mat

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

  
  

end program solve_tsplib
