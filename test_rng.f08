program test_rng
  use const, only: dp
  implicit none

  real(dp) :: rv(10)
  integer, allocatable :: seed(:)
  integer :: n
  integer :: u
  logical :: file_exists

  call random_seed(size=n)
  allocate(seed(n))
  write (*,*) "Seed size", n
  call random_seed(get=seed)
  write (*,*) "Seed from random_seed", seed
  call random_number(rv)
  write (*,*) "First 10 random values", rv
  call random_number(rv)
  write (*,*) "Next 10 random values", rv

  inquire(file="data/test_seed.dat", exist=file_exists)
  if (.not. file_exists) then
     open(newunit=u, file="data/test_seed.dat", status="new", action="write")
  else 
     open(newunit=u, file="data/test_seed.dat", status="replace", action="write")
  end if
  
  write (u,*) seed
  close(u)

  open(newunit=u, file="data/test_seed.dat", status="old")
  read (u,*) seed
  close(u)

  write (*,*) "Seed from file", seed
  call random_seed(put=seed)
  call random_number(rv)
  write (*,*) "First 10 random values", rv
  

end program test_rng
