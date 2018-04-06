module pretty_print
  use const, only: dp
  implicit None

contains
  subroutine print_matrix(matrix)
    real(dp), intent(in) :: matrix(:,:)

    ! Locals
    integer :: i
    
    do i = 1, size(matrix, dim=1)
       print *, "row", i, matrix(i, :)
    end do
  end subroutine print_matrix
end module pretty_print
