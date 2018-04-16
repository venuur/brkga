module pretty_print
  use const, only: dp
  implicit None
  private
  public print_matrix, iprint_matrix, dprint_matrix

  interface print_matrix
     module procedure iprint_matrix
     module procedure dprint_matrix
  end interface print_matrix
  
contains
  subroutine dprint_matrix(matrix)
    real(dp), intent(in) :: matrix(:,:)

    ! Locals
    integer :: i
    
    do i = 1, size(matrix, dim=1)
       print *, "row", i, matrix(i, :)
    end do
  end subroutine dprint_matrix

  subroutine iprint_matrix(matrix)
    integer, intent(in) :: matrix(:,:)

    ! Locals
    integer :: i
    
    do i = 1, size(matrix, dim=1)
       print *, "row", i, matrix(i, :)
    end do
  end subroutine iprint_matrix
end module pretty_print
