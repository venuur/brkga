module sort
  use const, only: dp
  implicit none
  private
  public minloc_sort_order
  
contains
  subroutine minloc_sort_order(key, key_order)
    real(dp), intent(in) :: key(:)
    integer, intent(out) :: key_order(:)

    ! Local Variables
    integer :: i, n, min_idx, tmp

    n = size(key)
    
    do i = 1, n
       key_order(i) = i
    end do

    do i = 1, n
       min_idx = minloc(key(key_order(i:n)), dim=1) + i - 1 !Correct index for slicing.
       ! write(*,*) "DEBUG key(key_order(i:n))"
       ! write(*,*) key(key_order(i:n))
       ! write(*,*) "DEBUG min_idx", min_idx

       tmp = key_order(i)
       key_order(i) = key_order(min_idx)
       key_order(min_idx) = tmp
       ! write(*,*) "DEBUG key_order after swap i", i, "and min_idx", min_idx
       ! write(*,*) key_order
    end do
  end subroutine minloc_sort_order
end module sort
