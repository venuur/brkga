module check_util
  implicit none
  private
  public check_err

contains

  subroutine check_err(err, msg)
    integer, intent(in) :: err
    character, intent(in) :: msg

    if (err /= 0) then
       print *, msg
       stop err
    end if
  end subroutine check_err

end module check_util
