module const
  !> basic constants used in code
  !> no const_ prefix due to they are fundamental
  !> so all variables are in capital letters
  implicit none

  integer, parameter :: DBL = 8, SG = 4
  real(kind=DBL), parameter :: PI = 4.d0 * atan(1.d0), PI2 = 2.d0 * atan(1.d0), PI4 = atan(1.d0)

end module const
