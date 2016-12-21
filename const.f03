module const
  !> basic constants used in code
  !> no const_ prefix due to they are fundamental
  !> so all variables are in capital letters
  implicit none

  integer, public, parameter :: DBL = 8, SG = 4
  double precision , public, parameter :: PI = 4.d0 * atan(1.d0), PI2 = 2.d0 * atan(1.d0), PI4 = atan(1.d0)

  double precision, public, parameter :: EPS = 1d-8
end module const
