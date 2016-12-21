module scheme
  use const
  implicit none
  private
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !                              variable scheme_flag explanation
  !>@scheme_flag = 0
  !     calculate test case
  !>@scheme_flag = 1
  !>    calculate primary case
  !>@scheme_flag = 2
  !>    calculate grid average case
  !>@scheme_flag = 3
  !>    calculate nodal value case
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  double precision, public, parameter :: scheme_a = 1.d0, scheme_t = 1.d-2, scheme_l = 3.d0

  integer, public :: scheme_step = 200, scheme_n = 100
  double precision, public, allocatable, dimension(:) :: scheme_u, scheme_grid_size, scheme_grid_node, scheme_flux
  double precision, public :: scheme_cfl, scheme_dt, scheme_dx

  integer, public :: scheme_flag = 1

  double precision , allocatable, dimension(:) :: u1, u2, k1, k2, k3, k4

  public :: scheme_init, scheme_update, scheme_error_calc

contains

  subroutine scheme_init()
    implicit none
    double precision :: l = scheme_l ! the length of area to solve
    double precision :: dx, tmp
    integer :: i
    integer :: n

    n = scheme_n

    allocate( scheme_u(n), scheme_grid_size(n), scheme_grid_node(0:n), scheme_flux(0:n) )
    scheme_cfl = 3.d-1
    dx = l / real( n, DBL )
    scheme_grid_node(0) = 0.d0

    do i = 1,n
       scheme_grid_size(i) = dx
       scheme_grid_node(i) = scheme_grid_node(i-1) + dx
    end do

    if ( scheme_flag == 1 .or. scheme_flag == 3 .or.scheme_flag == 0 ) then
       do i = 1,n
          scheme_u(i) = sin( 2.d0 * PI * scheme_grid_node(i-1) )
       end do
    else if ( scheme_flag == 2 ) then
       do i = 1,n
          tmp = sin( PI * scheme_grid_size(i) ) / ( PI * scheme_grid_size(i) )
          scheme_u(i) = tmp * sin( 2.d0 * PI * scheme_grid_node(i) - PI * scheme_grid_size(i) )
       end do
    end if

    scheme_dt = scheme_cfl * dx / scheme_a
    scheme_step = floor( scheme_t / scheme_dt ) + 1
    scheme_dt = scheme_t / scheme_step
    scheme_dx = dx

    ! allocate private variables
    allocate( u1(scheme_n), u2(scheme_n) )
    allocate( k1(scheme_n), k2(scheme_n), k3(scheme_n), k4(scheme_n) )

  end subroutine scheme_init


  subroutine scheme_update()
    !> update scheme for *ONE* step
    implicit none
    integer :: i, flag
    flag = scheme_flag

    if ( flag == 0 ) then
       do i = 1,scheme_n
          scheme_flux(i) = scheme_u(i)
       end do
       scheme_flux(0) = scheme_flux(scheme_n)
       u1 = scheme_u
       do i = 1,scheme_n
          u1(i) = scheme_u(i) - scheme_cfl * ( scheme_flux(i) - scheme_flux(i-1) )
       end do
       scheme_u = u1
    else
       u1 = scheme_u
       call flux_update()
       !! RK4 Process
       forall ( i = 1:scheme_n )
          k1(i) = scheme_dt * ( scheme_flux(i-1) - scheme_flux(i) ) / scheme_dx
       end forall
       scheme_u = scheme_u + 5.d-1 * k1
       call flux_update()
       forall ( i = 1:scheme_n )
          k2(i) = scheme_dt * ( scheme_flux(i-1) - scheme_flux(i) ) / scheme_dx
       end forall
       scheme_u = scheme_u + 5.d-1 * k2
       call flux_update()
       forall ( i = 1:scheme_n )
          k3(i) = scheme_dt * ( scheme_flux(i-1) - scheme_flux(i) ) / scheme_dx
       end forall
       scheme_u = scheme_u + k3
       call flux_update()
       forall ( i = 1:scheme_n )
          k4(i) = scheme_dt * ( scheme_flux(i-1) - scheme_flux(i) ) / scheme_dx
       end forall

       scheme_u = u1 + (k1 + 2.d0*k2 + 2.d0*k3 + k4) / 6.d0
    end if

  end subroutine scheme_update

  subroutine flux_update ()
    implicit none

    integer :: i, im1, ip1, ip2
    double precision :: tmp1, tmp2, tmp3, a

    a = scheme_a

    if ( scheme_flag == 2 ) then
       flux: do i = 1,scheme_n
          if ( i-1 < 1 ) then
             im1 = i - 1 + scheme_n
          else
             im1 = i - 1
          end if

          if ( i+1 > scheme_n ) then
             ip1 = i + 1 - scheme_n
          else
             ip1 = i + 1
          end if

          if ( i+2 > scheme_n ) then
             ip2 = i + 2 - scheme_n
          else
             ip2 = i + 2
          end if
          tmp1 = scheme_u(i) + scheme_u(ip1)
          tmp2 = scheme_u(im1) + scheme_u(ip2)
          tmp3 = scheme_u(ip2) - 3.d0*scheme_u(ip1) + 3.d0*scheme_u(i) - scheme_u(im1)
          scheme_flux(i) = 7.d0*a/12.d0*tmp1 - tmp2*a/12.d0 + kappa(a)*a/12.d0*tmp3
       end do flux
       scheme_flux(0) = scheme_flux(scheme_n)
    else if ( scheme_flag == 1 ) then
       flux2: do i = 1,scheme_n
          if ( i-1 < 1 ) then
             im1 = i - 1 + scheme_n
          else
             im1 = i - 1
          end if

          if ( i+1 > scheme_n ) then
             ip1 = i + 1 - scheme_n
          else
             ip1 = i + 1
          end if

          if ( i+2 > scheme_n ) then
             ip2 = i + 2 - scheme_n
          else
             ip2 = i + 2
          end if
          tmp1 = scheme_u(i) + scheme_u(ip1)
          tmp2 = scheme_u(im1) + scheme_u(ip2)
          tmp3 = scheme_u(ip2) - 3.d0*scheme_u(ip1) + 3.d0*scheme_u(i) - scheme_u(im1)
          scheme_flux(i) = 9.d0*a/16.d0*tmp1 - tmp2*a/16.d0 + kappa(a)*a/16.d0*tmp3
       end do flux2
       scheme_flux(0) = scheme_flux(scheme_n)
    else if ( scheme_flag == 3 ) then
       flux3: do i = 1,scheme_n
          if ( i-1 < 1 ) then
             im1 = i - 1 + scheme_n
          else
             im1 = i - 1
          end if

          if ( i+1 > scheme_n ) then
             ip1 = i + 1 - scheme_n
          else
             ip1 = i + 1
          end if

          if ( i+2 > scheme_n ) then
             ip2 = i + 2 - scheme_n
          else
             ip2 = i + 2
          end if
          tmp1 = scheme_u(i) + scheme_u(ip1)
          tmp2 = scheme_u(im1) + scheme_u(ip2)
          tmp3 = scheme_u(ip2) - 3.d0*scheme_u(ip1) + 3.d0*scheme_u(i) - scheme_u(im1)
          scheme_flux(i) = 9.d0*a/16.d0*tmp1 - tmp2*a/16.d0 + kappa(a)*a/16.d0*tmp3
       end do flux3
       scheme_flux(0) = scheme_flux(scheme_n)
    end if

  end subroutine flux_update

  subroutine scheme_error_calc ( u1, u2, err )
    implicit none
    double precision , intent(in), dimension(scheme_n) ::  u1, u2
    double precision , intent(out) :: err

    err = sum( scheme_grid_size * abs( u1 - u2 ) )

  end subroutine scheme_error_calc


  function isConverge ()
    implicit none

    logical :: isConverge
    isConverge = .true.
  end function isConverge

  pure function kappa (a)
    implicit none

    double precision :: kappa
    double precision, intent(in) :: a

    if ( abs(a) > epsilon(a) ) then
       kappa = a / abs(a)
    else
       kappa = a / (a*a + epsilon(a)*epsilon(a)) / (2 * epsilon(a))
    end if
  end function kappa


end module scheme
