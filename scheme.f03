module scheme
  use const
  implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !                              variable scheme_flag explanation
  !>@scheme_flag = 1
  !>    calculate primary case
  !>@scheme_flag = 2
  !>    calculate grid average case
  !>@scheme_flag = 3
  !>    calculate nodal value case
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  double precision, parameter :: scheme_a = 1.d0, scheme_t = 1.d0, scheme_l = 3.d0

  integer :: scheme_step = 200, scheme_n = 100
  double precision, allocatable, dimension(:) :: scheme_u, scheme_grid_size, scheme_grid_node, scheme_flux
  double precision :: scheme_cfl, scheme_dt

  integer :: scheme_flag = 1

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

    if ( scheme_flag == 1 .or. scheme_flag == 3 ) then
       do i = 1,n
          scheme_u(i) = sin( 2 * PI * scheme_grid_node(i-1) )
       end do
    else if ( scheme_flag == 2 ) then
       do i = 1,n
          tmp = sin( PI * scheme_grid_size(i) ) / ( PI * scheme_grid_size(i) )
          scheme_u(i) = tmp * sin( 2 * PI * scheme_grid_node(i) - PI * scheme_grid_size(i) )
       end do
    end if

    scheme_dt = scheme_cfl * dx / scheme_a
    scheme_step = floor( scheme_t / scheme_dt ) + 1
    scheme_dt = scheme_t / scheme_step

  end subroutine scheme_init


  subroutine scheme_update()
    double precision , allocatable, dimension(:) :: u1, u2
    integer :: i, flag
    flag = scheme_flag

    if ( flag == 0 ) then
       allocate( u1(scheme_n) )
       do i = 1,scheme_n
          scheme_flux(i) = scheme_u(i)
       end do
       scheme_flux(0) = scheme_flux(scheme_n)
       u1 = scheme_u
       do i = 1,scheme_n
          u1(i) = scheme_u(i) - scheme_cfl * ( scheme_flux(i) - scheme_flux(i-1) )
       end do
       scheme_u = u1
    else if ( flag == 1 ) then
    else if ( flag == 2 ) then
    else if ( flag == 3 ) then
       !TODO: lots of work to do...
    end if


  end subroutine scheme_update

  subroutine scheme_error_calc ( u1, u2, err )
    double precision , intent(inout), dimension(:) ::  u1, u2
    double precision , intent(out) :: err

    integer :: i, n
    n = scheme_n
    err = 0.d0
    do i = 1,n
       err = err + scheme_grid_size(i) * abs( u1(i) - u2(i) )
    end do

  end subroutine scheme_error_calc


  function isConverge ()
    logical :: isConverge
    isConverge = .true.
  end function isConverge


end module scheme
