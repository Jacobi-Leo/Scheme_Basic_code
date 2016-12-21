program main
  use scheme
  use const
  implicit none

  integer :: i
  character(len=32) :: arg
  double precision :: err, errBase
  double precision , dimension(:), allocatable :: ue, un

  do i = 0,2 ! three command line arguments
     if ( i == 1 ) then
        call get_command_argument( i, arg )
        read( arg, * ) scheme_n
     else if ( i == 2 ) then
        call get_command_argument( i, arg )
        read( arg, * ) scheme_flag
     end if
  end do

  allocate( ue(scheme_n), un(scheme_n) )
  call scheme_init()

  do i=1,scheme_step
     call scheme_update()
     !TODO: write out data
  end do
  do i = 1,scheme_n
     un(i) = scheme_u(i)
  end do

  ue = uExact(scheme_step)
  call scheme_error_calc( un, ue, err )
  ue = -0.d0 * ue
  call scheme_error_calc( un, ue, errBase )
  write(*,*) scheme_l/scheme_n, ',', err, ',', err/errBase
  ! do i = 1,scheme_n
  !    write(*,*) scheme_grid_node(i), scheme_u(i), ue(i)
  ! end do


contains
  function uExact (i)
    double precision , dimension(scheme_n) :: uExact
    integer, intent(in) :: i
    integer :: j
    double precision :: tmp, t


    t = scheme_dt * real( i, DBL )
    do j = 1,scheme_n
       tmp = sin( PI * scheme_grid_size(j) ) / ( PI * scheme_grid_size(j) )
       uExact(j) = tmp * sin( 2.d0 * PI * ( scheme_grid_node(j) - scheme_a * t ) - PI * scheme_grid_size(j) )
    end do

  end function uExact
end program
