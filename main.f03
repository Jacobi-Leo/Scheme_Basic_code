program main
  use scheme
  use const
  implicit none

  integer :: i, ip1, ip2, im1
  character(len=32) :: arg
  double precision :: err, errBase, tmp1, tmp2, tmp3
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
  end do
  do i = 1,scheme_n
     if ( scheme_flag == 2 ) then
        if ( i-1 < 1 ) then
           im1 = i - 1 + scheme_n
        else
           im1 = i - 1
        endif
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
        tmp1 = ( scheme_u(i) + scheme_u(ip1) ) * 5.d-1
        if ( scheme_a > 0 ) then
           tmp2 = ( scheme_u(im1) - 2.d0 * scheme_u(i) + scheme_u(ip1) ) / 6.d0
        else
           tmp2 = ( scheme_u(i) - 2.d0 * scheme_u(ip2) + scheme_u(ip2) ) / 6.d0
        end if
        un(i) = tmp1 + tmp2
     else
        un(i) = scheme_u(i)
     end if
  end do
  ue = uExact(scheme_step)

  un = scheme_u
  call scheme_error_calc( un, ue, err )
  ue = -0.d0 * ue
  call scheme_error_calc( un, ue, errBase )
  write(*,*) scheme_l/scheme_n, ',', err, ',', err/errBase

  ! write(*,*) scheme_step
  ! do i = 1,scheme_n
  !    write(*,*) scheme_grid_node(i), ",", scheme_u(i), ",",  ue(i)
  ! end do


contains
  function uExact (i)
    double precision , dimension(scheme_n) :: uExact, tmp
    integer, intent(in) :: i
    integer :: j
    double precision :: t


    t = scheme_dt * real( i, DBL )
    if ( scheme_flag == 2 ) then
       tmp = sin( PI * scheme_grid_size ) / ( PI * scheme_grid_size )
       uExact = tmp * sin( 2.d0 * PI * ( scheme_grid_node - scheme_a * t ) )
    else
       uExact = sin( 2.d0 * PI * (scheme_grid_node - scheme_a * t ))
    end if

  end function uExact
end program
