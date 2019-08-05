  include 'precision_module.f90'

  include 'timing_module.f90'

  program pi_serial
    use precision_module
    use timing_module
    implicit none
    integer :: i, j
    integer :: n_intervals
    real (dp) :: interval_width, x, total, pi
    real (dp) :: fortran_internal_pi

    call start_timing()
    n_intervals = 1000000
    fortran_internal_pi = 4.0_dp*atan(1.0_dp)
    print *, ' fortran_internal_pi = ', fortran_internal_pi
    print *, ' '
    do j = 1, 4
       interval_width = 1.0_dp/n_intervals
       total = 0.0_dp
       do i = 1, n_intervals
          x = interval_width*(real(i,dp)-0.5_dp)
          total = total + f(x)
       end do
       pi = interval_width*total
       print 100, n_intervals, time_difference()
       print 110, pi, abs(pi-fortran_internal_pi)
       n_intervals = n_intervals*10
    end do
100 format (' N intervals = ', i12, ' time = ', f8.3)
110 format (' pi = ', f20.16, /, ' difference = ', f20.16)
    call end_timing()
    stop

  contains

    real (dp) function f(x)
      implicit none
      real (dp), intent (in) :: x

      f = 4.0_dp/(1.0_dp+x*x)
    end function f

    end program pi_serial
