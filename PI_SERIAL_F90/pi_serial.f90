!*****************************************************************
!***********		Modified Version of pi_serial.f90		********
!*****************************************************************

! Louis C. Murguía

include 'precision_module.f90'

include 'timing_module.f90'

program pi_serial
	use precision_module
	use timing_module

	implicit none
	integer :: j

	! The correction: ip = selected_int_kind(12)
	integer(ip) :: i, n_intervals

	real (dp) :: interval_width, x, total, pi
	real (dp) :: fortran_internal_pi

	call start_timing()
	n_intervals = 100000000	! Starts with 1E8

	fortran_internal_pi = 4.0_dp*atan(1.0_dp)
	write(*,90) fortran_internal_pi

	do j = 1, 5
		interval_width = 1.0_dp/n_intervals
		total = 0.0_dp

		do i = 1 , n_intervals
			x = interval_width*(real(i,dp)-0.5_dp)
			total = total + f(x)
		end do

		pi = interval_width*total

		write(*,100) real(n_intervals), time_difference()
		write(*,110) pi, abs(pi-fortran_internal_pi)

		n_intervals = n_intervals*10
	end do

	90	format (/, 'Fortran internal pi = ', f21.19)
	100 format (/,'N intervals = ', ES7.1,5X, 'Time = ', f8.3)
	110 format (T4, 'pi = ', f20.16,/T4, 'Difference = ', f20.16)

	call end_timing()
	stop

	contains

		real (dp) function f(x)
			implicit none
			real (dp), intent (in) :: x

			f = 4.0_dp/(1.0_dp+x*x)
		end function f

end program pi_serial
