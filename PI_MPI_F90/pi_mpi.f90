!*****************************************************************
!**************	Modified Version of pi_mpi.f90		*************
!*****************************************************************

! Louis C. Murguía

include 'precision_module.f90'
include 'timing_module.f90'

program pi_mpi
	use precision_module
	use timing_module
	use mpi

	implicit none

	real (dp) :: fortran_internal_pi
	real (dp) :: partial_pi
	real (dp) :: total_pi
	real (dp) :: width
	real (dp) :: partial_sum
	real (dp) :: x
	integer :: this_process
	integer :: n_processes

	! The correction: ip = selected_int_kind(12)
	integer (ip) :: n
	integer (ip) :: i

	integer :: j
	integer :: error_number

	call mpi_init(error_number)
	call mpi_comm_size(mpi_comm_world, n_processes, &
		error_number)
	call mpi_comm_rank(mpi_comm_world, this_process, &
		error_number)

	n = 100000000	! Starts with 1E8
	fortran_internal_pi = 4.0_dp*atan(1.0_dp)

	if (this_process==0) then
		call start_timing()
		write(*,90) fortran_internal_pi
	end if

	do j = 1, 5
		width = 1.0_dp/n
		partial_sum = 0.0_dp

		do i = this_process + 1, n, n_processes
			x = width*(real(i,dp)-0.5_dp)
			partial_sum = partial_sum + f(x)
		end do

		partial_pi = width*partial_sum

		call mpi_reduce(partial_pi, total_pi, 1, &
			mpi_double_precision, mpi_sum, 0, &
			mpi_comm_world,error_number)

		if (this_process==0) then
			write(*,100) real(n), time_difference()
			write(*,110) total_pi, abs(total_pi &
									-fortran_internal_pi)
		end if

		n = n*10
	end do

	call mpi_finalize(error_number)

	90	format (/, 'Fortran internal pi = ', f21.19)
	100 format (/,'N intervals = ', ES7.1,5X, 'Time = ', f8.3)
	110 format (T4, 'pi = ', f20.16,/T4, 'Difference = ', f20.16)

	contains

		real (dp) function f(x)
			implicit none
			real (dp), intent (in) :: x

			f = 4.0_dp/(1.0_dp+x*x)
		end function f

end program pi_mpi

