module precision_module
	implicit none
	integer, parameter :: sp = selected_real_kind( 6, 37)
	integer, parameter :: dp = selected_real_kind( 15, 307)
	integer, parameter :: qp = selected_real_kind( 30, 291)
	! Correction in the kind of integers:
	! 	We need values at 1E12
	integer, parameter :: ip = selected_int_kind(12)
end module precision_module
