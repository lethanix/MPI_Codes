# Correction of pi_mpi.f90 & pi_serial.f90

The maximum integer value of variables: 
- _i_ and _n_ in mpi version,
- _i_ and _n_intervals_ in serial version,
was not enough.

The correction consisted in the change of precision of the integers to 8 bytes:
  ```fortran
  integer(kind=8) :: i, n
  
