! Fortran side of the same-machine comparison: the SimPlas routines that the
! library's symbolic transpose and composition replace, on a structured H8 mesh
! of nee^3 elements.  Usage: bench_f90 <nee> [reps]
program bench_f90
   use basfun
   use omp_lib
   implicit none
   integer :: nee, reps, nel, nn, i, ie, iex, iey, iez, r, s1, s2, base, nc
   integer, dimension(:), allocatable :: ia, ja, ija, iat, jat, ic, jc
   real(8), dimension(:), allocatable :: ttr, tmul
   real(8) :: t0
   integer(8) :: nnz
   character(32) :: arg
   nee = 100; reps = 3
   if (command_argument_count() >= 1) then
      call get_command_argument(1, arg); read (arg, *) nee
   end if
   if (command_argument_count() >= 2) then
      call get_command_argument(2, arg); read (arg, *) reps
   end if
   nel = nee**3; nn = (nee + 1)**3
   s1 = nee + 1; s2 = s1*s1
   allocate (ia(nel + 1), ja(8*nel))
   ia(1) = 1
   do i = 1, nel
      ia(i + 1) = ia(i) + 8
   end do
   ie = 0
   do iex = 0, nee - 1
      do iey = 0, nee - 1
         do iez = 0, nee - 1
            ie = ie + 1
            base = iex + iey*s1 + iez*s2 + 1
            ja(ia(ie)) = base
            ja(ia(ie) + 1) = base + 1
            ja(ia(ie) + 2) = base + 1 + s1
            ja(ia(ie) + 3) = base + s1
            ja(ia(ie) + 4) = base + s2
            ja(ia(ie) + 5) = base + 1 + s2
            ja(ia(ie) + 6) = base + 1 + s1 + s2
            ja(ia(ie) + 7) = base + s1 + s2
         end do
      end do
   end do
   allocate (ttr(reps), tmul(reps))
   do r = 1, reps
      if (allocated(iat)) deallocate (iat, jat, ija)
      t0 = omp_get_wtime()
      call manymanymatrixtransposeoseindex(nel, nn, ia, ja, ija, iat, jat)
      ttr(r) = omp_get_wtime() - t0
   end do
   do r = 1, reps
      if (allocated(ic)) deallocate (ic, jc)
      t0 = omp_get_wtime()
      call manymanymultiplicationsymbolic(nn, iat, jat, nel, ia, ja, nc, ic, jc)
      tmul(r) = omp_get_wtime() - t0
   end do
   nnz = int(ic(nc + 1), 8) - 1
   call sort(ttr, reps); call sort(tmul, reps)
   write (*, '(A,I0,A,I0,A,I0,A,I0,A,ES12.5,A,ES12.5,A,I0,A)') &
      '{"backend": "fortran", "threads": ', omp_get_max_threads(), ', "nee": ', nee, &
      ', "nel": ', nel, ', "nn": ', nn, ', "t_transpose": ', ttr((reps + 1)/2), &
      ', "t_product": ', tmul((reps + 1)/2), ', "nnz_product": ', nnz, '}'
contains
   subroutine sort(a, n)
      real(8), dimension(n) :: a
      integer :: n, i, j
      real(8) :: t
      do i = 2, n
         t = a(i); j = i - 1
         do while (j >= 1)
            if (a(j) <= t) exit
            a(j + 1) = a(j); j = j - 1
         end do
         a(j + 1) = t
      end do
   end subroutine sort
end program bench_f90
