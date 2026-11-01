! The SimPlas routines of the same-machine comparison, extracted verbatim from the solver's
! lib_general.f90 (module basfun, SimPlas 2026-10) so that bench_f90.f90 builds without the
! solver library: symbolic transpose with original-position index and two-pass symbolic
! product (per-thread stamped markers in the count pass, serial fill). Only the implicit
! integer declarations of the solver were made explicit.
module basfun
   use omp_lib
   implicit none
   integer, parameter :: omp_floor_stream = 65536  ! min count of ~1-op items to go parallel
   integer, parameter :: omp_floor_rows = 4096     ! min count of ~10-100-op items (rows, nodes) to go parallel
contains

   integer function manymanymaximumcolumn(na, ia, ja)
      integer, dimension(*) :: ia, ja
      integer :: na
      integer :: num2, ka
      num2 = 0
      do ka = 1, ia(na + 1) - 1
         num2 = max(num2, ja(ka))
      end do
      manymanymaximumcolumn = num2
   end function manymanymaximumcolumn

   subroutine manymanycreaterowpointers(n, list, what)
      integer :: n
      integer, dimension(*) :: list
      character(len=*), optional, intent(in) :: what
      integer :: lol, in, newv, in1
      if (n .le. 0) return
      lol = list(1)
      list(1) = 1
      do in = 1, n
         newv = list(in) + lol
         if (newv .lt. 0) then
            write (*, *) "FATAL: 32-bit integer overflow in a row-pointer prefix sum (n=", n, ")."
            if (present(what)) then
               write (*, *) trim(what)//" exceeds 2^31-1 entries."
            else
               write (*, *) "the total count exceeds 2^31-1 entries (32-bit index path)."
            end if
            error stop 1
         end if
         in1 = in + 1
         lol = list(in1)
         list(in1) = newv
      end do
   end subroutine manymanycreaterowpointers

   subroutine manymanyrevertrowpointers(n, list)
      integer :: n
      integer, dimension(*) :: list
      integer :: i
      if (n .le. 0) return
      do i = n, 1, -1
         list(i + 1) = list(i)
      end do
      list(1) = 1
   end subroutine manymanyrevertrowpointers

   subroutine manymanymultiplicationpart1(na, ia, ja, nb, ib, jb, nc, ic, what)
      integer, intent(in) :: na, nb
      integer, intent(out) :: nc
      character(len=*), optional, intent(in) :: what
      integer, dimension(*) :: ia, ja, ib, jb
      integer, dimension(:), allocatable, intent(out) :: ic
      integer, allocatable :: iw(:)
      integer :: ncb, ra, ka, kb, ca, cb, ldg
      ncb = manymanymaximumcolumn(nb, ib, jb)
      nc = na
      if (nc > 0) then
         allocate (ic(nc + 1))
      else
         allocate (ic(1))
         ic(1) = 1
         return
      end if
!$omp parallel private(iw, ra, ka, kb, ca, cb, ldg) shared(na, ia, ja, ib, jb, ic, ncb)
      if (ncb > 0) then
         allocate (iw(ncb))
         iw = 0
      end if
!$omp do
      do ra = 1, na
         ldg = 0
         if (ncb == 0) then
            ic(ra) = 0
            cycle
         end if
         do ka = ia(ra), ia(ra + 1) - 1
            ca = ja(ka)
            do kb = ib(ca), ib(ca + 1) - 1
               cb = jb(kb)
               if (iw(cb) .ne. ra) then
                  ldg = ldg + 1
                  iw(cb) = ra
               end if
            end do
         end do
         ic(ra) = ldg
      end do
!$omp end do
      if (ncb > 0) then
         deallocate (iw)
      end if
!$omp end parallel
      call manymanycreaterowpointers(nc, ic, what)
   end subroutine manymanymultiplicationpart1

   subroutine manymanymultiplicationpart2(na, ia, ja, nb, ib, jb, nc, ic, jc_out)
      integer, intent(in) :: na, nb, nc
      integer, dimension(*) :: ia, ja, ib, jb, ic
      integer, dimension(:), allocatable, intent(out) :: jc_out
      integer :: ncb, total_non_zeros_c, alloc_stat, ra, ka, ca, kb, cb, current_jc_writer_pos
      integer, allocatable :: iw(:)
      ncb = manymanymaximumcolumn(nb, ib, jb)
      if (nc > 0) then
         total_non_zeros_c = ic(nc + 1) - 1
      else
         total_non_zeros_c = 0
      end if
      if (total_non_zeros_c > 0) then
         allocate (jc_out(total_non_zeros_c), stat=alloc_stat)
         if (alloc_stat /= 0) then
            print *, "Error: Could not allocate jc_out in manymanymultiplicationpart2. Status:", alloc_stat
            stop 1
         end if
      else if (total_non_zeros_c == 0) then
         allocate (jc_out(1:0), stat=alloc_stat)
         if (alloc_stat /= 0) then
            print *, "Error: Could not allocate zero-sized jc_out. Status:", alloc_stat
            stop 1
         end if
      else
         print *, "Error: total_non_zeros_C is negative. This should not happen."
         stop 1
      end if
      if (ncb > 0) then
         allocate (iw(ncb), stat=alloc_stat)
         if (alloc_stat /= 0) then
            print *, "Error: Could not allocate iw in manymanymultiplicationpart2. Status:", alloc_stat
            if (allocated(jc_out)) deallocate (jc_out)
            stop 1
         end if
         iw = 0
      end if
      do ra = 1, na
         current_jc_writer_pos = ic(ra)
         if (ncb == 0) then
            cycle
         end if
         do ka = ia(ra), ia(ra + 1) - 1
            ca = ja(ka)
            do kb = ib(ca), ib(ca + 1) - 1
               cb = jb(kb)
               if (iw(cb) .ne. ra) then
                  iw(cb) = ra
                  if (current_jc_writer_pos < ic(ra + 1)) then
                     jc_out(current_jc_writer_pos) = cb
                     current_jc_writer_pos = current_jc_writer_pos + 1
                  else
                     write (*, *) "manymanymultiplicationpart2: row", ra, " has more columns than ic allows"
                     error stop "manymanymultiplicationpart2: part1/part2 count mismatch"
                  end if
               end if
            end do
         end do
      end do
      if (allocated(iw)) then
         deallocate (iw)
      end if
   end subroutine manymanymultiplicationpart2

   subroutine manymanymultiplicationsymbolic(na, ia, ja, nb, ib, jb, nc, ic, jc)
      integer :: na, nb, nc
      integer, dimension(*) :: ia, ja, ib, jb
      integer, dimension(:), allocatable :: ic, jc
      call manymanymultiplicationpart1(na, ia, ja, nb, ib, jb, nc, ic)
      call manymanymultiplicationpart2(na, ia, ja, nb, ib, jb, nc, ic, jc)
   end subroutine manymanymultiplicationsymbolic

   subroutine manymanymatrixtransposeoseindex(nra, nca, ia, ja, ija, iat, jat)
      integer, intent(in) :: nra, nca
      integer, dimension(*) :: ia, ja
      integer, dimension(:), allocatable, intent(out) :: ija, iat, jat
      integer :: r, c, k_csr, k_orig_row, k_csr_t, itemp, m
      m = max(nca, 0)
      allocate (iat(m + 1))
!$omp parallel do private(c) if(m .gt. omp_floor_stream)
      do c = 1, m + 1
         iat(c) = 0
      end do
!$omp end parallel do
!$omp parallel do private(r, k_csr, c) schedule(static) if(nra .gt. omp_floor_rows)
      do r = 1, nra
         do k_csr = ia(r), ia(r + 1) - 1
            c = ja(k_csr)
            if (c .lt. 1 .or. c .gt. m) cycle
!$omp atomic
            iat(c + 1) = iat(c + 1) + 1
         end do
      end do
!$omp end parallel do
      iat(1) = 1
      do c = 1, m
         iat(c + 1) = iat(c + 1) + iat(c)
      end do
      itemp = iat(m + 1) - 1
      allocate (jat(itemp), ija(itemp))
      do r = 1, nra
         k_orig_row = 0
         do k_csr = ia(r), ia(r + 1) - 1
            k_orig_row = k_orig_row + 1
            c = ja(k_csr)
            if (c .lt. 1 .or. c .gt. m) cycle
            k_csr_t = iat(c)
            iat(c) = k_csr_t + 1
            jat(k_csr_t) = r
            ija(k_csr_t) = k_orig_row
         end do
      end do
      call manymanyrevertrowpointers(m, iat)
   end subroutine manymanymatrixtransposeoseindex

end module basfun
