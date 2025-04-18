MODULE list_sorting
  USE omp_lib
  IMPLICIT NONE
CONTAINS
!> Function to compare two lists lexicographically
  INTEGER FUNCTION listscomparelexicographically(nsize1, arr1, nsize2, arr2)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nsize1, nsize2
    INTEGER, INTENT(IN) :: arr1(nsize1), arr2(nsize2)
    INTEGER :: i
! Initialize the result to 0 (equal)
    listscomparelexicographically = 0
! Compare by size first
    IF (nsize1 < nsize2) THEN
       listscomparelexicographically = -1
       RETURN
    ELSEIF (nsize1 > nsize2) THEN
       listscomparelexicographically = 1
       RETURN
    END IF
! Compare elements lexicographically
    DO i = 1, nsize1
       IF (arr1(i) < arr2(i)) THEN
          listscomparelexicographically = -1
          RETURN
       ELSEIF (arr1(i) > arr2(i)) THEN
          listscomparelexicographically = 1
          RETURN
       END IF
    END DO
  END FUNCTION listscomparelexicographically
!> Function to compare two lists lexicographically
  INTEGER FUNCTION listscomparelexicographicallyordered(nsize1, arr1, nsize2, arr2)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nsize1, nsize2
    INTEGER, INTENT(IN) :: arr1(nsize1), arr2(nsize2)
    INTEGER,DIMENSION(:),ALLOCATABLE::ARR1S,ARR2S
    INTEGER :: i
! Initialize the result to 0 (equal)
    listscomparelexicographically = 0
! Compare by size first
    IF (nsize1 < nsize2) THEN
       listscomparelexicographically = -1
       RETURN
    ELSEIF (nsize1 > nsize2) THEN
       listscomparelexicographically = 1
       RETURN
    END IF
    ! allocate
    allocate(arr1s(nsize1),arr2s(nsize2))
!*** copies
    arr1s(1:nsize1)=arr1(1:nsize1)
    arr2s(1:nsize2)=arr2(1:nsize2)
    call listsort(nsize1,arr1s)
    call listsort(nsize2,arr2s)
! Compare elements lexicographically
    DO i = 1, nsize1
       IF (arr1s(i) < arr2s(i)) THEN
          listscomparelexicographically = -1
          RETURN
       ELSEIF (arr1s(i) > arr2s(i)) THEN
          listscomparelexicographically = 1
          RETURN
       END IF
    END DO
    deallocate(arr1s,arr2s)
  END FUNCTION listscomparelexicographicallyordered
!> Subroutine to sort a permutation of indices based on lexicographic order
  SUBROUTINE manymanypermutationsortlexicographic(nel, elni, elno, perm)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nel
    INTEGER, INTENT(IN) :: elni(:), elno(:)
    INTEGER, INTENT(INOUT) :: perm(:)
    INTEGER :: curr_size, left_start, mid, right_end
    INTEGER, ALLOCATABLE :: temp_perm(:)
! Handle trivial cases
    IF (nel <= 1) RETURN
! Allocate temporary permutation array
    ALLOCATE(temp_perm(nel))
    curr_size = 1
! Iterative merge sort
    DO WHILE (curr_size <= nel - 1)
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(left_start, mid, right_end)
!$OMP DO SCHEDULE(dynamic)
       DO left_start = 1, nel, 2 * curr_size
          mid = MIN(left_start + curr_size - 1, nel)
          right_end = MIN(left_start + 2 * curr_size - 1, nel)
          IF (mid < right_end) THEN
             CALL manymanymergewithpermutation(elni, elno, perm, temp_perm, left_start, mid, right_end)
          ELSE
             temp_perm(left_start:right_end) = perm(left_start:right_end)
          END IF
       END DO
!$OMP END DO
!$OMP END PARALLEL
! Copy back to the main permutation array
       perm = temp_perm
       curr_size = 2 * curr_size
    END DO
! Deallocate temporary array
    DEALLOCATE(temp_perm)
  END SUBROUTINE manymanypermutationsortlexicographic
!> Subroutine to merge two sorted halves during the merge sort
  SUBROUTINE manymanymergewithpermutation(elni, elno, perm, temp_perm, left, mid, right)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: elni(:), elno(:), perm(:)
    INTEGER, INTENT(OUT) :: temp_perm(:)
    INTEGER, INTENT(IN) :: left, mid, right
    INTEGER :: i, j, k
    INTEGER :: is1, ie1, is2, ie2, inel1, inel2
    INTEGER :: icp
    i = left
    j = mid + 1
    k = left
! Merge two sorted halves
    DO WHILE (i <= mid .AND. j <= right)
       is1 = elni(perm(i))
       ie1 = elni(perm(i) + 1) - 1
       inel1 = ie1 - is1 + 1
       is2 = elni(perm(j))
       ie2 = elni(perm(j) + 1) - 1
       inel2 = ie2 - is2 + 1
       icp = listscomparelexicographically(inel1, elno(is1:ie1), inel2, elno(is2:ie2))
       IF (icp <= 0) THEN
          temp_perm(k) = perm(i)
          i = i + 1
       ELSE
          temp_perm(k) = perm(j)
          j = j + 1
       END IF
       k = k + 1
    END DO
! Copy any remaining elements
    IF (i <= mid) THEN
       temp_perm(k:right) = perm(i:mid)
    ELSEIF (j <= right) THEN
       temp_perm(k:right) = perm(j:right)
    END IF
  END SUBROUTINE manymanymergewithpermutation
!> give a unique identifier
  subroutine manymanygiveuniqueidentifier(nel,elni,elno,list, rank, perm)
    implicit none
    integer,dimension(*)::elni,elno,list,rank,perm
    integer, intent(in) :: nel          ! Size of the input list
    integer :: i, current_rank, prev_value ,nno1,nno2,ist1,ist2,iend1,iend2
! Initialize the variables
    current_rank = 1
    prev_value = rank(1)
    perm(rank(1)) = current_rank
! Loop through the rank array and assign unique indices
    do i = 2, nel
       ist1=elni(rank(i))
       iend1=elni(rank(i)+1)-1
       nno1=iend1-ist1+1
       ist2=elni(prev_value)
       iend2=elni(prev_value+1)-1
       nno2=iend2-ist2+1
       if(listscomparelexicographically(nno1,elno(ist1:iend1),nno2,elno(ist2:iend2)).ne.0)then
          current_rank = current_rank + 1
          prev_value = rank(i)
       end if
       perm(rank(i)) = current_rank
    end do
  end subroutine manymanygiveuniqueidentifier
END MODULE list_sorting
!-------------------------
!*** 
!-------------------------
PROGRAM test_lists_sort_large
  USE list_sorting
  USE omp_lib
  IMPLICIT none
  integer, parameter :: n = 7
!  integer :: list(n), rank(n), perm(n)
  INTEGER, ALLOCATABLE :: elni(:), elno(:), perm(:), list_lengths(:)
  INTEGER :: nel, total_length, num_lists
  INTEGER :: min_list_length, max_list_length
  INTEGER :: i, j, num_threads
  REAL :: start_time, end_time
  LOGICAL :: is_sorted
  INTEGER :: is1, ie1, is2, ie2, inel1, inel2, icp  
  num_lists = 20
  min_list_length = 2
  max_list_length = 12
  ALLOCATE(list_lengths(num_lists))
  CALL RANDOM_SEED()
  total_length = 0
  DO i = 1, num_lists
     CALL RANDOM_NUMBER(start_time)
     list_lengths(i) = min_list_length + INT((max_list_length - min_list_length + 1) * start_time)
     total_length = total_length + list_lengths(i)
  END DO
  nel = num_lists
  ALLOCATE(elni(nel + 1))
  ALLOCATE(elno(total_length))
  ALLOCATE(perm(nel))
  elni(1) = 1
  DO i = 1, nel
     elni(i + 1) = elni(i) + list_lengths(i)
  END DO
  DO i = 1, nel
     DO j = elni(i), elni(i + 1) - 1
        CALL RANDOM_NUMBER(start_time)
        elno(j) = INT(start_time * 100)
     END DO
  END DO
  DO i = 1, nel
     perm(i) = i
  END DO
  num_threads = omp_get_max_threads()
  PRINT *, 'Using ', num_threads, ' OpenMP threads.'
  CALL cpu_time(start_time)
  CALL manymanypermutationsortlexicographic(nel,elni, elno, perm)
  CALL cpu_time(end_time)
  PRINT *, 'Time taken for sorting: ', end_time - start_time, ' seconds'
  is_sorted = .TRUE.
  DO i = 1, nel - 1
     is1 = elni(perm(i))
     ie1 = elni(perm(i) + 1) - 1
     inel1 = ie1 - is1 + 1
     is2 = elni(perm(i + 1))
     ie2 = elni(perm(i + 1) + 1) - 1
     inel2 = ie2 - is2 + 1
     icp = listscomparelexicographically(inel1, elno(is1:ie1), inel2, elno(is2:ie2))
     IF (icp > 0) THEN
        is_sorted = .FALSE.
        EXIT
     END IF
  END DO
  IF (is_sorted) THEN
     PRINT *, 'Verification passed: The lists are sorted correctly.'
  ELSE
     PRINT *, 'Verification failed: The lists are not sorted correctly.'
  END IF
  DEALLOCATE(elni, elno, perm, list_lengths)
END PROGRAM test_lists_sort_large
