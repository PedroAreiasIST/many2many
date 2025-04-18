!****************
!*** remeshing 
!*** advances tip
!****************
PROGRAM remeshingshells
  INCLUDE 'simpar.inc'
!  LOGICAL::isucc
  CALL ssimplas(.FALSE.)
  IF(if3d)THEN
!     CALL splitedges3d()
  ELSE
!     CALL splitedges()
  END IF
  wres=.TRUE.
  CALL restartioc(.TRUE.)
END PROGRAM remeshingshells
