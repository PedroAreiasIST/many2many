PROGRAM remesh
  INTEGER::initiationsteps=0
  CHARACTER(100)::filename,texto1,texto2,texto3,texto4
  INTEGER(2)::RESULT
  INTEGER::result2
  LOGICAL::fich1
  nloops=250
  innerloops=1!HUGE(1)
  filename=""
  narg=COMMAND_ARGUMENT_COUNT() 
  IF(narg.GE.1)THEN    
     CALL GET_COMMAND_ARGUMENT(1,texto1)
     filename=TRIM(ADJUSTL(texto1))
  END IF
  IF(narg.GE.2)THEN
     CALL GET_COMMAND_ARGUMENT(2,texto2)
     READ(texto2,*)nloops
  END IF
  innerloops=1
!!$  IF(narg.GE.3)THEN
!!$     CALL GET_COMMAND_ARGUMENT(3,texto3)
!!$     READ(texto3,*)innerloops
!!$  END IF
!!$  idelete=1
!!$  IF(narg.GE.4)THEN
!!$     CALL GET_COMMAND_ARGUMENT(4,texto4)
!!$     READ(texto4,*)idelete
!!$  END IF
!!  WRITE(*,"(A,I3,A,I3,A,I3)")"nloops=",nloops,"innerloops",innerloops,"idelete",idelete
!!  READ(*,*)
!!  IF(idelete.EQ.1)RESULT2=system('yes | rm -r '//TRIM(ADJUSTL(filename))//'.resta')
!!  RESULT2=system('make all')
  DO i=1,nloops
     RESULT2=system('SimPlas.exe'//' '//TRIM(ADJUSTL(filename)))
!!     GOTO 1333
!!     IF(initiationsteps.EQ.0)THEN
!!     RESULT2=system('exec_initiation.exe'//' '//TRIM(ADJUSTL(filename)))
!!     OPEN(11,file="ielcracked.txt",status="unknown")
!!     READ(11,*)ielcracked
!!     CLOSE(11)
!!        IF(ielcracked.NE.0)initiationsteps=1
!!     END IF
!!1333 CONTINUE
     DO j=1,innerloops
        WRITE(*,*)"innerloop",j
        RESULT2=system('CrackAdvance.exe'//' '//TRIM(ADJUSTL(filename)))
!        OPEN(11,file="ielcracked.txt",status="unknown")
!        READ(11,*)ielcracked
!        CLOSE(11)
!        IF(ielcracked.EQ.0)EXIT
     END DO
     RESULT2=system('yes | cp -r funcsmooth.angle '//TRIM(ADJUSTL(filename))//".angulo")
  END DO
  RESULT2=system('SimPlas.exe'//' '//TRIM(ADJUSTL(filename)))
END PROGRAM remesh
