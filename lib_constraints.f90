!******************************
!*** imposed degree-of-freedom
!**********************-*******
SUBROUTINE mpcimposeddisplacement(imp)
  INCLUDE 'simpar.inc'  
  INTEGER::imp
  INTEGER,DIMENSION(:),ALLOCATABLE::nnods,ntyps
  REAL(8),DIMENSION(:),ALLOCATABLE::trm
  REAL(8),DIMENSION(:,:),ALLOCATABLE::trm2
  REAL(8)::rhs
  INTEGER::mnods,mtyps
  IOP=NINT(MPCPROP(IMP)%PROP(1))
  DO I=1,NTGL
     IGL=NINT(MPCPROP(IMP)%PROP(I*2))
     DES=MPCPROP(IMP)%PROP(1+I*2)*EXTERNLOAD(IOP)
!     RDE=MPCPROP(IMP)%PROP(1+I*2)*(EXTERNLOAD(IOP)-OLDEXTERNLOAD(IOP))
     IF(IGL.EQ.1)THEN
        DO INO=1,MPCPROP(IMP)%NSLAVES
           JNO=MPCPROP(IMP)%SLAVES(INO)        
           RHS=DES-NODOF(I,JNO,2)
           MNODS=JNO
           MTYPS=I
           call linearstoreboundarycondition(ass, mnods, mtyps, rhs)
        END DO
     END IF
  END DO
END SUBROUTINE mpcimposeddisplacement


!**************************************************
!*** list of pre-defined multiple point constraints
!**************************************************
SUBROUTINE limpc(imp)
  INCLUDE 'simpar.inc'
  CALL mpcimposeddisplacement(imp)
END SUBROUTINE limpc
