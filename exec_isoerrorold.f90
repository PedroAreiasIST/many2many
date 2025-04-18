!************************************
!*** constitutive testing module
!*** writes isoerror maps
!************************************
PROGRAM teste
  INCLUDE "simpar.inc"
  INTEGER,PARAMETER::nma=100
  REAL(8)::oldtemperature,newtemperature
  REAL(8),DIMENSION(nma)::psp,prp,varold,var,varold_save
  LOGICAL::totallagrangian,planestress
  REAL(8),DIMENSION(4)::relativestrain,equilibriumstress,oldbackstress,newbackstress,oldstrain,newstrain,oldstrain_save
  REAL(8),DIMENSION(6)::old6stress,new6stress,old6stress_save,strain
  REAL(8),DIMENSION(3,3)::oldgrad,newgrad,proposedgrad,oldframe,newframe,oldgrad_save,oldframe_save,gtemp,stretch,rotation
  REAL(8)::lengthratio
  REAL(8)::dheattemperature
  REAL(8),DIMENSION(3,200000)::xc
  REAL(8),DIMENSION(4)::dheatdstrain,dstressdtemperature
  REAL(8),DIMENSION(4,4)::dstressdstrain,dstressdstrain1
!******************
!*** initialization
!******************
  ierr=0
  ielem=1
  igaus=1
  ieqit=1000
  totallagrangian=.FALSE.
  planestress=.FALSE.
  equilibriumstress=0.0d00
  oldtemperature=0.0d00
  newtemperature=0.0d00
  varold=0.0d00
  var=0.0d00
  lengthratio=1.0d00
  time=0.0d00
  dtime=1.0d-3
  nvoigt=4
  prp=0.0d00
  psp=0.0d00
  irouss=1
  loadmode=1
  WRITE(*,*)"Select: 0 - GTN, 1 - Rousselier, 2 -Von mises"
  READ(*,*)irouss
  IF(irouss.EQ.1)THEN     
!*** Rousselier
     itypmat=9
     prp(1)=72400.0d00
     prp(2)=0.33d00
     prp(3)=352.0d00
!*** this is the truth
     f0=2.0d-2
     sigma1=174.51
     d=1.07291d00
     fn=0.042d00
     sn=0.1d00
     epsn=0.2d00
     q6=1.0d00
     fc=0.06d00
     fm=0.1d00
     prp(13)=f0
     prp(14)=sigma1
     prp(15)=d
     prp(16)=fn
     prp(17)=sn
     prp(18)=epsn
     prp(19)=q6
     prp(20)=fc
     prp(21)=fm
  ELSEIF(irouss.EQ.2)THEN
     prp(1)=72400.0
     prp(2)=0.33d00
     prp(3)=352.0
     itypmat=5
     prp(13)=0.0d00        
  ELSE
!*** GTN
     itypmat=8
     prp(1)=72400.0d00
     prp(2)=0.33d00
     prp(3)=352.0d00
     f0=2.0d-2
     q1=1.5d00
     q2=1.0d00
     q3=2.25d00
     fn=0.042d00
     sn=0.1d00
     epsn=0.2d00
     q6=1.0d00
     fc=0.06d00
     fm=0.1d00
     prp(13)=f0
     prp(14)=q1
     prp(15)=q2
     prp(16)=q3
     prp(17)=fn
     prp(18)=sn
     prp(19)=epsn
     prp(20)=q6
     prp(21)=fc
     prp(22)=fm
  END IF
  ALLOCATE(gphist(100,10,30,2))
  gphist=0.0d00
  gphist(2,1,1,1:2)=f0
!*********************
  oldstrain=0.0d00
  OLD6STRESS=0.0D00
  NEW6STRESS=0.0D00


  

  OLD6STRESS(1)=352.0d00
  OLD6STRESS(2)=352.0d00
  NEW6STRESS(1)=352.0d00
  NEW6STRESS(2)=352.0d00












  CALL TANGELAST(.FALSE.,3,6,PRP,DSTRESSDSTRAIN)
  CALL matrixinverse(6,det,dstressdstrain,dstressdstrain1)
  CALL matrixvectorproduct(6,6,dstressdstrain1,old6stress,strain)
  WRITE(*,*)"strain=",strain
  varold(7:12)=strain(1:6)
  var(7:12)=strain(1:6)
!*********************
  varold(2)=f0
  var(2)=f0
!********************* 
  DO id=1,3
     DO jd=1,3
        oldgrad(id,jd)=matrixkronecker(id,jd)
        oldframe(id,jd)=matrixkronecker(id,jd)
        newframe(id,jd)=matrixkronecker(id,jd)
     END DO
  END DO
  WRITE(*,"(A)")" Select which case you want (0-approx, 1-exact, 2-compare) "
  READ(*,*)iexact
!*** fileprovidechannel
  IU=fileprovidechannel()
  IF(iexact.EQ.0)THEN
     OPEN(UNIT=IU,FILE="Stresses.txt",status="unknown")
  ELSEIF(iexact.EQ.1)THEN
     OPEN(UNIT=IU,FILE="StressesExact.txt",status="unknown")
  ELSEIF(iexact.EQ.2)THEN
     OPEN(UNIT=IU,FILE="StressesCompare.txt",status="unknown")     
  END IF
!*** mvals
  mvals=200
!  fmax=0.00628027904754!!0.00209779961838!0.25d00
  fmax=0.0129460 !2572868147478!!0.00628027904754!!!0.00209779961838!0.25d00
!  fmax=0.025d00
  newgrad=0.0d00
  IF(iexact.EQ.0)THEN
     DO i=1,mvals+1
        f11=1.0d00*(i-1)*fmax/mvals
        DO j=1,mvals+1
           f22=1.0d00*(j-1)*fmax/mvals
           newgrad(1,1)=f11+1.0d00
           newgrad(2,2)=f22+1.0d00
           newgrad(3,3)=1.0d00
!*** relative

           proposedgrad=newgrad

           relativestrain(1)=0.5d00*(newgrad(1,1)*newgrad(1,1)-1.0d00)
           relativestrain(2)=0.5d00*(newgrad(2,2)*newgrad(2,2)-1.0d00)
           relativestrain(3)=0.0d00
           relativestrain(4)=0.0d00!!(newgrad(1,1)*newgrad(1,2)+newgrad(2,1)*newgrad(2,2))

           x=0.5d00*(proposedgrad(1,1)*proposedgrad(1,1)-1.0d00)/strain(1)
           y=0.5d00*(proposedgrad(2,2)*proposedgrad(2,2)-1.0d00)/strain(2)
           varold_save=varold
           DO ikk=1,5
              CALL INNERCONSTITUTIVELAYER(IERR,NMA,PSP,PRPANAL,IELEM,IGAUS,IEQIT,TOTALLAGRANGIAN,PLANESTRESS,ITYPMAT,OLDTEMPERATURE,NEWTEMPERATURE,&
                   RELATIVESTRAIN,PROPOSEDGRAD,OLDGRAD,NEWGRAD,HEAT,EQUILIBRIUMSTRESS,OLD6STRESS,NEW6STRESS,OLDBACKSTRESS,NEWBACKSTRESS,OLDSTRAIN,NEWSTRAIN,&
                   TIME,DTIME,OLDFRAME,NEWFRAME,NVOIGT,PRP,MVPG,VAROLD,VAR,LENGTHRATIO,DHEATDSTRAIN,DHEATDTEMPERATURE,DSTRESSDSTRAIN,DSTRESSDTEMPERATURE)
              varold=var
           END DO
           varold=varold_save
           WRITE(iu,"(8e17.9)")x,y,new6stress(1:6)
        END DO
     END DO
     CLOSE(iu)
  ELSEIF(iexact.EQ.1)THEN
     newgrad=0.0d00
     mexact=120
     oldgrad_save=oldgrad
     old6stress_save=old6stress
     oldstrain_save=oldstrain
     oldframe_save=oldframe
     varold_save=varold
!*** now cycles
     DO i=1,mvals+1
        f11=1.0d00*(i-1)*fmax/mvals
        DO j=1,mvals+1
!*** deformation gradient
           f22=1.0d00*(j-1)*fmax/mvals
!*** restores values
!*******************
           oldgrad=oldgrad_save
           old6stress=old6stress_save
           oldstrain=oldstrain_save
           oldframe=oldframe_save
           varold=varold_save
!*** end restores values
           mexact=MAX(i,j)
           DO imm=1,mexact
              newgrad(1,1)=(f11*imm)/(mexact*1.d00)+1.0d00
              newgrad(2,2)=(f22*imm)/(mexact*1.d00)+1.0d00
              newgrad(3,3)=1.0d00
!              CALL POLAR3D(newgrad,ROTATION,STRETCH)
!              CALL matrixmatrixproduct(3,3,3,newgrad,rotation,newgrad,0)
!              newframe=rotation
!*** relative  SUBROUTINE matrixinverse(n,det,a,ai,ifs) 
              CALL matrixinverse(3,det,oldgrad,gtemp)
              CALL matrixmatrixproduct(3,3,3,newgrad,gtemp,proposedgrad,0)

              relativestrain=0.0d00
              relativestrain(1)=0.5d00*(proposedgrad(1,1)*proposedgrad(1,1)-1.0d00)
              relativestrain(2)=0.5d00*(proposedgrad(2,2)*proposedgrad(2,2)-1.0d00)
              relativestrain(3)=0.0d00
              relativestrain(4)=0.0d00 !(proposedgrad(1,1)*proposedgrad(1,2)+proposedgrad(2,1)*proposedgrad(2,2))

              CALL INNERCONSTITUTIVELAYER(IERR,NMA,PSP,PRPANAL,IELEM,IGAUS,IEQIT,TOTALLAGRANGIAN,PLANESTRESS,ITYPMAT,OLDTEMPERATURE,NEWTEMPERATURE,&
                   RELATIVESTRAIN,PROPOSEDGRAD,OLDGRAD,NEWGRAD,HEAT,EQUILIBRIUMSTRESS,OLD6STRESS,NEW6STRESS,OLDBACKSTRESS,NEWBACKSTRESS,OLDSTRAIN,NEWSTRAIN,&
                   TIME,DTIME,OLDFRAME,NEWFRAME,NVOIGT,PRP,MVPG,VAROLD,VAR,LENGTHRATIO,DHEATDSTRAIN,DHEATDTEMPERATURE,DSTRESSDSTRAIN,DSTRESSDTEMPERATURE)


!              CALL INNERCONSTITUTIVELAYER(IERR,NMA,PSP,PRPANAL,IELEM,IGAUS,IEQIT,TOTALLAGRANGIAN,PLANESTRESS,ITYPMAT,OLDTEMPERATURE,NEWTEMPERATURE,&
!                   RELATIVESTRAIN,PROPOSEDGRAD,OLDGRAD,NEWGRAD,HEAT,EQUILIBRIUMSTRESS,OLD6STRESS,NEW6STRESS,OLDBACKSTRESS,NEWBACKSTRESS,OLDSTRAIN,NEWSTRAIN,&
!                   TIME,DTIME,OLDFRAME,NEWFRAME,NVOIGT,PRP,MVPG,VAROLD,VAR,LENGTHRATIO,DHEATDSTRAIN,DHEATDTEMPERATURE,DSTRESSDSTRAIN,DSTRESSDTEMPERATURE)

!*** save history

              oldgrad=newgrad
              old6stress=new6stress
              oldstrain=newstrain
              oldframe=newframe
              varold=var              
           END DO
           newgrad(1,1)=f11+1.0d00
           newgrad(2,2)=f22+1.0d00
           newgrad(3,3)=1.0d00
           x=0.5d00*(newgrad(1,1)*newgrad(1,1)-1.0d00)/strain(1)
           y=0.5d00*(newgrad(2,2)*newgrad(2,2)-1.0d00)/strain(2)
           WRITE(iu,"(10e17.9)")x,y,new6stress(1:6)
        END DO
     END DO
     CLOSE(iu)
  ELSE
     OPEN(UNIT=11,FILE="Stresses.txt",status="unknown")
     OPEN(UNIT=12,FILE="StressesExact.txt",status="unknown")
!!     WRITE(iu,*)(mvals+1)**2
!!     WRITE(iu,"(A)")"Values"
     ik=0
     DO i=1,mvals+1
        DO j=1,mvals+1
           ik=ik+1
!*** deformation gradient
           READ(11,*)rx,ry,s11a,s22a,s33a,s12a,s13a,s23a
           READ(12,*)rx,ry,s11e,s22e,s33e,s12e,s13e,s23e
           rtop=SQRT((s11e-s11a)**2+(s22e-s22a)**2+(s33e-s33a)**2+(s12e-s12a)**2+(s13e-s13a)**2+(s23e-s23a)**2)
           rbot=SQRT(s11e**2+s22e**2+s33e**2+s12e**2+s13e**2+s23e**2)
           rtop2=(s11a+s22a+s33a-s11e-s22e-s33e)
           rbot2=s11e+s22e+s33e
           WRITE(iu,"(5e17.9)")rx,ry,rtop/rbot
           xc(1,ik)=rx
           xc(2,ik)=ry
           xc(3,ik)=rtop/rbot                      
        END DO
     END DO
     nc=(mvals+1)**2
     CALL outputensight(nc,xc)
     CLOSE(iu)       
  END IF
!*** calls
END PROGRAM teste
!
SUBROUTINE outputensight(nc,xc)
  USE OVERALL
  REAL(8),DIMENSION(3,*)::xc
  REAL(8)::apara,bpara
  LOGICAL::HASTHICK,ifparaview
  REAL(8),DIMENSION(:,:),ALLOCATABLE::SCALAR,ESCALAR
  REAL(8),DIMENSION(:,:,:),ALLOCATABLE::VECTOR,EVECTOR,TENS,ETENSOR
  CHARACTER(30),DIMENSION(:),ALLOCATABLE::CSCALAR,CESCALAR,CVECTOR,CEVECTOR,CTENSOR,CETENSOR
  INTEGER,DIMENSION(:),ALLOCATABLE::NELPR,NCNOS
!******************************
  REAL(8),DIMENSION(6)::COO
  REAL(8),DIMENSION(MVPE)::COE
  REAL(8),DIMENSION(MVPG)::COV
  REAL(8),DIMENSION(2,6)::COO2
  REAL(8),DIMENSION(2,MVPE)::COE2
  REAL(8),DIMENSION(2,MVPG)::COV2
  CHARACTER(6),DIMENSION(:),ALLOCATABLE::NAMES
  nscalar=1
  nescalar=0
  nvector=0
  nevector=0
  ntensor=0
  netensor=0
  NNO=NC
  NEL=NC
!*** ALLOCATES THE QUANTITIES
  CALL allocsafe(MAX(1,NSCALAR),NNO,SCALAR)
  CALL allocsafe(MAX(1,NESCALAR),NEL,ESCALAR)
  CALL allocsafe(3,MAX(1,NVECTOR),NNO,VECTOR)
  CALL allocsafe(3,MAX(1,NEVECTOR),NEL,EVECTOR)
  CALL allocsafe(6,MAX(1,NTENSOR),NNO,TENS)
  CALL allocsafe(6,MAX(1,NETENSOR),NEL,ETENSOR)
  CALL allocsafe(MAX(1,NSCALAR),CSCALAR)
  CALL allocsafe(MAX(1,NESCALAR),CESCALAR)
  CALL allocsafe(MAX(1,NVECTOR),CVECTOR)
  CALL allocsafe(MAX(1,NEVECTOR),CEVECTOR)
  CALL allocsafe(MAX(1,NTENSOR),CTENSOR)
  CALL allocsafe(MAX(1,NETENSOR),CETENSOR)
!**** fill what lacks
  cscalar(1)="Error"
!*** at the nodes
  DO ino=1,nc
     scalar(1,ino)=xc(3,ino)
  END DO
!*** PROVIDES ADDITIONAL REQUIRED QUANTITIES
  CALL allocsafe(nel,nelpr)
  DO iel=1,Nc
     nelpr(iel)=1!nelpre(iel)
     IF(itypoutput.EQ.0.AND.nelpr(iel).EQ.1)nelpr(iel)=0
  END DO
  CALL allocsafe(ntel,ncnos)
  DO I=1,NTEL
     ncnos(i)=nnosc(i)
  END DO
  CALL allocsafe(ntel,names)
  DO I=1,NTEL
     NAMES(I)=NOMES(I)
  END DO
!*** CALLS THE ENSIGHT OUTPUT ROUTINE
  ifparaview=.FALSE.
  nomef="merda"
  iouts=0
  CALL fileoutputensight_BIN(&
       Iouts,NOMEF, &
       NNO,xc(1:3,1:nc), &
       NEL, &
       NSCALAR,NVECTOR,NTENSOR, &
       CSCALAR,CVECTOR,CTENSOR, &
       SCALAR,VECTOR,TENS, &
       NESCALAR,NEVECTOR,NETENSOR, &
       CESCALAR,CEVECTOR,CETENSOR, &
       ESCALAR,EVECTOR,ETENSOR,NAMES,NELPR,NCNOS,ELNI,ELNO,ifparaview)
  iouts=iouts+1
END SUBROUTINE OUTPUTENSIGHT

