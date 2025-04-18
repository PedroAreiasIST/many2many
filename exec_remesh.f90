program remesh
  use remeshsimplex  
  include 'simpar.inc'  
  type(originalmeshtype) :: om
  type(renewedmeshtype) :: nm
  integer, parameter :: nl = 13
  character(20),dimension(:),allocatable :: elementgroup,nodegroup
  integer, dimension(:), allocatable :: nodelist,nogri,nogro
  integer, dimension(:), allocatable :: eltyp
  real(8), dimension(:, :), allocatable :: noco2
  real(8),dimension(:),allocatable :: xi
  open(umal, file = "original.malha", status = "unknown")
  open(ucut, file = "cuts.txt", status = "unknown")
!---------
!*** mesh
!---------
  do i = 1, nl
     read (umal, *)
  end do
!--------------------------------
!*** reading of node coordinates
!--------------------------------
  read (umal, *) om%nno
  allocate(noco(3, om%nno))
  do in = 1, om%nno
     read (umal, *) ino, (noco(ikk, ino), ikk = 1, 3)
  end do
!--------------------------------------
!*** reading of element connectivities
!--------------------------------------
  read (umal, *)
  read (umal, *) om%nel
  allocate(om%elni(om%nel + 1))
  allocate(eltyp(om%nel))
  allocate(elementgroup(om%nel))
  do iel = 1, om%nel
     read (umal, *) jel, eltyp(jel)
     if (jel .ne. iel) stop "wrong element ordering in the mesh file"
     om%elni(jel) = nnosc(eltyp(jel))
  end do
  call manymanycreaterowpointers(om%nel, om%elni)
  allocate(om%elno(om%elni(iel + 1) - 1))
  do i = om%nel, 1, -1
     backspace (umal)
  end do
  do iel = 1, om%nel
     read (umal, *) jel, ity, (om%elno(om%elni(jel) - 1 + k), k = 1, nnosc(ity)),elementgroup(iel)
     elementgroup(iel)=trim(adjustl(elementgroup(iel)))
  end do
!----------------
!*** node groups
!----------------
  read(umal,*)
  read(umal,*)ngroupno
  allocate(nodegroup(ngroupno),nodelist(ngroupno))
  do i=1,ngroupno
     read(umal,*)nodelist(i),nodegroup(i)
     nodegroup(i)=trim(adjustl(nodegroup(i)))
  end do
  close(umal)
!----------------------------------
!*** read the requests for cutting
!----------------------------------
  read(ucut, *)om%nmarkednodepairs
  allocate(om%markednodepairs(2, om%nmarkednodepairs), xi(om%nmarkednodepairs))
  do i = 1, om%nmarkednodepairs
     read(ucut, *)om%markednodepairs(1, i), om%markednodepairs(2, i), xi(i)
  end do
  close(ucut)
  call splitmesh(om, nm)
!-------------------
!*** write new mesh
!-------------------
  open(umal, file = "remeshed.malha", status = "unknown")
  do i = 1, nl
     write(umal, *)"Remeshed at least once"
  end do
  write(umal, *)nm%nno
  allocate(noco2(3, nm%nno))
  do in = 1, nm%nno
     in1 = nm%parentnodes(1,in)
     in2 = nm%parentnodes(2,in)
     if(in.gt.om%nno)then
        xiv=xi(in-om%nno)
        if(in1.eq.in2)then
           stop "problems in remesh execution - part I"
        end if
        noco2(1:3, in) = 0.5d00*(1.0d00-xiv)*noco(1:3,in1)+0.5d00*(1.0d00 + xiv)*noco(1:3,in2)
     else
        if(in1.ne.in2)then
           stop "problems in remesh execution - part II"
        end if
        noco2(1:3, in) = noco(1:3,in1)
     end if
     write(umal, "(I9,3E17.5)") in, (noco2(ikk, in), ikk = 1, 3)        
  end do
  write(umal,*)"ELEMENTS (ELEMENT NUMBER, TOPOLOGY TYPE, CONNECTIVITIES, MAYBE NORMAL VECTOR)"
  write(umal,"(I9,A)")nm%nel," -> THIS IS THE TOTAL NUMBER OF ELEMENTS"
  do iel=1,nm%nel        
     ia0=nm%elni(iel)
     ia1=nm%elni(iel+1)-2
     nn=ia1+1-ia0
     ity=0
     select case(nn)
     case(1)
        ity=1
     case(3)
        ity=3
     case(4)
        ity=5
     case default
        write(*,*)"error in mesh output"
        stop
     end select
     ieo=nm%elno(nm%elni(iel+1)-1)
     if(ieo.le.0)then
        stop "error in ieo"
     end if
     write(umal,"(*(I9),A)")iel,ity,(nm%elno(nm%elni(iel)-1+ik),ik=1,nn),trim(adjustl(elementgroup(ieo)))
  end do
!-------------------------------------------------
!*** integer, dimension(:), allocatable::nodelist
!*** nodelist(oldcounter)=oldnode
!*** nodegroup(oldcounter)=name
!*** nogro(nogri(oldnode)): counters of node 
!-------------------------------------------------
  call manymanymatrixtransposeoseindicesfromlist(ngroupno,om%nno,nogri,nodelist,nogro)
  ngroupno2=0
  do in=1,nm%nno
     ifather = nm%parentnodes(1,in)
     imother = nm%parentnodes(2,in)     
     if(ifather.eq.imother)then
        do ik=nogri(imother),nogri(imother+1)-1
           ngroupno2=ngroupno2+1
        end do
     else
        do ik=nogri(imother),nogri(imother+1)-1
           ngroupno2=ngroupno2+1
        end do
        do ik=nogri(ifather),nogri(ifather+1)-1
           ngroupno2=ngroupno2+1
        end do
     end if
  end do  
  write(umal,*)ngroupno2
  do in=1,nm%nno
     ifather = nm%parentnodes(1,in)
     imother = nm%parentnodes(2,in)     
     if(ifather.eq.imother)then
        do ik=nogri(imother),nogri(imother+1)-1
           nog=nogro(ik)
           write(umal,"(A)")in,trim(adjustl(nodegroup(nog)))
        end do
     else
        do ik=nogri(imother),nogri(imother+1)-1
           nog=nogro(ik)
           write(umal,"(A)")in,trim(adjustl(nodegroup(nog)))
        end do
        do ik=nogri(ifather),nogri(ifather+1)-1
           nog=nogro(ik)
           write(umal,"(A)")in,trim(adjustl(nodegroup(nog)))
        end do
     end if
  end do
  close(umal)  
!-----------------        
!*** read restart
!-----------------
!  call RESTARTIOC(.false.)
end program remesh
