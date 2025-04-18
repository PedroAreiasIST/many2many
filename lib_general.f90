!> general library
!> ----------------------
!> author:
!> This module contains general utility functions and procedures
!>---------------
!> Pedro Areias
!> IST
!>---------------
!>
!> period 1996 - 2024
!> with collaborations:
!>
!>  Timon Rabczuk
!>  Karel Matous
!>  Ted Belytschko
!>  J. Leonel Fernandes
!>  J.H. Song
!>  J César de Sá
!>  Renato Natal Jorge
!>  Eugenio Garcao
!>  Filipe Leal
!>  N. Sukumar
!>  D. Dias da Costa
!>  Rui Melicio
!>  Chat GPT-4
!>
!> modules:
!> --------------
!> basfun
!> bucketsort
!> linear
!----------------
!>
!> usage:
!> triggers on Microsoft Code:
!>
!> --------------------------------------
!> alloc*
!> conjug*
!> element*
!> eps*
!> equation*
!> file*
!> func*
!> gauss*
!> geo*
!> list*
!> manymany*
!> matrix*
!> moving*
!> radbas*
!> scalar*
!> stress*
!> string*
!> time*
!> vect*
!> voigt*
!> nesterov*
!> bucket*
!> linear*
!> --------------------------------------
!> depends on:
!> intel mkl
!> --------------------------------------
!>
!> basic functions module
MODULE basfun
  SAVE
!--------------
!*** parameters
!--------------
  INTEGER, PARAMETER::lcharacter = 25
!------------------------
!*** general declarations
!------------------------
  PRIVATE::scalarclampinteger, scalarclampreal
  PRIVATE::allocsafe1, allocsafe2, allocsafe1a, allocsafe1l, allocsafe3, allocsafe4, allocsafe5, allocsafe6, allocsafe7, allocsafe8
  INTERFACE scalarclamp
    MODULE PROCEDURE scalarclampinteger, scalarclampreal
  END INTERFACE scalarclamp
  INTERFACE allocsafe
    MODULE PROCEDURE allocsafe1, allocsafe2, allocsafe1a, allocsafe1l, allocsafe3, allocsafe4, allocsafe5, allocsafe6, allocsafe7, allocsafe8
  END INTERFACE allocsafe
!---------
!*** FILES
!---------
CONTAINS
  SUBROUTINE allocsafe1a(num, lis)
    CHARACTER(*), DIMENSION(:), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis)
      IF (igash .EQ. num) THEN
        DO i = 1, num
          lis(i) = ""
        END DO
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num))
      DO i = 1, num
        lis(i) = ""
      END DO
    END IF
  END SUBROUTINE allocsafe1a
  SUBROUTINE allocsafe1(num, lis)
    INTEGER, DIMENSION(:), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis)
      IF (igash .EQ. num) THEN
        CALL listsetconstant(num, lis)
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num), stat=kkk)
      IF (kkk .NE. 0) THEN
        WRITE (*, *) "fail to allocate integer array with"
        WRITE (*, *) num, " elements"
      END IF
      CALL listsetconstant(num, lis)
    END IF
  END SUBROUTINE allocsafe1
  SUBROUTINE allocsafe1l(num, lis)
    LOGICAL, DIMENSION(:), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis)
      IF (igash .EQ. num) THEN
        DO i = 1, num
          lis(i) = .FALSE.
        END DO
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num))
      DO i = 1, num
        lis(i) = .FALSE.
      END DO
    END IF
  END SUBROUTINE allocsafe1l
  SUBROUTINE allocsafe2(num, lis)
    REAL(8), DIMENSION(:), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis)
      IF (igash .EQ. num) THEN
        CALL vectsetconst(num, lis)
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num), stat=kkk)
      IF (kkk .NE. 0) THEN
        WRITE (*, *) "fail to allocate real array with "
        WRITE (*, *) num, " elements"
      END IF
      CALL vectsetconst(num, lis)
    END IF
  END SUBROUTINE allocsafe2
  SUBROUTINE allocsafe3(num1, num2, lis)
    INTEGER, DIMENSION(:, :), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis, 1)
      jgash = size(lis, 2)
      IF (igash .EQ. num1 .AND. jgash .EQ. num2) THEN
        CALL listsetconstant(num1*num2, lis)
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num1 .GT. 0 .AND. num2 .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num1, num2))
      CALL listsetconstant(num1*num2, lis)
    END IF
  END SUBROUTINE allocsafe3
  SUBROUTINE allocsafe4(num1, num2, lis)
    REAL(8), DIMENSION(:, :), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis, 1)
      jgash = size(lis, 2)
      IF (igash .EQ. num1 .AND. jgash .EQ. num2) THEN
        CALL vectsetconst(num1*num2, lis)
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num1 .GT. 0 .AND. num2 .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num1, num2))
      CALL vectsetconst(num1*num2, lis)
    END IF
  END SUBROUTINE allocsafe4
  SUBROUTINE allocsafe5(num1, num2, num3, lis)
    INTEGER, DIMENSION(:, :, :), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis, 1)
      jgash = size(lis, 2)
      kgash = size(lis, 3)
      IF (igash .EQ. num1 .AND. jgash .EQ. num2 .AND. kgash .EQ. num3) THEN
        CALL listsetconstant(num1*num2*num3, lis)
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num1 .GT. 0 .AND. num2 .GT. 0 .AND. num3 .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num1, num2, num3))
      CALL listsetconstant(num1*num2*num3, lis)
    END IF
  END SUBROUTINE allocsafe5
  SUBROUTINE allocsafe6(num1, num2, num3, lis)
    REAL(8), DIMENSION(:, :, :), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis, 1)
      jgash = size(lis, 2)
      kgash = size(lis, 3)
      IF (igash .EQ. num1 .AND. jgash .EQ. num2 .AND. kgash .EQ. num3) THEN
        CALL vectsetconst(num1*num2*num3, lis)
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num1 .GT. 0 .AND. num2 .GT. 0 .AND. num3 .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num1, num2, num3))
      CALL vectsetconst(num1*num2*num3, lis)
    END IF
  END SUBROUTINE allocsafe6
  SUBROUTINE allocsafe7(num1, num2, num3, num4, lis)
    INTEGER, DIMENSION(:, :, :, :), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis, 1)
      jgash = size(lis, 2)
      kgash = size(lis, 3)
      lgash = size(lis, 4)
      IF (igash .EQ. num1 .AND. jgash .EQ. num2 .AND. kgash .EQ. num3 .AND. lgash .EQ. num4) THEN
        CALL listsetconstant(num1*num2*num3*num4, lis)
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num1 .GT. 0 .AND. num2 .GT. 0 .AND. num3 .GT. 0 .AND. num4 .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num1, num2, num3, num4))
      CALL listsetconstant(num1*num2*num3*num4, lis)
    END IF
  END SUBROUTINE allocsafe7
  SUBROUTINE allocsafe8(num1, num2, num3, num4, lis)
    REAL(8), DIMENSION(:, :, :, :), ALLOCATABLE::lis
    IF (allocated(lis)) THEN
      igash = size(lis, 1)
      jgash = size(lis, 2)
      kgash = size(lis, 3)
      lgash = size(lis, 4)
      IF (igash .EQ. num1 .AND. jgash .EQ. num2 .AND. kgash .EQ. num3 .AND. lgash .EQ. num4) THEN
        CALL vectsetconst(num1*num2*num3*num4, lis)
      ELSE
        DEALLOCATE (lis)
      END IF
    END IF
    IF (num1 .GT. 0 .AND. num2 .GT. 0 .AND. num3 .GT. 0 .AND. num4 .GT. 0 .AND. .NOT. allocated(lis)) THEN
      ALLOCATE (lis(num1, num2, num3, num4))
      CALL vectsetconst(num1*num2*num3*num4, lis)
    END IF
  END SUBROUTINE allocsafe8
!-----------------------------------------------------------------
!> Clamps an integer value within a specified range.
!>
!> @param in The input integer value to be clamped.
!> @param imin The minimum allowable value.
!> @param imax The maximum allowable value.
!> @return The clamped integer value.
!-----------------------------------------------------------------
  INTEGER FUNCTION scalarclampinteger(in, imin, imax)
    INTEGER::in, imax, imin
    IF (imax .GE. imin) THEN
      scalarclampinteger = max(imin, min(imax, in))
    ELSE
      scalarclampinteger = max(imin, in)
    END IF
  END FUNCTION scalarclampinteger
!-----------------------------------------------------------------
!> Clamps a real value within a specified range
!>
!> @param in The input real value to be clamped.
!> @param imin The minimum allowable value.
!> @param imax The maximum allowable value.
!> @return The clamped real value.
!-----------------------------------------------------------------
  REAL(8) FUNCTION scalarclampreal(in, imin, imax)
    REAL(8)::in, imax, imin
    IF (imax .GE. imin) THEN
      scalarclampreal = max(imin, min(imax, in))
    ELSE
      scalarclampreal = max(imin, in)
    END IF
  END FUNCTION scalarclampreal
!--------------------------------------------------------------------------
!> This subroutine performs the conjugate gradient method for solving
!> systems of linear equations.
!>
!> @param n        The dimension of the system.
!> @param niter    The number of iterations to perform.
!> @param q        The right-hand side vector of the system.
!> @param proc     A procedure to apply the matrix-vector product.
!> @param histphi  An array to store the history of the phi values.
!> @param histgrad An array to store the history of the gradient values.
!> @param tol      The tolerance for convergence.
!> @param nfe      The number of function evaluations.
!--------------------------------------------------------------------------
  SUBROUTINE conjuggradconjugategradient(n, niter, q, proc, histphi, histgrad, tol, nfe)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n)::q, gphi, dx, dxo, dir, gphio
    REAL(8), DIMENSION(niter)::histgrad, histphi
    EXTERNAL::proc
!------------------
!*** initial values
!------------------
    dir = 0.0d00
    stp = 0.0d00
    dx = 0.d00
    nfe = 0
    DO iter = 0, niter-1
      gphio = gphi
      CALL proc(n, q, phi, gphi)
      nfe = nfe+1
      dxo = dx
      dx = -gphi
      IF (iter .EQ. 0) THEN
        beta = 0.0d00
      ELSE
        beta = max(0.0d00, vectdot(n, dx, dx-dxo)/vectdot(n, dxo, dxo))
      END IF
      dir = dx+beta*dir
      CALL conjuggradlinesearch(proc, n, q, phi, gphi, dir, stp, nfe)
      q = q+stp*dir
      histgrad(iter+1) = vectnorm2(n, gphi)
      histphi(iter+1) = phi
      IF (vectnorm2(n, gphi) .LE. tol) EXIT
    END DO
  END SUBROUTINE conjuggradconjugategradient
  !----------------------------------------------------------------------
  !> Performs a conjugate gradient line search optimization.
  !>
  !> This subroutine is used to find the minimum of a function using the
  !> conjugate gradient method with a line search.
  !>
  !> @param proc The procedure to be optimized.
  !> @param n The number of variables.
  !> @param q The current point in the variable space.
  !> @param phi The value of the function at the current point.
  !> @param g The gradient of the function at the current point.
  !> @param dir The search direction.
  !> @param stp The step size.
  !> @param nfe The number of function evaluations.
  !----------------------------------------------------------------------
  SUBROUTINE conjuggradlinesearch(proc, n, q, phi, g, dir, stp, nfe)
    IMPLICIT REAL(8) (a-h, o-z)
    EXTERNAL::proc
    REAL(8), DIMENSION(n)::g, q, gash, dir, gphii
!*** miter=5
!*** gives the smallest
!*** step as 9.53674e-7
    INTEGER::miter = 11
    stp0 = stp
    IF (abs(stp0) .LE. 1.0d-30) THEN
      stp0 = 1.0d00
      miter = 11
    END IF
    c = 1.0d-4
    gp = min(0.0d00, vectdot(n, g, dir))
    DO i = 0, miter
      stp = 4.0d00*stp0*0.5d00**i
      gash = q+stp*dir
      CALL proc(n, gash, phii, gphii)
      nfe = nfe+1
      IF (phii .LE. phi+c*stp*gp) EXIT
    END DO
  END SUBROUTINE conjuggradlinesearch
!> coth(x)
  REAL(8) FUNCTION coth(x)
    IMPLICIT REAL(8) (a-h, o-z)
    rtemp = exp(2.0d00*x)
    coth = (rtemp+1.0d00)/(rtemp-1.0d00)
  END FUNCTION coth
!> csch(x)
  REAL(8) FUNCTION csch(x)
    IMPLICIT REAL(8) (a-h, o-z)
    csch = 1.0d00/sinh(x)
  END FUNCTION csch
  !> use Gerschgorin circle theorem for the determination of critical time step
  !---------------------------------------------------------------------------
  !> Calculates the critical time step for an element.
  !!
  !! This function computes the critical time step for a given element based
  !! on the provided material properties and element mass.
  !!
  !! @param[in] meq     Material equation number.
  !! @param[in] neq     Number of equations.
  !! @param[in] emat    Element material properties.
  !! @param[in] emas    Element mass.
  !! @param[in] indmaps Index mappings.
  !! @return            The critical time step for the element.
  !---------------------------------------------------------------------------
  REAL(8) FUNCTION elementcriticaltimestep(meq, neq, emat, emas, indmaps)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(meq, *)::emat
    REAL(8), DIMENSION(*)::emas
    INTEGER, DIMENSION(*)::indmaps
!-------------------------
!*** checks for zero mass
!-------------------------
    DO ieq = 1, neq
      IF (emas(indmaps(ieq)) .LE. 0.0d00) THEN
        WRITE (*, *) "error in deltatcrit-ind"
      END IF
    END DO
!------------------------
!*** norma inf. de k.m-1
!------------------------
    rmx = 0.0d00
    DO ieq = 1, neq
      rco = 0.0d00
      DO jeq = 1, neq
        IF (emas(indmaps(jeq)) .GT. 0.0d00) THEN
          rco = rco+abs(emat(indmaps(ieq), indmaps(jeq))/emas(indmaps(jeq)))
        END IF
      END DO
      rmx = max(rmx, rco)
    END DO
    romega = sqrt(abs(rmx))
    elementcriticaltimestep = 2.0d00/romega
  END FUNCTION elementcriticaltimestep
  !> \brief Constructs an orthonormal basis from a given basis.
  !>
  !> This subroutine takes a 3x3 matrix `basis` and constructs an orthonormal basis `orthbasis`.
  !> It uses the eigenvalues and eigenvectors of the product of `basis` with itself.
  !>
  !> \param[in] basis A 3x3 matrix representing the input basis.
  !> \param[out] orthbasis A 3x3 matrix representing the orthonormal basis.
  SUBROUTINE elementframe(basis, orthbasis)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, 3)::basis, orthbasis, c
    REAL(8), DIMENSION(3, 3)::eigvc
    REAL(8), DIMENSION(3)::eigc
    ! Compute the product of `basis` with itself and store in `c`
    CALL matrixmatrixproduct(3, 3, 3, basis, basis, c, 2)
    ! Compute the eigenvalues (`eigc`) and eigenvectors (`eigvc`) of `c`
    CALL matrixeigensystem(c, eigc, eigvc, 3, ier)
    ! Construct the orthonormal basis
    DO i = 1, 3
      DO j = 1, 3
        rtemp = 0.0d00
        DO k = 1, 3
          DO l = 1, 3
            rtemp = rtemp+basis(i, k)*eigvc(k, l)*eigvc(j, l)/sqrt(eigc(l))
          END DO
        END DO
        orthbasis(i, j) = rtemp
      END DO
    END DO
  END SUBROUTINE elementframe
  !> @brief Computes the strain for a three-dimensional tetrahedral element.
  !>
  !> This subroutine calculates strain components for a tetrahedral element
  !> based on the provided number of dimensions, number of nodes, nodal
  !> coordinates, nodal values, and volume. The gradient of the shape
  !> functions is also utilized to compute the strain.
  !>
  !> @param ndi   The number of dimensions (usually 3).
  !> @param nno   The number of nodes in the tetrahedral element.
  !> @param rno   Array containing the nodal coordinates.
  !> @param rna   Array containing the nodal values.
  !> @param vol   Element volume used in strain computation.
  !> @param grad  Gradient of the shape functions at each node.
  !> @param strain The resulting strain tensor for the tetrahedral element.
  SUBROUTINE elementtritetstrain(ndi, nno, rno, rna, vol, grad, strain)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(nno, ndi)::rno, rna
    REAL(8), DIMENSION(ndi, nno)::der
    REAL(8), DIMENSION(3, 3)::grad
    REAL(8), DIMENSION(6)::strain
    CALL elementtritetshapefder(ndi, nno, rno, der, vol)
    SELECT CASE (ndi)
    CASE (3)
      CALL elementtetstrain(der, rna, grad, strain)
    CASE (2)
      CALL elementtristrain(der, rna, grad, strain)
    END SELECT
  END SUBROUTINE elementtritetstrain
  !> @brief Computes the shape function derivatives for a tetrahedron element.
  !>
  !> This routine calculates the partial derivatives of the shape functions
  !> within a tetrahedral element, as well as the element volume. These values
  !> are typically needed for finite element computations involving elemental
  !> stiffness matrices and residuals.
  !>
  !> @param xco An array containing the coordinates of the tetrahedron's vertices.
  !> @param der A 2D array that will store the computed shape function derivatives.
  !> @param vol A scalar to store the computed volume of the tetrahedron element.
  SUBROUTINE elementtetshapefder(xco, der, vol)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(4, 3)::xco
    REAL(8), DIMENSION(3, 4)::der
    REAL(8), DIMENSION(3, 3)::jaco, ijac
    jaco(1, 1) = xco(1, 1)-xco(3, 1)
    jaco(2, 1) = xco(1, 2)-xco(3, 2)
    jaco(3, 1) = xco(1, 3)-xco(3, 3)
    jaco(1, 2) = xco(2, 1)-xco(3, 1)
    jaco(2, 2) = xco(2, 2)-xco(3, 2)
    jaco(3, 2) = xco(2, 3)-xco(3, 3)
    jaco(1, 3) = xco(4, 1)-xco(3, 1)
    jaco(2, 3) = xco(4, 2)-xco(3, 2)
    jaco(3, 3) = xco(4, 3)-xco(3, 3)
    CALL matrixinverse(3, dete, jaco, ijac)
    vol = dete/6.0d00
    der(1, 1) = ijac(1, 1)
    der(2, 1) = ijac(1, 2)
    der(3, 1) = ijac(1, 3)
    der(1, 2) = ijac(2, 1)
    der(2, 2) = ijac(2, 2)
    der(3, 2) = ijac(2, 3)
    der(1, 3) = -ijac(1, 1)-ijac(2, 1)-ijac(3, 1)
    der(2, 3) = -ijac(1, 2)-ijac(2, 2)-ijac(3, 2)
    der(3, 3) = -ijac(1, 3)-ijac(2, 3)-ijac(3, 3)
    der(1, 4) = ijac(3, 1)
    der(2, 4) = ijac(3, 2)
    der(3, 4) = ijac(3, 3)
  END SUBROUTINE elementtetshapefder
  !> @brief Computes strain in a tetrahedral element based on derivatives of shape functions.
  !!
  !! This subroutine calculates the strain components by combining the
  !! derivatives of the shape functions with the nodal data for displacements
  !! in a tetrahedral (tet) element. It uses:
  !! - der : The derivatives of shape functions with respect to global coordinates.
  !! - rna : The nodal displacement values for each node in the element.
  !! - grad : The gradient values computed for the element.
  !! - strain : The resulting strain tensor components.
  SUBROUTINE elementtetstrain(der, rna, grad, strain)
    IMPLICIT NONE
    DOUBLE PRECISION v(70), der(3, 4), rna(4, 3), grad(3, 3), strain(6)
    v(65) = der(1, 1)*rna(1, 3)+der(1, 2)*rna(2, 3)+der(1, 3)*rna(3, 3)+der(1, 4)*rna(4, 3)
    v(64) = der(2, 1)*rna(1, 3)+der(2, 2)*rna(2, 3)+der(2, 3)*rna(3, 3)+der(2, 4)*rna(4, 3)
    v(63) = der(3, 1)*rna(1, 3)+der(3, 2)*rna(2, 3)+der(3, 3)*rna(3, 3)+der(3, 4)*rna(4, 3)
    v(62) = der(1, 1)*rna(1, 2)+der(1, 2)*rna(2, 2)+der(1, 3)*rna(3, 2)+der(1, 4)*rna(4, 2)
    v(61) = der(2, 1)*rna(1, 2)+der(2, 2)*rna(2, 2)+der(2, 3)*rna(3, 2)+der(2, 4)*rna(4, 2)
    v(60) = der(3, 1)*rna(1, 2)+der(3, 2)*rna(2, 2)+der(3, 3)*rna(3, 2)+der(3, 4)*rna(4, 2)
    v(59) = der(1, 1)*rna(1, 1)+der(1, 2)*rna(2, 1)+der(1, 3)*rna(3, 1)+der(1, 4)*rna(4, 1)
    v(58) = der(2, 1)*rna(1, 1)+der(2, 2)*rna(2, 1)+der(2, 3)*rna(3, 1)+der(2, 4)*rna(4, 1)
    v(57) = der(3, 1)*rna(1, 1)+der(3, 2)*rna(2, 1)+der(3, 3)*rna(3, 1)+der(3, 4)*rna(4, 1)
    grad(1, 1) = v(59)
    grad(1, 2) = v(58)
    grad(1, 3) = v(57)
    grad(2, 1) = v(62)
    grad(2, 2) = v(61)
    grad(2, 3) = v(60)
    grad(3, 1) = v(65)
    grad(3, 2) = v(64)
    grad(3, 3) = v(63)
    strain(1) = ((-1d0)+(v(59)*v(59))+(v(62)*v(62))+(v(65)*v(65)))/2d0
    strain(2) = ((-1d0)+(v(58)*v(58))+(v(61)*v(61))+(v(64)*v(64)))/2d0
    strain(3) = ((-1d0)+(v(57)*v(57))+(v(60)*v(60))+(v(63)*v(63)))/2d0
    strain(4) = v(58)*v(59)+v(61)*v(62)+v(64)*v(65)
    strain(5) = v(57)*v(59)+v(60)*v(62)+v(63)*v(65)
    strain(6) = v(57)*v(58)+v(60)*v(61)+v(63)*v(64)
  END SUBROUTINE elementtetstrain
  !> @brief Computes the strain for a triangular element given the derivatives
  !>        and nodal values.
  !>
  !> @param der   Derivatives of shape functions with respect to coordinates.
  !> @param rna   Array containing nodal values of the field variable.
  !> @param grad  Gradients used to evaluate strain.
  !> @param strain Output array for resulting strain components.
  SUBROUTINE elementtristrain(der, rna, grad, strain)
    IMPLICIT NONE
    DOUBLE PRECISION v(45), der(2, 3), rna(3, 2), grad(3, 3), strain(6)
    v(40) = der(1, 1)*rna(1, 2)+der(1, 2)*rna(2, 2)+der(1, 3)*rna(3, 2)
    v(39) = der(2, 1)*rna(1, 2)+der(2, 2)*rna(2, 2)+der(2, 3)*rna(3, 2)
    v(38) = der(1, 1)*rna(1, 1)+der(1, 2)*rna(2, 1)+der(1, 3)*rna(3, 1)
    v(37) = der(2, 1)*rna(1, 1)+der(2, 2)*rna(2, 1)+der(2, 3)*rna(3, 1)
    grad(1, 1) = v(38)
    grad(1, 2) = v(37)
    grad(1, 3) = 0d0
    grad(2, 1) = v(40)
    grad(2, 2) = v(39)
    grad(2, 3) = 0d0
    grad(3, 1) = 0d0
    grad(3, 2) = 0d0
    grad(3, 3) = 1d0
    strain(1) = ((-1d0)+(v(38)*v(38))+(v(40)*v(40)))/2d0
    strain(2) = ((-1d0)+(v(37)*v(37))+(v(39)*v(39)))/2d0
    strain(3) = 0d0
    strain(4) = v(37)*v(38)+v(39)*v(40)
    strain(5) = 0d0
    strain(6) = 0d0
  END SUBROUTINE elementtristrain
  !> @brief Calculates shape function partial derivatives for triangular elements.
  !! This subroutine computes partial derivatives of shape functions
  !! for a triangular element, based on the provided coordinates.
  !!
  !! @param xco A real array containing the coordinates of the element’s nodes.
  !! @param der A real array to store the computed partial derivatives.
  !! @param vol A real variable to hold the element’s area or volume measure.
  SUBROUTINE elementtrishapefder(xco, der, vol)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, 2)::xco
    REAL(8), DIMENSION(2, 3)::der
    rgashgg1860 = -(xco(1, 2)*xco(2, 1))
    rgashgg1861 = xco(1, 1)*xco(2, 2)
    rgashgg1862 = xco(1, 2)*xco(3, 1)
    rgashgg1863 = -(xco(2, 2)*xco(3, 1))
    rgashgg1864 = -(xco(1, 1)*xco(3, 2))
    rgashgg1865 = xco(2, 1)*xco(3, 2)
    rgashgg1866 = rgashgg1860+rgashgg1861+rgashgg1862+rgashgg1863+rgashgg1864+rgashgg1865
    rgashgg1870 = 1.0d00/rgashgg1866
    rgashgg1872 = -xco(1, 2)
    rgashgg1873 = xco(3, 2)+rgashgg1872
    rgashgg1868 = -xco(2, 2)
    rgashgg1869 = xco(1, 2)+rgashgg1868
    rgashgg1881 = -xco(3, 1)
    rgashgg1882 = xco(1, 1)+rgashgg1881
    rgashgg1878 = -xco(1, 1)
    rgashgg1879 = xco(2, 1)+rgashgg1878
    vol = 5.d-1*rgashgg1866
    der(1, 1) = -(rgashgg1869*rgashgg1870)-rgashgg1870*rgashgg1873
    der(1, 2) = rgashgg1870*rgashgg1873
    der(1, 3) = rgashgg1869*rgashgg1870
    der(2, 1) = -(rgashgg1870*rgashgg1879)-rgashgg1870*rgashgg1882
    der(2, 2) = rgashgg1870*rgashgg1882
    der(2, 3) = rgashgg1870*rgashgg1879
  END SUBROUTINE elementtrishapefder
  !!--------------------------------------------------------------------
  !! @brief     Computes the shape functions and derivatives for a
  !!            triangular tetrahedral element in a finite element mesh
  !!
  !! @param[in] ndi  Number of dimensions for the simulation
  !! @param[in] nno  Number of nodes in the element
  !! @param[in] xco  Array containing the coordinates of the element’s nodes
  !! @param[out] der Array to store the computed derivatives
  !! @param[out] vol Element volume or area as needed by the calling routine
  !!
  !! @details
  !! This subroutine calculates the shape functions and their derivatives
  !! for a 3D triangular tetrahedral element, typically used in finite
  !! element analyses. The volume (or area in 2D) is also determined. These
  !! computations are fundamental for assembling the global system of
  !! equations related to the problem being analyzed.
  !!
  !!--------------------------------------------------------------------
  SUBROUTINE elementtritetshapefder(ndi, nno, xco, der, vol)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(nno, ndi)::xco
    REAL(8), DIMENSION(ndi, nno)::der
    SELECT CASE (ndi)
    CASE (3)
      CALL elementtetshapefder(xco, der, vol)
    CASE (2)
      CALL elementtrishapefder(xco, der, vol)
    END SELECT
  END SUBROUTINE elementtritetshapefder
  !> check if two numbers are similar
  !> Returns whether two real numbers are considered sufficiently close to each other.
  !>
  !> @param r1  First real number to compare.
  !> @param r2  Second real number to compare.
  !> @return    Returns .TRUE. if both values are within an acceptable tolerance level
  !>            of each other; otherwise, returns .FALSE.
  LOGICAL FUNCTION epsaresame(r1, r2)
    IMPLICIT REAL(8) (a-h, o-z)
    epsaresame = .FALSE.
    small = tiny(small)
    IF (abs(r1-r2) .LE. small .OR. abs(r2-r1) .LE. small) epsaresame = .TRUE.
  END FUNCTION epsaresame
  !>
  !! Calculates the machine epsilon (the smallest number eps such that 1.0 + eps > 1.0)
  !! This function determines the floating-point precision of the system
  !!
  !! @returns The machine epsilon value as a double precision real number
  !!
  !! @note This is a fundamental numerical parameter that represents the upper
  !! bound on the relative error due to rounding in floating point arithmetic
  REAL(8) FUNCTION epsmach()
    epsmach = 1.0d-32
  END FUNCTION epsmach
  !> Solves an equation using Brent's method
  !!
  !! This subroutine finds the root of a function using Brent's method,
  !! which combines bisection, secant method and inverse quadratic interpolation.
  !!
  !! @param ax    Lower bound of the interval [ax,bx] where root is searched
  !! @param bx    Upper bound of the interval [ax,bx] where root is searched
  !! @param f     Function whose root is to be found
  !! @param z     Value to be compared with f (the root f(x)=z is found)
  !! @param t     Accuracy goal: the routine stops when |f(x)-z| < t
  !!
  !! @note The function f(x) must be continuous and must change sign in [ax,bx]
  !! @note The routine may fail if these conditions are not met
  SUBROUTINE equationbrent(ax, bx, f, z, t)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8)::e = 1.0d-12
    INTEGER, PARAMETER::itmax = 200
    EXTERNAL::f
    tol = max(t, epsmach())
!*** should be |f(b)|<=|f(a)|
    a = ax
    b = bx
    fa = f(a)
    fb = f(b)
    d = 0.0d00
    IF (fa*fb .GT. 0.0d00) THEN
      WRITE (*, *) "brent not bracketed"
      STOP
    END IF
    IF (abs(fa) .LE. tol) THEN
      z = a
    ELSE IF (abs(fb) .LE. tol) THEN
      z = b
    ELSE IF (abs(fa) .LT. abs(fb)) THEN
      c = b
      fc = fb
      DO iter = 1, itmax
        IF (((fb .GT. 0.0d00) .AND. (fc .GT. 0.0d00)) .OR. ((fb .LT. 0.0d00) .AND. (fc .LT. 0.0d00))) THEN
          c = a
          fc = fa
          d = b-a
          e = d
        END IF
        IF (abs(fc) .LT. abs(fb)) THEN
          a = b
          b = c
          c = a
          fa = fb
          fb = fc
          fc = fa
        END IF
        xm = 0.5d00*(c-b)
        IF ((abs(xm) .LE. tol) .OR. (abs(fb) .LE. tol)) THEN
          z = b
          EXIT
        END IF
        IF ((abs(e) .GE. tol) .AND. (abs(fa) .GT. abs(fb))) THEN
          s = fb/fa
          IF (abs(a-c) .LT. epsmach()) THEN
!inverse quadratic interpolation
            p = 2.0d00*xm*s
            q = 1.0d00-s
          ELSE
!linear interpolation
            q = fa/fc
            r = fb/fc
            p = s*(2.0d00*xm*q*(q-r)-(b-a)*(r-1.0d00))
            q = (q-1.0d00)*(r-1.0d00)*(s-1.0d00)
          END IF
          IF (p .GT. 0.0d00) q = -q
          p = abs(p)
          IF ((2.0d00*p) .LT. min(abs(e*q), 3.0d00*xm*q-abs(tol*q))) THEN
            e = d
            d = p/q
          ELSE
            d = xm
            e = d
          END IF
        ELSE
          d = xm
          e = d
        END IF
        a = b
        fa = fb
        IF (abs(d) .GT. tol) THEN
          b = b+d
        ELSE
          b = b+sign(tol, xm)
        END IF
        fb = f(b)
      END DO
      z = b
    END IF
  END SUBROUTINE equationbrent
  !> Solves a cubic equation of the form at*x^3 + bt*x^2 + ct*x + dt = 0
  !! @param at Real input, coefficient of x^3 term
  !! @param bt Real input, coefficient of x^2 term
  !! @param ct Real input, coefficient of x term
  !! @param dt Real input, independent term
  !! @param x1 Real output, first root of the cubic equation
  !! @param x2 Real output, second root of the cubic equation
  !! @param x3 Real output, third root of the cubic equation
  !! @note The roots can be real or complex depending on the coefficients
  !! @see https://en.wikipedia.org/wiki/Cubic_equation for mathematical background
  SUBROUTINE equationcubic(at, bt, ct, dt, x1, x2, x3)
    IMPLICIT REAL(8) (a-z)
    INTEGER::is
    small = tiny(small)
    x1 = 0.0d00
    x2 = 0.0d00
    x3 = 0.0d00
    IF (abs(at) .LE. small) THEN
!*** inclui casos em que b e/ou c possam ser zero
      CALL equationquadratic(x1, x2, bt, ct, dt, is)
    ELSE
!*** divide todos os termos por at
      rtemp = 1.0d00/at
      a = bt*rtemp
      b = ct*rtemp
      c = dt*rtemp
!*** define algumas constantes
      dois = 2.0d00
      tres = 3.0d00
      r13 = 1.0d00/tres
      seis = tres*dois
      nove = seis+tres
      r19 = 1.0d00/nove
      r154 = 1.0d00/(seis*nove)
      pi = 4.0d00*atan(1.0d00)
      a2 = a*a
      a3 = a2*a
      q = (a2-tres*b)*r19
      r = (dois*a3-nove*a*b+nove*tres*c)*r154
      r2 = r*r
      q3 = q*q*q
      r13a = r13*a
      IF (r2 .LT. q3) THEN
        sq3 = sqrt(q3)
        sq = sqrt(q)
        sq2 = dois*sq
        theta = acos(r/sq3)
        doispi = dois*pi
        x1 = -sq2*cos(theta*r13)-r13a
        x2 = -sq2*cos((theta+doispi)*r13)-r13a
        x3 = -sq2*cos((theta-doispi)*r13)-r13a
      ELSE
        ar = abs(r)
        aa = -sign(r, 1.0d00)*(ar+sqrt(r2-q3))**(1.0d00/3.0d00)
        IF (abs(aa) .LE. small) THEN
          bb = 0.0d00
        ELSE
          bb = q/aa
        END IF
        x1 = (aa+bb)-r13a
      END IF
    END IF
  END SUBROUTINE equationcubic
!> solve linear equation
  REAL(8) FUNCTION equationlinear(x0, x1, f0, f1)
    IMPLICIT REAL(8) (a-h, o-z)
    IF (abs(f0-f1) .LE. tiny(f0) .OR. sign(1.0d00, f0)*sign(1.0d00, f1) .GT. 0.0d00) STOP "error equationlinear"
    equationlinear = (x1*f0-x0*f1)/(f0-f1)
  END FUNCTION equationlinear
!> solve quadratic equation
  !> Solves a quadratic equation in the form ax^2 + bx + c = 0
  !! @param[out] z1    First root of the equation
  !! @param[out] z2    Second root of the equation
  !! @param[in]  a     Coefficient of the quadratic term
  !! @param[in]  b     Coefficient of the linear term
  !! @param[in]  c     Independent term
  !! @param[out] is    Status flag: 0 = success, 1 = no real solutions
  SUBROUTINE equationquadratic(z1, z2, a, b, c, is)
    IMPLICIT REAL(8) (a-h, o-z)
    small = epsmach()
    z1 = 0.0d00
    z2 = 0.0d00
    is = 0
    ! Handle the case where a is effectively zero (linear equation)
    IF (abs(a) .LE. small) THEN
      IF (abs(b) .GT. small) THEN
        is = 1
        z1 = -c/b
      END IF
      RETURN
    END IF
    ! Compute discriminant
    d = b*b-4.0d00*a*c
    ! No real roots
    IF (d .LT. -small) RETURN
    ! One real root (repeated)
    IF (abs(d) .LE. small) THEN
      is = 1
      z1 = -0.5d00*b/a
      RETURN
    END IF
    ! Two real roots - use numerically stable formula
    is = 2
    IF (b .GE. 0.0d00) THEN
      q = -0.5d00*(b+sign(sqrt(d), b))
    ELSE
      q = -0.5d00*(b-sqrt(d))
    END IF
    z1 = q/a
    z2 = c/q
    ! Ensure z1 <= z2
    IF (z1 .GT. z2) THEN
      temp = z1
      z1 = z2
      z2 = temp
    END IF
  END SUBROUTINE equationquadratic
!> solve equation with safe Newton
  SUBROUTINE equationsafenewton(x1, x2, fx, dfx, x, tol, ife)
    IMPLICIT REAL(8) (a-h, o-z)
    EXTERNAL::fx, dfx
    INTEGER, PARAMETER::maxit = 300
    ife = 0
    tol1 = 2.0d00*epsmach()*min(abs(x1), abs(x2))+0.5d00*max(0.0d00, tol)
    fl = fx(x1)
    fh = fx(x2)
    ife = ife+2
    IF ((fl .GT. 0.0d00 .AND. fh .GT. 0.0d00) .OR. (fl .LT. 0.0d00 .AND. fh .LT. 0.0d00)) THEN
      STOP "pre-bracket the function in equationsafenewton"
    END IF
    IF (abs(fl) .LE. 1.0d-14) THEN
      x = x1
      RETURN
    ELSE IF (abs(fh) .LE. 1.0d-14) THEN
      x = x2
      RETURN
    ELSE IF (fl .LT. 0.0d00) THEN
      xl = x1
      xh = x2
    ELSE
      xh = x1
      xl = x2
    END IF
    IF (abs(x) .LE. 1.0d-14 .OR. x .GT. x2 .OR. x .LT. x1) x = 0.5d00*(x1+x2)
    dxold = abs(x2-x1)
    dx = dxold
    f = fx(x)
    df = dfx(x)
    ife = ife+1
    DO j = 1, maxit
      IF (((x-xh)*df-f)*((x-xl)*df-f) .GT. 0.0d00 &
          .OR. abs(2.0d00*f) .GT. abs(dxold*df)) THEN
        dxold = dx
        dx = 0.5d00*(xh-xl)
        x = xl+dx
        IF (abs(xl-x) .LE. 1.0d-14) RETURN
      ELSE
        dxold = dx
        dx = f/df
        temp = x
        x = x-dx
        IF (abs(temp-x) .LE. 1.0d-14) RETURN
      END IF
      IF (abs(dx) .LT. tol1) RETURN
      f = fx(x)
      IF (abs(f) .LE. 1.0d-14) RETURN
      df = dfx(x)
      ife = ife+1
      IF (f .LT. 0.0d00) THEN
        xl = x
      ELSE
        xh = x
      END IF
    END DO
    ife = -ife
  END SUBROUTINE equationsafenewton
!>--------------
!> dynrelaxdense
!>--------------
!> mitera:    maximum nr of iterations for a
!> na:        number of dimensions of a
!> a:         a unknowns
!> tolera:    tolerance for nonlinear iteration for a
!> scala:     scaling of variables a
!> resjaca:   callback for residual and jacobian in a
!> miterq:    maximum nr of iterations
!> nq:        number of dimensions of q
!> q:         q unknowns
!> tolerq:    tolerance for nonlinear iteration for q
!> scalq:     scaling of variables q
!> resjacq:   callback for residual and jacobian in q
!> iconv:     has reached convergence?
!> miter:     number of substeps
!> tol:       tolerance for substepping, can be zero
!> iter:      number of iterations performed
  SUBROUTINE equationdynrelaxdense(mitera, na, a, tolera, scala, resjaca, miterq, nq, q, tolerq, scalq, resjacq, iconv, miter, tol, iter)
    IMPLICIT REAL(8) (a-h, o-z)
    EXTERNAL::resjaca, resjacq
    REAL(8)::tol, tolera, tolerq
    INTEGER::iconva, iconvq, iconv, miter, mitera, miterq
    REAL(8), DIMENSION(na)::a, scala
    REAL(8), DIMENSION(nq)::q, scalq
    iconv = 1
    CALL equationdynrelax(miter, na, a, solvea, nq, q, solveq, tol, iter)
  CONTAINS
    SUBROUTINE solvea(nna, aa)
      IMPLICIT REAL(8) (a-h, o-z)
      REAL(8), DIMENSION(nna)::aa
      CALL equationdoglegdense(nna, mitera, scala, tolera, resjaca, aa, iconva)
      IF (iconva .EQ. 0) THEN
        WRITE (*, *) "failed to converge in a it, a=", aa
        WRITE (*, *) "failed to converge in a it, q=", q
        READ (*, *)
        iconv = 0
      END IF
    END SUBROUTINE solvea
    SUBROUTINE solveq(nnq, qq, qtil)
      IMPLICIT REAL(8) (a-h, o-z)
      REAL(8), DIMENSION(nnq)::qq, qtil
      qtil = qq
      CALL equationdoglegdense(nnq, miterq, scalq, tolerq, resjacq, qtil, iconvq)
      IF (iconvq .EQ. 0) THEN
        WRITE (*, *) "failed to converge in q it, a=", a
        WRITE (*, *) "failed to converge in q it, q=", qq
        READ (*, *)
        iconv = 0
      END IF
    END SUBROUTINE solveq
  END SUBROUTINE equationdynrelaxdense
!>-------------------
!> dynamic relaxation
!>-------------------
!> miter:              number of iterations
!> na:                 number of "a" unknowns
!> a[na]:              "a" unknowns
!> sola(na,a):         solution for a
!> nq:                 number of "q" unknowns
!> q[nq]:              "q" unknowns
!> solq(nq,q,qtil)     from q calculates qtil
!> tol:                error tolerance
!> iter (out)          number of performed substeps
  SUBROUTINE equationdynrelax(miter, na, a, sola, nq, q, solq, tol, iter)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8)::tol
    REAL(8), DIMENSION(na)::a
    REAL(8), DIMENSION(nq)::q, qtil, rq, rqold, rq0
    EXTERNAL::sola, solq
!> solution stage
    CALL sola(na, a)
    CALL solq(nq, q, qtil)
    iconv = 0
    rq0 = qtil-q
    rq = rq0
    w = 0.5d00
    q = (1.0d00-w)*q+w*qtil
!> iteration stage
!> of course, above was the
!> first "iteration"
    DO iter = 1, miter-1
!> solution stage
      CALL sola(na, a)
      CALL solq(nq, q, qtil)
      rqold = rq
      rq = qtil-q
!> check for convergence
      IF (miter .GT. 2) THEN
        IF ((iter .GE. 2) .AND. (vectnorm2(nq, rq) .LE. tol*vectnorm2(nq, rq0))) THEN
          iconv = 1
          EXIT
        END IF
      ELSE
        IF (vectnorm2(nq, rq) .LE. tol*vectnorm2(nq, rq0)) THEN
          iconv = 1
          EXIT
        END IF
      END IF
      gash = vectdot(nq, rqold-rq, rqold-rq)
      IF (abs(gash) .GT. 1.0d-20) THEN
        w = w*(1.0d00+vectdot(nq, rqold-rq, rq)/vectdot(nq, rqold-rq, rqold-rq))
      END IF
      IF (w .LT. 0.0d00) w = 0.0001d00
      IF (w .GT. 1.0d00) w = 1.0d00
      q = (1.0d00-w)*q+w*qtil
    END DO
  END SUBROUTINE equationdynrelax
!>---------------------------------------
!> perform a newton iteration with dogleg
!> with dogleg on a dense system
!>---------------------------------------
!> n:             number of unknowns
!> miter:         maximum number of iterations
!> scal[n]:       scaling diagonal matrix
!> toler:         tolerance
!> resjaccall:    residual and jacobian callback
!> a[n]:          unknowns and solution
!> iconv:         0-> not converged, 1-> converged
  SUBROUTINE equationdoglegdense(n, miter, scal, toler, resjaccall, a, iconv)
    IMPLICIT NONE
    EXTERNAL::resjaccall
    INTEGER::n, miter, iconv, iter
    REAL(8)::toler
    REAL(8), DIMENSION(n)::a, scal, res
    REAL(8), DIMENSION(n, n)::jac, ijac
    CALL equationdoglegnewton(n, miter, scal, toler, statedet, jacttimesres, jactimesda, jacinvtimesres, a, iconv, iter, .FALSE.)
  CONTAINS
    SUBROUTINE statedet(nn, aa)
      INTEGER::nn
      REAL(8)::det
      REAL(8), DIMENSION(nn)::aa
      CALL resjaccall(n, aa, res, jac)
      CALL matrixinverse(n, det, jac, ijac)
    END SUBROUTINE statedet
    SUBROUTINE jacttimesres(nn, dasd)
      INTEGER::nn
      REAL(8), DIMENSION(nn)::dasd
      CALL matrixtranspvectorproduct(nn, nn, jac, res, dasd)
    END SUBROUTINE jacttimesres
    SUBROUTINE jactimesda(nn, dasd, tprod)
      INTEGER::nn
      REAL(8), DIMENSION(nn)::dasd, tprod
      CALL matrixvectorproduct(nn, nn, jac, dasd, tprod)
    END SUBROUTINE jactimesda
    SUBROUTINE jacinvtimesres(nn, danew)
      INTEGER::nn
      REAL(8), DIMENSION(nn)::danew
      CALL matrixvectorproduct(nn, nn, ijac, res, danew)
    END SUBROUTINE jacinvtimesres
  END SUBROUTINE equationdoglegdense
!>-------------------------------
!> perform a newton iteration
!> with dogleg on a sparse system
!>-------------------------------
!>
!> n:number of unknowns
!> miter: maximum number of iterations
!> scal[n]: scaling diagonal matrix
!> toler: tolerance
!> rescall: residual callback rescall(n,res)
!> jaccall: jacobian callback jaccall(n,ma,mia,mja)
!> a(n): unknowns and solution
!> iconv: 0-> not converged, 1-> converged
  SUBROUTINE equationdoglegsparse(refresh, symm, n, miter, scal, toler, rescall, jaccall, a, iconv)
    IMPLICIT NONE
    LOGICAL::refresh, symm
    EXTERNAL::rescall, jaccall
    INTEGER::n, miter, iconv, iter
    REAL(8)::toler
    REAL(8), DIMENSION(n)::a, scal, res
    REAL(8), DIMENSION(:), ALLOCATABLE::ma
    INTEGER, DIMENSION(:), ALLOCATABLE::mia, mja
    CALL equationdoglegnewton(n, miter, scal, toler, statedet, jacttimesres, jactimesda, jacinvtimesres, a, iconv, iter, .FALSE.)
  CONTAINS
    SUBROUTINE statedet(nn, aa)
      INTEGER::nn
      REAL(8), DIMENSION(nn)::aa
      CALL rescall(n, aa, res)
      CALL jaccall(n, ma, mia, mja)
    END SUBROUTINE statedet
    SUBROUTINE jacttimesres(nn, dasd)
      INTEGER::nn
      REAL(8), DIMENSION(nn)::dasd
      CALL manymanymatrixtransposedtimesvector(nn, mia, mja, ma, res, dasd)
    END SUBROUTINE jacttimesres
    SUBROUTINE jactimesda(nn, dasd, tprod)
      INTEGER::nn
      REAL(8), DIMENSION(nn)::dasd, tprod
      CALL manymanymatrixtimesvector(nn, mia, mja, ma, dasd, tprod)
    END SUBROUTINE jactimesda
    SUBROUTINE jacinvtimesres(nn, danew)
      INTEGER::nn
      REAL(8), DIMENSION(nn)::danew
      CALL manymanymklsolve(refresh, symm, 1, nn, mia, mja, ma, res, danew)
    END SUBROUTINE jacinvtimesres
  END SUBROUTINE equationdoglegsparse
!>---------------
!> scale a vector
!> for dogleg
!>---------------
!> back:
!>     .true. == convert to original
!>     .false. == convert to scaled
!> n: size of vector
!> scal(n): scaling diagonal matrix
!> updated(n) (io): updated vector
  SUBROUTINE equationdoglegscale(back, n, scal, updated)
    IMPLICIT NONE
    LOGICAL::back
    INTEGER::n, i
    REAL(8)::sn
    REAL(8), DIMENSION(n)::scal, updated
    sn = sqrt(1.0d00*n)
    SELECT CASE (back)
    CASE (.FALSE.)
      DO i = 1, n
        updated(i) = updated(i)*scal(i)/sn
      END DO
    CASE (.TRUE.)
      DO i = 1, n
        updated(i) = updated(i)*sn/scal(i)
      END DO
    END SELECT
  END SUBROUTINE equationdoglegscale
!>----------------------------------------------------
!> perform a newton iteration with dogleg trust region
!>----------------------------------------------------
!> n:                   number of unknowns
!> miter:               maximum number of iterations
!> scal[n]:             scaling matrix
!> toler:               tolerance for convergence
!> statecall:           callback for state determination
!> jacttimesrescall:    jacobian transpose times residual callback
!> jactimesdacall:      jacobian times da callback
!> jacinvtimesrescall:  inverse jacobian times residual callback
!> a:                   trial value and solution
!> iconv:               .true. if convergence was achieved
!> iter:                number of iterations performed
  SUBROUTINE equationdoglegnewton(n, miter, scal, toler, statecall, jacttimesrescall, jactimesdacall, jacinvtimesrescall, a, iconv, iter, verboose)
    IMPLICIT NONE
    EXTERNAL::statecall, jacttimesrescall, jactimesdacall, jacinvtimesrescall
    INTEGER::n, miter, iconv, i, iter, it
    LOGICAL::verboose
    REAL(8)::sne, rda, toler, t, ssd, gamma, eta, c, ctemp, dtemp, etemp
    REAL(8), DIMENSION(n)::a, scal
    REAL(8), DIMENSION(n)::dasd, tprod, danew, da, dans
    INTEGER::algos
    REAL(8)::errors
    REAL(8), DIMENSION(n)::path
!--------------
! zero outputs:
!--------------
    algos = 0
    errors = 0.0d00
    iconv = 0
    path = a
!-----------------
! iteration cycle:
!-----------------
    DO it = 1, miter
      iter = it
!-------------------------------------
! calculate all (which can be costly):
!-------------------------------------
      CALL statecall(n, a)
!-------------------------
! steepest descent/cauchy:
! only depends on state
!-------------------------
      CALL jacttimesrescall(n, dasd)
      dasd = -dasd
!----------------------
! dasd here is original
!----------------------
      DO i = 1, 2
        CALL equationdoglegscale(.TRUE., n, scal, dasd)
      END DO
!-----------------------------------------------------
! above was a double-scaling to avoid ill-conditioning
! dasd here is still the original(!) but "hat hat"
!-----------------------------------------------------
      CALL jactimesdacall(n, dasd, tprod)
!------------------------------------------------------------
! above is the product of j and da (original) stored in tprod
!------------------------------------------------------------
      t = vectdot(n, dasd, dasd)/vectdot(n, tprod, tprod)
      dasd = t*dasd
!-------------------------------------------------
! dasd still original, but now with correct length
!-------------------------------------------------
      CALL equationdoglegscale(.FALSE., n, scal, dasd)
!-------------------
! dasd is now scaled
! (dasd hat)
!-------------------
      ssd = sqrt(dot_product(dasd, dasd))
      IF (ssd .GT. 1.0d00) THEN
        da = dasd
        algos = 1
      ELSE
        CALL jacinvtimesrescall(n, danew)
        danew = -danew
!---------------------
! danew is newton step
!---------------------
        CALL equationdoglegscale(.FALSE., n, scal, danew)
!--------------------
! danew is now scaled
!--------------------
        sne = sqrt(dot_product(danew, danew))
!--------------------------------------
! sne is the norm of scaled newton step
!--------------------------------------
        IF (sne .LE. 1.0d00) THEN
          da = danew
          algos = 3
!---------------------------------------------------------
! if it is less than or equal to one, then accept the step
! note that da is scaled
!---------------------------------------------------------
        ELSE
!-------------------------------------
! parameters for the dogleg algorithm:
!-------------------------------------
          gamma = ssd/sne
          eta = 0.8d00*gamma+0.2d00
          danew = eta*danew
!----------------------------------------------------------------------------------------
! danew is now the cutted the newton step to solve for the intersection with trust region
!----------------------------------------------------------------------------------------
          dans = danew-dasd
!--------------------------------------------------------
! dans equals newton minus steepest descent (both scaled)
!--------------------------------------------------------
          ctemp = dot_product(dans, dasd)
          dtemp = dot_product(dans, dans)
          etemp = dot_product(dasd, dasd)
!-------------------------------------------
! above temporary values for the dogleg root
!-------------------------------------------
          c = (-ctemp+sqrt(ctemp**2+dtemp*(1.0d00-etemp)))/dtemp
          da = c*danew+(1.0d00-c)*dasd
!-----------------------------
! above is the root for dogleg
!-----------------------------
          algos = 2
        END IF
      END IF
      rda = sqrt(dot_product(da, da))
      errors = rda
!--------------------------------------------
! now don't forget to set it back to physical
! coordinates
!--------------------------------------------
      CALL equationdoglegscale(.TRUE., n, scal, da)
!-----------------------
! da is now the original
! update a:
!-----------------------
      a = a+da
      path = a
      IF (verboose) WRITE (*, "(I3,E13.4,*(E13.4))") algos, errors, path
!----------------------------
! now checks for convergence:
!----------------------------
      IF ((rda .LE. toler) .AND. (it .GE. 3)) THEN
        iconv = 1
        EXIT
      END IF
    END DO
  END SUBROUTINE equationdoglegnewton
!> close all files from 1 to 200
  SUBROUTINE filecloseall()
    LOGICAL::ope
    INTEGER, PARAMETER::mu = 200
    DO iu = 1, mu
      INQUIRE (unit=iu, opened=ope, iostat=ic1)
      IF (ic1 .NE. 0) EXIT
      IF (ope) THEN
        CLOSE (iu, iostat=ic2, status="keep")
        IF (ic2 .NE. 0) EXIT
      END IF
    END DO
  END SUBROUTINE filecloseall
!> count lines up to a string
  FUNCTION filecountlines(iuni, str)
    INTEGER, PARAMETER::mst = 160
    CHARACTER(mst)::str2
    CHARACTER(*)::str
    icon = 0
    jlen = len_trim(trim(str))
    REWIND (iuni)
    DO
      READ (iuni, "(a)", iostat=io) str2
      IF (io .NE. 0) EXIT
      IF (str(1:jlen) .EQ. str2(1:jlen)) THEN
        icon = icon+1
      END IF
    END DO
    filecountlines = icon
  END FUNCTION filecountlines
!> opens file with specific format and extension
  SUBROUTINE fileopen(nome, exte, iuni, ijob, unfr, ierr)
    INTEGER, PARAMETER::mst = 160
    CHARACTER(*)::nome, exte, ijob
    CHARACTER(mst)::ijob2, nome2, exte2
    LOGICAL::unfr
    ijob2 = " "
    ijob2(1:len(ijob)) = ijob
    CALL stringlowercase(ijob2)
    nome2 = " "
    nome2(1:len(nome)) = nome
    CALL stringlowercase(nome2)
    exte2 = " "
    exte2(1:len(exte)) = exte
    CALL stringlowercase(exte2)
    IF (iuni .LE. 0) iuni = fileprovidechannel()
    CLOSE (iuni, iostat=igash)
    IF (unfr) THEN
      IF (ijob2 .EQ. "substitui") THEN
        OPEN (iuni, file=trim(nome2)//"."//trim(exte2), status='replace', &
              form='unformatted', iostat=ierr, access='sequential')
      ELSEIF (ijob2 .EQ. "continua") THEN
        OPEN (iuni, file=trim(nome2)//"."//trim(exte2), status='unknown', &
              position="append", form='unformatted', iostat=ierr, access='sequential')
      ELSEIF (ijob2 .EQ. "desconhecido") THEN
        OPEN (iuni, file=trim(nome2)//"."//trim(exte2), status='unknown', &
              form='unformatted', iostat=ierr, access='sequential')
      ELSEIF (ijob2 .EQ. "newvector") THEN
        OPEN (iuni, file=trim(nome2)//"."//trim(exte2), status='new', &
              form='unformatted', iostat=ierr, access='sequential')
      END IF
    ELSE
      IF (ijob2 .EQ. "substitui") THEN
        OPEN (iuni, file=trim(nome2)//"."//trim(exte2), status='replace', &
              iostat=ierr, access='sequential')
      ELSEIF (ijob2 .EQ. "continua") THEN
        OPEN (iuni, file=trim(nome2)//"."//trim(exte2), status='unknown', &
              position="append", iostat=ierr, access='sequential')
      ELSEIF (ijob2 .EQ. "desconhecido") THEN
        OPEN (iuni, file=trim(nome2)//"."//trim(exte2), status='unknown', &
              iostat=ierr, access='sequential')
      ELSEIF (ijob2 .EQ. "newvector") THEN
        OPEN (iuni, file=trim(nome2)//"."//trim(exte2), status='new', &
              iostat=ierr, access='sequential')
      END IF
    END IF
  END SUBROUTINE fileopen
  !> output to ensight
  SUBROUTINE fileoutputensight( &
    number, filename, &
    nnoe, x, &
    nele, &
    nscalar, nvector, ntensor, &
    cscalar, cvector, ctensor, &
    scalar, vector, tens, &
    nescalar, nevector, netensor, &
    cescalar, cevector, cetensor, &
    escalar, evector, etensor, names, nelpr, ncnos, elni, elno)
  
    !---------------------------------------------------------------------
    ! This subroutine writes simulation output files for CEI Ensight and
    ! ParaView in a fixed, ASCII format. It uses implicit declarations for
    ! variables whose names begin with I-N (e.g., i, j, k, itp, etc.).
    !
    ! External routines used:
    !   - fileprovidechannel(): returns a file unit number.
    !   - allocsafe(nele, ellist): allocates the integer array ellist.
    !
    ! The following variables are implicitly declared:
    !   i, j, k, itp, it, in, ik, iti, ilimit, ikount, nloop, ntemp, iz, ikk
    !---------------------------------------------------------------------
  
    INTEGER, PARAMETER :: mtp = 7
    CHARACTER(80)     :: texto
    CHARACTER(79)     :: ext
    REAL(8), DIMENSION(3, *) :: x
    CHARACTER(*)      :: filename
    CHARACTER(3)      :: pred
    CHARACTER(6), DIMENSION(*) :: names
    INTEGER, DIMENSION(*)   :: nelpr, ncnos, elni, elno
    REAL(8), DIMENSION(:, :) :: scalar
    REAL(8), DIMENSION(:, :, :) :: vector
    REAL(8), DIMENSION(:, :, :) :: tens
    REAL(8), DIMENSION(:, :) :: escalar
    REAL(8), DIMENSION(:, :, :) :: evector
    REAL(8), DIMENSION(:, :, :) :: etensor
    CHARACTER(*), DIMENSION(*) :: cscalar, cvector, ctensor, cescalar, cevector, cetensor
    INTEGER, DIMENSION(:), ALLOCATABLE :: ellist
    CHARACTER(3)     :: advstring
  
    ! Check that the number of nodes or elements does not exceed limits.
    IF (nnoe .GT. 99999999 .OR. nele .GT. 99999999) THEN
      WRITE (*, *) "too large problem, more than 99999999 elements or nodes"
      STOP
    END IF
  
    ! Limit the number of output steps.
    IF (number .GT. 9999) THEN
      WRITE (*, *) "too many output files, please limit them to 9999 steps"
      STOP
    END IF
  
    ! Write the step number into texto using an i4 format.
    WRITE (texto, "(i4)") number
  
    ! Select a prefix for the step number so file names sort naturally.
    IF (number .LE. 9) THEN
      pred = "000"
    ELSEIF (number .LE. 99) THEN
      pred = "00"
    ELSEIF (number .LE. 999) THEN
      pred = "0"
    ELSE
      pred = ""
    END IF
  
    !--- Open the geometry file (.geo) ---
    ! Obtain a file unit from fileprovidechannel() and build the file extension.
    iun = fileprovidechannel()
    texto = trim(adjustl(pred)) // trim(adjustl(texto))
    ext = trim(texto)
    OPEN (iun, file = trim(adjustl(filename)) // ".geo" // trim(adjustl(ext)), status="unknown")
  
    ! Write header lines for the geometry file.
    texto = "line 1"
    WRITE (iun, "(a)") texto
    texto = "line 2"
    WRITE (iun, "(a)") texto
    WRITE (iun, "(a)") "node id given"
    WRITE (iun, "(a)") "element id given"
    WRITE (iun, "(a)") "coordinates"
    WRITE (iun, "(i8)") nnoe
  
    ! Write node data: each node’s id and its three coordinates.
    DO in = 1, nnoe
      WRITE (iun, "(i8,3e12.5)") in, (x(i, in), i = 1, 3)
    END DO
  
    ! Write part header and connectivity data.
    WRITE (iun, "(a)") "part 1"
    texto = "only one part"
    WRITE (iun, "(a)") texto
    CALL allocsafe(nele, ellist)
  
    DO itp = 1, mtp
      ikount = 0
      DO i = 1, nele
        it = nelpr(i)
        IF (it .EQ. itp) THEN
          ikount = ikount + 1
          ellist(ikount) = i
        END IF
      END DO
      IF (ikount .NE. 0) THEN
        texto = names(itp)
        WRITE (iun, "(a)") " "
        WRITE (iun, "(a)") adjustl(texto)
        WRITE (iun, "(i8)") ikount
        iti = 0
        ilimit = ncnos(itp)
        DO i = 1, ikount
          WRITE (iun, "(*(i8))") ellist(i), (elno(ikk), ikk = elni(ellist(i)), elni(ellist(i)) + ilimit - 1)
        END DO
      END IF
    END DO
  
    CLOSE (iun)
  
    !--- Write the case file (.case) ---
    iun = fileprovidechannel()
    OPEN (iun, file = trim(adjustl(filename)) // ".case", status="unknown")
    WRITE (iun, "(a)") "FORMAT"
    WRITE (iun, "(a)") "type:   ensight"
    WRITE (iun, "(a)") "GEOMETRY"
    WRITE (iun, "(a)") "model: 1" // " " // trim(adjustl(filename)) // ".geo" // "****"
    WRITE (iun, "(a)") "VARIABLE"
  
    ! List node-based variables.
    DO i = 1, nscalar
      WRITE (iun, "(a)") "scalar per node:" // " 1 " // trim(cscalar(i)) // " " // trim(adjustl(filename)) // trim(cscalar(i)) // ".res" // "****"
    END DO
    DO i = 1, nvector
      WRITE (iun, "(a)") "vector per node:" // " 1 " // trim(cvector(i)) // " " // trim(adjustl(filename)) // trim(cvector(i)) // ".res" // "****"
    END DO
    DO i = 1, ntensor
      WRITE (iun, "(a)") "tensor symm per node:" // " 1 " // trim(ctensor(i)) // " " // trim(adjustl(filename)) // trim(ctensor(i)) // ".res" // "****"
    END DO
  
    ! List element-based variables.
    DO i = 1, nescalar
      WRITE (iun, "(a)") "scalar per element:" // " 1 " // trim(cescalar(i)) // " " // trim(adjustl(filename)) // trim(cescalar(i)) // ".res" // "****"
    END DO
    DO i = 1, nevector
      WRITE (iun, "(a)") "vector per element:" // " 1 " // trim(cevector(i)) // " " // trim(adjustl(filename)) // trim(cevector(i)) // ".res" // "****"
    END DO
    DO i = 1, netensor
      WRITE (iun, "(a)") "tensor symm per element:" // " 1 " // trim(cetensor(i)) // " " // trim(adjustl(filename)) // trim(cetensor(i)) // ".res" // "****"
    END DO
  
    ! Write time information.
    WRITE (iun, "(a)") "TIME"
    WRITE (iun, "(a,i8)") "time set:" // " ", 1
    WRITE (iun, "(a,i8)") "number of steps:" // " ", number + 1
    WRITE (iun, "(a,i8)") "filename start number:" // " ", 0
    WRITE (iun, "(a,i8)") "filename increment:" // " ", 1
    WRITE (iun, "(a)") "time values:"
  
    nloop = (number + 1) / 5
    ntemp = mod(number + 1, 5)
    ik = 0
    DO i = 1, nloop
      iz = ik
      WRITE (iun, "(5i8)") (j, j = iz, iz + 4)
      ik = iz + 5
    END DO
    iz = ik
    WRITE (iun, "(5i8)") (i, i = iz, iz + ntemp - 1)
    CLOSE (iun)
  
    !--- Write node-based result files ---
    DO i = 1, nscalar
      iun = fileprovidechannel()
      OPEN (iun, file = trim(adjustl(filename)) // trim(cscalar(i)) // ".res" // trim(adjustl(ext)), status="unknown")
      WRITE (iun, "(a)") trim(cscalar(i))
      ik = 1
      advstring = "no"
      DO j = 1, nnoe
        ik = ik + 1
        WRITE (iun, "(e12.5)", advance = trim(advstring)) scalar(i, j)
        IF (ik .EQ. 6) THEN
          ik = 0
          advstring = "yes"
        ELSE
          advstring = "no"
        END IF
      END DO
      CLOSE (iun)
    END DO
  
    DO i = 1, nvector
      iun = fileprovidechannel()
      OPEN (iun, file = trim(adjustl(filename)) // trim(cvector(i)) // ".res" // trim(adjustl(ext)), status="unknown")
      WRITE (iun, "(a)") trim(cvector(i))
      ik = 1
      advstring = "no"
      DO j = 1, nnoe
        ik = ik + 1
        WRITE (iun, "(3e12.5)", advance = trim(advstring)) (vector(k, i, j), k = 1, 3)
        IF (ik .EQ. 2) THEN
          ik = 0
          advstring = "yes"
        ELSE
          advstring = "no"
        END IF
      END DO
      CLOSE (iun)
    END DO
  
    DO i = 1, ntensor
      iun = fileprovidechannel()
      OPEN (iun, file = trim(adjustl(filename)) // trim(ctensor(i)) // ".res" // trim(adjustl(ext)), status="unknown")
      WRITE (iun, "(a)") trim(ctensor(i))
      advstring = "yes"
      DO j = 1, nnoe
        WRITE (iun, "(6e12.5)", advance = trim(advstring)) (tens(k, i, j), k = 1, 6)
      END DO
      CLOSE (iun)
    END DO
  
    !--- Write element-based result files ---
    DO i = 1, nescalar
      iun = fileprovidechannel()
      OPEN (iun, file = trim(adjustl(filename)) // trim(cescalar(i)) // ".res" // trim(adjustl(ext)), status="unknown")
      WRITE (iun, "(a)") trim(cescalar(i))
      WRITE (iun, "(a)") "part 1"
      DO itp = 1, mtp
        ikount = 0
        DO j = 1, nele
          it = nelpr(j)
          IF (it .EQ. itp) ikount = ikount + 1
        END DO
        IF (ikount .NE. 0) THEN
          WRITE (iun, "(a)") " " // trim(names(itp)) // " "
          ik = 1
          advstring = "no"
          DO j = 1, nele
            it = nelpr(j)
            IF (it .EQ. itp) THEN
              ik = ik + 1
              WRITE (iun, "(e12.5)", advance = trim(advstring)) escalar(i, j)
              IF (ik .EQ. 6) THEN
                ik = 0
                advstring = "yes"
              ELSE
                advstring = "no"
              END IF
            END IF
          END DO
        END IF
      END DO
      CLOSE (iun)
    END DO
  
    DO i = 1, nevector
      iun = fileprovidechannel()
      OPEN (iun, file = trim(adjustl(filename)) // trim(cevector(i)) // ".res" // trim(adjustl(ext)), status="unknown")
      WRITE (iun, "(a)") trim(cevector(i))
      WRITE (iun, "(a)") "part 1"
      DO itp = 1, mtp
        ikount = 0
        DO j = 1, nele
          it = nelpr(j)
          IF (it .EQ. itp) ikount = ikount + 1
        END DO
        IF (ikount .NE. 0) THEN
          WRITE (iun, "(a)") " " // trim(names(itp)) // " "
          ik = 1
          advstring = "no"
          DO j = 1, nele
            it = nelpr(j)
            IF (it .EQ. itp) THEN
              ik = ik + 1
              WRITE (iun, "(3e12.5)", advance = trim(advstring)) (evector(k, i, j), k = 1, 3)
              IF (ik .EQ. 2) THEN
                ik = 0
                advstring = "yes"
              ELSE
                advstring = "no"
              END IF
            END IF
          END DO
        END IF
      END DO
      CLOSE (iun)
    END DO
  
    DO i = 1, netensor
      iun = fileprovidechannel()
      OPEN (iun, file = trim(adjustl(filename)) // trim(cetensor(i)) // ".res" // trim(adjustl(ext)), status="unknown")
      WRITE (iun, "(a)") trim(cetensor(i))
      WRITE (iun, "(a)") "part 1"
      DO itp = 1, mtp
        ikount = 0
        DO j = 1, nele
          it = nelpr(j)
          IF (it .EQ. itp) ikount = ikount + 1
        END DO
        IF (ikount .NE. 0) THEN
          WRITE (iun, "(a)") " " // trim(names(itp)) // " "
          advstring = "yes"
          DO j = 1, nele
            it = nelpr(j)
            IF (it .EQ. itp) THEN
              WRITE (iun, "(6e12.5)", advance = trim(advstring)) (etensor(k, i, j), k = 1, 6)
            END IF
          END DO
        END IF
      END DO
      CLOSE (iun)
    END DO
  END SUBROUTINE fileoutputensight
!> positions file at a string
  SUBROUTINE filepositionsatastring(iuni, str, ierr)
    INTEGER, PARAMETER::mst = 160
    CHARACTER(mst)::str2
    CHARACTER(*)::str
    ierr = 1
    jlen = len_trim(trim(str))
    DO
      READ (iuni, "(a)", iostat=io) str2
      IF (io .NE. 0) EXIT
      IF (str(1:jlen) .EQ. str2(1:jlen)) THEN
        ierr = 0
        EXIT
      END IF
    END DO
    BACKSPACE (iuni, iostat=ierr)
  END SUBROUTINE filepositionsatastring
!> positions file at a line number
  SUBROUTINE filepositions(iu, ik)
    IF (iu .LE. 0) RETURN
    REWIND (iu, iostat=io)
    DO jk = 1, ik-1
      READ (iu, *, iostat=io)
      IF (io .NE. 0) EXIT
    END DO
  END SUBROUTINE filepositions
!> returns a channel for a file
  INTEGER FUNCTION fileprovidechannel()
    LOGICAL::op = .FALSE.
    INTEGER::iu, ie
    iu = 20
    DO
      INQUIRE (unit=iu, opened=op, iostat=ie)
      IF (ie .NE. 0) CYCLE
      IF (.NOT. op) EXIT
      iu = iu+1
      IF (iu .GT. 200) STOP "more than 200-20 units opened - please check your code (.c, .cpp and .f*)"
    END DO
    fileprovidechannel = iu
  END FUNCTION fileprovidechannel
!> reads a line from a file
  SUBROUTINE filereadsaline(tex1, ires, iu)
    CHARACTER(*)::tex1
    ires = 0
    DO
      READ (unit=iu, fmt="(a)", iostat=io, err=122) tex1
      IF (io .NE. 0) THEN
        IF (io .LT. 0) ires = 1
        IF (io .GT. 0) ires = 2
        EXIT
      END IF
      CALL stringreplacenonprinting(tex1)
      CALL stringremovecommas(tex1)
      tex1 = adjustl(tex1)
      IF (tex1 .EQ. " " .OR. tex1(1:1) .EQ. "*") THEN
        CYCLE
      ELSE
        EXIT
      END IF
    END DO
122 CONTINUE
    ires = 3
  END SUBROUTINE filereadsaline
!> remove extension from a string
  SUBROUTINE fileremoveextension(ext)
    INTEGER(4)::i
    CHARACTER(*)::ext
    CHARACTER(len(ext)+8)::str
    str = "rm -r *."//trim(adjustl(ext))
    i = system(trim(str))
    str = "del *."//trim(adjustl(ext))
    i = system(str)
  END SUBROUTINE fileremoveextension
!> linear interpolation and derivative
  SUBROUTINE funclinearinterpol(n, x, y, xl, yl, dy)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x, y
    dy = 0.0d00
    IF (xl .LT. x(1)) THEN
      yl = y(1)
      RETURN
    END IF
    IF (xl .GT. x(n)) THEN
      yl = y(n)
      RETURN
    END IF
    IF (n .GE. 2) THEN
      IF (xl .LE. x(2)) THEN
        CALL funclinearint(x(1), x(2), y(1), y(2), xl, yl, dy)
        RETURN
      END IF
    ELSE
      yl = y(1)
      dy = 0.0d00
      RETURN
    END IF
    i1 = vectbinarysearch(n, x, xl)
    i2 = i1+1
    CALL funclinearint(x(i1), x(i2), y(i1), y(i2), xl, yl, dy)
  CONTAINS
    SUBROUTINE funclinearint(x0, x1, y0, y1, x, y, dy)
      IMPLICIT REAL(8) (a-h, o-z)
      tol = tiny(tol)
      xi = x0
      xf = x1
      yi = y0
      yf = y1
      IF (xi .GT. xf) THEN
        xt = xi
        xi = xf
        xf = xt
        yt = yi
        yi = yf
        yf = yt
      END IF
      IF (abs(xi-xf) .LE. tol) THEN
        y = 0.5d00*(yi+yf)
        dy = 0.0d00
      ELSEIF (x .GT. xf) THEN
        y = yf
        dy = 0.0d00
      ELSEIF (x .LT. xi) THEN
        y = yi
        dy = 0.0d00
      ELSE
        dy = (yf-yi)/(xf-xi)
        y = yi+dy*(x-xi)
      END IF
    END SUBROUTINE funclinearint
  END SUBROUTINE funclinearinterpol
!> mangasarian function
  REAL(8) FUNCTION funcmangasarian(ityp, x, error)
    IMPLICIT REAL(8) (a-h, o-z)
    alpha = log(2.0d00)/error
    SELECT CASE (ityp)
    CASE (1)
      IF (x .GE. 0.0d00) THEN
        funcmangasarian = x+(1.0d00/alpha)*log(1.0d00+exp(-alpha*x))
      ELSE
        funcmangasarian = x+(1.0d00/alpha)*(-x*alpha+log(1.0d00+exp(x*alpha)))
      END IF
    CASE (2)
      IF (x .GE. 0.0d00) THEN
        funcmangasarian = 1.0d00/(1.0d00+exp(-alpha*x))
      ELSE
        funcmangasarian = exp(alpha*x)/(exp(alpha*x)+1.0d00)
      END IF
    CASE (3)
      IF (x .GE. 0.0d00) THEN
        funcmangasarian = alpha*exp(-alpha*x)/(1.0d00+2.0d00*exp(-alpha*x)+exp(-2.0d00*alpha*x))
      ELSE
        funcmangasarian = alpha*exp(alpha*x)/(1.0d00+2.0d00*exp(alpha*x)+exp(2.0d00*alpha*x))
      END IF
    CASE default
      funcmangasarian = 0.0d00
    END SELECT
  END FUNCTION funcmangasarian
!> bell shaped function
  SUBROUTINE functionweightbell(w, s, smax)
    IMPLICIT REAL(8) (a-h, o-z)
    pi = 4.0d00*atan(1.0d00)
    ss = s/smax
    w = 0.0d00
    IF (ss .LE. 0.5d00) THEN
      w = (2.0d00/3.0d00)-4.0d00*ss*ss+4.0d00*ss*ss*ss
    ELSE IF (ss .LE. 1.0d00 .AND. ss .GT. 0.5d00) THEN
      w = (4.0d00/3.0d00)-4.0d00*ss+4.0d00*ss*ss-(4.0d00/3.0d00)*ss*ss*ss
    ELSE
      w = 0.0d00
    END IF
  END SUBROUTINE functionweightbell
!> gauss point weights and positions in 1d
  SUBROUTINE gaussweightsandpositions1d(n, w, p)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::n
    REAL(8), DIMENSION(*)::w, p
    SELECT CASE (n)
    CASE (1)
      w(1) = 2.0d00
      p(1) = 0.0d00
    CASE (2)
      w(1) = 1.0d00
      w(2) = 1.0d00
      p(1) = 1.0d00/sqrt(3.0d00)
      p(2) = -1.0d00/sqrt(3.0d00)
    CASE (3)
      w(1) = 0.88888889
      w(2) = 0.55555555
      w(3) = 0.55555555
      p(1) = 0.0d00
      p(2) = 0.77459667
      p(3) = -0.77459667
    CASE (4)
      w(1) = 0.65214515
      w(2) = 0.65214515
      w(3) = 0.34785485
      w(4) = 0.34785485
      p(1) = 0.33998104
      p(2) = -0.33998104
      p(3) = 0.86113631
      p(4) = -0.86113631
    CASE (5)
      w(1) = 0.56888889
      w(2) = 0.47862867
      w(3) = 0.47862867
      w(4) = 0.23692689
      w(5) = 0.23692689
      p(1) = 0.0d00
      p(2) = 0.53846931
      p(3) = -0.53846931
      p(4) = 0.90617985
      p(5) = -0.90617985
    CASE (6)
      w(1) = 0.1713244924
      w(2) = 0.1713244924
      w(3) = 0.3607615730
      w(4) = 0.3607615730
      w(5) = 0.4679139346
      w(6) = 0.4679139346
      p(1) = 0.9324695142
      p(2) = -0.9324695142
      p(3) = 0.6612093865
      p(4) = -0.6612093865
      p(5) = 0.2386191861
      p(6) = -0.2386191861
    CASE (7)
      w(1) = 0.4179591836734693877
      w(2) = 0.3818300505051189449
      w(3) = 0.3818300505051189449
      w(4) = 0.2797053914892766679
      w(5) = 0.2797053914892766679
      w(6) = 0.1294849661688696932
      w(7) = 0.1294849661688696932
      p(1) = 0.0d00
      p(2) = 0.4058451513773971669
      p(3) = -0.4058451513773971669
      p(4) = -0.7415311855993944398
      p(5) = 0.7415311855993944398
      p(6) = -0.9491079123427585245
      p(7) = 0.9491079123427585245
    CASE (8)
      w(1) = 0.362684
      w(2) = 0.362684
      w(3) = 0.313707
      w(4) = 0.313707
      w(5) = 0.222381
      w(6) = 0.222381
      w(7) = 0.101229
      w(8) = 0.101229
      p(1) = -0.183435
      p(2) = 0.183435
      p(3) = -0.525532
      p(4) = 0.525532
      p(5) = -0.796666
      p(6) = 0.796666
      p(7) = -0.96029
      p(8) = 0.96029
    CASE (9)
      w(1) = 0.330239
      w(2) = 0.180648
      w(3) = 0.180648
      w(4) = 0.0812744
      w(5) = 0.0812744
      w(6) = 0.312347
      w(7) = 0.312347
      w(8) = 0.260611
      w(9) = 0.260611
      p(1) = 0.
      p(2) = -0.836031
      p(3) = 0.836031
      p(4) = -0.96816
      p(5) = 0.96816
      p(6) = -0.324253
      p(7) = 0.324253
      p(8) = -0.613371
      p(9) = 0.613371
    CASE (10)
      w(1) = 0.295524
      w(2) = 0.295524
      w(3) = 0.269267
      w(4) = 0.269267
      w(5) = 0.219086
      w(6) = 0.219086
      w(7) = 0.149451
      w(8) = 0.149451
      w(9) = 0.0666713
      w(10) = 0.0666713
      p(1) = -0.148874
      p(2) = 0.148874
      p(3) = -0.433395
      p(4) = 0.433395
      p(5) = -0.67941
      p(6) = 0.67941
      p(7) = -0.865063
      p(8) = 0.865063
      p(9) = -0.973907
      p(10) = 0.973907
    CASE (11)
      p(1) = -0.125233
      p(2) = 0.125233
      p(3) = -0.367831
      p(4) = 0.367831
      p(5) = -0.587318
      p(6) = 0.587318
      p(7) = -0.769903
      p(8) = 0.769903
      p(9) = -0.904117
      p(10) = 0.904117
      p(11) = -0.981561
      p(12) = 0.981561
      w(1) = 0.272925
      w(2) = 0.262805
      w(3) = 0.262805
      w(4) = 0.233194
      w(5) = 0.233194
      w(6) = 0.18629
      w(7) = 0.18629
      w(8) = 0.12558
      w(9) = 0.12558
      w(10) = 0.0556686
      w(11) = 0.0556686
    CASE (12)
      p(1) = -0.125233
      p(2) = 0.125233
      p(3) = -0.367831
      p(4) = 0.367831
      p(5) = -0.587318
      p(6) = 0.587318
      p(7) = -0.769903
      p(8) = 0.769903
      p(9) = -0.904117
      p(10) = 0.904117
      p(11) = -0.981561
      p(12) = 0.981561
      w(1) = 0.249147
      w(2) = 0.249147
      w(3) = 0.233493
      w(4) = 0.233493
      w(5) = 0.203167
      w(6) = 0.203167
      w(7) = 0.160078
      w(8) = 0.160078
      w(9) = 0.106939
      w(10) = 0.106939
      w(11) = 0.0471753
      w(12) = 0.0471753
    CASE (13)
      p(1) = 0.
      p(2) = -0.230458
      p(3) = 0.230458
      p(4) = -0.448493
      p(5) = 0.448493
      p(6) = -0.642349
      p(7) = 0.642349
      p(8) = -0.801578
      p(9) = 0.801578
      p(10) = -0.917598
      p(11) = 0.917598
      p(12) = -0.984183
      p(13) = 0.984183
      w(1) = 0.232552
      w(2) = 0.226283
      w(3) = 0.226283
      w(4) = 0.207816
      w(5) = 0.207816
      w(6) = 0.178146
      w(7) = 0.178146
      w(8) = 0.138874
      w(9) = 0.138874
      w(10) = 0.0921215
      w(11) = 0.0921215
      w(12) = 0.040484
      w(13) = 0.040484
    CASE (14)
      p(1) = -0.108055
      p(2) = 0.108055
      p(3) = -0.319112
      p(4) = 0.319112
      p(5) = -0.515249
      p(6) = 0.515249
      p(7) = -0.687293
      p(8) = 0.687293
      p(9) = -0.827201
      p(10) = 0.827201
      p(11) = -0.928435
      p(12) = 0.928435
      p(13) = -0.986284
      p(14) = 0.986284
      w(1) = 0.215264
      w(2) = 0.215264
      w(3) = 0.205198
      w(4) = 0.205198
      w(5) = 0.185538
      w(6) = 0.185538
      w(7) = 0.157203
      w(8) = 0.157203
      w(9) = 0.121519
      w(10) = 0.121519
      w(11) = 0.0801581
      w(12) = 0.0801581
      w(13) = 0.0351195
      w(14) = 0.0351195
    CASE (15)
      p(1) = 0.
      p(2) = -0.201194
      p(3) = 0.201194
      p(4) = -0.394151
      p(5) = 0.394151
      p(6) = -0.570972
      p(7) = 0.570972
      p(8) = -0.724418
      p(9) = 0.724418
      p(10) = -0.848207
      p(11) = 0.848207
      p(12) = -0.937273
      p(13) = 0.937273
      p(14) = -0.987993
      p(15) = 0.987993
      w(1) = 0.202578
      w(2) = 0.198431
      w(3) = 0.198431
      w(4) = 0.186161
      w(5) = 0.186161
      w(6) = 0.166269
      w(7) = 0.166269
      w(8) = 0.139571
      w(9) = 0.139571
      w(10) = 0.107159
      w(11) = 0.107159
      w(12) = 0.070366
      w(13) = 0.070366
      w(14) = 0.0307532
      w(15) = 0.0307532
    CASE (16)
      p(1) = -0.989400935; w(1) = 0.02715245941
      p(2) = -0.9445750231; w(2) = 0.06225352394
      p(3) = -0.8656312024; w(3) = 0.0951585117
      p(4) = -0.7554044084; w(4) = 0.124628971
      p(5) = -0.6178762444; w(5) = 0.1495959888
      p(6) = -0.4580167777; w(6) = 0.1691565194
      p(7) = -0.2816035508; w(7) = 0.182603415
      p(8) = -0.0950125098; w(8) = 0.1894506105
      p(9) = 0.09501250984; w(9) = 0.1894506105
      p(10) = 0.2816035508; w(10) = 0.182603415
      p(11) = 0.4580167777; w(11) = 0.1691565194
      p(12) = 0.6178762444; w(12) = 0.1495959888
      p(13) = 0.7554044084; w(13) = 0.1246289713
      p(14) = 0.8656312024; w(14) = 0.0951585117
      p(15) = 0.9445750231; w(15) = 0.062253524
      p(16) = 0.989400935; w(16) = 0.0271524594
    CASE (17)
      p(1) = 0.
      p(2) = -0.178484
      p(3) = 0.178484
      p(4) = -0.351232
      p(5) = 0.351232
      p(6) = -0.512691
      p(7) = 0.512691
      p(8) = -0.657671
      p(9) = 0.657671
      p(10) = -0.781514
      p(11) = 0.781514
      p(12) = -0.880239
      p(13) = 0.880239
      p(14) = -0.950676
      p(15) = 0.950676
      p(16) = -0.990575
      p(17) = 0.990575
      w(1) = 0.179446
      w(2) = 0.176563
      w(3) = 0.176563
      w(4) = 0.168004
      w(5) = 0.168004
      w(6) = 0.154046
      w(7) = 0.154046
      w(8) = 0.135136
      w(9) = 0.135136
      w(10) = 0.111884
      w(11) = 0.111884
      w(12) = 0.0850361
      w(13) = 0.0850361
      w(14) = 0.0554595
      w(15) = 0.0554595
      w(16) = 0.0241483
      w(17) = 0.0241483
    CASE (18)
      p(1) = -0.084775
      p(2) = 0.084775
      p(3) = -0.251886
      p(4) = 0.251886
      p(5) = -0.411751
      p(6) = 0.411751
      p(7) = -0.559771
      p(8) = 0.559771
      p(9) = -0.691687
      p(10) = 0.691687
      p(11) = -0.803705
      p(12) = 0.803705
      p(13) = -0.892602
      p(14) = 0.892602
      p(15) = -0.955824
      p(16) = 0.955824
      p(17) = -0.991565
      p(18) = 0.991565
      w(1) = 0.169142
      w(2) = 0.169142
      w(3) = 0.164276
      w(4) = 0.164276
      w(5) = 0.154685
      w(6) = 0.154685
      w(7) = 0.140643
      w(8) = 0.140643
      w(9) = 0.122555
      w(10) = 0.122555
      w(11) = 0.100942
      w(12) = 0.100942
      w(13) = 0.0764257
      w(14) = 0.0764257
      w(15) = 0.0497145
      w(16) = 0.0497145
      w(17) = 0.021616
      w(18) = 0.021616
    CASE (19)
      p(1) = 0.
      p(2) = -0.160359
      p(3) = 0.160359
      p(4) = -0.316564
      p(5) = 0.316564
      p(6) = -0.464571
      p(7) = 0.464571
      p(8) = -0.600545
      p(9) = 0.600545
      p(10) = -0.720966
      p(11) = 0.720966
      p(12) = -0.822715
      p(13) = 0.822715
      p(14) = -0.903156
      p(15) = 0.903156
      p(16) = -0.960208
      p(17) = 0.960208
      p(18) = -0.992407
      p(19) = 0.992407
      w(1) = 0.161054
      w(2) = 0.158969
      w(3) = 0.158969
      w(4) = 0.152766
      w(5) = 0.152766
      w(6) = 0.142607
      w(7) = 0.142607
      w(8) = 0.128754
      w(9) = 0.128754
      w(10) = 0.111567
      w(11) = 0.111567
      w(12) = 0.09149
      w(13) = 0.09149
      w(14) = 0.0690445
      w(15) = 0.0690445
      w(16) = 0.0448142
      w(17) = 0.0448142
      w(18) = 0.0194618
      w(19) = 0.0194618
    CASE (20)
      p(1) = -0.0765265
      p(2) = 0.0765265
      p(3) = -0.227786
      p(4) = 0.227786
      p(5) = -0.373706
      p(6) = 0.373706
      p(7) = -0.510867
      p(8) = 0.510867
      p(9) = -0.636054
      p(10) = 0.636054
      p(11) = -0.746332
      p(12) = 0.746332
      p(13) = -0.839117
      p(14) = 0.839117
      p(15) = -0.912234
      p(16) = 0.912234
      p(17) = -0.963972
      p(18) = 0.963972
      p(19) = -0.993129
      p(20) = 0.993129
      w(1) = 0.152753
      w(2) = 0.152753
      w(3) = 0.149173
      w(4) = 0.149173
      w(5) = 0.142096
      w(6) = 0.142096
      w(7) = 0.131689
      w(8) = 0.131689
      w(9) = 0.118195
      w(10) = 0.118195
      w(11) = 0.10193
      w(12) = 0.10193
      w(13) = 0.0832767
      w(14) = 0.0832767
      w(15) = 0.062672
      w(16) = 0.062672
      w(17) = 0.0406014
      w(18) = 0.0406014
      w(19) = 0.017614
      w(20) = 0.017614
    CASE default
      STOP "error in gaussweightsandpositions1d"
    END SELECT
  END SUBROUTINE gaussweightsandpositions1d
!> gauss point weights and positions for some common cases
  SUBROUTINE gaussweightsandpositions(igaus, pespg, pospg)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::pespg, pospg
    SELECT CASE (igaus)
    CASE (1)
      pespg(1) = 2.0d00
      pospg(1) = 0.0d00
    CASE (2)
      pespg(1) = 1.0d00
      pespg(2) = 1.0d00
      pospg(1) = -0.577350269189626d00
      pospg(2) = 0.577350269189626d00
    CASE (3)
      pespg(1) = 0.555555555555556d00
      pespg(2) = 0.888888888888889d00
      pespg(3) = 0.555555555555556d00
      pospg(1) = -0.774596669241483d00
      pospg(2) = 0.0d00
      pospg(3) = 0.774596669241483d00
    CASE (4)
      pespg(1) = 0.347854845137d00
      pespg(2) = 0.652145154863d00
      pespg(3) = pespg(2)
      pespg(4) = pespg(1)
      pospg(1) = -0.861136311594d00
      pospg(2) = -0.339981043585d00
      pospg(3) = -pospg(2)
      pospg(4) = -pospg(1)
    CASE (-1)
      pospg(1) = 1.0d00/3.0d00
      pospg(2) = 1.0d00/3.0d00
      pespg(1) = 0.5d00
    CASE (-3)
      pospg(1) = 0.666666666666667d00!gp1
      pospg(2) = 0.166666666666667d00!gp1
      pospg(3) = 0.166666666666667d00!gp2
      pospg(4) = 0.666666666666667d00!gp2
      pospg(5) = 0.166666666666667d00!gp3
      pospg(6) = 0.166666666666667d00!gp3
      pespg(1) = 0.166666666666667d00!w1
      pespg(2) = 0.166666666666667d00!w2
      pespg(3) = 0.166666666666667d00!w3
    CASE (-6)
      const1 = 0.44594849091597
      const2 = 0.10810301816807
      const3 = 0.09157621350977
      const4 = 0.81684757298046
      w1 = 0.22338158967801
      w2 = 0.10995174365532
      pospg(1) = const1 !gp1
      pospg(2) = const1 !gp1
      pospg(3) = const1 !gp2
      pospg(4) = const2 !gp2
      pospg(5) = const2 !gp3
      pospg(6) = const1 !gp3
      pospg(7) = const3 !gp4
      pospg(8) = const3 !gp4
      pospg(9) = const3 !gp5
      pospg(10) = const4 !gp5
      pospg(11) = const4 !gp6
      pospg(12) = const3 !gp6
      pespg(1:3) = w1
      pespg(4:6) = w2
      DO i = 1, 6
        pespg(i) = 0.5d00*pespg(i)
      END DO
    CASE default
      WRITE (*, *) "error in gaussweightsandpositions"
    END SELECT
  END SUBROUTINE gaussweightsandpositions
!> gauss point weights and positions for a tet
  SUBROUTINE gaussweightsandpositionstet(igaus, pespg, pospg)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::pespg, pospg
    SELECT CASE (igaus)
    CASE (-1)
      pospg(1) = 0.25d00
      pospg(2) = 0.25d00
      pospg(3) = 0.25d00
      pespg(1) = 1.0d00/6.0d00
    CASE (-4)
      rbot = 0.05d00
      a = (5.0d00-sqrt(5.0d00))*rbot!0.1381966
      b = (5.0d00+3.0d00*sqrt(5.0d00))*rbot!0.5854102
      w = 0.041666666666666664
      pospg(1) = b!1
      pospg(5) = a!2
      pospg(9) = a!3
      pospg(2) = a!4
      pospg(6) = a!1
      pospg(10) = b!2
      pospg(3) = a!3
      pospg(7) = a!4
      pospg(11) = a!1
      pospg(4) = a!2
      pospg(8) = b!3
      pospg(12) = a!4
      pespg(1:4) = w
    CASE (-5)
      pospg(1) = 0.25000d00!1
      pospg(2) = 0.50000d00!2
      pospg(3) = 0.16666d00!3
      pospg(4) = 0.16666d00!4
      pospg(5) = 0.16666d00!5
      pospg(6) = 0.25000d00!1
      pospg(7) = 0.16666d00!2
      pospg(8) = 0.50000d00!3
      pospg(9) = 0.16666d00!4
      pospg(10) = 0.16666d00!5
      pospg(11) = 0.25000d00!1
      pospg(12) = 0.16666d00!2
      pospg(13) = 0.16666d00!3
      pospg(14) = 0.16666d00!4
      pospg(15) = 0.50000d00!5
      pespg(1) = -4.0d00/5.0d00
      pespg(2) = 9.0d00/20.0d00
      pespg(3) = 9.0d00/20.0d00
      pespg(4) = 9.0d00/20.0d00
      pespg(5) = 9.0d00/20.0d00
      DO ig = 1, 5
        pespg(ig) = pespg(ig)*0.16666d00
      END DO
    END SELECT
  END SUBROUTINE gaussweightsandpositionstet
!> area of a triangle
  REAL(8) FUNCTION geo2dareatriangle(x1, x2, x3)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(2)::x1, x2, x3
    geo2dareatriangle = 0.5d00*((x2(1)-x1(1))*(x3(2)-x1(2))-(x2(2)-x1(2))*(x3(1)-x1(1)))
  END FUNCTION geo2dareatriangle
!> coordinate xi for the projection o x on pq
!> returns xi and found
  SUBROUTINE geo2ddeterminescoordinate(p, q, x, xi, found)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(2)::p, q, x
    LOGICAL::found
    found = .FALSE.
    bot = vectdot(2, p-q, p-q)
    IF (abs(bot) .GT. epsmach()) THEN
      xi = (vectdot(2, p, p)-vectdot(2, q, q)+2.0d00*vectdot(2, q-p, x))/bot
      IF (xi .GT. -1.0d00 .AND. xi .LT. 1.0d00) THEN
        found = .TRUE.
      END IF
    END IF
  END SUBROUTINE geo2ddeterminescoordinate
!> determines
!> intersection between a moving
!> point and a moving line segment
!> t -> time of occurrence, should be between 0 and 1 for positive id ... should provide the
!> time step CUT
!> xi -> position at the line segment, should be between -1 and 1 for positive id
!> xb,ux: coordinates of point X, relative displacement of X
!> pb,up: coordinates of point P, relative displacement of P
!> qb,uq: coordinates of point Q, relative displacement of Q
  SUBROUTINE geo2dintersectcoefficients(xb, ux, pb, up, qb, uq, a, b, c)
    IMPLICIT NONE
    REAL(8) v(28), xb(2), ux(2), pb(2), up(2), qb(2), uq(2), a, b, c
    v(1) = xb(1)
    v(2) = xb(2)
    v(3) = ux(1)
    v(4) = ux(2)
    v(5) = pb(1)
    v(6) = pb(2)
    v(7) = up(1)
    v(18) = v(3)-v(7)
    v(8) = up(2)
    v(19) = -v(4)+v(8)
    v(9) = qb(1)
    v(10) = qb(2)
    v(11) = uq(1)
    v(20) = v(11)-v(3)
    v(12) = uq(2)
    v(21) = v(12)-v(4)
    a = v(19)*v(20)+v(18)*v(21)
    b = v(10)*v(18)-v(1)*v(19)-v(18)*v(2)-v(2)*v(20)+v(1)*v(21)-v(21)*v(5)+v(20)*v(6)+v(19)*v(9)
    c = v(1)*v(10)-v(10)*v(5)+v(2)*v(5)-v(1)*v(6)-v(2)*v(9)+v(6)*v(9)
  END SUBROUTINE geo2dintersectcoefficients
!> set_intersection between line and line segment
!> providing: xi of segment (-1:1)
!>             t of line (-infty:+infty)
  SUBROUTINE geo2dintersectlines(a, b, xline, tline, xi, t, found)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(2)::a, b
    REAL(8), DIMENSION(2)::xline, tline, sm
    REAL(8), DIMENSION(2, 2)::mtx, mi
    LOGICAL::found
    found = .FALSE.
    xi = 0.0d00
    t = 0.0d00
    mtx(1, 1) = a(1)-b(1)
    mtx(2, 1) = a(2)-b(2)
    mtx(1, 2) = 2.0d00*tline(1)
    mtx(2, 2) = 2.0d00*tline(2)
    det = mtx(1, 1)*mtx(2, 2)-mtx(1, 2)*mtx(2, 1)
    IF (abs(det) .GT. epsmach()) THEN
      sm(1:2) = -2.0d00*xline(1:2)+a(1:2)+b(1:2)
      mi(1, 1) = mtx(2, 2)/det
      mi(1, 2) = -mtx(1, 2)/det
      mi(2, 1) = -mtx(2, 1)/det
      mi(2, 2) = mtx(1, 1)/det
      xi = mi(1, 1)*sm(1)+mi(1, 2)*sm(2)
      t = mi(2, 1)*sm(1)+mi(2, 2)*sm(2)
      IF (xi .GT. -1.0d00 .AND. xi .LT. 1.0d00) THEN
        found = .TRUE.
      END IF
    END IF
  END SUBROUTINE geo2dintersectlines
!> intersection between a moving point and a moving line segment
!> t -> time of occurrence, should be between 0 and 1 for positive id ... should provide the
!> time step CUT
!> xi -> position at the line segment, should be between -1 and 1 for positive id
!> xb,ux: coordinates of point X, relative displacement of X
!> pb,up: coordinates of point P, relative displacement of P
!> qb,uq: coordinates of point Q, relative displacement of Q
  SUBROUTINE geo2dintersectmovingpoint(xb, ux, pb, up, qb, uq, t, xi, found)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), PARAMETER::tol_edge = 8.0d-1
    LOGICAL::found
    LOGICAL::foundpoint
    REAL(8), DIMENSION(2)::xb, pb, qb, ux, up, uq, pt, qt
    found = .FALSE.
    foundpoint = .FALSE.
!*** checks if displacements are significative with respect to face dimensions
    rs = vectnorm2(2, pb-qb)
    IF (abs(rs) .LE. 1.0d-20) RETURN
    IF (max(vectnorm2(2, ux), vectnorm2(2, up), vectnorm2(2, uq)) .LE. tol_edge*rs) RETURN
!*** second degree equation to obtain the time [0,1]
    CALL geo2dintersectcoefficients(xb, ux, pb, up, qb, uq, a, b, c)
!*** solve it for xi
    CALL equationquadratic(z1, z2, a, b, c, is)
    IF (is .EQ. 1) THEN
      IF (z1 .LT. 1.0d00 .AND. z1 .GT. 0.0d00) THEN
        pt = pb+z1*(up-ux)
        qt = qb+z1*(uq-ux)
        CALL geo2ddeterminescoordinate(pt, qt, xb, xi, foundpoint)
        IF (foundpoint) THEN
          t = z1
          found = .TRUE.
          RETURN
        END IF
      END IF
    ELSEIF (is .EQ. 2) THEN
      IF (z1 .LT. 1.0d00 .AND. z1 .GT. 0.0d00) THEN
        pt = pb+z1*(up-ux)
        qt = qb+z1*(uq-ux)
        CALL geo2ddeterminescoordinate(pt, qt, xb, xi, foundpoint)
        IF (foundpoint) THEN
          t = z1
          found = .TRUE.
          RETURN
        END IF
      END IF
      IF (z2 .LT. 1.0d00 .AND. z2 .GT. 0.0d00) THEN
        pt = pb+z2*(up-ux)
        qt = qb+z2*(uq-ux)
        CALL geo2ddeterminescoordinate(pt, qt, xb, xi, foundpoint)
        IF (foundpoint) THEN
          t = z2
          found = .TRUE.
          RETURN
        END IF
      END IF
    END IF
  END SUBROUTINE geo2dintersectmovingpoint
!> given a triangle by its coordinates
!> given values of a given quantity at the nodes
!> given a value v
!> calculate coordinates of level set
  SUBROUTINE geo2dlevelsettriangle(xt, vt, v, x1, x2, in)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(2, 3)::xt
    REAL(8), DIMENSION(3)::vt
    REAL(8), DIMENSION(2)::x1, x2
    in = 0
    IF (abs(vt(1)) .LE. 1.0d-20 .AND. abs(vt(2)) .LE. 1.0d-20 .AND. abs(vt(3)) .LE. 1.0d-20) THEN
      RETURN
    END IF
    DO i = 1, 3
      i1 = scalarmodularposition(3, i)
      i2 = scalarmodularposition(3, i+1)
      rtol = 1.0d-20*max(abs(vt(i1)), abs(vt(i2)))
      IF (abs(vt(i1)-vt(i2)) .LE. rtol) THEN
        vt1 = vt(i1)-rtol
        vt2 = vt(i2)+rtol
      ELSE
        vt1 = vt(i1)
        vt2 = vt(i2)
      END IF
      IF (abs(vt2-vt1) .LE. 1.0d-20) THEN
        xi = 2.5d00
      ELSE
        xi = (vt2-v)/(vt2-vt1)
      END IF
      IF (xi .GE. 0.0d00 .AND. xi .LE. 1.0d00) THEN
        in = in+1
        IF (in .EQ. 1) THEN
          DO id = 1, 2
            x1(id) = xi*xt(id, i1)+(1.0d00-xi)*xt(id, i2)
          END DO
        ELSE
          DO id = 1, 2
            x2(id) = xi*xt(id, i1)+(1.0d00-xi)*xt(id, i2)
          END DO
        END IF
      END IF
    END DO
  END SUBROUTINE geo2dlevelsettriangle
!> determines the parent-domain coordinates
!> of the projection of xn onto the line segment x1 x2
!> and the gap g
  SUBROUTINE geo2dprojline(xn, x1, x2, xi, g)
    IMPLICIT NONE
    REAL(8) v(5001), xn(2), x1(2), x2(2), xi, g
    v(236) = -x1(1)/2d0+x2(1)/2d0
    v(54) = (-2d0)*xn(1)
    v(62) = (-2d0)*xn(2)
    v(33) = (-2d0)*x1(1)
    v(82) = -v(33)+v(54)
    v(46) = (-2d0)*x1(2)
    v(83) = -v(46)+v(62)
    v(55) = 2d0*x2(1)
    v(84) = -v(54)-v(55)
    v(28) = v(33)+v(55)
    v(22) = -x1(1)+x2(1)
    v(41) = (v(22)*v(22))
    v(63) = 2d0*x2(2)
    v(85) = -v(62)-v(63)
    v(44) = v(46)+v(63)
    v(18) = -x1(2)+x2(2)
    v(221) = v(18)*v(22)
    v(219) = v(18)/2d0
    v(49) = (v(18)*v(18))
    v(20) = v(41)+v(49)
    v(210) = 1d0/(2d0*v(20))
    v(120) = 1d0/v(20)**0.25d1
    v(223) = 0.15d1*v(120)
    v(121) = v(223)*v(44)
    v(75) = 1d0/v(20)**0.15d1
    v(222) = -(v(41)*v(75))
    v(220) = v(49)*v(75)
    v(208) = v(75)/2d0
    v(77) = v(208)*v(44)
    v(76) = v(208)*v(28)
    v(123) = v(121)*v(221)-v(76)
    v(70) = (2d0*v(28))/v(20)**3
    v(45) = 1d0/v(20)**2
    v(216) = v(44)*v(45)
    v(214) = -(v(28)*v(45))
    v(209) = -(v(45)*v(82))
    v(165) = v(214)*v(84)
    v(111) = v(209)*v(44)
    v(109) = v(209)*v(28)
    v(108) = 2d0/v(20)
    v(19) = 1d0/sqrt(v(20))
    v(235) = v(18)*v(19)
    v(233) = -(v(19)*v(22))
    v(212) = v(19)*v(219)
    v(21) = v(210)*v(28)
    v(211) = -(v(21)*v(22))
    v(124) = 1d0+v(211)
    v(129) = -(v(124)*v(19))
    v(149) = -v(211)
    v(158) = v(149)*v(19)
    v(35) = x1(1)**2+x1(2)**2-x2(1)**2-x2(2)**2+v(28)*xn(1)+v(44)*xn(2)
    v(213) = -(v(35)*v(70))
    v(98) = v(213)*v(28)
    v(92) = 2d0*v(35)*v(45)
    v(99) = v(165)-v(92)-v(98)
    v(166) = v(108)-v(165)-v(99)
    v(201) = -(v(166)*v(212)*v(22))
    v(113) = -v(109)+v(99)
    v(196) = -(v(113)*v(212)*v(22))
    v(110) = -v(108)+2d0*v(109)+v(92)+v(98)
    v(185) = -(v(110)*v(212)*v(22))
    v(90) = v(213)*v(44)
    v(100) = v(214)*v(85)-v(90)
    v(168) = -v(100)+v(216)*v(84)
    v(239) = v(168)*v(236)
    v(114) = v(100)-v(111)
    v(215) = v(114)/2d0
    v(192) = v(215)*x2(1)
    v(191) = -(v(215)*x1(1))
    v(237) = v(191)+v(192)
    v(190) = v(215)*x2(2)
    v(189) = -(v(215)*x1(2))
    v(112) = v(111)+v(214)*v(83)+v(90)
    v(232) = v(112)*v(236)
    v(231) = v(112)*v(219)
    v(61) = -(v(216)*v(35))
    v(106) = -v(61)-v(85)/v(20)
    v(217) = v(106)/2d0
    v(128) = v(217)*v(22)
    v(195) = -(v(128)*v(19))
    v(118) = -v(217)
    v(104) = v(61)-v(83)/v(20)
    v(218) = v(104)/2d0
    v(126) = v(218)*v(22)
    v(116) = -v(218)
    v(53) = -(v(28)*v(92))/2d0
    v(105) = -v(53)-v(84)/v(20)
    v(238) = v(105)+v(166)*v(236)
    v(152) = v(105)*v(219)
    v(198) = v(152)*v(19)
    v(117) = -v(105)/2d0
    v(103) = v(53)-v(82)/v(20)
    v(115) = -v(103)/2d0
    v(39) = v(35)/v(20)
    v(150) = -(v(115)*v(18))
    v(37) = (-1d0)+v(39)
    v(225) = v(37)/2d0
    v(151) = (v(37)-v(104)*x1(2)+v(104)*x2(2))/2d0
    v(228) = v(19)*(v(126)*v(18)-v(151)*v(22))
    v(172) = v(151)*v(19)
    v(125) = (v(37)-v(103)*x1(1)+v(103)*x2(1))/2d0
    v(227) = v(19)*(v(125)*v(18)-v(150)*v(22))
    v(177) = v(128)*v(220)
    v(174) = v(151)*v(222)
    v(170) = v(126)*v(220)
    v(51) = v(221)*v(75)
    v(180) = v(128)*v(51)
    v(173) = v(126)*v(51)
    v(169) = -(v(151)*v(51))
    v(40) = 1d0+v(39)
    v(226) = -v(40)/2d0
    v(153) = (-v(40)-v(106)*x1(2)+v(106)*x2(2))/2d0
    v(230) = v(19)*(v(128)*v(18)-v(153)*v(22))
    v(181) = v(153)*v(222)
    v(179) = v(153)*v(19)
    v(176) = -(v(153)*v(51))
    v(234) = -v(176)-v(177)
    v(127) = (-v(40)-v(105)*x1(1)+v(105)*x2(1))/2d0
    v(229) = v(19)*(v(127)*v(18)-v(152)*v(22))
    v(188) = v(127)*v(220)
    v(187) = -(v(127)*v(19))
    v(50) = v(225)*x1(1)+v(226)*x2(1)+xn(1)
    v(224) = v(49)*v(50)
    v(147) = v(123)*v(50)
    v(146) = v(147)+v(173)
    v(144) = v(177)+v(50)*(-(v(121)*v(49))+v(44)*v(75))
    v(140) = v(223)*v(224)*v(28)
    v(138) = v(140)+v(125)*v(220)
    v(135) = v(195)+v(50)*v(77)
    v(132) = -(v(50)*v(76))
    v(130) = v(132)-v(125)*v(19)
    v(42) = v(225)*x1(2)+v(226)*x2(2)+xn(2)
    v(157) = v(176)+v(123)*v(42)
    xi = v(39)
    g = v(19)*(-(v(22)*v(42))+v(18)*v(50))
  END SUBROUTINE geo2dprojline
!> determines if a quad is convex
  LOGICAL FUNCTION geo2dquadisconvex(x1, x2, x3, x4)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(2)::x1, x2, x3, x4
    geo2dquadisconvex = .FALSE.
    IF (geo2dareatriangle(x1, x2, x4) .LE. 0.0d00) RETURN
    IF (geo2dareatriangle(x2, x3, x4) .LE. 0.0d00) RETURN
    IF (geo2dareatriangle(x1, x2, x3) .LE. 0.0d00) RETURN
    IF (geo2dareatriangle(x1, x3, x4) .LE. 0.0d00) RETURN
    geo2dquadisconvex = .TRUE.
  END FUNCTION geo2dquadisconvex
!> determines the parent domain coordinates xi1 and xi2
!> of the intersection between line segments ab and cd
!> ires=1 if success
  SUBROUTINE geo2dsegmentintersection(a, b, c, d, ires, xi1, xi2)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(2)::a, b, c, d, n1, n2
    LOGICAL::i1, i2
    ires = 0
    xi1 = 0.0d00
    xi2 = 0.0d00
    tol = epsmach()
    n1(1) = b(2)-a(2)
    n1(2) = a(1)-b(1)
    n2(1) = d(2)-c(2)
    n2(2) = c(1)-d(1)
    CALL geogeneralintersection(a, b, c, n2, xi1, ires)
    IF (ires .EQ. 0) RETURN
    CALL geogeneralintersection(c, d, a, n1, xi2, ires)
    IF (ires .EQ. 0) RETURN
    i1 = .FALSE.
    i2 = .FALSE.
    IF (xi1 .GT. -1.0d00+tol .AND. xi1 .LT. 1.0d00-tol) THEN
      i1 = .TRUE.
    END IF
    IF (xi2 .GT. -1.0d00+tol .AND. xi2 .LT. 1.0d00-tol) THEN
      i2 = .TRUE.
    END IF
    IF (i1 .AND. i2) ires = 1
  END SUBROUTINE geo2dsegmentintersection
!> determines area of a 3d quad
  REAL(8) FUNCTION geo3dareaquad(x1, x2, x3, x4)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3, x4
    REAL(8), DIMENSION(3)::xc5
!> area triangulos
    xc5 = (x1+x2+x3+x4)*0.25d00
    t1 = geo3dareatriangle(xc5, x4(1:3), x1(1:3))
    t2 = geo3dareatriangle(xc5, x1(1:3), x2(1:3))
    t3 = geo3dareatriangle(xc5, x2(1:3), x3(1:3))
    t4 = geo3dareatriangle(xc5, x3(1:3), x4(1:3))
    geo3dareaquad = t1+t2+t3+t4
  END FUNCTION geo3dareaquad
!> determines area of a 3d triangle
  REAL(8) FUNCTION geo3dareatriangle(x1, x2, x3)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3
    REAL(8), DIMENSION(3)::vn
    CALL geo3dcrossprod(x2(1:3)-x1(1:3), x3(1:3)-x1(1:3), vn)
    geo3dareatriangle = vectnorm2(3, vn)*0.5d00
  END FUNCTION geo3dareatriangle
!> cross product of 3d vectors
  SUBROUTINE geo3dcrossprod(v1, v2, v)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::v1, v2, v
    v(1) = v1(2)*v2(3)-v1(3)*v2(2)
    v(2) = v2(1)*v1(3)-v2(3)*v1(1)
    v(3) = v1(1)*v2(2)-v1(2)*v2(1)
  END SUBROUTINE geo3dcrossprod
!> determines the normal vector to a 3d triangle or a quad
!> nnf is the number of nodes
  SUBROUTINE geo3dgeneralnormal(xcf, vn, nnf)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, *)::xcf
    REAL(8), DIMENSION(3)::vn, xt1
    CALL vectsetconst(3, vn)
    SELECT CASE (nnf)
    CASE (2)
      DO i = 1, 2
        xt1(i) = xcf(i, 2)-xcf(i, 1)
      END DO
      vn(2) = -xt1(1)
      vn(1) = xt1(2)
    CASE (3)
      CALL geo3dnormaltotri(xcf(1:3, 1), xcf(1:3, 2), xcf(1:3, 3), vn)
    CASE (4)
      CALL geo3dnormaltoquad(xcf(1, 1), xcf(1, 2), xcf(1, 3), xcf(1, 4), vn)
    END SELECT
    CALL vectnormalize(3, vn)
  END SUBROUTINE geo3dgeneralnormal
!> determines the volume of a hex
!> using volumes of tets
  REAL(8) FUNCTION geo3dhexvolume(x1, x2, x3, x4, x5, x6, x7, x8)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3, x4, x5, x6, x7, x8
!*** divides in 6 tets
    geo3dhexvolume = &
      geo3dtetvolume(x4, x7, x8, x1)+ &
      geo3dtetvolume(x1, x4, x6, x8)+ &
      geo3dtetvolume(x3, x5, x8, x1)+ &
      geo3dtetvolume(x1, x2, x5, x8)+ &
      geo3dtetvolume(x1, x3, x7, x8)+ &
      geo3dtetvolume(x2, x6, x8, x1)
  END FUNCTION geo3dhexvolume
!> determines the internal angle between two 3d vectors
  REAL(8) FUNCTION geo3dinternalangle(a, b)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::a, b, v
    CALL geo3dcrossprod(a, b, v)
    rd = vectdot(3, a, b)
    geo3dinternalangle = atan2(vectnorm2(3, v), rd)
  END FUNCTION geo3dinternalangle
!> determines the intersection between a 3d triangle and a plane
!> triangle edges: 12,23,31
!> two edges at most are flagged
  SUBROUTINE geo3dintersecttriangleandplane(xt, p, n, klad, ilad, xii)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, 3)::xt
    REAL(8), DIMENSION(3)::p, n
    REAL(8), DIMENSION(2)::xii
    INTEGER, DIMENSION(2)::ilad
    tol = epsmach()
    klad = 0
    DO ia = 1, 3
      in1 = ia
      in2 = scalarmodularposition(3, ia+1)
      CALL geogeneralintersection(xt(in1, 1:3), xt(in2, 1:3), p, n, xi, ires)
      IF (ires .EQ. 1 .AND. xi .GT. -1.0d00+tol .AND. xi .LT. 1.0d00-tol) THEN
        klad = klad+1
        ilad(klad) = ia
        xii(klad) = xi
      END IF
    END DO
  END SUBROUTINE geo3dintersecttriangleandplane
!> determines normal vector to a quad
  SUBROUTINE geo3dnormaltoquad(x1, x2, x3, x4, n)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3, x4
    REAL(8), DIMENSION(3)::n
    CALL geo3dcrossprod(x3(1:3)-x1(1:3), x4(1:3)-x2(1:3), n)
    CALL vectnormalize(3, n)
  END SUBROUTINE geo3dnormaltoquad
!> determines normal vector to a triangle
  SUBROUTINE geo3dnormaltotri(x1, x2, x3, n)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3, n
    CALL geo3dcrossprod(x2(1:3)-x1(1:3), x3(1:3)-x1(1:3), n)
    CALL vectnormalize(3, n)
  END SUBROUTINE geo3dnormaltotri
!> determines a orthogonal and normed frame
!> to a surface given two independent vectors w1 and w2
  SUBROUTINE geo3dorthogonalframe(w1, w2, e1, e2, e3)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::v1, v2, e1, e2, e3, temp1, temp2, w1, w2
    CALL vectcopy(3, v1, w1)
    CALL vectcopy(3, v2, w2)
    CALL vectnormalize(3, v1)
    CALL vectnormalize(3, v2)
    CALL geo3dcrossprod(v1, v2, e3)
    CALL vectnormalize(3, e3)
    CALL geo3dcrossprod(v2, e3, temp1)
    CALL vectnormalize(3, temp1)
    CALL geo3dcrossprod(e3, v1, temp2)
    CALL vectnormalize(3, temp2)
    e1 = v1+temp1
    e2 = v2+temp2
    CALL vectnormalize(3, e1)
    CALL vectnormalize(3, e2)
  END SUBROUTINE geo3dorthogonalframe
!> calculates an orthogona projection matrix
  SUBROUTINE geo3dorthogonalprojectionmatrix(vr, z, mat)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(4, 4)::mat, mat1, mat2
    REAL(8), DIMENSION(3)::z, vr
    CALL geo3dtranslationmatrix(-z(1), -z(2), -z(3), mat)
    mat2 = 0.0d00
    mat2(4, 4) = 1.0d00
    CALL geo3drotationfromanglevector(vr, mat2(1:3, 1:3))
    CALL matrixmatrixproduct(4, 4, 4, mat2, mat, mat1, 0)
    CALL geo3dtranslationmatrix(z(1), z(2), z(3), mat2)
    CALL matrixmatrixproduct(4, 4, 4, mat2, mat1, mat, 0)
  END SUBROUTINE geo3dorthogonalprojectionmatrix
!> projects a vector v in a plane with normal n
  SUBROUTINE geo3dprojectinplane(ndi, v, vt, n)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::v, vt, n
    REAL(8), DIMENSION(ndi)::vsave
    CALL vectcopy(ndi, vsave, v)
    DO i = 1, ndi
      s = vsave(i)
      r = n(i)
      DO j = 1, ndi
        s = s-r*n(j)*vsave(j)
      END DO
      vt(i) = s
    END DO
  END SUBROUTINE geo3dprojectinplane
!> determines the shape functions ff
!> corresponding to an incident point x
!> and given coordinates x1...x4
  SUBROUTINE geo3dprojtet(x, x1, x2, x3, x4, ff)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3, x4, x, a, b, c
    REAL(8), DIMENSION(4)::ff
    REAL(8), DIMENSION(3, 3)::lhs
    REAL(8), DIMENSION(3)::rhs
    DO i = 1, 3
      a(i) = x1(i)-x2(i)
      b(i) = x1(i)-x3(i)
      c(i) = x1(i)-x4(i)
    END DO
    lhs(1:3, 1) = a
    lhs(1:3, 2) = b
    lhs(1:3, 3) = c
    rgash = matrix123determinantonly(3, lhs)
    IF (abs(rgash) .LT. epsmach()) THEN
      ff = 1.0d20
    ELSE
      rhs = x1-x
      CALL matrixsolvesystem(3, lhs, rhs)
      ff(1) = 1.0d00-rhs(1)-rhs(2)-rhs(3)
      ff(2) = rhs(1)
      ff(3) = rhs(2)
      ff(4) = rhs(3)
    END IF
  END SUBROUTINE geo3dprojtet
!> determines the shape functions ff
!> corresponding to an incident point x
!> and given coordinates x1...x3
  SUBROUTINE geo3dprojtriangle(x, x1, x2, x3, ff)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3, x, ff, a, b, c
    REAL(8), DIMENSION(2, 2)::lhs
    REAL(8), DIMENSION(2)::rhs
    DO i = 1, 3
      a(i) = x3(i)-x(i)
      b(i) = x3(i)-x1(i)
      c(i) = x3(i)-x2(i)
    END DO
    lhs(1, 1) = vectdot(3, b, b)
    lhs(1, 2) = vectdot(3, b, c)
    lhs(2, 1) = vectdot(3, b, c)
    lhs(2, 2) = vectdot(3, c, c)
    rhs(1) = vectdot(3, a, b)
    rhs(2) = vectdot(3, a, c)
    rgash = matrix123determinantonly(2, lhs)
    IF (abs(rgash) .GT. epsmach()) THEN
      CALL matrixsolvesystem(2, lhs, rhs)
      ff(1) = rhs(1)
      ff(2) = rhs(2)
      ff(3) = 1.0d00-ff(1)-ff(2)
    ELSE
      ff = 1.0d99
    END IF
  END SUBROUTINE geo3dprojtriangle
!> determines a projection with scaling of a triangle
!> x is the incident point
!> x1,x2,x3 is the triangle
!> a (output) is the distance
!> ff (output) are the shape functions
!> isucc is 1 if succesful and 0 if not
  SUBROUTINE geo3dprojtriscale(x, x1, x2, x3, a, ff, isucc)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::isucc
    REAL(8), DIMENSION(3)::x1, x2, x3, x, ff
    REAL(8), DIMENSION(3, 3)::lhs
    REAL(8), DIMENSION(3)::rhs
    isucc = 0
    lhs(1:3, 1) = x1-x2
    lhs(1:3, 2) = x1-x3
    lhs(1:3, 3) = x
    rhs(1:3) = x1
    rd = matrix123determinantonly(3, lhs)
    IF (abs(rd) .LE. 1.0d-20) RETURN
    CALL matrixsolvesystem(3, lhs, rhs)
    xi1 = rhs(1)
    xi2 = rhs(2)
    a = rhs(3)
    xi3 = 1.0d00-xi1-xi2
    IF (xi1 .GT. 1.0d00 .OR. xi1 .LT. 0.0d00) RETURN
    IF (xi2 .GT. 1.0d00 .OR. xi2 .LT. 0.0d00) RETURN
    IF (xi3 .GT. 1.0d00 .OR. xi3 .LT. 0.0d00) RETURN
    IF (a .LE. 0.0d00) RETURN
    isucc = 1
    ff(1) = 1.0d00-xi1-xi2
    ff(2) = xi1
    ff(3) = xi2
  END SUBROUTINE geo3dprojtriscale
!> determines the coordinates of intersection of a line
!> with direction n and passing through x1
!> x2, x3, x4
!> xitriang
  SUBROUTINE geo3dprojtrianglesemi(x1, n, x2, x3, x4, xitriang)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3, x4, n
    REAL(8), DIMENSION(2)::xitriang
    REAL(8), DIMENSION(3, 3)::lhs
    REAL(8), DIMENSION(3)::rhs, a, b, c
    DO i = 1, 3
      a(i) = n(i)
      b(i) = x4(i)-x2(i)
      c(i) = x4(i)-x3(i)
    END DO
    lhs(1:3, 1) = a
    lhs(1:3, 2) = b
    lhs(1:3, 3) = c
    rhs(1:3) = x4-x1
    CALL matrixsolvesystem(3, lhs, rhs)
    xitriang(1) = rhs(2)
    xitriang(2) = rhs(3)
  END SUBROUTINE geo3dprojtrianglesemi
!> returns t,dt,ddt
!> dr is the angle-vector
!> t0 is previous director
!> t is the new director
!> dt is the derivative of t
!> ddt is the second derivative of t
  SUBROUTINE geo3drotationandderivatives(dr, t0, t, dt, ddt)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(5001)::v
    REAL(8), DIMENSION(3)::dr, t0, t
    REAL(8), DIMENSION(3, 3), OPTIONAL::dt
    REAL(8), DIMENSION(3, 3, 3), OPTIONAL::ddt
    IF (vectnorm2(3, dr) .LE. epsmach()) dr = 1.0d-6
    v(330) = dr(1)*dr(2)
    v(329) = dr(1)*dr(3)
    v(327) = dr(2)*dr(3)
    v(36) = 2d0*dr(1)
    v(23) = dr(1)**2
    v(37) = 2d0*dr(2)
    v(13) = dr(2)**2
    v(97) = -v(13)-v(23)
    v(346) = t0(3)*v(97)
    v(38) = 2d0*dr(3)
    v(14) = dr(3)**2
    v(314) = v(13)+v(14)+v(23)
    v(318) = sqrt(v(314))
    v(316) = 1d0/v(318)
    v(313) = v(316)/2d0
    v(123) = v(313)*v(38)
    v(333) = dr(3)*v(123)
    v(133) = v(123)/2d0
    v(120) = v(313)*v(37)
    v(132) = v(120)/2d0
    v(118) = v(314)
    v(122) = -(v(120)/v(118))
    v(317) = v(122)/2d0
    v(126) = v(317)*v(38)
    v(117) = v(313)*v(36)
    v(131) = v(117)/2d0
    v(119) = -(v(117)/v(118))
    v(315) = v(119)/2d0
    v(128) = v(315)*v(37)
    v(125) = v(315)*v(38)
    v(86) = -v(14)-v(23)
    v(345) = t0(2)*v(86)
    v(77) = -v(13)-v(14)
    v(341) = t0(1)*v(77)
    v(39) = v(316)
    v(130) = v(315)*v(36)+v(39)
    v(129) = v(317)*v(37)+v(39)
    v(127) = -((v(133)*v(38))/v(118))+v(39)
    v(12) = v(318)
    v(338) = -(dr(1)/v(12))
    v(337) = dr(2)/v(12)
    v(334) = -(dr(3)/v(12))
    v(319) = dsin(v(12))
    v(168) = -(v(123)*v(319))
    v(167) = v(120)*v(319)
    v(166) = -(v(117)*v(319))
    v(141) = 1d0/v(12)**3
    v(320) = (-2d0)*v(141)
    v(148) = v(123)*v(320)
    v(146) = v(117)*v(320)
    v(58) = v(12)/2d0
    v(134) = dcos(v(58))
    v(137) = v(133)*v(134)
    v(136) = v(132)*v(134)
    v(135) = v(131)*v(134)
    v(59) = dsin(v(58))
    v(321) = (-8d0)*v(141)*v(59)
    v(155) = v(137)*v(321)
    v(153) = v(136)*v(321)
    v(150) = v(135)*v(321)
    v(57) = (v(59)*v(59))
    v(143) = (12d0*v(57))/v(12)**4
    v(144) = v(120)*v(143)+v(153)
    v(142) = v(117)*v(143)+v(150)
    v(61) = (-4d0)*v(141)*v(57)
    v(47) = 1d0/v(12)**2
    v(339) = v(117)*v(47)
    v(332) = -(dr(2)*v(47))
    v(322) = 2d0*v(47)
    v(206) = dr(1)*v(339)
    v(188) = v(333)*v(47)
    v(335) = 2d0*v(188)
    v(152) = v(134)*v(322)
    v(149) = v(322)*v(59)
    v(323) = -(v(149)*v(59))
    v(154) = v(136)*v(152)+v(153)+v(132)*v(323)
    v(326) = v(144)+v(154)
    v(151) = v(150)+v(135)*v(152)+v(131)*v(323)
    v(325) = v(142)+v(151)
    v(62) = v(134)*v(149)
    v(324) = v(61)+v(62)
    v(186) = v(123)*v(62)
    v(183) = v(120)*v(62)
    v(181) = v(117)*v(62)
    v(162) = v(130)*v(324)+v(117)*v(325)
    v(163) = v(162)*v(327)
    v(161) = v(129)*v(324)+v(120)*v(326)
    v(160) = v(128)*v(324)+v(120)*v(325)
    v(159) = v(123)*(v(123)*v(143)+v(137)*v(152)+2d0*v(155)+v(133)*v(323))+v(127)*v(324)
    v(158) = v(126)*v(324)+v(123)*v(326)
    v(157) = v(125)*v(324)+v(123)*v(325)
    v(64) = v(186)+v(123)*v(61)
    v(282) = -(v(38)*v(64))
    v(226) = dr(3)*v(64)
    v(222) = dr(2)*v(64)
    v(328) = 2d0*v(222)
    v(231) = v(161)*v(327)+v(328)
    v(230) = v(159)*v(327)+v(328)
    v(213) = -(v(36)*v(64))
    v(210) = -v(328)
    v(63) = v(183)+v(120)*v(61)
    v(262) = -(v(37)*v(63))
    v(220) = dr(2)*v(63)
    v(60) = v(181)+v(117)*v(61)
    v(340) = dr(1)*v(60)
    v(243) = -(v(36)*v(60))
    v(217) = dr(3)*v(60)
    v(250) = -v(213)
    v(238) = -v(213)
    v(228) = v(250)+v(162)*v(329)
    v(225) = v(238)+v(159)*v(329)
    v(216) = dr(2)*v(60)
    v(331) = 2d0*v(216)
    v(223) = v(162)*v(330)+v(331)
    v(219) = v(161)*v(330)+v(331)
    v(208) = -v(331)
    v(165) = v(216)+v(157)*v(327)
    v(164) = v(217)+v(160)*v(327)
    v(65) = dr(3)*v(216)
    v(44) = dcos(v(12))
    v(172) = v(120)*v(166)+v(128)*v(44)
    v(171) = v(123)*v(168)+v(127)*v(44)
    v(169) = v(123)*v(166)+v(125)*v(44)
    v(46) = v(123)*v(44)
    v(185) = v(46)/v(12)
    v(177) = v(332)*v(46)
    v(45) = v(120)*v(44)
    v(197) = -(v(45)/v(12))
    v(195) = v(332)*v(45)
    v(43) = v(117)*v(44)
    v(205) = -(v(43)/v(12))
    v(182) = v(181)+v(205)
    v(18) = v(319)
    v(336) = v(18)*v(47)
    v(199) = dr(1)*v(151)+v(62)
    v(193) = v(18)*v(333)
    v(192) = dr(3)*v(336)
    v(194) = (-2d0)*v(185)+2d0*v(186)+v(127)*v(192)+v(148)*v(193)+v(171)*v(334)+v(335)*v(46)
    v(190) = v(182)+v(125)*v(192)+v(146)*v(193)+v(169)*v(334)+v(335)*v(43)
    v(175) = v(195)-dr(2)*v(167)*v(320)-v(62)
    v(174) = -(dr(2)*v(151))
    v(54) = -(dr(2)*v(336))
    v(198) = -v(183)+v(120)*(v(175)+v(195))-2d0*v(197)+v(337)*(-(v(120)*v(167))+v(129)*v(44))+v(129)*v(54)
    v(196) = v(120)*v(174)-v(182)+v(117)*v(195)+v(172)*v(337)+v(128)*v(54)
    v(180) = dr(2)*(v(148)*v(168)+v(171)/v(12))+2d0*v(123)*v(177)+v(127)*v(54)
    v(179) = v(123)*v(175)+v(120)*v(177)+v(185)+v(337)*(-(v(123)*v(167))+v(126)*v(44))+v(126)*v(54)
    v(178) = v(123)*v(174)+v(117)*v(177)+v(169)*v(337)+v(125)*v(54)
    v(55) = dr(2)*v(185)+v(123)*v(54)
    v(52) = -(v(18)/v(12))
    v(56) = -(dr(3)*v(185))+v(18)*v(188)+v(52)
    v(53) = -(dr(2)*v(197))-v(52)+v(120)*v(54)
    v(49) = dr(1)*v(336)
    v(207) = v(182)+v(117)*v(199)+v(205)+v(206)*v(43)+v(338)*(v(117)*v(166)+v(130)*v(44))+v(130)*v(49)
    v(203) = v(197)+v(120)*v(199)+v(172)*v(338)+v(206)*v(45)+v(128)*v(49)
    v(202) = -v(185)+v(123)*v(199)+dr(1)*(-(v(169)/v(12))+v(339)*v(46))+v(125)*v(49)
    v(51) = -(dr(1)*v(185))+v(123)*v(49)
    v(50) = dr(1)*v(197)+v(120)*v(49)
    v(48) = dr(1)*v(205)+v(117)*v(49)+v(52)
    v(16) = -v(323)
    v(229) = v(16)+v(220)+v(226)+v(158)*v(327)
    v(215) = v(16)+v(340)
    v(224) = v(215)+v(226)+v(157)*v(329)
    v(218) = v(215)+v(220)+v(160)*v(330)
    v(211) = (-2d0)*v(16)
    v(214) = v(211)+v(282)
    v(343) = v(214)+v(282)
    v(212) = v(211)+v(243)
    v(344) = v(212)+v(243)
    v(209) = v(211)+v(262)
    v(342) = v(209)+v(262)
    v(101) = -(v(16)*v(37))
    v(96) = -(v(16)*v(36))
    v(93) = -(v(16)*v(38))
    v(73) = dr(1)*v(16)
    v(74) = dr(1)*v(220)+v(73)
    v(71) = dr(2)*v(16)
    v(72) = dr(2)*v(340)+v(71)
    v(70) = dr(1)*v(226)+v(73)
    v(68) = dr(3)*v(16)
    v(69) = v(329)*v(60)+v(68)
    v(67) = dr(3)*v(222)+v(71)
    v(66) = dr(3)*v(220)+v(68)
    v(29) = dr(2)*v(68)
    v(26) = dr(1)*v(68)
    v(20) = dr(1)*v(71)
    v(293) = t0(3)*(v(163)+v(196))+t0(2)*(-v(178)+v(218))+t0(1)*(v(208)+v(160)*v(77))
    v(294) = t0(2)*(v(163)+v(190))+t0(3)*(v(178)+v(224))+t0(1)*(-v(238)+v(157)*v(77))
    v(296) = t0(3)*(v(164)+v(203))+t0(1)*(v(178)+v(218))+t0(2)*(-v(331)+v(160)*v(86))
    v(297) = t0(1)*(v(163)-v(190))+t0(3)*(v(165)+v(202))+t0(2)*(v(213)-v(250)+v(157)*v(86))
    v(299) = t0(1)*(v(163)-v(196))+t0(2)*(v(164)-v(203))+t0(3)*(2d0*v(208)+v(160)*v(97))
    v(300) = t0(2)*(v(165)-v(202))+t0(1)*(-v(178)+v(224))+t0(3)*(v(213)+v(157)*v(97))
    v(302) = t0(3)*(v(165)+v(179))+t0(2)*(v(164)-v(180))+t0(1)*(2d0*v(210)+v(158)*v(77))
    v(304) = t0(1)*(v(164)+v(180))+t0(3)*(-v(178)+v(229))+t0(2)*(v(210)+v(158)*v(86))
    v(306) = t0(1)*(v(165)-v(179))+t0(2)*(v(178)+v(229))+t0(3)*(v(210)+v(158)*v(97))
    t(1) = t0(2)*(v(168)+v(20))+t0(3)*(v(167)+v(26))+t0(1)*(1d0+v(16)*v(77))
    t(2) = t0(1)*(-v(168)+v(20))+t0(3)*(v(166)+v(29))+t0(2)*(1d0+v(16)*v(86))
    t(3) = t0(1)*(-v(167)+v(26))+t0(2)*(-v(166)+v(29))+t0(3)*(1d0+v(16)*v(97))
    IF (present(dt)) THEN
      dt(1, 1) = v(341)*v(60)+t0(3)*(-v(50)+v(69))+t0(2)*(v(51)+v(72))
      dt(1, 2) = t0(3)*(v(53)+v(65))+t0(2)*(-v(55)+v(74))+t0(1)*(v(101)+v(63)*v(77))
      dt(1, 3) = t0(2)*(v(56)+v(65))+t0(3)*(v(55)+v(70))+t0(1)*(v(64)*v(77)+v(93))
      dt(2, 1) = t0(3)*(v(48)+v(65))+t0(1)*(-v(51)+v(72))+t0(2)*(v(60)*v(86)+v(96))
      dt(2, 2) = v(345)*v(63)+t0(3)*(v(50)+v(66))+t0(1)*(v(55)+v(74))
      dt(2, 3) = t0(1)*(-v(56)+v(65))+t0(3)*(v(51)+v(67))+t0(2)*(v(64)*v(86)+v(93))
      dt(3, 1) = t0(2)*(-v(48)+v(65))+t0(1)*(v(50)+v(69))+t0(3)*(v(96)+v(60)*v(97))
      dt(3, 2) = t0(1)*(-v(53)+v(65))+t0(2)*(-v(50)+v(66))+t0(3)*(v(101)+v(63)*v(97))
      dt(3, 3) = v(346)*v(64)+t0(2)*(-v(51)+v(67))+t0(1)*(-v(55)+v(70))
    END IF
    IF (present(ddt)) THEN
      ddt(1, 1, 1) = t0(2)*(v(202)+v(223))+t0(3)*(-v(203)+v(228))+v(162)*v(341)
      ddt(1, 1, 2) = v(293)
      ddt(1, 1, 3) = v(294)
      ddt(1, 2, 1) = v(293)
      ddt(1, 2, 2) = t0(3)*(v(164)+v(198))+t0(2)*(-v(179)+v(219))+t0(1)*(v(342)+v(161)*v(77))
      ddt(1, 2, 3) = v(302)
      ddt(1, 3, 1) = v(294)
      ddt(1, 3, 2) = v(302)
      ddt(1, 3, 3) = t0(2)*(v(165)+v(194))+t0(3)*(v(180)+v(225))+t0(1)*(v(343)+v(159)*v(77))
      ddt(2, 1, 1) = t0(3)*(v(163)+v(207))+t0(1)*(-v(202)+v(223))+t0(2)*(v(344)+v(162)*v(86))
      ddt(2, 1, 2) = v(296)
      ddt(2, 1, 3) = v(297)
      ddt(2, 2, 1) = v(296)
      ddt(2, 2, 2) = t0(1)*(v(179)+v(219))+t0(3)*(-v(196)+v(231))+v(161)*v(345)
      ddt(2, 2, 3) = v(304)
      ddt(2, 3, 1) = v(297)
      ddt(2, 3, 2) = v(304)
      ddt(2, 3, 3) = t0(1)*(v(165)-v(194))+t0(3)*(v(190)+v(230))+t0(2)*(v(343)+v(159)*v(86))
      ddt(3, 1, 1) = t0(2)*(v(163)-v(207))+t0(1)*(v(203)+v(228))+t0(3)*(v(344)+v(162)*v(97))
      ddt(3, 1, 2) = v(299)
      ddt(3, 1, 3) = v(300)
      ddt(3, 2, 1) = v(299)
      ddt(3, 2, 2) = t0(1)*(v(164)-v(198))+t0(2)*(v(196)+v(231))+t0(3)*(v(342)+v(161)*v(97))
      ddt(3, 2, 3) = v(306)
      ddt(3, 3, 1) = v(300)
      ddt(3, 3, 2) = v(306)
      ddt(3, 3, 3) = t0(1)*(-v(180)+v(225))+t0(2)*(-v(190)+v(230))+v(159)*v(346)
    END IF
  END SUBROUTINE geo3drotationandderivatives
!> returns the rotation matrix from the angle-vector
  SUBROUTINE geo3drotationfromanglevector(vet, rot)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, 3)::rot
    REAL(8), DIMENSION(3)::vet, vec
    vec = vet
    ang = vectnorm2(3, vec)
    CALL vectnormalize(3, vec)
    c = cos(ang)
    s = sin(ang)
    wx = vec(1)
    wy = vec(2)
    wz = vec(3)
    c1 = 1.0d00-c
    rot(1, 1) = c+wx*wx*c1; rot(1, 2) = wx*wy*c1-wz*s; rot(1, 3) = wy*s+wx*wz*c1
    rot(2, 1) = wz*s+wx*wy*c1; rot(2, 2) = c+wy*wy*c1; rot(2, 3) = -wx*s+wy*wz*c1
    rot(3, 1) = -wy*s+wx*wz*c1; rot(3, 2) = wx*s+wy*wz*c1; rot(3, 3) = c+wz*wz*c1
  END SUBROUTINE geo3drotationfromanglevector
!> calculates a rotation matrix (rot)
!> from two vectors: newvector is the new director and oldvector is the
!> old director
  SUBROUTINE geo3drotationfromtwovectors(newvector, oldvector, rot)
    IMPLICIT NONE
    REAL(8) v(42), newvector(3), oldvector(3), rot(3, 3)
    v(36) = 1d0/sqrt(oldvector(1)**2+oldvector(2)**2+oldvector(3)**2)
    v(35) = 1d0/sqrt(newvector(1)**2+newvector(2)**2+newvector(3)**2)
    v(16) = v(36)*oldvector(1)
    v(18) = v(36)*oldvector(2)
    v(19) = v(36)*oldvector(3)
    v(20) = newvector(1)*v(35)
    v(22) = newvector(2)*v(35)
    v(23) = newvector(3)*v(35)
    v(24) = -(v(19)*v(22))+v(18)*v(23)
    v(25) = v(19)*v(20)-v(16)*v(23)
    v(26) = -(v(18)*v(20))+v(16)*v(22)
    v(27) = 1d0/(1d0+v(16)*v(20)+v(18)*v(22)+v(19)*v(23))
    v(37) = v(24)*v(27)
    v(28) = (v(25)*v(25))
    v(29) = (v(26)*v(26))
    v(30) = v(25)*v(37)
    v(31) = v(26)*v(37)
    v(32) = (v(24)*v(24))
    v(33) = v(25)*v(26)*v(27)
    rot(1, 1) = 1d0+v(27)*(-v(28)-v(29))
    rot(1, 2) = -v(26)+v(30)
    rot(1, 3) = v(25)+v(31)
    rot(2, 1) = v(26)+v(30)
    rot(2, 2) = 1d0+v(27)*(-v(29)-v(32))
    rot(2, 3) = -v(24)+v(33)
    rot(3, 1) = -v(25)+v(31)
    rot(3, 2) = v(24)+v(33)
    rot(3, 3) = 1d0+v(27)*(-v(28)-v(32))
  END SUBROUTINE geo3drotationfromtwovectors
!> given an axis iax, calculates the rotation
!> matrix mat
  SUBROUTINE geo3drotationmatrix(iax, r, mat)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(4, 4)::mat
    CALL vectsetconst(4*4, mat, 0.0d00)
    cr = cos(r)
    sr = sin(r)
    SELECT CASE (iax)
    CASE (1)
      mat(1, 1) = 1.0d00
      mat(2, 2) = cr
      mat(3, 3) = cr
      mat(4, 4) = 1.0d00
      mat(2, 3) = -sr
      mat(3, 2) = sr
    CASE (2)
      mat(2, 2) = 1.0d00
      mat(1, 1) = cr
      mat(3, 3) = cr
      mat(4, 4) = 1.0d00
      mat(1, 3) = sr
      mat(3, 1) = -sr
    CASE (3)
      mat(3, 3) = 1.0d00
      mat(1, 1) = cr
      mat(2, 2) = cr
      mat(4, 4) = 1.0d00
      mat(1, 2) = -sr
      mat(2, 1) = sr
    END SELECT
  END SUBROUTINE geo3drotationmatrix
!> determines the rotation
!> of a set of nodes (#=nn)
!> from deformed and undeformed coordinates
!> x0: undeformed coordinates
!> x: deformed coordinates
  SUBROUTINE geo3drotationsetofpoints(nn, ndi, x0, x, rot, drot)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(ndi, *)::x0, x
    REAL(8), DIMENSION(ndi, ndi)::rot
    REAL(8), DIMENSION(ndi*ndi, ndi*ndi)::a, ai
    REAL(8), DIMENSION(ndi*ndi)::b, xsol
    REAL(8), DIMENSION(ndi, ndi, ndi, nn)::drot
    a = 0.0d00
    b = 0.0d00
    DO K = 1, nn
      DO i = 1, 3
        DO j = 1, 3
          ij = i*3-3+j
          DO m = 1, 3
            DO n = 1, 3
              mn = m*3-3+n
              a(mn, ij) = a(mn, ij)+(x0(j, 1)-x0(j, K))*(x0(n, 1)-x0(n, K))*matrixkronecker(i, m)
            END DO
          END DO
        END DO
      END DO
    END DO
    CALL matrixinverse(9, det, a, ai)
    DO K = 1, nn
      DO m = 1, 3
        DO n = 1, 3
          mn = m*3-3+n
          b(mn) = b(mn)+(x(m, k)-x(m, 1))*(x0(n, k)-x0(n, 1))
        END DO
      END DO
    END DO
    CALL matrixvectorproduct(9, 9, ai, b, xsol)
! rotation
    DO i = 1, 3
      DO j = 1, 3
        ij = i*3-3+j
        rot(i, j) = xsol(ij)
      END DO
    END DO
! derivative of rotation
    drot = 0.0d00
    DO i = 1, 3
      DO j = 1, 3
        ij = i*3-3+j
        DO k = 1, nn
          DO m = 1, 3
            DO n = 1, 3
              mn = m*3-3+n
              drot(i, j, m, k) = drot(i, j, m, k)+ai(ij, mn)*(x0(n, k)-x0(n, 1))*(1.0d00-matrixkronecker(k, 1))
            END DO
          END DO
        END DO
      END DO
    END DO
!HERE
  END SUBROUTINE geo3drotationsetofpoints
!> performs a scaling operation around a point
!> s1,s2,s3 are the scaling factors for the 3 axix
!> np is the number of points
!> xp is the point list
!> xm is the center
  SUBROUTINE geo3dscalearoundapoint(s1, s2, s3, np, xp, xm)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, *)::xp
    REAL(8), DIMENSION(4, 4)::mat
    REAL(8), DIMENSION(3)::xm, xt
    CALL geo3dscalematrixaroundpoint(s1, s2, s3, xm, mat)
    DO i = 1, np
      CALL matrixvectorproduct(3, 3, mat(1:3, 1:3), xp(1:3, i), xt(1:3))
      xp(1:3, i) = xt
    END DO
  END SUBROUTINE geo3dscalearoundapoint
!> determines a scaling matrix around a point z
  SUBROUTINE geo3dscalematrixaroundpoint(s1, s2, s3, z, mat)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(4, 4)::mat, mat1, mat2
    REAL(8), DIMENSION(3)::z
    CALL geo3dtranslationmatrix(-z(1), -z(2), -z(3), mat)
    CALL geo3dscalematrix(s1, s2, s3, mat2)
    CALL matrixmatrixproduct(4, 4, 4, mat2, mat, mat1, 0)
    CALL geo3dtranslationmatrix(z(1), z(2), z(3), mat2)
    CALL matrixmatrixproduct(4, 4, 4, mat2, mat1, mat, 0)
  END SUBROUTINE geo3dscalematrixaroundpoint
!> determines a scaling matrix
  SUBROUTINE geo3dscalematrix(s1, s2, s3, mat)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(4, 4)::mat
    CALL vectsetconst(4*4, mat)
    mat(1, 1) = s1
    mat(2, 2) = s2
    mat(3, 3) = s3
    mat(4, 4) = 1.0d00
  END SUBROUTINE geo3dscalematrix
!> determines which side of the plane is a given point xi
!> xp is a point belonging to the plane
!> np is the plane normal
  INTEGER FUNCTION geo3dsideofplane(np, xp, xi)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), PARAMETER::rtol = 1.0d-9
    REAL(8), DIMENSION(3)::np, xp, xi, xd
    xd = xi-xp
    rd = vectnorm2(3, xd)*rtol
    r = 0.0d00
    DO i = 1, 3
      r = r+xd(i)*np(i)
    END DO
    IF (abs(r) .LE. rd) THEN
      geo3dsideofplane = 0
    ELSE IF (r .LT. 0.0d00) THEN
      geo3dsideofplane = -1
    ELSE
      geo3dsideofplane = 1
    END IF
  END FUNCTION geo3dsideofplane
!> swaps the connectivities of a tet if it is inverted
  SUBROUTINE geo3dtetcorrect(xno, conec)
    INTEGER, DIMENSION(*)::conec
    REAL(8), DIMENSION(3, *)::xno
    IF (geo3dtetvolume(xno(1:3, conec(1)), xno(1:3, conec(2)), xno(1:3, conec(3)), xno(1:3, conec(4))) .LT. 0.0d00) THEN
      CALL scalarswap(conec(1), conec(2))
    END IF
  END SUBROUTINE geo3dtetcorrect
!> returns the matlab quality estimator for a tet
  REAL(8) FUNCTION geo3dtetquality(x1, x2, x3, x4)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3, x4
    q = 36.0d00*sqrt(2.0d00)*geo3dtetvolume(x1, x2, x3, x4)
    h1 = vectnorm2(3, x1-x2)
    h2 = vectnorm2(3, x2-x3)
    h3 = vectnorm2(3, x3-x1)
    h4 = vectnorm2(3, x1-x4)
    h5 = vectnorm2(3, x2-x4)
    h6 = vectnorm2(3, x3-x4)
    q = q/(h1**3+h2**3+h3**3+h4**3+h5**3+h6**3)
    geo3dtetquality = q
  END FUNCTION geo3dtetquality
!> returns the parent-domain coordinate violation for a tet
  REAL(8) FUNCTION geo3dtetviolation(xi)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::xi
    geo3dtetviolation = 0.0d00
    DO i = 1, 3
      geo3dtetviolation = max(xi(i)-1.0d00, -xi(i), geo3dtetviolation)
    END DO
    xi4 = 1.0d00-xi(1)-xi(2)-xi(3)
    geo3dtetviolation = max(xi4-1.0d00, -xi4, geo3dtetviolation)
    IF (abs(xi(1)+xi(2)+xi(3)+xi4-1.0d00) .GT. 1.0d-20) geo3dtetviolation = 1.0d30
  END FUNCTION geo3dtetviolation
!> returns the volume of a tet
  REAL(8) FUNCTION geo3dtetvolume(x1, x2, x3, x4)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, 3)::jaco
    REAL(8), DIMENSION(3)::x1, x2, x3, x4
    jaco(1, 1) = x1(1)-x3(1)
    jaco(2, 1) = x1(2)-x3(2)
    jaco(3, 1) = x1(3)-x3(3)
    jaco(1, 2) = x2(1)-x3(1)
    jaco(2, 2) = x2(2)-x3(2)
    jaco(3, 2) = x2(3)-x3(3)
    jaco(1, 3) = -x3(1)+x4(1)
    jaco(2, 3) = -x3(2)+x4(2)
    jaco(3, 3) = -x3(3)+x4(3)
    dete = matrix123determinantonly(3, jaco)
    geo3dtetvolume = dete*0.16666666666667
  END FUNCTION geo3dtetvolume
!> determines a translation matrix for displacement u1,u2,u3
  SUBROUTINE geo3dtranslationmatrix(u1, u2, u3, mat)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(4, 4)::mat
    CALL vectsetconst(4*4, mat)
    CALL matrixaddidentity(4, mat)
    mat(1, 4) = u1
    mat(2, 4) = u2
    mat(3, 4) = u3
  END SUBROUTINE geo3dtranslationmatrix
!> returns the quality of a triangle
  REAL(8) FUNCTION geo3dtrianglequality(x1, x2, x3)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3)::x1, x2, x3
    q = 4.0d00*sqrt(3.0d00)*geo3dareatriangle(x1, x2, x3)
    h1 = vectnorm2(3, x2-x1)
    h2 = vectnorm2(3, x3-x2)
    h3 = vectnorm2(3, x3-x1)
    q = q/(h1**2+h2**2+h3**2)
    geo3dtrianglequality = q
  END FUNCTION geo3dtrianglequality
!> determines a rotation matrix rf from a previous rotation
!> matrix ri and the spin tensor w
  SUBROUTINE geo3dupdaterotationusingskew(ndi, w, ri, rf)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(ndi, ndi)::w, ri, rf
    REAL(8), DIMENSION(3, 3)::rot
    REAL(8), DIMENSION(3)::vec
    SELECT CASE (ndi)
    CASE (2)
      CALL vectsetconst(2, vec, 0.0d00)
      vec(3) = w(2, 1)
    CASE (3)
      vec(1) = w(3, 2)
      vec(2) = w(1, 3)
      vec(3) = w(2, 1)
    END SELECT
    CALL geo3drotationfromanglevector(vec, rot)
    CALL matrixmatrixproduct(ndi, ndi, ndi, rot(1:ndi, 1:ndi), ri, rf, 1)
  END SUBROUTINE geo3dupdaterotationusingskew
!> determines the intersection between a line and a line segment
!> or a line and a plane
!> xi is the parent-domain coordinate of the intersection
  SUBROUTINE geogeneralintersection(a, b, xline, nline, xi, ires)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(:)::a, b, xline, nline
    ndi = size(a)
    CALL vectnormalize(ndi, nline)
    ires = 0
    r1 = vectdot(ndi, nline, a-xline)
    r2 = vectdot(ndi, nline, b-xline)
    IF (abs(r1-r2) .GT. epsmach()) THEN
      ires = 1
      xi = (r1+r2)/(r1-r2)
    END IF
  END SUBROUTINE geogeneralintersection
!> determines the violation in parent-domain coordinate violation
!> given xi for a triangle
  REAL(8) FUNCTION geotriangviolation(xi)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(2)::xi
    geotriangviolation = 0.0d00
    DO i = 1, 2
      geotriangviolation = max(xi(i)-1.0d00, -xi(i), geotriangviolation)
    END DO
    xi3 = 1.0d00-xi(1)-xi(2)
    geotriangviolation = max(xi3-1.0d00, -xi3, geotriangviolation)
  END FUNCTION geotriangviolation
!> use binary search to search a list
  INTEGER FUNCTION listbinarysearch(n, l, sorted, u)
    IMPLICIT INTEGER(a-z)
    LOGICAL::sorted
    INTEGER::u
    INTEGER, DIMENSION(*)::l
    IF (.NOT. sorted) THEN
      listbinarysearch = listlinearsearch(n, l, u)
    ELSE
      listbinarysearch = 0
      IF (n .GE. 2) THEN
        i = 1
        j = n
        DO
          k = i+((j-i)/2)
          IF (u .LT. l(k)) THEN
            j = k
          ELSE
            i = k
          END IF
          IF (i+1 .GE. j) EXIT
        END DO
        listbinarysearch = i
      ELSEIF (n .EQ. 1) THEN
        listbinarysearch = 1
      END IF
      IF (l(listbinarysearch) .NE. u) listbinarysearch = 0
    END IF
  END FUNCTION listbinarysearch
!> list lexicographical comparison
  INTEGER FUNCTION listscomparelexicographically(nsize1, arr1, nsize2, arr2)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: nsize1, nsize2
    INTEGER, INTENT(in) :: arr1(nsize1), arr2(nsize2)
    INTEGER :: i
! Initialize the result to 0 (equal)
    listscomparelexicographically = 0
    IF (nsize1 .LT. nsize2) THEN
      listscomparelexicographically = -1
    ELSEIF (nsize1 .GT. nsize2) THEN
      listscomparelexicographically = +1
    ELSE
      DO i = 1, nsize1
        IF (arr1(i) < arr2(i)) THEN
          listscomparelexicographically = -1
          RETURN
        ELSEIF (arr1(i) > arr2(i)) THEN
          listscomparelexicographically = 1
          RETURN
        END IF
      END DO
    END IF
! If no differences were found, the lists are equal
  END FUNCTION listscomparelexicographically
!> copies a list i=j
  SUBROUTINE listcopy(n, i, j)
    INTEGER::n
    INTEGER, DIMENSION(*)::i, j
    i(1:n) = j(1:n)
  END SUBROUTINE listcopy
!> determines the difference of two lists
  SUBROUTINE listdifference(n1, l1t, sorted1, n2, l2t, sorted2, n, l)
    INTEGER, DIMENSION(*)::l1t, l2t
    LOGICAL::sorted1, sorted2
    INTEGER, DIMENSION(n1)::l1
    INTEGER, DIMENSION(n2)::l2
    INTEGER, DIMENSION(:), ALLOCATABLE::l
    CALL listcopy(n1, l1, l1t)
    CALL listcopy(n2, l2, l2t)
    CALL allocsafe(0, l)
    IF (.NOT. sorted1) CALL listsort(n1, l1)
    IF (.NOT. sorted2) CALL listsort(n2, l2)
    i = 1
    j = 1
    n = 0
    DO
      IF (l1(i) .EQ. l2(j)) THEN
        i = i+1
        j = j+1
      ELSEIF (l1(i) .GT. l2(j)) THEN
        j = j+1
      ELSE
        n = n+1
        CALL listinsert(l, n, l1(i))
        i = i+1
      END IF
      IF (i .GT. n1 .OR. j .GT. n2) EXIT
    END DO
  END SUBROUTINE listdifference
!> reallocates a list l with n elements
  SUBROUTINE listdynamic(n, l)
    INTEGER, DIMENSION(:), ALLOCATABLE::l, lt
    IF (n .LE. 0) THEN
      IF (allocated(l)) DEALLOCATE (l)
    ELSE
      IF (allocated(l)) THEN
        nl = size(l)
        IF (nl .LT. n) THEN
          ALLOCATE (lt(nl), stat=kkk)
          IF (kkk .NE. 0) THEN
            WRITE (*, *) "fail to allocate a ", nl, " dimensional"
            WRITE (*, *) "array"
            STOP
          END IF
          CALL listcopy(nl, lt, l)
          CALL allocsafe(n*2, l)
          CALL listcopy(nl, l, lt)
          DEALLOCATE (lt)
        END IF
      ELSE
        CALL allocsafe(n*2, l)
      END IF
    END IF
  END SUBROUTINE listdynamic
!> check if two lists coincide
  LOGICAL FUNCTION listifcoincide(n, l1, sorted1, l2, sorted2)
    INTEGER, DIMENSION(*)::l1, l2
    LOGICAL::sorted1, sorted2
    INTEGER, DIMENSION(n)::l1c, l2c
    listifcoincide = .TRUE.
    CALL listcopy(n, l1c, l1)
    CALL listcopy(n, l2c, l2)
    IF (.NOT. sorted1) CALL listsort(n, l1c)
    IF (.NOT. sorted2) CALL listsort(n, l2c)
    DO i = 1, n
      IF (l1c(i) .NE. l2c(i)) THEN
        listifcoincide = .FALSE.
        RETURN
      END IF
    END DO
  END FUNCTION listifcoincide
!> inserts an element k in the list l at position i
  SUBROUTINE listinsert(l, i, k)
    INTEGER::k
    INTEGER, DIMENSION(:), ALLOCATABLE::l
    ktemp = k
    CALL listdynamic(i, l)
    IF (i .GT. 0) l(i) = ktemp
  END SUBROUTINE listinsert
!> inserts an element m in a list
!> if it is not there
  SUBROUTINE listinsertifnotthere(n, l, sorted, m)
    LOGICAL::sorted
    INTEGER, DIMENSION(:), ALLOCATABLE::l
    IF (n .EQ. 0) THEN
      n = n+1
    ELSEIF (0 .EQ. listbinarysearch(n, l, sorted, m)) THEN
      n = n+1
    END IF
    CALL listinsert(l, n, m)
  END SUBROUTINE listinsertifnotthere
!> obtains the intersection between two lists
  SUBROUTINE listintersection(n1, l1t, sorted1, n2, l2t, sorted2, n, l)
    INTEGER, DIMENSION(*)::l1t, l2t
    LOGICAL::sorted1, sorted2
    INTEGER, DIMENSION(n1)::l1
    INTEGER, DIMENSION(n2)::l2
    INTEGER, DIMENSION(:), ALLOCATABLE::l
    CALL listcopy(n1, l1, l1t)
    CALL listcopy(n2, l2, l2t)
    CALL allocsafe(0, l)
    IF (.NOT. sorted1) CALL listsort(n1, l1)
    IF (.NOT. sorted2) CALL listsort(n2, l2)
    i = 1
    j = 1
    n = 0
    IF (n1*n2 .LE. 0) RETURN
    DO
      IF (l1(i) .EQ. l2(j)) THEN
        n = n+1
        CALL listinsert(l, n, l1(i))
        i = i+1
        j = j+1
      ELSEIF (l1(i) .GT. l2(j)) THEN
        j = j+1
      ELSE
        i = i+1
      END IF
      IF (i .GT. n1 .OR. j .GT. n2) EXIT
    END DO
  END SUBROUTINE listintersection
!> performs a linear search on a list
  INTEGER FUNCTION listlinearsearch(n, l, m)
    INTEGER::n, m
    INTEGER, DIMENSION(*)::l
    listlinearsearch = 0
    DO i = 1, n
      k = l(i)
      IF (k .EQ. m) THEN
        listlinearsearch = i
        EXIT
      END IF
    END DO
  END FUNCTION listlinearsearch
  INTEGER FUNCTION listnorm2(n, l)
    INTEGER, DIMENSION(*)::l
    listnorm2 = 0
    DO i = 1, n
      listnorm2 = listnorm2+l(i)*l(i)
    END DO
    listnorm2 = nint(sqrt(1.0d00*listnorm2))
  END FUNCTION listnorm2
!> performs a permutation of a list
  SUBROUTINE listpermute(n, iperm, lis, ijob)
    INTEGER, DIMENSION(*)::iperm, lis
    INTEGER, DIMENSION(n)::list
    CALL listcopy(n, list, lis)
    SELECT CASE (ijob)
    CASE (0)
      DO i = 1, n
        IF (list(i) .EQ. 0) STOP "zero in listpermute"
        lis(i) = iperm(list(i))
      END DO
    CASE (1)
      DO i = 1, n
        IF (iperm(i) .EQ. 0) STOP "zero in listpermute"
        lis(i) = list(iperm(i))
      END DO
    CASE (2)
      DO i = 1, n
        IF (iperm(i) .EQ. 0) STOP "zero in listpermute"
        lis(iperm(i)) = list(i)
      END DO
    CASE (3)
      DO i = 1, n
        IF (iperm(i) .EQ. 0) STOP "zero in listpermute"
        lis(iperm(i)) = i
      END DO
    END SELECT
  END SUBROUTINE listpermute
!> print a vector
  SUBROUTINE listprint(mtx)
    INTEGER, DIMENSION(:)::mtx
    m = size(mtx)
    DO i = 1, m
      WRITE (*, "(a,i4,I10)") "row, value", i, mtx(i)
    END DO
  END SUBROUTINE listprint
!> remove repeated values in a list
  SUBROUTINE listremoverepeated(n, l, sorted)
    INTEGER, DIMENSION(*)::l
    INTEGER, DIMENSION(n)::m
    LOGICAL::sorted
    IF (n .LE. 0) RETURN
    IF (.NOT. sorted) CALL listsort(n, l)
    iant = 0
    ik = 0
    DO i = 1, n
      j = l(i)
      IF (j .NE. iant) THEN
        ik = ik+1
        m(ik) = j
        iant = j
      END IF
    END DO
    CALL listcopy(ik, l, m)
    DO ikk = ik+1, n
      l(ikk) = 0
    END DO
    n = ik
  END SUBROUTINE listremoverepeated
!> sets a list to a constant k
  SUBROUTINE listsetconstant(n, l, k)
    INTEGER, DIMENSION(*)::l
    INTEGER, OPTIONAL::k
    IF (n .LE. 0) RETURN
    IF (present(k)) THEN
      l(1:n) = k
    ELSE
      l(1:n) = 0
    END IF
  END SUBROUTINE listsetconstant
!> sorts a list
  SUBROUTINE listsort(n, arr)
    INTEGER, DIMENSION(*)::arr
    INTEGER, DIMENSION(n)::per
    CALL listsortpermutation(n, arr, per)
    CALL listpermute(n, per, arr, 1)
  END SUBROUTINE listsort
!> returns a sort permutation in a list
  SUBROUTINE listsortpermutation(n, arr, per)
    INTEGER, DIMENSION(*)::arr
    INTEGER, DIMENSION(*)::per
    DO i = 1, n
      per(i) = i
    END DO
    DO i = n/2, 1, -1
      CALL listsd(i, n)
    END DO
    DO i = n, 2, -1
      l = per(i)
      per(i) = per(1)
      per(1) = l
      CALL listsd(1, i-1)
    END DO
  CONTAINS
    SUBROUTINE listsd(l, m)
      INTEGER::m, a
      ia = per(l)
      a = arr(ia)
      jold = l
      j = l+l
      DO
        IF (j .GT. m) EXIT
        IF (j .LT. m) THEN
          IF (arr(per(j)) < arr(per(j+1))) j = j+1
        END IF
        IF (a .GE. arr(per(j))) EXIT
        per(jold) = per(j)
        jold = j
        j = j+j
      END DO
      per(jold) = ia
    END SUBROUTINE listsd
  END SUBROUTINE listsortpermutation
!> swaps two lists
  SUBROUTINE listswap(n, i, j)
    INTEGER, DIMENSION(*)::i, j
    DO k = 1, n
      l = i(k)
      i(k) = j(k)
      j(k) = l
    END DO
  END SUBROUTINE listswap
!> determine a symmetric difference between two lists
  SUBROUTINE listsymmetricdifference(n1, l1t, sorted1, n2, l2t, sorted2, n, l)
    INTEGER::n
    INTEGER, DIMENSION(*)::l1t, l2t
    LOGICAL::sorted1, sorted2
    INTEGER, DIMENSION(:), ALLOCATABLE::la, lb, l
    CALL listdifference(n1, l1t, sorted1, n2, l2t, sorted2, na, la)
    CALL listdifference(n2, l2t, sorted2, n1, l1t, sorted1, nb, lb)
    CALL listunion(na, la, nb, lb, n, l)
  END SUBROUTINE listsymmetricdifference
!> trims a list to size n
  SUBROUTINE listtrim(n, l)
    INTEGER, DIMENSION(:), ALLOCATABLE::l, lt
    IF (n .LE. 0) THEN
      IF (allocated(l)) DEALLOCATE (l)
    ELSE
      IF (allocated(l)) THEN
        nl = size(l)
        ALLOCATE (lt(n))
        CALL listcopy(n, lt, l)
        CALL allocsafe(n, l)
        CALL listcopy(n, l, lt)
        DEALLOCATE (lt)
      ELSE
        CALL allocsafe(n, l)
      END IF
    END IF
  END SUBROUTINE listtrim
!> determines the union of two lists
  SUBROUTINE listunion(n1, l1, n2, l2, n, l)
    INTEGER, DIMENSION(*)::l1, l2
    INTEGER, DIMENSION(:), ALLOCATABLE::l
    CALL allocsafe(n1+n2, l)
    DO i = 1, n1
      l(i) = l1(i)
    END DO
    DO i = 1, n2
      l(i+n1) = l2(i)
    END DO
    n = n1+n2
    CALL listremoverepeated(n, l, .FALSE.)
  END SUBROUTINE listunion
!> provides an edge, face or volume given a list of nodes
  INTEGER FUNCTION manymanyfindfromdeps(nn, ln, inopoly, jnopoly, ipolnod, jpolnod)
    INTEGER, DIMENSION(*)::ln, inopoly, jnopoly, ipolnod, jpolnod
    INTEGER, DIMENSION(nn)::lp
    manymanyfindfromdeps = 0
    ino = ln(1)
    DO ik = inopoly(ino), inopoly(ino+1)-1
      ifa = jnopoly(ik)
      DO jk = ipolnod(ifa), ipolnod(ifa+1)-1
        lno = jpolnod(jk)
        lp(jk+1-ipolnod(ifa)) = lno
      END DO
      IF (nn .EQ. ipolnod(ifa+1)-ipolnod(ifa)) THEN
        IF (listifcoincide(nn, ln, .FALSE., lp, .FALSE.)) THEN
          manymanyfindfromdeps = ifa
          EXIT
        END IF
      END IF
    END DO
  END FUNCTION manymanyfindfromdeps
!> dual local graph
!> neighbors, etc
!> using a conventional nomenclature
  SUBROUTINE manymanylocaldual(iel, elni, elno, noil, noel, nl, lista)
    INTEGER, DIMENSION(*)::elni, elno, noil, noel
    INTEGER, DIMENSION(:), ALLOCATABLE::lista
    nl = 0
    DO j = elni(iel), elni(iel+1)-1
      k = elno(j)
      DO l = noil(k), noil(k+1)-1
        m = noel(l)
        IF (m .NE. iel) THEN
          nl = nl+1
        END IF
      END DO
    END DO
    CALL allocsafe(nl, lista)
    nl = 0
    DO j = elni(iel), elni(iel+1)-1
      k = elno(j)
      DO l = noil(k), noil(k+1)-1
        m = noel(l)
        IF (m .NE. iel) THEN
          nl = nl+1
          lista(nl) = m
        END IF
      END DO
    END DO
    CALL listremoverepeated(nl, lista, .FALSE.)
  END SUBROUTINE manymanylocaldual
!> allocation of destinations given the pointers
  SUBROUTINE manymanycreaterowpointersandallocate(ma, ia, ja)
    INTEGER, DIMENSION(*)::ia
    INTEGER, DIMENSION(:), ALLOCATABLE::ja
    IF (ma .GT. 0) THEN
      IF (ia(ma+1) .LE. 0) THEN
        CALL manymanycreaterowpointers(ma, ia)
      END IF
      itemp = ia(ma+1)-1
      CALL allocsafe(itemp, ja)
    END IF
  END SUBROUTINE manymanycreaterowpointersandallocate
!> allocation of destinations given the pointers for the values
  SUBROUTINE manymanycreaterowpointersandallocatearray(ma, ia, va)
    INTEGER, DIMENSION(*)::ia
    REAL(8), DIMENSION(:), ALLOCATABLE::va
    IF (ma .GT. 0) THEN
      IF (ia(ma+1) .LE. 0) THEN
        CALL manymanycreaterowpointers(ma, ia)
      END IF
      itemp = ia(ma+1)-1
      CALL allocsafe(itemp, va)
    END IF
  END SUBROUTINE manymanycreaterowpointersandallocatearray
!> many to many csr from dense matrix
  SUBROUTINE manymanyfromdense(na, ia, ja, va, nr, nc, a)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER, DIMENSION(:), ALLOCATABLE::ia, ja
    REAL(8), DIMENSION(:), ALLOCATABLE::va
    REAL(8), DIMENSION(nr, *)::a
    na = nr
    small = tiny(1.0d00)
    next = 1
    CALL allocsafe(na+1, ia)
    ia(1) = 1
    DO ir = 1, nr
      DO ic = 1, nc
        IF (abs(a(ir, ic)) .GT. small) next = next+1
        ia(ir+1) = next
      END DO
    END DO
    next = 1
    CALL allocsafe(ia(na+1)-ia(1), ja)
    CALL allocsafe(ia(na+1)-ia(1), va)
    DO ir = 1, nr
      DO ic = 1, nc
        IF (abs(a(ir, ic)) .GT. small) THEN
          ja(next) = ic
          va(next) = a(ir, ic)
          next = next+1
        END IF
      END DO
    END DO
  END SUBROUTINE manymanyfromdense
!> many to many csr to dense
  SUBROUTINE manymanytodense(na, ia, ja, va, nr, nc, a)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER, DIMENSION(*)::ia, ja
    REAL(8), DIMENSION(*)::va
    REAL(8), DIMENSION(:, :), ALLOCATABLE::a
    nr = na
    nc = manymanymaximumcolumn(na, ia, ja)
    CALL allocsafe(nr, nc, a)
    DO i = 1, na
      DO j = ia(i), ia(i+1)-1
        a(i, ja(j)) = va(j)
      END DO
    END DO
  END SUBROUTINE manymanytodense
!> many to many addition
!> calculates ic for a sparse sum
!> assumes nra=nrb but nca
!> can be different than ncb
  SUBROUTINE manymanyadditionpart1(nra, ia, ja, nrb, ib, jb, ic)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::ra, ca, cb
    INTEGER, DIMENSION(*)::ia, ib, ja, jb
    INTEGER, DIMENSION(:), ALLOCATABLE::ic, iw
    IF (nra .NE. nrb) THEN
      STOP "nra should equal nrb in manymanyaddition"
    END IF
    nca = manymanymaximumcolumn(nra, ia, ja)
    ncb = manymanymaximumcolumn(nrb, ib, jb)
    CALL allocsafe(nra+1, ic)
    CALL allocsafe(max(nca, ncb), iw)
    len = 0
    ic(1) = 1
    DO ra = 1, nra
      DO ka = ia(ra), ia(ra+1)-1
        len = len+1
        ca = ja(ka)
        iw(ca) = len
      END DO
      DO kb = ib(ra), ib(ra+1)-1
        cb = jb(kb)
        IF (iw(cb) .EQ. 0) THEN
          len = len+1
          iw(cb) = len
        END IF
      END DO
      DO ka = ia(ra), ia(ra+1)-1
        iw(ja(ka)) = 0
      END DO
      DO kb = ib(ra), ib(ra+1)-1
        iw(jb(kb)) = 0
      END DO
      ic(ra+1) = len+1
    END DO
    CALL allocsafe(0, iw)
  END SUBROUTINE manymanyadditionpart1
!> many to many addition
!> after ic, calculates jc for a sparse sum
  SUBROUTINE manymanyadditionpart2(nra, ia, ja, ib, jb, ic, jc)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::ra, ca, cb
    INTEGER, DIMENSION(*)::ia, ib, ic, ja, jb
    INTEGER, DIMENSION(:), ALLOCATABLE::jc, iw
    itemp = ic(nra+1)-1
    CALL allocsafe(itemp, jc)
    nca = manymanymaximumcolumn(nra, ia, ja)
    ncb = manymanymaximumcolumn(nra, ib, jb)
    CALL allocsafe(max(nca, ncb), iw)
    len = 0
    DO ra = 1, nra
      DO ka = ia(ra), ia(ra+1)-1
        ca = ja(ka)
        len = len+1
        jc(len) = ca
        iw(ca) = len
      END DO
      DO kb = ib(ra), ib(ra+1)-1
        cb = jb(kb)
        IF (iw(cb) .EQ. 0) THEN
          len = len+1
          jc(len) = cb
          iw(cb) = len
        END IF
      END DO
      DO ka = ia(ra), ia(ra+1)-1
        iw(ja(ka)) = 0
      END DO
      DO kb = ib(ra), ib(ra+1)-1
        iw(jb(kb)) = 0
      END DO
    END DO
    CALL allocsafe(0, iw)
  END SUBROUTINE manymanyadditionpart2
!> after ic and jc, calculates vc for a sparse sum
  SUBROUTINE manymanyadditionpart3(nra, alpha_a, ia, ja, va, alpha_b, ib, jb, vb, ic, jc, vc)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::ra, ca, cb
    INTEGER, DIMENSION(*)::ia, ib, ic, ja, jb, jc
    REAL(8), DIMENSION(*)::va, vb
    INTEGER, DIMENSION(:), ALLOCATABLE::iw
    REAL(8), DIMENSION(:), ALLOCATABLE::vc
    itemp = ic(nra+1)-1
    CALL allocsafe(itemp, vc)
    ncc = manymanymaximumcolumn(nra, ic, jc)
    CALL allocsafe(ncc, iw)
    len = 0
    DO ra = 1, nra
      DO ka = ia(ra), ia(ra+1)-1
        len = len+1
        ca = ja(ka)
        vc(len) = alpha_a*va(ka)
        iw(ca) = len
      END DO
      DO kb = ib(ra), ib(ra+1)-1
        cb = jb(kb)
        IF (iw(cb) .EQ. 0) THEN
          len = len+1
          vc(len) = alpha_b*vb(kb)
          iw(cb) = len
        ELSE
          vc(iw(cb)) = vc(iw(cb))+alpha_b*vb(iw(cb))
        END IF
      END DO
      DO ka = ia(ra), ia(ra+1)-1
        iw(ja(ka)) = 0
      END DO
      DO kb = ib(ra), ib(ra+1)-1
        iw(jb(kb)) = 0
      END DO
    END DO
    CALL allocsafe(0, iw)
  END SUBROUTINE manymanyadditionpart3
!> complete sparse sum
!> performs the three operations in sequence
  SUBROUTINE manymanyaddition(na, alpha_a, ia, ja, va, alpha_b, nb, ib, jb, vb, ic, jc, vc)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER, DIMENSION(*)::ia, ib, ja, jb
    REAL(8), DIMENSION(*)::va, vb
    INTEGER, DIMENSION(:), ALLOCATABLE::ic, jc
    REAL(8), DIMENSION(:), ALLOCATABLE::vc
    CALL manymanyadditionpart1(na, ia, ja, nb, ib, jb, ic)
    CALL manymanyadditionpart2(na, ia, ja, ib, jb, ic, jc)
    CALL manymanyadditionpart3(na, alpha_a, ia, ja, va, alpha_b, ib, jb, vb, ic, jc, vc)
  END SUBROUTINE manymanyaddition
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
    ALLOCATE (temp_perm(nel))
    curr_size = 1
! Iterative merge sort
    DO WHILE (curr_size <= nel-1)
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(left_start, mid, right_end)
!$OMP DO SCHEDULE(dynamic)
      DO left_start = 1, nel, 2*curr_size
        mid = min(left_start+curr_size-1, nel)
        right_end = min(left_start+2*curr_size-1, nel)
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
      curr_size = 2*curr_size
    END DO
! Deallocate temporary array
    DEALLOCATE (temp_perm)
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
    j = mid+1
    k = left
! Merge two sorted halves
    DO WHILE (i <= mid .AND. j <= right)
      is1 = elni(perm(i))
      ie1 = elni(perm(i)+1)-1
      inel1 = ie1-is1+1
      is2 = elni(perm(j))
      ie2 = elni(perm(j)+1)-1
      inel2 = ie2-is2+1
      icp = listscomparelexicographically(inel1, elno(is1:ie1), inel2, elno(is2:ie2))
      IF (icp <= 0) THEN
        temp_perm(k) = perm(i)
        i = i+1
      ELSE
        temp_perm(k) = perm(j)
        j = j+1
      END IF
      k = k+1
    END DO
! Copy any remaining elements
    IF (i <= mid) THEN
      temp_perm(k:right) = perm(i:mid)
    ELSEIF (j <= right) THEN
      temp_perm(k:right) = perm(j:right)
    END IF
  END SUBROUTINE manymanymergewithpermutation
!> give a unique identifier
  SUBROUTINE manymanygiveuniqueidentifier(nel, elni, elno, rank, perm)
    IMPLICIT NONE
    INTEGER, DIMENSION(*)::elni, elno, rank, perm
    INTEGER, INTENT(in) :: nel          ! Size of the input list
    INTEGER :: i, current_rank, prev_value, nno1, nno2, ist1, ist2, iend1, iend2
! Initialize the variables
    current_rank = 1
    prev_value = rank(1)
    perm(rank(1)) = current_rank
! Loop through the rank array and assign unique indices
    DO i = 2, nel
      ist1 = elni(rank(i))
      iend1 = elni(rank(i)+1)-1
      nno1 = iend1-ist1+1
      ist2 = elni(prev_value)
      iend2 = elni(prev_value+1)-1
      nno2 = iend2-ist2+1
      IF (listscomparelexicographically(nno1, elno(ist1:iend1), nno2, elno(ist2:iend2)) .NE. 0) THEN
        current_rank = current_rank+1
        prev_value = rank(i)
      END IF
      perm(rank(i)) = current_rank
    END DO
  END SUBROUTINE manymanygiveuniqueidentifier
!> multiplies a sparse matrix (csr) by a vector
!> w=va.v
  SUBROUTINE manymanymatrixtimesvector(nra, ia, ja, va, v, w)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::ra
    INTEGER, DIMENSION(*)::ia, ja
    REAL(8), DIMENSION(*)::va
    REAL(8), DIMENSION(*)::v, w
!$omp parallel
!$omp do schedule(auto)
    DO ra = 1, nra
      t = 0.0d00
      DO ka = ia(ra), ia(ra+1)-1
        t = t+va(ka)*v(ja(ka))
      END DO
      w(ra) = t
    END DO
!$omp end do
!$omp end parallel
  END SUBROUTINE manymanymatrixtimesvector
!> multiplies a transposed of a sparse matrix
!> by a vector
!> w=va^T.v
  SUBROUTINE manymanymatrixtransposedtimesvector(nra, ia, ja, va, v, w)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::ra
    INTEGER, DIMENSION(*)::ia, ja
    REAL(8), DIMENSION(*)::va, v, w
    CALL vectsetconst(manymanymaximumcolumn(nra, ia, ja), w)
!$omp parallel do
    DO ra = 1, nra
      DO ka = ia(ra), ia(ra+1)-1
        w(ja(ka)) = w(ja(ka))+v(ra)*va(ka)
      END DO
    END DO
!$omp end parallel do
  END SUBROUTINE manymanymatrixtransposedtimesvector
!> marks all «rows/columns» reached by row i
!> depth first search for a given row i
!> dependence matrix
  SUBROUTINE manymanydepthfirstsearch(nra, ia, ja, i, nm, mk)
    INTEGER::ra, ca
    INTEGER, DIMENSION(:), ALLOCATABLE::stack
    INTEGER, DIMENSION(*)::mk
    INTEGER, DIMENSION(*)::ia, ja
    ra = i
    CALL allocsafe(ia(nra+1)-1, stack)
    nstack = 1
    nm = 0
    stack(nstack) = ra
    DO
      IF (nstack .EQ. 0) EXIT
      ra = stack(nstack)
      nstack = nstack-1
      IF (mk(ra) .EQ. 0) THEN
        nm = nm+1
        mk(ra) = 1
        DO ka = ia(ra), ia(ra+1)-1
          ca = ja(ka)
          nstack = nstack+1
          stack(nstack) = ca
        END DO
      END IF
    END DO
    CALL allocsafe(0, stack)
  END SUBROUTINE manymanydepthfirstsearch
!> l.x=b symbolic lower triangular solve part 1
  SUBROUTINE manymanyltimesxequalsbpart1(ncl, ilt, jlt, ixt, jxt, ncb, ibt, jbt)
    INTEGER::cl, rb, cb, nm
    INTEGER, DIMENSION(*)::ibt, jbt
    INTEGER, DIMENSION(:), ALLOCATABLE::ilt, jlt, ixt, jxt, mk
    ncx = ncl
    CALL allocsafe(ncl, mk)
    CALL allocsafe(ncx+1, ixt)
    DO cb = 1, ncb
      mk = 0
      nm = 0
      DO kb = ibt(cb), ibt(cb+1)-1
        rb = jbt(kb)
        cl = rb
        CALL manymanydepthfirstsearch(ncl, ilt, jlt, cl, nm, mk)
      END DO
      ixt(cb) = nm+1
    END DO
    CALL manymanycreaterowpointersandallocate(ncx, ixt, jxt)
    DO cb = 1, ncb
      mk = 0
      nm = 0
      DO kb = ibt(cb), ibt(cb+1)-1
        rb = jbt(kb)
        cl = rb
        CALL manymanydepthfirstsearch(ncl, ilt, jlt, cl, nm, mk)
      END DO
      l = 0
      DO cl = 1, ncl
        IF (mk(cl) .EQ. 1) THEN
          l = l+1
          jxt(ixt(cb)-1+l) = cl
        END IF
      END DO
      jxt(ixt(cb+1)-1) = cb+ncl
    END DO
    CALL allocsafe(0, mk)
  END SUBROUTINE manymanyltimesxequalsbpart1
!> l.x=b symbolic lower triangular solve part 2
  SUBROUTINE manymanyltimesxequalsbpart2(nrl, ilt, jlt, vlt, ixt, jxt, vxt, ncb, ibt, jbt, vbt)
    INTEGER::rl, cl
    INTEGER, DIMENSION(*)::ilt, jlt
    REAL(8), DIMENSION(*)::vlt
    INTEGER::rx, cx
    INTEGER, DIMENSION(*)::ixt, jxt
    REAL(8), DIMENSION(:), ALLOCATABLE::vxt
    INTEGER::rb, cb
    INTEGER, DIMENSION(*)::ibt, jbt
    REAL(8), DIMENSION(*)::vbt
    INTEGER, DIMENSION(:), ALLOCATABLE::place
    nrx = nrl
    ncx = ncb
    nrb = nrl
    CALL allocsafe(nrx, place)
    CALL allocsafe(ixt(ncx+1)-1, vxt)
    DO cb = 1, ncb
      cx = cb
      DO k = ixt(cx), ixt(cx+1)-2
        rx = jxt(k)
        place(rx) = k
      END DO
      DO k = ibt(cb), ibt(cb+1)-1
        rb = jbt(k)
        l = place(rb)
        vxt(l) = vbt(k)/vlt(ilt(rb))
      END DO
      cx = cb
      DO kx = ixt(cx), ixt(cx+1)-2
        rx = jxt(kx)
        cl = rx
        DO kl = ilt(cl), ilt(cl+1)-1
          rl = jlt(kl)
          IF (rx .GE. rl) CYCLE
          l = place(rl)
          vxt(l) = vxt(l)-vxt(kx)*vlt(kl)/vlt(ilt(rl))
        END DO
      END DO
      vxt(ixt(cx+1)-1) = -1.0d00 !*** sets to -1
    END DO
    CALL allocsafe(0, place)
  END SUBROUTINE manymanyltimesxequalsbpart2
!> topological ordering of rows/columns in ia,ja
!> with a being rectangular and perhaps including
!> self edges
  SUBROUTINE manymanytoporderingrectangle(nra, mca, ia, ja, acyclic, increasingorder)
    INTEGER::ra, ca
    INTEGER, DIMENSION(*)::ia, ja
    INTEGER, DIMENSION(:), ALLOCATABLE::ind, l, increasingorder
    LOGICAL::acyclic
    CALL allocsafe(nra, ind)
    CALL allocsafe(nra, l)
    m = 1
    DO ra = 1, nra
      DO ka = ia(ra), ia(ra+1)-1
        IF (ja(ka) .NE. ra .AND. ja(ka) .LE. nra) ind(ja(ka)) = ind(ja(ka))+1
      END DO
    END DO
    ik = 0
    DO ra = 1, nra
      IF (ind(ra) .EQ. 0) THEN
        ik = ik+1
        l(nra+1-ik) = ra
      END IF
    END DO
    mk = nra
    CALL allocsafe(mca, increasingorder)
    DO WHILE (ik .NE. 0)
      ra = l(mk)
      mk = mk-1
      ik = ik-1
      increasingorder(m) = ra
      m = m+1
      DO j = ia(ra), ia(ra+1)-1
        ca = ja(j)
        IF (ca .NE. ra) THEN
          IF (ca .LE. nra) THEN
            ind(ca) = ind(ca)-1
            IF (ind(ca) .EQ. 0) THEN
              ik = ik+1
              l(mk+1-ik) = ca
            END IF
          END IF
        END IF
      END DO
    END DO
    IF (m .EQ. nra+1) THEN
      acyclic = .TRUE.
    ELSE
      acyclic = .FALSE.
    END IF
    DO ra = 1, nra/2
      i1 = increasingorder(nra+1-ra)
      increasingorder(nra+1-ra) = increasingorder(ra)
      increasingorder(ra) = i1
    END DO
    DEALLOCATE (ind, l)
  END SUBROUTINE manymanytoporderingrectangle
!> topological sorting
!> c dieter jungnickel
!> for a square matrix
  SUBROUTINE manymanytopordering(na, ia, ja, acyclic, top)
    INTEGER, DIMENSION(*)::ia, ja
    INTEGER, DIMENSION(:), ALLOCATABLE::top
    LOGICAL::acyclic
    CALL manymanytoporderingrectangle(na, na, ia, ja, acyclic, top)
  END SUBROUTINE manymanytopordering
!> sparse multiplication part 1
!> calculation of ic
  SUBROUTINE manymanymultiplicationpart1(na, ia, ja, nb, ib, jb, nc, ic)
    INTEGER::ra, ca, cb
    INTEGER, DIMENSION(*)::ia, ja, ib, jb
    INTEGER, DIMENSION(:), ALLOCATABLE::iw, ic
    INTEGER :: ncb
    ncb = manymanymaximumcolumn(nb, ib, jb)
    CALL allocsafe(ncb, iw)
    nc = na
    CALL allocsafe(nc+1, ic)
!!$OMP PARALLEL DO PRIVATE(ra,ldg,llast,ka,ca,kb,cb,k)
    DO ra = 1, na
      ldg = 0
      llast = -1
      DO ka = ia(ra), ia(ra+1)-1
        ca = ja(ka)
        DO kb = ib(ca), ib(ca+1)-1
          cb = jb(kb)
          IF (iw(cb) .EQ. 0) THEN
            ldg = ldg+1
            iw(cb) = llast
            llast = cb
          END IF
        END DO
      END DO
      ic(ra) = ldg
      DO k = 1, ldg
        ka = iw(llast)
        iw(llast) = 0
        llast = ka
      END DO
    END DO
!!$OMP END PARALLEL DO
    CALL manymanycreaterowpointers(nc, ic)
    IF (allocated(iw)) DEALLOCATE (iw)
  END SUBROUTINE manymanymultiplicationpart1
!> sparse multiplication part 2
!> calculation of jc
  SUBROUTINE manymanymultiplicationpart2(na, ia, ja, nb, ib, jb, nc, ic, jc)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::ira, ica, len
    INTEGER, DIMENSION(*)::ia, ja, ib, jb, ic
    INTEGER, DIMENSION(:), ALLOCATABLE::jc, iw
    itemp = ic(nc+1)-1
    CALL allocsafe(itemp, jc)
    ncb = manymanymaximumcolumn(nb, ib, jb)
    CALL allocsafe(ncb, iw)
    len = 0
!$OMP PARALLEL DO PRIVATE(ira, ini, ifi, iza, ica, jni, jfi, izb, icb, ip, izc) REDUCTION(+:len)
    DO ira = 1, na
      ini = ia(ira)
      ifi = ia(ira+1)-1
      DO iza = ini, ifi
        ica = ja(iza)
        jni = ib(ica)
        jfi = ib(ica+1)-1
        DO izb = jni, jfi
          icb = jb(izb)
          ip = iw(icb)
          IF (ip .EQ. 0) THEN
            len = len+1
            jc(len) = icb
            iw(icb) = len
          END IF
        END DO
      END DO
      DO izc = ic(ira), len
        iw(jc(izc)) = 0
      END DO
    END DO
!$OMP END PARALLEL DO
    CALL allocsafe(0, iw)
  END SUBROUTINE manymanymultiplicationpart2
!> sparse multiplication part 3
!> calculation of vc
  SUBROUTINE manymanymultiplicationpart3(na, ia, ja, va, nb, ib, jb, vb, nc, ic, jc, vc)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::ira, ica, len
    REAL(8), DIMENSION(*)::va, vb
    REAL(8), DIMENSION(:), ALLOCATABLE::vc
    INTEGER, DIMENSION(*)::ia, ja, ib, jb, ic, jc
    INTEGER, DIMENSION(:), ALLOCATABLE::iw
    INTEGER::ini, ifi, iza, jni, jfi, izb, icb, ip, izc
    REAL(8)::scal
    itemp = ic(nc+1)-1
    CALL allocsafe(itemp, vc)
    ncb = manymanymaximumcolumn(nb, ib, jb)
    CALL allocsafe(ncb, iw)
    len = 0
!$OMP PARALLEL PRIVATE(ira, ini, ifi, iza, scal, ica, jni, jfi, izb, icb, ip, izc) REDUCTION(+:len)
    DO ira = 1, na
      ini = ia(ira)
      ifi = ia(ira+1)-1
      DO iza = ini, ifi
        scal = va(iza)
        ica = ja(iza)
        jni = ib(ica)
        jfi = ib(ica+1)-1
        DO izb = jni, jfi
          icb = jb(izb)
!$OMP CRITICAL
          ip = iw(icb)
          IF (ip .EQ. 0) THEN
            len = len+1
            vc(len) = scal*vb(izb)
            iw(icb) = len
          ELSE
            vc(ip) = vc(ip)+scal*vb(izb)
          END IF
!$OMP END CRITICAL
        END DO
      END DO
      DO izc = ic(ira), len
        iw(jc(izc)) = 0
      END DO
    END DO
!$OMP END PARALLEL
    CALL allocsafe(0, iw)
  END SUBROUTINE manymanymultiplicationpart3
!> complete sparse multiplication
  SUBROUTINE manymanymultiplication(na, ia, ja, nb, ib, jb, nc, ic, jc, va, vb, vc)
    INTEGER, DIMENSION(*)::ia, ja, ib, jb
    INTEGER, DIMENSION(:), ALLOCATABLE::ic, jc
    REAL(8), DIMENSION(*)::va, vb
    REAL(8), DIMENSION(:), ALLOCATABLE::vc
    CALL manymanymultiplicationpart1(na, ia, ja, nb, ib, jb, nc, ic)
    CALL manymanymultiplicationpart2(na, ia, ja, nb, ib, jb, nc, ic, jc)
    CALL manymanymultiplicationpart3(na, ia, ja, va, nb, ib, jb, vb, nc, ic, jc, vc)
  END SUBROUTINE manymanymultiplication
!> manymanymultiplication only indices
  SUBROUTINE manymanymultiplicationsymbolic(na, ia, ja, nb, ib, jb, nc, ic, jc)
    INTEGER, DIMENSION(*)::ia, ja, ib, jb
    INTEGER, DIMENSION(:), ALLOCATABLE::ic, jc
    CALL manymanymultiplicationpart1(na, ia, ja, nb, ib, jb, nc, ic)
    CALL manymanymultiplicationpart2(na, ia, ja, nb, ib, jb, nc, ic, jc)
  END SUBROUTINE manymanymultiplicationsymbolic
!> convert a general sparse matrix to a symmetric form
  SUBROUTINE manymanyobtainsymm(na, ia, ja, va, ias, jas, vas)
    IMPLICIT REAL(a-h, o-z)
    REAL(8), DIMENSION(*)::va
    INTEGER, DIMENSION(*)::ia, ja
    REAL(8), DIMENSION(:), ALLOCATABLE::vas
    INTEGER, DIMENSION(:), ALLOCATABLE::ias, jas
    CALL allocsafe(na+1, ias)
    DO i = 1, na
      k = 0
      DO j = ia(i), ia(i+1)-1
        IF (ja(j) .GE. i) THEN
          k = k+1
        END IF
      END DO
      ias(i) = k
    END DO
    CALL manymanycreaterowpointers(na, ias)
    CALL allocsafe(ias(na+1)-1, jas)
    CALL allocsafe(ias(na+1)-1, vas)
    DO i = 1, na
      k = 0
      DO j = ia(i), ia(i+1)-1
        IF (ja(j) .GE. i) THEN
          k = k+1
          jas(ias(i)-1+k) = ja(j)
          vas(ias(i)-1+k) = va(j)
        END IF
      END DO
    END DO
  END SUBROUTINE manymanyobtainsymm
!> obtain a global clique addressing for all elements with
!> ielrows and ielcols
  SUBROUTINE manymanycliqueaddress(nel, ielrow, ielcol, iclique)
    INTEGER::nel
    INTEGER, DIMENSION(*)::ielrow, ielcol
    INTEGER, DIMENSION(:), ALLOCATABLE::iclique
    CALL allocsafe(nel+1, iclique)
    DO iel = 1, nel
      iclique(iel) = (ielrow(iel+1)-ielrow(iel))*(ielcol(iel+1)-ielcol(iel))
    END DO
    CALL manymanycreaterowpointers(nel, iclique)
  END SUBROUTINE manymanycliqueaddress
!> locates a component of a clique in a global clique matrix
  INTEGER FUNCTION manymanyindexincliquematrix(iclique, ieldof, iel, iedof, jedof)
    INTEGER, DIMENSION(*)::iclique, ieldof
    manymanyindexincliquematrix = iclique(iel)-1+matrixaddressing(ieldof(iel+1)-ieldof(iel), iedof, jedof)
  END FUNCTION manymanyindexincliquematrix
!> performs a symbolic assembling
!> given ieldof,jeldof
!> returns iclique, jclique, ndof and idofdof/jdofdof
!> and iclique, jclique
  SUBROUTINE manymanyfromcliquespart1(nel, ieldof, jeldof, iclique, jclique, ndof, idofdof, jdofdof)
    INTEGER, DIMENSION(:)::ieldof, jeldof
    INTEGER, DIMENSION(:), ALLOCATABLE::iw, idofel, jdofel, ijle, idofdof, jdofdof, jclique, iclique
!--------------------------------------------
!*** check if both ndof and nel are positive
!--------------------------------------------
    IF (ndof .LE. 0) THEN
      WRITE (*, '(a)') "symbolic assembling only works with a-priori established number of dofs"
      STOP
    END IF
    IF (nel .LE. 0) THEN
      WRITE (*, '(a)') "symbolic assembling only works with more than 0 elements"
      STOP
    END IF
!---------------------------------------------------------------------
!*** matrixtransposeose ieldof and jeldof to obtain idofel and jdofel
!---------------------------------------------------------------------
    CALL manymanymatrixtransposeoseindex(nel, ndof, ieldof, jeldof, ijle, idofel, jdofel)
!-------------------------------------------------------------------
!*** sets address pointers for clique position in final destination
!-------------------------------------------------------------------
    CALL manymanycliqueaddress(nel, ieldof, ieldof, iclique)
!------------------------------------------------------------------------
!*** multiplies idofel/jdofel by ieldof/jeldof to obtain idofdof/jdofdof
!------------------------------------------------------------------------
    CALL manymanymultiplicationpart1(ndof, idofel, jdofel, nel, ieldof, jeldof, igash, idofdof)
    CALL manymanycreaterowpointersandallocate(ndof, idofdof, jdofdof)
    CALL allocsafe(manymanymaximumcolumn(nel, ieldof, jeldof), iw)
!------------------------------------------------
!*** determines final positions in sparse matrix
!------------------------------------------------
    DO ist = 1, 2
      nnz = 0
      llpmax = 0
      DO idof = 1, ndof
        DO kdofe = idofel(idof), idofel(idof+1)-1! elements from dofs
          iel = jdofel(kdofe)! element in counter kdofe for dof idof
          iedof = ijle(kdofe)! local dof in element corresponding to row
          jedof = 0 ! other element dof counter
          DO kedof = ieldof(iel), ieldof(iel+1)-1 ! again, element dofs, column
            jedof = jedof+1 ! local dof in element corresponding to column
            idofdofb = jeldof(kedof) ! global dof for column
            inz = iw(idofdofb) ! if this was not yet accounted for... this is row
            IF (inz .EQ. 0) THEN
              nnz = nnz+1
              jdofdof(nnz) = idofdofb
              iw(idofdofb) = nnz
              llp = manymanyindexincliquematrix(iclique, ieldof, iel, iedof, jedof)
              IF (ist .EQ. 1) llpmax = max(llpmax, llp)
              IF (ist .EQ. 2) jclique(llp) = nnz
            ELSE
              llp = manymanyindexincliquematrix(iclique, ieldof, iel, iedof, jedof)
              IF (ist .EQ. 1) llpmax = max(llpmax, llp)
              IF (ist .EQ. 2) jclique(llp) = inz
            END IF
          END DO
        END DO
        DO izc = idofdof(idof), nnz
          iw(jdofdof(izc)) = 0
        END DO
      END DO
      IF (ist .EQ. 1) THEN
        CALL allocsafe(llpmax, jclique)
        iw = 0
      END IF
    END DO
!-------------------------------
!*** deallocates working arrays
!-------------------------------
    CALL allocsafe(0, iw)
    CALL allocsafe(0, idofel)
    CALL allocsafe(0, jdofel)
  END SUBROUTINE manymanyfromcliquespart1
!> from cliques part 2 (open space for matrix)
  SUBROUTINE manymanyfromcliquespart2(ndof, idofdof, va)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER, DIMENSION(*)::idofdof
    REAL(8), DIMENSION(:), ALLOCATABLE::va
    CALL allocsafe(idofdof(ndof+1)-1, va)
  END SUBROUTINE manymanyfromcliquespart2
!> from cliques part 3 (numerical assembling)
  SUBROUTINE manymanyfromcliquespart3(ndof, nel, ieldof, jeldof, iclique, jclique, cliquematrix, idofdof, va)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER, DIMENSION(*)::ieldof, jeldof, iclique, jclique, idofdof
    REAL(8), DIMENSION(*)::va, cliquematrix
    CALL vectsetconst(idofdof(ndof+1)-1, va, 0.0d00)
    DO iel = 1, nel
      nedof = ieldof(iel+1)-ieldof(iel)
      DO jedof = 1, nedof
        jtemp = ieldof(iel)-1+jedof
        jdof = jeldof(jtemp)
        DO iedof = 1, nedof
          itemp = ieldof(iel)-1+iedof
          idof = jeldof(itemp)
          ktemp = matrixaddressing(nedof, iedof, jedof)
          ltemp = manymanyindexincliquematrix(iclique, ieldof, iel, iedof, jedof)
          imatrix = jclique(ltemp)
          va(imatrix) = va(imatrix)+cliquematrix(ltemp)
        END DO
      END DO
    END DO
  END SUBROUTINE manymanyfromcliquespart3
!> mkl linear sparse solution
  SUBROUTINE manymanymklsolve(refresh, symm, nrhs, na, ia, ja, va, x, b)
    USE omp_lib
    USE mkl_pardiso
    LOGICAL::refresh
    LOGICAL::symm
!----------------------------------
!*** internal solver memory pointer
!*** not to be touched
!----------------------------------
    TYPE(mkl_pardiso_handle), DIMENSION(64), SAVE::pt
!-----------------------
!*** all other variables
!-----------------------
    INTEGER maxfct, mnum, mtype, phase, nrhs, error, msglvl
    INTEGER error1
    INTEGER i
    INTEGER, DIMENSION(:), ALLOCATABLE::idum
    INTEGER, DIMENSION(64)::iparm
!----------
!*** arrays
!----------
    INTEGER, DIMENSION(*)::ia, ja
    REAL(8), DIMENSION(*)::va, b, x
    INTEGER, DIMENSION(:), ALLOCATABLE::ias, jas
    REAL(8), DIMENSION(:), ALLOCATABLE::vas
!--------------------------------------
!*** if it is symmetric, then contracts
!*** indices
!--------------------------------------
    CALL allocsafe(na, idum)
! phase: 11-> analysis
!        22-> factorization
!        33-> refinement
! nrhs: number or rhs
    maxfct = 1
    mnum = 1
    iparm = 0
    error = 0
    error1 = 0
    msglvl = 0
    mtype = 1 ! structurally symmetric
!-----------------------------
!*** stage 1 reset and refresh
!-----------------------------
    IF (refresh) THEN
      phase = -1 ! release internal memory
      idum(1) = 0
      CALL pardiso(pt, maxfct, mnum, mtype, phase, na, va, ia, ja, idum, nrhs, iparm, msglvl, b, x, error1)
      IF (error1 .NE. 0) THEN
        WRITE (*, *) 'the following error on release stage was detected: ', error1
        STOP
      END IF
      phase = 11
      DO i = 1, 64
        pt(i)%dummy = 0
      END DO
      CALL pardisoinit(pt, mtype, iparm)
      CALL pardiso(pt, maxfct, mnum, mtype, phase, na, va, ia, ja, &
                   idum, nrhs, iparm, msglvl, b, x, error)
      IF (error .NE. 0) THEN
        WRITE (*, *) 'the following error was detected: ', error
        WRITE (*, *) 'at phase : ', phase
        STOP
      END IF
    END IF
!-------------------------
!*** stage 2 factorization
!-------------------------
    phase = 22
    CALL pardiso(pt, maxfct, mnum, mtype, phase, na, va, ia, ja, &
                 idum, nrhs, iparm, msglvl, b, x, error)
    IF (error .NE. 0) THEN
      WRITE (*, *) 'the following error was detected: ', error
      WRITE (*, *) 'at phase : ', phase
      STOP
    END IF
!------------------------------------------------------
!*** stage 3 back substitution and iterative refinement
!------------------------------------------------------
    phase = 33
    CALL pardiso(pt, maxfct, mnum, mtype, phase, na, va, ia, ja, &
                 idum, nrhs, iparm, msglvl, b, x, error)
    IF (error .NE. 0) THEN
      WRITE (*, *) 'the following error was detected: ', error
      WRITE (*, *) 'at phase : ', phase
      STOP
    END IF
    CALL allocsafe(0, ias)
    CALL allocsafe(0, jas)
    CALL allocsafe(0, vas)
    CALL allocsafe(0, idum)
  END SUBROUTINE manymanymklsolve
!> determines smallest
!> eigenvalue
!> and corresponding eigenvector
!> of a sparse matrix
  SUBROUTINE manymanymkeigen(symm, na, ia, ja, va, eig, eigv)
    IMPLICIT REAL(8) (a-h, o-z)
    LOGICAL::symm
    REAL(8), DIMENSION(*)::va, eigv
    REAL(8), DIMENSION(:), ALLOCATABLE::sol
    INTEGER, DIMENSION(*)::ia, ja
    INTEGER, PARAMETER::miter = 20
    CALL allocsafe(na, sol)
!-------------------
!*** initial vector
!-------------------
    CALL random_seed(iseed)
    CALL random_number(eigv(1:na))
    CALL vectnormalize(na, eigv)
!-------------
!*** 20 steps
!-------------
    CALL manymanymklsolve(.TRUE., symm, 1, na, ia, ja, va, sol, eigv)
    eigv(1:na) = sol(1:na)
    CALL vectnormalize(na, eigv)
    CALL manymanymatrixtimesvector(na, ia, ja, va, eigv, sol)
    eig = vectdot(na, sol, eigv)
    DO k = 1, miter
      CALL manymanymklsolve(.FALSE., symm, 1, na, ia, ja, va, sol, eigv)
      eigv(1:na) = sol(1:na)
      CALL vectnormalize(na, eigv)
      CALL manymanymatrixtimesvector(na, ia, ja, va, eigv, sol)
      eig = vectdot(na, sol, eigv)
    END DO
    DEALLOCATE (sol)
  END SUBROUTINE manymanymkeigen
!> manymanyidentity
!> identity matrix in csr sparse format
  SUBROUTINE manymanyidentity(na, ia, ja, a)
    INTEGER, DIMENSION(:), ALLOCATABLE::ia, ja
    REAL(8), DIMENSION(:), ALLOCATABLE::a
    IF (na .GT. 0) THEN
      CALL allocsafe(na+1, ia)
      DO i = 1, na
        ia(i) = 1
      END DO
      CALL manymanycreaterowpointersandallocate(na, ia, ja)
      DO i = 1, na
        ja(ia(i)) = i
      END DO
      CALL allocsafe(ia(na+1)-ia(1), a)
      DO i = 1, na
        a(ja(ia(i))) = 1.0d00
      END DO
    END IF
  END SUBROUTINE manymanyidentity
!> obtains the number of active columns for row i (if j==0)
!> or the position of column j in the matrix for row i
  INTEGER FUNCTION manymanyiaddress(ipo, i, j)
    INTEGER, DIMENSION(*)::ipo
    IF (j .EQ. 0) THEN
      manymanyiaddress = ipo(i+1)-ipo(i)
    ELSE
      manymanyiaddress = ipo(i)-1+j
    END IF
  END FUNCTION manymanyiaddress
!> maximum column number
  INTEGER FUNCTION manymanymaximumcolumn(na, ia, ja)
    INTEGER, DIMENSION(*)::ia, ja
    INTEGER:: na
    num2 = 0
    DO ka = 1, ia(na+1)-1
      num2 = max(num2, ja(ka))
    END DO
    manymanymaximumcolumn = num2
  END FUNCTION manymanymaximumcolumn
!> cleans up the relation
!> provides cleaned ja
  SUBROUTINE manymanycleansupsymbol(nra, ia, ja)
    INTEGER, DIMENSION(:)::ia, ja
    INTEGER, DIMENSION(:), ALLOCATABLE::icolsp
    nca = manymanymaximumcolumn(nra, ia, ja)
    CALL allocsafe(nca, icolsp)
    il = 0
    DO i = 1, nra
      ns = ia(i)
      nf = ia(i+1)-1
      ia(i) = 0
      DO j = ns, nf
        IF (ja(j) .GT. 0) icolsp(ja(j)) = 0
      END DO
      DO j = ns, nf
        k = ja(j)
        IF (k .GT. 0) THEN
          IF (icolsp(k) .EQ. 0) THEN
            ia(i) = ia(i)+1
            il = il+1
            ja(il) = k
            icolsp(k) = il
          END IF
        END IF
      END DO
    END DO
    IF (nra .LE. 0) RETURN
    CALL MANYMANYCREATEROWPOINTERS(NRA, IA)
    DO i = il+1, ia(nra+1)-1
      ja(i) = 0
    END DO
  END SUBROUTINE manymanycleansupsymbol
!> example
!> list in
!> 5, 3, 1, 2, 4, ?
!> list out
!> 1, 6, 9,10,12,16
!> attention to the dimensions of list:
!> list(n+1)
  SUBROUTINE manymanycreaterowpointers(n, list)
    INTEGER, DIMENSION(*)::list
    IF (n .LE. 0) RETURN
    lol = list(1)
    list(1) = 1
    DO in = 1, n
      newv = list(in)+lol
      in1 = in+1
      lol = list(in1)
      list(in1) = newv
    END DO
  END SUBROUTINE manymanycreaterowpointers
!> puts the stuff back
!> list in
!> 1, 6, 9, 10, 12, 16
!> list out
!> 5, 3, 1,2,4,?
  SUBROUTINE manymanyrevertrowpointers(n, list)
    INTEGER, DIMENSION(*)::list
    IF (n .LE. 0) RETURN
    DO i = n, 1, -1
      list(i+1) = list(i)
    END DO
    list(1) = 1
  END SUBROUTINE manymanyrevertrowpointers
!> sparse matrixtransposeose
!> only indices
!> symbolic
!> jat is ordered on output, which is useful
  SUBROUTINE manymanymatrixtransposeoseindex(nra, nca, ia, ja, ija, iat, jat)
    INTEGER::ra, ca
    INTEGER, DIMENSION(*)::ia, ja
    INTEGER, DIMENSION(:), ALLOCATABLE::iat, jat, ija
    CALL allocsafe(nca+1, iat)
    DO ra = 1, nra
      DO ka = ia(ra), ia(ra+1)-1
        ca = ja(ka)
        iat(ca) = iat(ca)+1
      END DO
    END DO
    CALL manymanycreaterowpointers(nca, iat)
    itemp = iat(nca+1)-1
    CALL allocsafe(itemp, jat)
    CALL allocsafe(itemp, ija)
    DO ra = 1, nra
      ik = 0
      DO ka = ia(ra), ia(ra+1)-1
        ik = ik+1
        ca = ja(ka)
        nextv = iat(ca)
        iat(ca) = nextv+1
        jat(nextv) = ra
        ija(nextv) = ik
      END DO
    END DO
    DO ca = nca, 1, -1
      iat(ca+1) = iat(ca)
    END DO
    iat(1) = 1
  END SUBROUTINE manymanymatrixtransposeoseindex
!> transposes a list
!> given num1 and lis1 calculates the rest
!> lis1: 3 5 1 (given)
!> ipo2: 1 2 2 3 3 4 (output)
!> lis2: 3 1 2 (output)
  SUBROUTINE manymanymatrixtransposeoseindicesfromlist(num1, num2, ipo2, lis1, lis2)
    INTEGER, DIMENSION(*)::lis1
    INTEGER, DIMENSION(:), ALLOCATABLE::ipo2, lis2
    IF (num1 .LE. 0) RETURN
    n = num1
    IF (num2 .EQ. 0) THEN
      num2 = 0
      DO i = 1, n
        num2 = max(num2, lis1(i))
      END DO
    END IF
    IF (num2 .LE. 0 .OR. num1 .LE. 0) RETURN
    IF (allocated(ipo2)) DEALLOCATE (ipo2)
    ALLOCATE (ipo2(num2+1)); CALL listsetconstant(num2+1, ipo2, 0)
    DO i = 1, num1
      j = lis1(i)
      ipo2(j) = ipo2(j)+1
    END DO
    CALL manymanycreaterowpointers(num2, ipo2)
    IF (allocated(LIS2)) DEALLOCATE (LIS2)
    ALLOCATE (lis2(ipo2(num2+1)-1))
    DO i = 1, num1
      j = lis1(i)
      nextv = ipo2(j)
      ipo2(j) = nextv+1
      lis2(nextv) = i
    END DO
    CALL manymanyrevertrowpointers(num2, ipo2)
  END SUBROUTINE manymanymatrixtransposeoseindicesfromlist
!> given ia and ja
!> just calls manymanymatrixtransposeoseindex
  SUBROUTINE manymanymatrixtransposeoseindices(nra, nca, ia, iat, ja, jat)
    INTEGER, DIMENSION(*)::ia, ja
    INTEGER, DIMENSION(:), ALLOCATABLE::iat, jat, ija
    CALL manymanymatrixtransposeoseindex(nra, nca, ia, ja, ija, iat, jat)
  END SUBROUTINE manymanymatrixtransposeoseindices
!> sparse matrixtransposeose
!> with values
  SUBROUTINE manymanymatrixtransposewithvalues(nra, nca, ia, ja, va, iat, jat, vat)
    INTEGER::ra, ca
    INTEGER, DIMENSION(*)::ia, ja
    REAL(8), DIMENSION(*)::va
    INTEGER, DIMENSION(:), ALLOCATABLE::iat, jat
    REAL(8), DIMENSION(:), ALLOCATABLE::vat
    CALL allocsafe(nca+1, iat)
    DO ra = 1, nra
      DO ka = ia(ra), ia(ra+1)-1
        ca = ja(ka)
        iat(ca) = iat(ca)+1
      END DO
    END DO
    CALL manymanycreaterowpointers(nca, iat)
    itemp = iat(nca+1)-1
    CALL allocsafe(itemp, jat)
    CALL allocsafe(itemp, vat)
    DO ra = 1, nra
      DO ka = ia(ra), ia(ra+1)-1
        ca = ja(ka)
        nextv = iat(ca)
        iat(ca) = nextv+1
        jat(nextv) = ra
        vat(nextv) = va(ka)
      END DO
    END DO
    DO ca = nca, 1, -1
      iat(ca+1) = iat(ca)
    END DO
    iat(1) = 1
  END SUBROUTINE manymanymatrixtransposewithvalues
!> row permutations
!> of a sparse matrix
  SUBROUTINE manymanyrowpermutation(nra, ia, ja, va, ianew, janew, vanew, newold)
    INTEGER::ra
    INTEGER, DIMENSION(*)::ia, ja, ianew, janew, newold
    REAL(8), DIMENSION(*)::va, vanew
    DO ra = 1, nra
      ianew(newold(ra)+1) = ia(ra+1)-ia(ra)
    END DO
    ianew(1) = 1
    DO ra = 1, nra
      ianew(ra+1) = ianew(ra+1)+ianew(ra)
    END DO
    DO ra = 1, nra
      ko = ianew(newold(ra))-1
      DO ka = ia(ra), ia(ra+1)-1
        ko = ko+1
        janew(ko) = ja(ka)
        vanew(ko) = va(ka)
      END DO
    END DO
  END SUBROUTINE manymanyrowpermutation
!> column permutations
!> of a sparse matrix
  SUBROUTINE manymanycolumnpermutation(nra, ia, ja, janew, newold)
    INTEGER, DIMENSION(*)::ia, ja, janew, newold
    nnz = ia(nra+1)-1
    DO kz = 1, nnz
      janew(kz) = newold(ja(kz))
    END DO
  END SUBROUTINE manymanycolumnpermutation
!> makes a postscript plot of a sparse matrix
  SUBROUTINE manymanyplotstructure(na, ia, ja, name, iunt, size)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER, DIMENSION(*)::ia, ja
    CHARACTER(*)::name
    CHARACTER(3)::munt
    munt = "cm"
    haf = 0.5d00
    zero = 0.0d00
    conv = 2.54d00
    nrow = na
    ncol = manymanymaximumcolumn(na, ia, ja)
    siz = size
    nr = nrow
    nc = ncol
    n = nc
    n = nr
    maxdim = max(nrow, ncol)
    m = 1+maxdim
    nc = nc+1
    nr = nr+1
    IF (munt .EQ. 'cm' .OR. munt .EQ. 'cm') THEN
      u2dot = 72.0/conv
      paperx = 21.0
    ELSE
      u2dot = 72.0
      paperx = 8.5*conv
      siz = siz*conv
    END IF
    lrmrgn = nint((paperx-siz)/2.0)
    botmrgn = 2.0
    scfct = siz*u2dot/m
    frlw = 0.25
    fnstit = 0.5
    ltit = len_trim(name)
    ytitof = 1.0
    xtit = paperx/2.0
    ytit = botmrgn+siz*nr/m+ytitof
    xl = lrmrgn*u2dot-scfct*frlw/2
    xr = (lrmrgn+siz)*u2dot+scfct*frlw/2
    yb = botmrgn*u2dot-scfct*frlw/2
    yt = (botmrgn+siz*nr/m)*u2dot+scfct*frlw/2
    IF (ltit .GT. 0) THEN
      yt = yt+(ytitof+fnstit*0.70)*u2dot
    END IF
    delt = 10.0
    xl = xl-delt
    xr = xr+delt
    yb = yb-delt
    yt = yt+delt
    WRITE (iunt, 10) '%!'
    WRITE (iunt, 10) '%%creator: pspltm routine'
    WRITE (iunt, 12) '%%boundingbox:', xl, yb, xr, yt
    WRITE (iunt, 10) '%%endcomments'
    WRITE (iunt, 10) '/cm {72 mul 2.54 div} def'
    WRITE (iunt, 10) '/mc {72 div 2.54 mul} def'
    WRITE (iunt, 10) '/pnum { 72 div 2.54 mul 20 string'
    WRITE (iunt, 10) 'cvs print ( ) print} def'
    WRITE (iunt, 10) '/cshow {dup stringwidth pop -2 div 0 rmoveto show} def'
    WRITE (iunt, 10) 'gsave'
    IF (ltit .GT. 0) THEN
      WRITE (iunt, *) '/helvetica findfont ', fnstit, ' cm scalefont setfont '
      WRITE (iunt, *) xtit, ' cm ', ytit, ' cm moveto '
      WRITE (iunt, '(3a)') '(', name(1:ltit), ') cshow'
    END IF
    WRITE (iunt, *) lrmrgn, ' cm ', botmrgn, ' cm translate'
    WRITE (iunt, *) siz, ' cm ', m, ' div dup scale '
    WRITE (iunt, *) frlw, ' setlinewidth'
    WRITE (iunt, 10) 'newpath'
    WRITE (iunt, 11) 0, 0, ' moveto'
    WRITE (iunt, 11) nc, 0, ' lineto'
    WRITE (iunt, 11) nc, nr, ' lineto'
    WRITE (iunt, 11) 0, nr, ' lineto'
    WRITE (iunt, 10) 'closepath stroke'
    WRITE (iunt, *) ' 0.2 setlinewidth'
    WRITE (iunt, 10) '1 1 translate'
    WRITE (iunt, 10) '0.8 setlinewidth'
    WRITE (iunt, 10) '/p {moveto 0 -.40 rmoveto '
    WRITE (iunt, 10) '           0  .80 rlineto stroke} def'
    DO ii = 1, nrow
      istart = ia(ii)
      ilast = ia(ii+1)-1
      DO k = istart, ilast
        WRITE (iunt, 11) ja(k)-1, nrow-ii, ' p'
      END DO
    END DO
    WRITE (iunt, 10) 'showpage'
    RETURN
10  FORMAT(a)
11  FORMAT(2(i6, 1x), a)
12  FORMAT(a, 4(1x, f9.2))
  END SUBROUTINE manymanyplotstructure
!> csc sparsedense lx=b with l
!> being lower triangular
!> and sparse
!> diagonals are at il(rl)
!> the beggining of each line
  SUBROUTINE manymanysparsedenselxbcsc(nrl, il, jl, vx, x, b)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(nrl)::x, b
    REAL(8), DIMENSION(*)::vx
    INTEGER, DIMENSION(*)::il, jl
    INTEGER::rl
    x = b
    DO rl = 1, nrl
      x(rl) = x(rl)/vx(il(rl))
      t = x(rl)
      DO kl = il(rl)+1, il(rl+1)-1
        x(jl(kl)) = x(jl(kl))-t*vx(kl)
      END DO
    END DO
  END SUBROUTINE manymanysparsedenselxbcsc
!> csr sparsedense ux=b with u
!> being upper triangular
!> and sparse
!> diagonals are at iu(ru)
!> the beggining of each line
  SUBROUTINE manymanysparsdenseuxbcsr(nru, iu, ju, vu, x, b)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(nru)::x, b
    REAL(8), DIMENSION(*)::vu
    INTEGER, DIMENSION(*)::iu, ju
    INTEGER::ru
    x(nru) = b(nru)/vu(iu(nru))
    DO ru = nru-1, 1, -1
      t = b(ru)
      DO ku = iu(ru)+1, iu(ru+1)-1
        t = t-vu(ku)*x(ju(ku))
      END DO
      x(ru) = t/vu(iu(ru))
    END DO
  END SUBROUTINE manymanysparsdenseuxbcsr
!> determinant of a 1x1 matrix
  SUBROUTINE matrix11determinant(detm, m, dm)
    IMPLICIT NONE
    REAL(8) detm, m(1, 1), dm(1, 1)
    detm = m(1, 1)
    dm(1, 1) = 1.0d00
  END SUBROUTINE matrix11determinant
!> determinant and derivative of a 1x1, 2x2 or 3x3 matrix
  SUBROUTINE matrix123determinant(detm, m, dm)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(:, :)::m
    REAL(8), DIMENSION(:, :)::dm
    SELECT CASE (size(m, 1))
    CASE (1)
      CALL matrix11determinant(detm, m, dm)
    CASE (2)
      CALL matrix22determinant(detm, m, dm)
    CASE (3)
      CALL matrix33determinant(detm, m, dm)
    CASE default
      WRITE (*, *) " error in matrix123determinant "
    END SELECT
  END SUBROUTINE matrix123determinant
!> determinant of any 1x1, 2x2 or 3x3 matrix
  REAL(8) FUNCTION matrix123determinantonly(ndi, a)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(ndi, *)::a
    SELECT CASE (ndi)
    CASE (1)
      matrix123determinantonly = a(1, 1)
    CASE (2)
      matrix123determinantonly = a(1, 1)*a(2, 2)-a(1, 2)*a(2, 1)
    CASE (3)
      matrix123determinantonly = a(1, 1)*a(2, 2)*a(3, 3)+a(1, 2)*a(2, 3)*a(3, 1)+ &
                                 a(1, 3)*a(2, 1)*a(3, 2)-a(1, 3)*a(2, 2)*a(3, 1)- &
                                 a(1, 2)*a(2, 1)*a(3, 3)-a(1, 1)*a(2, 3)*a(3, 2)
    CASE default
      matrix123determinantonly = 0.0d00
      WRITE (*, *) " error in matrix123determinantonly "
      STOP
    END SELECT
  END FUNCTION matrix123determinantonly
  SUBROUTINE matrix22determinant(detm, m, dm)
    IMPLICIT NONE
    REAL(8) detm, m(2, 2), dm(2, 2)
    detm = m(1, 1)*m(2, 2)-m(1, 2)*m(2, 1)
    dm(1, 1) = m(2, 2)
    dm(1, 2) = -m(2, 1)
    dm(2, 1) = -m(1, 2)
    dm(2, 2) = m(1, 1)
  END SUBROUTINE matrix22determinant
  SUBROUTINE matrix33determinant(detm, m, dm)
    IMPLICIT NONE
    REAL(8) v(5001), detm, m(3, 3), dm(3, 3)
    v(22) = -(m(2, 2)*m(3, 1))+m(2, 1)*m(3, 2)
    v(21) = -(m(2, 3)*m(3, 1))+m(2, 1)*m(3, 3)
    v(20) = -(m(2, 3)*m(3, 2))+m(2, 2)*m(3, 3)
    detm = m(1, 1)*v(20)-m(1, 2)*v(21)+m(1, 3)*v(22)
    dm(1, 1) = v(20)
    dm(1, 2) = -v(21)
    dm(1, 3) = v(22)
    dm(2, 1) = m(1, 3)*m(3, 2)-m(1, 2)*m(3, 3)
    dm(2, 2) = -(m(1, 3)*m(3, 1))+m(1, 1)*m(3, 3)
    dm(2, 3) = m(1, 2)*m(3, 1)-m(1, 1)*m(3, 2)
    dm(3, 1) = -(m(1, 3)*m(2, 2))+m(1, 2)*m(2, 3)
    dm(3, 2) = m(1, 3)*m(2, 1)-m(1, 1)*m(2, 3)
    dm(3, 3) = -(m(1, 2)*m(2, 1))+m(1, 1)*m(2, 2)
  END SUBROUTINE matrix33determinant
!> adds main diagonal to a matrix
  SUBROUTINE matrixaddidentity(n, a)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a
    CALL matrixaddscalar(n, a, 1.0d00)
  END SUBROUTINE matrixaddidentity
!> returns global address for a vector representing a matrix
!> where m is the number of rows
!> column major mode
  INTEGER FUNCTION matrixaddressing(m, i, j)!m-number of rows, i-row, j-column
    matrixaddressing = i+(j-1)*m
  END FUNCTION matrixaddressing
!> adds a scalar
  SUBROUTINE matrixaddscalar(n, a, esc)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a
    DO i = 1, n
      a(i, i) = a(i, i)+esc
    END DO
  END SUBROUTINE matrixaddscalar
!> obtains a skew-symmetrized version of a matrix
  SUBROUTINE matrixassymetrize(n, mat, mats)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::mat, mats
    DO j = 1, n
      DO i = 1, n
        mats(i, j) = 0.5d00*(mat(i, j)-mat(j, i))
      END DO
    END DO
  END SUBROUTINE matrixassymetrize
!> matrix broyden update
  SUBROUTINE matrixbroydenupdate(n, dx, df, j1old, j1new)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(N)::DX, DF
    REAL(8), DIMENSION(N, N)::J1OLD, J1NEW
    REAL(8), DIMENSION(N)::TOPLEFT, TOPRIGHT
    REAL(8), DIMENSION(N)::TEMP
    REAL(8)::BOT
    CALL matrixvectorproduct(n, n, J1OLD, df, temp)
    bot = vectdot(n, dx, temp)
    TOPLEFT = DX-TEMP
    DO JD = 1, N
      TOPRIGHT(JD) = 0.0d00
      DO ID = 1, N
        TOPRIGHT(JD) = TOPRIGHT(JD)+DX(ID)*J1OLD(ID, JD)
      END DO
    END DO
    DO ID = 1, N
      DO JD = 1, N
        J1NEW(ID, JD) = J1OLD(ID, JD)+TOPLEFT(ID)*TOPRIGHT(JD)/BOT
      END DO
    END DO
  END SUBROUTINE matrixbroydenupdate
!> returns a condition number
  REAL(8) FUNCTION matrixconditionnumbber(n, a)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a
    REAL(8), DIMENSION(n, n)::ai
    CALL matrixinverse(n, det, a, ai)
    matrixconditionnumbber = matrixnormfrobenius(n, a)*matrixnormfrobenius(n, ai)
  END FUNCTION matrixconditionnumbber
!> returns the determinant from the factos in lu factorization
  SUBROUTINE matrixdeterminantfromlu(n, a, kpiv, detm)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a
    INTEGER, DIMENSION(*)::kpiv
    REAL(8), DIMENSION(2)::det
!*** determinant
    det(1) = 1.0d+00
    det(2) = 0.0d+00
    DO i = 1, n
      IF (kpiv(i) /= i) THEN
        det(1) = -det(1)
      END IF
      det(1) = det(1)*a(i, i)
      IF (abs(det(1)) .LE. epsmach()) THEN
        EXIT
      END IF
      DO WHILE (abs(det(1)) < 1.0d+00)
        det(1) = det(1)*10.0d+00
        det(2) = det(2)-1.0d+00
      END DO
      DO WHILE (10.0d+00 <= abs(det(1)))
        det(1) = det(1)/10.0d+00
        det(2) = det(2)+1.0d+00
      END DO
    END DO
    detm = det(1)*10.0d00**det(2)
  END SUBROUTINE matrixdeterminantfromlu
!> calculates eigenvalues and eigenvectors of a matrix at
  SUBROUTINE matrixeigensystem(at, d, v, n, ierr)
    USE, INTRINSIC :: iso_fortran_env, ONLY: real64
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: n             ! Size of the matrix
    INTEGER :: ierr          ! Error code (0 if successful)
    REAL(real64), DIMENSION(n, *) :: at  ! Input matrix (n x n)
    REAL(real64), DIMENSION(*), INTENT(OUT) :: d   ! Eigenvalues (size n)
    REAL(real64), DIMENSION(n, *), INTENT(OUT) :: v   ! Eigenvectors (n x n)
! Local variables
    INTEGER :: ip, iq, i, j
    REAL(real64), DIMENSION(n)     :: b, z
    REAL(real64), DIMENSION(n, n)  :: a
    REAL(real64) :: fn, sm, tresh, g, h, theta, c, s, tau, t
! Parameters
    REAL(real64), PARAMETER :: rze = 0.0d0    ! Zero
    REAL(real64), PARAMETER :: rum = 1.0d0    ! Unity
    REAL(real64), PARAMETER :: rce = 100.0d0  ! Used in checks
    REAL(real64), PARAMETER :: small = 1.0d-19  ! Small tolerance
    INTEGER, PARAMETER      :: miter = 600       ! Max iterations
    REAL(real64), PARAMETER :: EPS = epsilon(1.0d0) ! Machine precision
! Copy the input matrix 'at' into 'a'
    CALL vectcopy(n*n, a, at)
! Initialize error flag
    ierr = 0
! Initialize v to the identity matrix
    DO ip = 1, n
      DO iq = 1, n
        v(iq, ip) = rze
      END DO
      v(ip, ip) = rum
    END DO
! Initialize d, b, and z
    DO ip = 1, n
      b(ip) = a(ip, ip)
      d(ip) = b(ip)
      z(ip) = rze
    END DO
! Compute a stopping criterion based on the Frobenius norm
    fn = matrixnormfrobenius(n, a)
    fn = small*fn
! Jacobi rotation iteration
    DO i = 1, miter
! Compute the sum of the absolute values of the off-diagonal elements
      sm = rze
      DO ip = 1, n-1
        DO iq = ip+1, n
          sm = sm+abs(a(ip, iq))
        END DO
      END DO
! If off-diagonal elements are small enough, we are done
      IF (sm <= fn) RETURN
! Set the threshold
      IF (i < 4) THEN
        tresh = 0.2d0*sm/(n**2)
      ELSE
        tresh = rze
      END IF
! Perform Jacobi rotations
      DO ip = 1, n-1
        DO iq = ip+1, n
          g = rce*abs(a(ip, iq))
! Check if adding g does not change ABS(d(ip)) and ABS(d(iq)) in a floating-point sense
! Original condition: (ABS(d(ip))+g == ABS(d(ip))) and similarly for d(iq)
          IF ((i > 4) .AND. &
              (abs((abs(d(ip))+g)-abs(d(ip))) < EPS*max(abs(d(ip)), 1.0d0)) .AND. &
              (abs((abs(d(iq))+g)-abs(d(iq))) < EPS*max(abs(d(iq)), 1.0d0))) THEN
            a(ip, iq) = rze
          ELSEIF (abs(a(ip, iq)) > tresh) THEN
            h = d(iq)-d(ip)
! Original condition: (ABS(h)+g == ABS(h))
            IF (abs((abs(h)+g)-abs(h)) < EPS*max(abs(h), 1.0d0)) THEN
              t = a(ip, iq)/h
            ELSE
              theta = 0.5d0*h/a(ip, iq)
              t = 1.0d0/(abs(theta)+sqrt(rum+theta**2))
              IF (theta < rze) t = -t
            END IF
            c = rum/sqrt(rum+t**2)
            s = t*c
            tau = s/(rum+c)
            h = t*a(ip, iq)
            z(ip) = z(ip)-h
            z(iq) = z(iq)+h
            d(ip) = d(ip)-h
            d(iq) = d(iq)+h
            a(ip, iq) = rze
! Update elements a(j, ip) and a(j, iq)
            DO j = 1, ip-1
              g = a(j, ip)
              h = a(j, iq)
              a(j, ip) = g-s*(h+g*tau)
              a(j, iq) = h+s*(g-h*tau)
            END DO
            DO j = ip+1, iq-1
              g = a(ip, j)
              h = a(j, iq)
              a(ip, j) = g-s*(h+g*tau)
              a(j, iq) = h+s*(g-h*tau)
            END DO
            DO j = iq+1, n
              g = a(ip, j)
              h = a(iq, j)
              a(ip, j) = g-s*(h+g*tau)
              a(iq, j) = h+s*(g-h*tau)
            END DO
! Update eigenvector matrix v
            DO j = 1, n
              g = v(j, ip)
              h = v(j, iq)
              v(j, ip) = g-s*(h+g*tau)
              v(j, iq) = h+s*(g-h*tau)
            END DO
          END IF
        END DO
      END DO
! Update b and d
      DO ip = 1, n
        b(ip) = b(ip)+z(ip)
        d(ip) = b(ip)
        z(ip) = rze
      END DO
    END DO
! If we reach here, we have not converged within miter iterations
    ierr = 1
  END SUBROUTINE matrixeigensystem
!> invert a matrix and calculates the determinant
  SUBROUTINE matrixinverse(n, det, a, ai)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::n
    REAL(8), DIMENSION(5001)::v
    REAL(8), DIMENSION(n, *)::a, ai
    REAL(8), DIMENSION(n, n)::adecomp
    INTEGER, DIMENSION(n)::kpiv
    IF (n .LE. 0) RETURN
    IF (n .EQ. 1) THEN
      det = a(1, 1)
      ai(1, 1) = 1.0d00/det
      RETURN
    END IF
    IF (n .EQ. 2) THEN
      v(9) = -(a(1, 2)*a(2, 1))+a(1, 1)*a(2, 2)
      ai(1, 1) = a(2, 2)/v(9)
      ai(1, 2) = -(a(1, 2)/v(9))
      ai(2, 1) = -(a(2, 1)/v(9))
      ai(2, 2) = a(1, 1)/v(9)
      det = v(9)
      RETURN
    END IF
    IF (n .EQ. 3) THEN
      v(28) = -(a(2, 2)*a(3, 1))+a(2, 1)*a(3, 2)
      v(24) = -(a(2, 3)*a(3, 1))+a(2, 1)*a(3, 3)
      v(20) = -(a(2, 3)*a(3, 2))+a(2, 2)*a(3, 3)
      v(19) = a(1, 1)*v(20)-a(1, 2)*v(24)+a(1, 3)*v(28)
      ai(1, 1) = v(20)/v(19)
      ai(1, 2) = (a(1, 3)*a(3, 2)-a(1, 2)*a(3, 3))/v(19)
      ai(1, 3) = (-(a(1, 3)*a(2, 2))+a(1, 2)*a(2, 3))/v(19)
      ai(2, 1) = -(v(24)/v(19))
      ai(2, 2) = (-(a(1, 3)*a(3, 1))+a(1, 1)*a(3, 3))/v(19)
      ai(2, 3) = (a(1, 3)*a(2, 1)-a(1, 1)*a(2, 3))/v(19)
      ai(3, 1) = v(28)/v(19)
      ai(3, 2) = (a(1, 2)*a(3, 1)-a(1, 1)*a(3, 2))/v(19)
      ai(3, 3) = (-(a(1, 2)*a(2, 1))+a(1, 1)*a(2, 2))/v(19)
      det = v(19)
      RETURN
    END IF
    IF (n .EQ. 4) THEN
      v(73) = -(a(1, 2)*a(2, 1))+a(1, 1)*a(2, 2)
      v(72) = a(1, 2)*a(3, 1)-a(1, 1)*a(3, 2)
      v(71) = a(1, 3)*a(2, 1)-a(1, 1)*a(2, 3)
      v(70) = -(a(1, 4)*a(2, 1))+a(1, 1)*a(2, 4)
      v(69) = -(a(1, 3)*a(3, 1))+a(1, 1)*a(3, 3)
      v(68) = a(1, 4)*a(3, 1)-a(1, 1)*a(3, 4)
      v(67) = -(a(1, 3)*a(2, 2))+a(1, 2)*a(2, 3)
      v(66) = a(1, 4)*a(2, 2)-a(1, 2)*a(2, 4)
      v(65) = -(a(1, 4)*a(2, 3))+a(1, 3)*a(2, 4)
      v(64) = a(1, 3)*a(3, 2)-a(1, 2)*a(3, 3)
      v(63) = -(a(1, 4)*a(3, 2))+a(1, 2)*a(3, 4)
      v(62) = a(1, 4)*a(3, 3)-a(1, 3)*a(3, 4)
      v(33) = -(a(3, 2)*a(4, 1))+a(3, 1)*a(4, 2)
      v(34) = -(a(3, 3)*a(4, 1))+a(3, 1)*a(4, 3)
      v(35) = -(a(3, 3)*a(4, 2))+a(3, 2)*a(4, 3)
      v(55) = a(2, 3)*v(33)-a(2, 2)*v(34)+a(2, 1)*v(35)
      v(36) = -(a(3, 4)*a(4, 1))+a(3, 1)*a(4, 4)
      v(37) = -(a(3, 4)*a(4, 2))+a(3, 2)*a(4, 4)
      v(50) = a(2, 4)*v(33)-a(2, 2)*v(36)+a(2, 1)*v(37)
      v(38) = -(a(3, 4)*a(4, 3))+a(3, 3)*a(4, 4)
      v(45) = a(2, 4)*v(34)-a(2, 3)*v(36)+a(2, 1)*v(38)
      v(40) = a(2, 4)*v(35)-a(2, 3)*v(37)+a(2, 2)*v(38)
      v(39) = a(1, 1)*v(40)-a(1, 2)*v(45)+a(1, 3)*v(50)-a(1, 4)*v(55)
      ai(1, 1) = v(40)/v(39)
      ai(1, 2) = (a(4, 2)*v(62)+a(4, 3)*v(63)+a(4, 4)*v(64))/v(39)
      ai(1, 3) = (a(4, 2)*v(65)+a(4, 3)*v(66)+a(4, 4)*v(67))/v(39)
      ai(1, 4) = (-(a(3, 2)*v(65))-a(3, 3)*v(66)-a(3, 4)*v(67))/v(39)
      ai(2, 1) = -(v(45)/v(39))
      ai(2, 2) = (-(a(4, 1)*v(62))+a(4, 3)*v(68)+a(4, 4)*v(69))/v(39)
      ai(2, 3) = (-(a(4, 1)*v(65))+a(4, 3)*v(70)+a(4, 4)*v(71))/v(39)
      ai(2, 4) = (a(3, 1)*v(65)-a(3, 3)*v(70)-a(3, 4)*v(71))/v(39)
      ai(3, 1) = v(50)/v(39)
      ai(3, 2) = (-(a(4, 1)*v(63))-a(4, 2)*v(68)+a(4, 4)*v(72))/v(39)
      ai(3, 3) = (-(a(4, 1)*v(66))-a(4, 2)*v(70)+a(4, 4)*v(73))/v(39)
      ai(3, 4) = (a(3, 1)*v(66)+a(3, 2)*v(70)-a(3, 4)*v(73))/v(39)
      ai(4, 1) = -(v(55)/v(39))
      ai(4, 2) = (-(a(4, 1)*v(64))-a(4, 2)*v(69)-a(4, 3)*v(72))/v(39)
      ai(4, 3) = (-(a(4, 1)*v(67))-a(4, 2)*v(71)-a(4, 3)*v(73))/v(39)
      ai(4, 4) = (a(3, 1)*v(67)+a(3, 2)*v(71)+a(3, 3)*v(73))/v(39)
      det = v(39)
      RETURN
    END IF
    IF (n .EQ. 5) THEN
      v(270) = a(4, 2)*a(5, 5)
      v(269) = a(4, 3)*a(5, 5)
      v(268) = a(4, 4)*a(5, 5)
      v(267) = a(4, 1)*a(5, 5)
      v(266) = a(1, 1)*a(5, 5)
      v(265) = a(4, 2)*a(5, 4)
      v(264) = a(4, 3)*a(5, 4)
      v(263) = a(4, 5)*a(5, 4)
      v(262) = a(4, 1)*a(5, 4)
      v(261) = a(1, 1)*a(5, 4)
      v(260) = a(4, 2)*a(5, 3)
      v(259) = a(4, 4)*a(5, 3)
      v(258) = a(4, 5)*a(5, 3)
      v(257) = a(4, 1)*a(5, 3)
      v(256) = a(1, 1)*a(5, 3)
      v(255) = a(4, 3)*a(5, 2)
      v(254) = a(4, 4)*a(5, 2)
      v(253) = a(4, 5)*a(5, 2)
      v(252) = a(4, 1)*a(5, 2)
      v(251) = a(1, 1)*a(5, 2)
      v(250) = a(4, 3)*a(5, 1)
      v(249) = a(4, 4)*a(5, 1)
      v(248) = a(4, 5)*a(5, 1)
      v(247) = a(4, 2)*a(5, 1)
      v(246) = a(1, 2)*a(5, 1)
      v(245) = a(2, 2)*a(3, 5)
      v(244) = a(1, 2)*a(3, 5)
      v(243) = a(2, 1)*a(3, 5)
      v(242) = a(1, 1)*a(3, 5)
      v(241) = a(2, 2)*a(3, 4)
      v(240) = a(1, 2)*a(3, 4)
      v(239) = a(2, 1)*a(3, 4)
      v(238) = a(1, 1)*a(3, 4)
      v(237) = a(2, 2)*a(3, 3)
      v(236) = a(1, 2)*a(3, 3)
      v(235) = a(2, 1)*a(3, 3)
      v(234) = a(1, 1)*a(3, 3)
      v(233) = a(1, 5)*a(2, 4)
      v(232) = a(1, 4)*a(2, 5)
      v(231) = a(2, 3)*a(3, 2)
      v(230) = a(1, 3)*a(3, 2)
      v(229) = a(2, 1)*a(3, 2)
      v(228) = a(1, 1)*a(3, 2)
      v(227) = a(2, 3)*a(3, 1)
      v(226) = a(1, 3)*a(3, 1)
      v(225) = a(2, 2)*a(3, 1)
      v(224) = a(1, 2)*a(3, 1)
      v(210) = a(2, 3)*v(224)
      v(209) = a(1, 3)*v(225)
      v(192) = a(2, 4)*v(224)
      v(191) = a(1, 4)*v(225)
      v(186) = a(2, 5)*v(224)
      v(185) = a(1, 5)*v(225)
      v(162) = a(2, 4)*v(226)
      v(161) = a(1, 4)*v(227)
      v(156) = a(2, 5)*v(226)
      v(155) = a(1, 5)*v(227)
      v(150) = a(3, 1)*v(232)
      v(149) = a(3, 1)*v(233)
      v(212) = a(2, 3)*v(228)
      v(211) = a(1, 3)*v(229)
      v(194) = a(2, 4)*v(228)
      v(193) = a(1, 4)*v(229)
      v(188) = a(2, 5)*v(228)
      v(187) = a(1, 5)*v(229)
      v(120) = a(2, 4)*v(230)
      v(119) = a(1, 4)*v(231)
      v(114) = a(2, 5)*v(230)
      v(113) = a(1, 5)*v(231)
      v(108) = a(3, 2)*v(232)
      v(107) = a(3, 2)*v(233)
      v(214) = a(2, 2)*v(234)
      v(213) = a(1, 2)*v(235)
      v(290) = -v(209)+v(210)+v(211)-v(212)-v(213)+v(214)
      v(164) = a(2, 4)*v(234)
      v(163) = a(1, 4)*v(235)
      v(158) = a(2, 5)*v(234)
      v(157) = a(1, 5)*v(235)
      v(122) = a(2, 4)*v(236)
      v(121) = a(1, 4)*v(237)
      v(116) = a(2, 5)*v(236)
      v(115) = a(1, 5)*v(237)
      v(102) = a(3, 3)*v(232)
      v(101) = a(3, 3)*v(233)
      v(196) = a(2, 2)*v(238)
      v(195) = a(1, 2)*v(239)
      v(288) = v(191)-v(192)-v(193)+v(194)+v(195)-v(196)
      v(166) = a(2, 3)*v(238)
      v(165) = a(1, 3)*v(239)
      v(284) = -v(161)+v(162)+v(163)-v(164)-v(165)+v(166)
      v(152) = a(2, 5)*v(238)
      v(151) = a(1, 5)*v(239)
      v(124) = a(2, 3)*v(240)
      v(123) = a(1, 3)*v(241)
      v(278) = v(119)-v(120)-v(121)+v(122)+v(123)-v(124)
      v(110) = a(2, 5)*v(240)
      v(109) = a(1, 5)*v(241)
      v(104) = a(1, 3)*a(2, 5)*a(3, 4)
      v(103) = a(1, 5)*a(2, 3)*a(3, 4)
      v(190) = a(2, 2)*v(242)
      v(189) = a(1, 2)*v(243)
      v(287) = -v(185)+v(186)+v(187)-v(188)-v(189)+v(190)
      v(160) = a(2, 3)*v(242)
      v(159) = a(1, 3)*v(243)
      v(283) = v(155)-v(156)-v(157)+v(158)+v(159)-v(160)
      v(154) = a(2, 4)*v(242)
      v(153) = a(1, 4)*v(243)
      v(282) = -v(149)+v(150)+v(151)-v(152)-v(153)+v(154)
      v(118) = a(2, 3)*v(244)
      v(117) = a(1, 3)*v(245)
      v(277) = -v(113)+v(114)+v(115)-v(116)-v(117)+v(118)
      v(112) = a(2, 4)*v(244)
      v(111) = a(1, 4)*v(245)
      v(276) = v(107)-v(108)-v(109)+v(110)+v(111)-v(112)
      v(106) = a(1, 3)*a(2, 4)*a(3, 5)
      v(105) = a(1, 4)*a(2, 3)*a(3, 5)
      v(275) = -v(101)+v(102)+v(103)-v(104)-v(105)+v(106)
      v(202) = a(4, 3)*v(246)
      v(201) = a(1, 3)*v(247)
      v(174) = a(4, 5)*v(246)
      v(173) = a(4, 4)*v(246)
      v(172) = a(1, 4)*v(247)
      v(171) = a(1, 5)*v(247)
      v(134) = a(1, 3)*v(248)
      v(133) = a(1, 4)*v(248)
      v(132) = a(1, 3)*v(249)
      v(131) = a(1, 5)*v(249)
      v(130) = a(1, 4)*v(250)
      v(129) = a(1, 5)*v(250)
      v(204) = a(4, 3)*v(251)
      v(203) = a(1, 3)*v(252)
      v(178) = a(4, 5)*v(251)
      v(177) = a(4, 4)*v(251)
      v(176) = a(1, 4)*v(252)
      v(175) = a(1, 5)*v(252)
      v(80) = a(1, 3)*v(253)
      v(79) = a(1, 4)*v(253)
      v(78) = a(1, 3)*v(254)
      v(77) = a(1, 5)*v(254)
      v(76) = a(1, 4)*v(255)
      v(75) = a(1, 5)*v(255)
      v(52) = -v(247)+v(252)
      v(206) = a(4, 2)*v(256)
      v(205) = a(1, 2)*v(257)
      v(289) = -v(201)+v(202)+v(203)-v(204)-v(205)+v(206)
      v(138) = a(4, 5)*v(256)
      v(137) = a(4, 4)*v(256)
      v(136) = a(1, 4)*v(257)
      v(135) = a(1, 5)*v(257)
      v(86) = a(1, 2)*v(258)
      v(85) = a(1, 4)*v(258)
      v(84) = a(1, 2)*v(259)
      v(83) = a(1, 5)*v(259)
      v(82) = a(1, 4)*v(260)
      v(81) = a(1, 5)*v(260)
      v(57) = -v(255)+v(260)
      v(54) = -v(250)+v(257)
      v(180) = a(4, 2)*v(261)
      v(179) = a(1, 2)*v(262)
      v(285) = v(172)-v(173)-v(176)+v(177)+v(179)-v(180)
      v(142) = a(4, 5)*v(261)
      v(141) = a(4, 3)*v(261)
      v(140) = a(1, 3)*v(262)
      v(279) = -v(130)+v(132)+v(136)-v(137)-v(140)+v(141)
      v(139) = a(1, 5)*v(262)
      v(92) = a(1, 2)*v(263)
      v(91) = a(1, 3)*v(263)
      v(90) = a(1, 2)*v(264)
      v(89) = a(1, 5)*v(264)
      v(88) = a(1, 3)*v(265)
      v(271) = v(76)-v(78)-v(82)+v(84)+v(88)-v(90)
      v(87) = a(1, 5)*v(265)
      v(59) = -v(259)+v(264)
      v(58) = -v(254)+v(265)
      v(55) = -v(249)+v(262)
      v(182) = a(4, 2)*v(266)
      v(181) = a(1, 2)*v(267)
      v(286) = -v(171)+v(174)+v(175)-v(178)-v(181)+v(182)
      v(146) = a(4, 4)*v(266)
      v(145) = a(4, 3)*v(266)
      v(144) = a(1, 3)*v(267)
      v(280) = v(129)-v(134)-v(135)+v(138)+v(144)-v(145)
      v(143) = a(1, 4)*v(267)
      v(281) = -v(131)+v(133)+v(139)-v(142)-v(143)+v(146)
      v(98) = a(1, 2)*v(268)
      v(97) = a(1, 3)*v(268)
      v(96) = a(1, 2)*v(269)
      v(95) = a(1, 4)*v(269)
      v(273) = -v(83)+v(85)+v(89)-v(91)-v(95)+v(97)
      v(94) = a(1, 3)*v(270)
      v(272) = -v(75)+v(80)+v(81)-v(86)-v(94)+v(96)
      v(93) = a(1, 4)*v(270)
      v(274) = v(77)-v(79)-v(87)+v(92)+v(93)-v(98)
      v(68) = -v(263)+v(268)
      v(65) = -v(258)+v(269)
      v(64) = -v(253)+v(270)
      v(62) = -v(248)+v(267)
      v(51) = a(3, 3)*v(52)-a(3, 2)*v(54)+a(3, 1)*v(57)
      v(53) = a(3, 4)*v(52)-a(3, 2)*v(55)+a(3, 1)*v(58)
      v(56) = a(3, 4)*v(54)-a(3, 3)*v(55)+a(3, 1)*v(59)
      v(60) = a(3, 4)*v(57)-a(3, 3)*v(58)+a(3, 2)*v(59)
      v(216) = -(a(2, 4)*v(51))+a(2, 3)*v(53)-a(2, 2)*v(56)+a(2, 1)*v(60)
      v(61) = a(3, 5)*v(52)-a(3, 2)*v(62)+a(3, 1)*v(64)
      v(63) = a(3, 5)*v(54)-a(3, 3)*v(62)+a(3, 1)*v(65)
      v(66) = a(3, 5)*v(57)-a(3, 3)*v(64)+a(3, 2)*v(65)
      v(198) = -(a(2, 5)*v(51))+a(2, 3)*v(61)-a(2, 2)*v(63)+a(2, 1)*v(66)
      v(67) = a(3, 5)*v(55)-a(3, 4)*v(62)+a(3, 1)*v(68)
      v(69) = a(3, 5)*v(58)-a(3, 4)*v(64)+a(3, 2)*v(68)
      v(168) = -(a(2, 5)*v(53))+a(2, 4)*v(61)-a(2, 2)*v(67)+a(2, 1)*v(69)
      v(70) = a(3, 5)*v(59)-a(3, 4)*v(65)+a(3, 3)*v(68)
      v(126) = -(a(2, 5)*v(56))+a(2, 4)*v(63)-a(2, 3)*v(67)+a(2, 1)*v(70)
      v(72) = -(a(2, 5)*v(60))+a(2, 4)*v(66)-a(2, 3)*v(69)+a(2, 2)*v(70)
      v(71) = -(a(1, 2)*v(126))+a(1, 3)*v(168)-a(1, 4)*v(198)+a(1, 5)*v(216)+a(1, 1)*v(72)
      ai(1, 1) = v(72)/v(71)
      ai(1, 2) = (a(3, 5)*v(271)+a(3, 4)*v(272)+a(3, 2)*v(273)+a(3, 3)*v(274))/v(71)
      ai(1, 3) = (-(a(2, 5)*v(271))-a(2, 4)*v(272)-a(2, 2)*v(273)-a(2, 3)*v(274))/v(71)
      ai(1, 4) = (a(5, 2)*v(275)+a(5, 3)*v(276)+a(5, 4)*v(277)+a(5, 5)*v(278))/v(71)
      ai(1, 5) = (-(a(4, 2)*v(275))-a(4, 3)*v(276)-a(4, 4)*v(277)-a(4, 5)*v(278))/v(71)
      ai(2, 1) = -(v(126)/v(71))
      ai(2, 2) = (-(a(3, 1)*v(273))+a(3, 5)*v(279)+a(3, 4)*v(280)+a(3, 3)*v(281))/v(71)
      ai(2, 3) = (a(2, 1)*v(273)-a(2, 5)*v(279)-a(2, 4)*v(280)-a(2, 3)*v(281))/v(71)
      ai(2, 4) = (-(a(5, 1)*v(275))+a(5, 3)*v(282)+a(5, 4)*v(283)+a(5, 5)*v(284))/v(71)
      ai(2, 5) = (a(4, 1)*v(275)-a(4, 3)*v(282)-a(4, 4)*v(283)-a(4, 5)*v(284))/v(71)
      ai(3, 1) = v(168)/v(71)
      ai(3, 2) = (-(a(3, 1)*v(274))-a(3, 2)*v(281)+a(3, 5)*v(285)+a(3, 4)*v(286))/v(71)
      ai(3, 3) = (a(2, 1)*v(274)+a(2, 2)*v(281)-a(2, 5)*v(285)-a(2, 4)*v(286))/v(71)
      ai(3, 4) = (-(a(5, 1)*v(276))-a(5, 2)*v(282)+a(5, 4)*v(287)+a(5, 5)*v(288))/v(71)
      ai(3, 5) = (a(4, 1)*v(276)+a(4, 2)*v(282)-a(4, 4)*v(287)-a(4, 5)*v(288))/v(71)
      ai(4, 1) = -(v(198)/v(71))
      ai(4, 2) = (-(a(3, 1)*v(272))-a(3, 2)*v(280)-a(3, 3)*v(286)+a(3, 5)*v(289))/v(71)
      ai(4, 3) = (a(2, 1)*v(272)+a(2, 2)*v(280)+a(2, 3)*v(286)-a(2, 5)*v(289))/v(71)
      ai(4, 4) = (-(a(5, 1)*v(277))-a(5, 2)*v(283)-a(5, 3)*v(287)+a(5, 5)*v(290))/v(71)
      ai(4, 5) = (a(4, 1)*v(277)+a(4, 2)*v(283)+a(4, 3)*v(287)-a(4, 5)*v(290))/v(71)
      ai(5, 1) = v(216)/v(71)
      ai(5, 2) = (-(a(3, 1)*v(271))-a(3, 2)*v(279)-a(3, 3)*v(285)-a(3, 4)*v(289))/v(71)
      ai(5, 3) = (a(2, 1)*v(271)+a(2, 2)*v(279)+a(2, 3)*v(285)+a(2, 4)*v(289))/v(71)
      ai(5, 4) = (-(a(5, 1)*v(278))-a(5, 2)*v(284)-a(5, 3)*v(288)-a(5, 4)*v(290))/v(71)
      ai(5, 5) = (a(4, 1)*v(278)+a(4, 2)*v(284)+a(4, 3)*v(288)+a(4, 4)*v(290))/v(71)
      det = v(71)
      RETURN
    END IF
    IF (n .EQ. 6) THEN
      v(863) = a(1, 3)*a(3, 2)
      v(850) = a(3, 2)*a(4, 1)
      v(846) = a(1, 1)*a(2, 2)
      v(845) = a(2, 5)*a(3, 1)
      v(844) = a(1, 5)*a(2, 2)
      v(843) = a(2, 1)*a(3, 5)
      v(841) = a(3, 2)*a(5, 1)
      v(832) = a(1, 1)*a(4, 2)
      v(831) = a(1, 2)*a(4, 1)
      v(827) = a(2, 2)*a(5, 1)
      v(822) = a(4, 1)*a(5, 5)
      v(821) = a(4, 5)*a(5, 2)
      v(810) = a(1, 6)*a(4, 1)
      v(800) = a(2, 5)*a(5, 6)
      v(799) = a(1, 6)*a(2, 1)
      v(798) = a(2, 6)*a(3, 1)
      v(797) = a(1, 6)*a(3, 1)
      v(793) = a(1, 4)*a(2, 1)
      v(792) = a(2, 1)*a(4, 4)
      v(789) = a(2, 6)*a(4, 1)
      v(788) = a(2, 1)*a(5, 4)
      v(787) = a(2, 5)*a(4, 6)
      v(786) = a(2, 6)*a(5, 1)
      v(780) = a(3, 1)*a(4, 4)
      v(779) = a(3, 4)*a(4, 1)
      v(777) = a(3, 6)*a(4, 1)
      v(776) = a(3, 1)*a(5, 4)
      v(775) = a(4, 6)*a(5, 1)
      v(774) = a(3, 6)*a(5, 1)
      v(767) = a(3, 6)*a(4, 5)
      v(766) = a(3, 6)*a(4, 2)
      v(762) = a(1, 6)*a(4, 2)
      v(760) = a(1, 3)*a(2, 4)
      v(755) = a(3, 4)*a(4, 5)
      v(754) = a(2, 6)*a(4, 4)
      v(753) = a(2, 4)*a(3, 5)
      v(752) = a(3, 4)*a(4, 3)
      v(742) = a(1, 2)*a(5, 6)
      v(739) = a(2, 2)*a(5, 6)
      v(738) = a(3, 2)*a(5, 6)
      v(737) = a(2, 2)*a(3, 6)
      v(736) = a(1, 6)*a(2, 2)
      v(735) = a(3, 6)*a(5, 2)
      v(730) = a(2, 5)*a(3, 3)
      v(729) = a(1, 5)*a(2, 4)
      v(728) = a(2, 4)*a(5, 5)
      v(727) = a(2, 3)*a(5, 5)
      v(726) = a(1, 5)*a(2, 3)
      v(725) = a(3, 5)*a(5, 3)
      v(724) = a(3, 4)*a(5, 3)
      v(718) = a(1, 4)*a(2, 2)
      v(717) = a(2, 2)*a(4, 4)
      v(716) = a(2, 2)*a(5, 4)
      v(712) = a(1, 4)*a(2, 3)
      v(711) = a(1, 3)*a(4, 4)
      v(710) = a(1, 4)*a(4, 3)
      v(709) = a(1, 3)*a(2, 5)
      v(708) = a(2, 3)*a(5, 4)
      v(703) = a(1, 2)*a(3, 4)
      v(702) = a(4, 4)*a(5, 6)
      v(701) = a(1, 4)*a(4, 2)
      v(700) = a(3, 4)*a(4, 2)
      v(699) = a(1, 2)*a(3, 6)
      v(698) = a(4, 6)*a(5, 4)
      v(697) = a(1, 6)*a(3, 2)
      v(696) = a(4, 6)*a(5, 2)
      v(691) = a(1, 3)*a(3, 4)
      v(690) = a(1, 5)*a(3, 3)
      v(689) = a(1, 5)*a(3, 4)
      v(688) = a(4, 4)*a(5, 5)
      v(687) = a(4, 3)*a(5, 5)
      v(686) = a(1, 3)*a(5, 4)
      v(685) = a(3, 3)*a(4, 5)
      v(684) = a(4, 5)*a(5, 3)
      v(679) = a(4, 3)*a(5, 6)
      v(678) = a(1, 5)*a(4, 2)
      v(677) = a(1, 3)*a(5, 6)
      v(676) = a(1, 2)*a(4, 5)
      v(675) = a(4, 1)*a(5, 6)
      v(674) = a(1, 1)*a(5, 6)
      v(673) = a(1, 6)*a(5, 5)
      v(672) = a(4, 2)*a(5, 5)
      v(671) = a(1, 3)*a(4, 6)
      v(670) = a(1, 2)*a(5, 5)
      v(669) = a(1, 1)*a(5, 5)
      v(668) = a(2, 6)*a(5, 4)
      v(667) = a(4, 1)*a(5, 4)
      v(666) = a(4, 2)*a(5, 4)
      v(665) = a(4, 3)*a(5, 4)
      v(664) = a(1, 6)*a(5, 3)
      v(663) = a(4, 6)*a(5, 3)
      v(662) = -(a(4, 1)*a(5, 3))
      v(661) = a(1, 1)*a(5, 3)
      v(660) = a(1, 4)*a(5, 3)
      v(659) = a(4, 4)*a(5, 3)
      v(658) = a(1, 6)*a(5, 2)
      v(657) = a(1, 3)*a(5, 2)
      v(656) = a(1, 5)*a(5, 2)
      v(655) = a(1, 4)*a(2, 6)
      v(654) = a(1, 1)*a(2, 4)
      v(653) = a(4, 1)*a(5, 2)
      v(652) = a(1, 1)*a(5, 2)
      v(651) = a(1, 4)*a(5, 2)
      v(650) = a(4, 4)*a(5, 2)
      v(649) = a(1, 6)*a(5, 1)
      v(648) = a(4, 2)*a(5, 1)
      v(647) = -(a(1, 2)*a(5, 1))
      v(646) = a(1, 3)*a(2, 2)
      v(645) = a(1, 4)*a(5, 1)
      v(644) = a(2, 4)*a(4, 6)
      v(643) = a(2, 1)*a(3, 6)
      v(642) = a(1, 1)*a(3, 6)
      v(641) = a(1, 2)*a(2, 3)
      v(640) = a(2, 3)*a(3, 5)
      v(639) = a(2, 1)*a(3, 4)
      v(638) = a(1, 1)*a(3, 4)
      v(637) = a(2, 2)*a(3, 3)
      v(636) = a(2, 1)*a(3, 3)
      v(635) = a(1, 4)*a(3, 3)
      v(634) = a(1, 3)*a(2, 6)
      v(633) = a(1, 5)*a(3, 2)
      v(632) = a(2, 1)*a(3, 2)
      v(631) = a(1, 1)*a(3, 2)
      v(630) = a(1, 4)*a(3, 2)
      v(629) = a(2, 3)*a(3, 1)
      v(628) = a(2, 2)*a(3, 1)
      v(627) = a(1, 4)*a(3, 1)
      v(626) = a(2, 4)*a(3, 1)
      v(594) = -(a(1, 2)*v(626))
      v(593) = a(2, 2)*v(627)
      v(589) = a(1, 3)*v(626)
      v(588) = -(a(2, 3)*v(627))
      v(541) = a(1, 2)*v(629)
      v(540) = -(a(1, 3)*v(628))
      v(537) = a(1, 6)*v(628)
      v(534) = a(3, 1)*v(634)
      v(398) = a(1, 5)*v(629)
      v(397) = a(1, 6)*v(629)
      v(596) = a(2, 4)*v(631)
      v(595) = -(a(2, 1)*v(630))
      v(584) = a(2, 3)*v(630)
      v(543) = -(a(2, 3)*v(631))
      v(542) = a(1, 3)*v(632)
      v(531) = -(a(1, 6)*v(632))
      v(303) = a(2, 6)*v(633)
      v(285) = a(2, 3)*v(633)
      v(283) = -(a(3, 2)*v(634))
      v(281) = a(2, 3)*v(697)
      v(763) = v(281)+v(283)
      v(590) = a(2, 1)*v(635)
      v(585) = -(a(2, 2)*v(635))
      v(545) = a(1, 1)*v(637)
      v(544) = -(a(1, 2)*v(636))
      v(855) = v(540)+v(542)+v(544)+v(545)
      v(852) = v(541)+v(543)+v(855)
      v(401) = a(1, 5)*v(636)
      v(399) = -(a(1, 6)*v(636))
      v(814) = v(397)+v(399)
      v(304) = -(a(1, 6)*v(637))
      v(770) = v(304)+v(763)
      v(278) = a(1, 5)*v(637)
      v(598) = -(a(2, 2)*v(638))
      v(597) = a(1, 2)*v(639)
      v(866) = v(593)+v(594)+v(595)+v(596)+v(597)+v(598)
      v(592) = a(2, 3)*v(638)
      v(591) = -(a(1, 3)*v(639))
      v(865) = v(588)+v(589)+v(590)+v(591)+v(592)
      v(587) = -(a(3, 4)*v(641))
      v(864) = v(584)+v(585)+v(587)
      v(400) = a(1, 1)*v(640)
      v(802) = v(398)-v(400)-v(401)
      v(371) = a(3, 5)*v(655)
      v(282) = -(a(1, 6)*v(640))
      v(280) = a(3, 5)*v(641)
      v(757) = v(278)+v(280)-v(285)
      v(539) = -(a(2, 2)*v(642))
      v(538) = a(1, 2)*v(643)
      v(536) = a(2, 3)*v(642)
      v(535) = -(a(1, 3)*v(643))
      v(873) = -v(397)-v(399)+v(534)+v(535)+v(536)
      v(533) = -(a(3, 6)*v(641))
      v(872) = v(281)+v(304)+v(533)
      v(408) = -(a(3, 6)*v(709))
      v(586) = a(4, 1)*v(646)
      v(600) = a(3, 4)*v(586)+a(4, 4)*v(852)+a(4, 1)*(a(2, 4)*(a(1, 2)*a(3, 3)-v(863))+ &
      v(864))+a(4, 2)*(-(a(3, 3)*v(654))+v(865))+a(4, 3&
      &)*v(866)
      v(421) = a(4, 5)*v(630)
      v(403) = a(2, 6)*v(685)
      v(407) = a(3, 5)*v(644)
      v(405) = -(a(1, 5)*v(644))
      v(341) = -(a(3, 5)*v(671))
      v(563) = a(4, 4)*v(647)
      v(562) = -(a(4, 3)*v(645))
      v(561) = a(4, 2)*v(645)
      v(530) = a(5, 1)*v(646)
      v(517) = a(4, 6)*v(647)
      v(516) = -(a(4, 3)*v(647))
      v(515) = -(a(1, 3)*v(648))
      v(514) = a(1, 6)*v(648)
      v(473) = a(2, 4)*v(647)
      v(346) = -(a(4, 5)*v(649))
      v(344) = a(4, 3)*v(649)
      v(567) = a(1, 1)*v(650)
      v(566) = -(a(1, 3)*v(650))
      v(565) = a(4, 3)*v(651)
      v(564) = -(a(4, 1)*v(651))
      v(521) = a(4, 6)*v(652)
      v(520) = -(a(4, 3)*v(652))
      v(519) = a(1, 3)*v(653)
      v(518) = -(a(1, 6)*v(653))
      v(476) = a(5, 2)*v(654)
      v(830) = -v(473)-v(476)
      v(239) = a(5, 2)*v(655)
      v(232) = -(a(2, 6)*v(656))
      v(213) = a(4, 5)*v(657)
      v(205) = a(4, 3)*v(656)
      v(202) = -(a(4, 6)*v(657))
      v(194) = a(4, 3)*v(658)
      v(185) = a(4, 6)*v(656)
      v(180) = -(a(4, 5)*v(658))
      v(571) = -(a(1, 1)*v(659))
      v(570) = a(1, 2)*v(659)
      v(569) = -(a(4, 2)*v(660))
      v(568) = a(4, 1)*v(660)
      v(523) = a(4, 2)*v(661)
      v(522) = a(1, 2)*v(662)
      v(833) = v(515)+v(519)+v(522)+v(523)
      v(868) = v(516)+v(520)+v(833)
      v(354) = a(4, 6)*v(661)
      v(352) = -(a(4, 5)*v(661))
      v(349) = a(1, 6)*v(662)
      v(184) = -(a(1, 5)*v(663))
      v(183) = a(1, 2)*v(663)
      v(182) = a(5, 3)*v(678)
      v(179) = a(4, 5)*v(664)
      v(178) = -(a(4, 2)*v(664))
      v(177) = a(5, 3)*v(676)
      v(577) = a(1, 1)*v(665)
      v(576) = -(a(1, 2)*v(665))
      v(575) = -(a(1, 1)*v(666))
      v(574) = a(1, 3)*v(666)
      v(870) = v(565)+v(566)+v(569)+v(570)+v(574)+v(576)
      v(573) = a(1, 2)*v(667)
      v(869) = v(561)+v(563)+v(564)+v(567)+v(573)+v(575)
      v(572) = -(a(1, 3)*v(667))
      v(871) = v(562)+v(568)+v(571)+v(572)+v(577)
      v(614) = a(4, 4)*v(530)+a(4, 3)*v(830)+a(2, 4)*v(833)+a(2, 3)*v(869)+a(2, 1)*v(870)+&
      &a(2, 2)*v(871)
      v(377) = a(1, 1)*v(668)
      v(233) = a(1, 5)*v(668)
      v(231) = a(1, 2)*v(668)
      v(751) = -v(231)+v(239)
      v(383) = a(2, 6)*v(669)
      v(360) = -(a(4, 6)*v(669))
      v(358) = a(4, 3)*v(669)
      v(778) = -v(352)-v(358)
      v(355) = a(4, 1)*v(673)
      v(241) = -(a(5, 5)*v(655))
      v(745) = v(233)+v(241)
      v(240) = a(2, 6)*v(670)
      v(741) = v(232)+v(240)
      v(204) = -(a(4, 6)*v(670))
      v(203) = a(5, 5)*v(671)
      v(201) = a(1, 3)*v(672)
      v(196) = a(1, 6)*v(672)
      v(195) = -(a(4, 3)*v(673))
      v(193) = a(4, 3)*v(670)
      v(680) = -v(177)+v(182)+v(193)-v(201)-v(205)+v(213)
      v(525) = -(a(4, 2)*v(674))
      v(524) = a(1, 2)*v(675)
      v(875) = v(514)+v(517)+v(518)+v(521)+v(524)+v(525)
      v(366) = a(4, 5)*v(674)
      v(364) = -(a(4, 3)*v(674))
      v(362) = -(a(1, 5)*v(675))
      v(361) = a(1, 3)*v(675)
      v(790) = v(344)+v(349)+v(354)+v(361)+v(364)
      v(216) = a(5, 6)*v(676)
      v(215) = -(a(4, 5)*v(677))
      v(214) = a(4, 2)*v(677)
      v(208) = -(a(5, 6)*v(678))
      v(683) = v(180)+v(185)+v(196)+v(204)+v(208)+v(216)
      v(207) = a(1, 5)*v(679)
      v(682) = v(179)+v(184)+v(195)+v(203)+v(207)+v(215)
      v(206) = -(a(1, 2)*v(679))
      v(681) = v(178)+v(183)+v(194)+v(202)+v(206)+v(214)
      v(511) = a(2, 6)*v(680)+a(2, 5)*v(681)+a(2, 2)*v(682)+a(2, 3)*v(683)
      v(506) = a(3, 6)*v(680)+a(3, 5)*v(681)+a(3, 2)*v(682)+a(3, 3)*v(683)
      v(73) = -(a(5, 2)*a(6, 1))+a(5, 1)*a(6, 2)
      v(74) = -(a(5, 3)*a(6, 1))+a(5, 1)*a(6, 3)
      v(75) = -(a(5, 3)*a(6, 2))+a(5, 2)*a(6, 3)
      v(80) = a(4, 3)*v(73)-a(4, 2)*v(74)+a(4, 1)*v(75)
      v(76) = -(a(5, 4)*a(6, 1))+a(5, 1)*a(6, 4)
      v(77) = -(a(5, 4)*a(6, 2))+a(5, 2)*a(6, 4)
      v(85) = a(4, 4)*v(73)-a(4, 2)*v(76)+a(4, 1)*v(77)
      v(78) = -(a(5, 4)*a(6, 3))+a(5, 3)*a(6, 4)
      v(93) = a(4, 4)*v(75)-a(4, 3)*v(77)+a(4, 2)*v(78)
      v(89) = a(4, 4)*v(74)-a(4, 3)*v(76)+a(4, 1)*v(78)
      v(79) = -(a(3, 4)*v(80))+a(3, 3)*v(85)-a(3, 2)*v(89)+a(3, 1)*v(93)
      v(81) = -(a(5, 5)*a(6, 1))+a(5, 1)*a(6, 5)
      v(82) = -(a(5, 5)*a(6, 2))+a(5, 2)*a(6, 5)
      v(86) = a(4, 5)*v(73)-a(4, 2)*v(81)+a(4, 1)*v(82)
      v(83) = -(a(5, 5)*a(6, 3))+a(5, 3)*a(6, 5)
      v(94) = a(4, 5)*v(75)-a(4, 3)*v(82)+a(4, 2)*v(83)
      v(90) = a(4, 5)*v(74)-a(4, 3)*v(81)+a(4, 1)*v(83)
      v(84) = -(a(3, 5)*v(80))+a(3, 3)*v(86)-a(3, 2)*v(90)+a(3, 1)*v(94)
      v(87) = -(a(5, 5)*a(6, 4))+a(5, 4)*a(6, 5)
      v(96) = a(4, 5)*v(78)-a(4, 4)*v(83)+a(4, 3)*v(87)
      v(95) = a(4, 5)*v(77)-a(4, 4)*v(82)+a(4, 2)*v(87)
      v(91) = a(4, 5)*v(76)-a(4, 4)*v(81)+a(4, 1)*v(87)
      v(88) = -(a(3, 5)*v(85))+a(3, 4)*v(86)-a(3, 2)*v(91)+a(3, 1)*v(95)
      v(92) = -(a(3, 5)*v(89))+a(3, 4)*v(90)-a(3, 3)*v(91)+a(3, 1)*v(96)
      v(97) = -(a(3, 5)*v(93))+a(3, 4)*v(94)-a(3, 3)*v(95)+a(3, 2)*v(96)
      v(602) = a(2, 5)*v(79)-a(2, 4)*v(84)+a(2, 3)*v(88)-a(2, 2)*v(92)+a(2, 1)*v(97)
      v(98) = -(a(5, 6)*a(6, 1))+a(5, 1)*a(6, 6)
      v(99) = -(a(5, 6)*a(6, 2))+a(5, 2)*a(6, 6)
      v(102) = a(4, 6)*v(73)-a(4, 2)*v(98)+a(4, 1)*v(99)
      v(100) = -(a(5, 6)*a(6, 3))+a(5, 3)*a(6, 6)
      v(108) = a(4, 2)*v(100)+a(4, 6)*v(75)-a(4, 3)*v(99)
      v(105) = a(4, 1)*v(100)+a(4, 6)*v(74)-a(4, 3)*v(98)
      v(101) = a(3, 3)*v(102)-a(3, 2)*v(105)+a(3, 1)*v(108)-a(3, 6)*v(80)
      v(103) = -(a(5, 6)*a(6, 4))+a(5, 4)*a(6, 6)
      v(110) = -(a(4, 4)*v(100))+a(4, 3)*v(103)+a(4, 6)*v(78)
      v(109) = a(4, 2)*v(103)+a(4, 6)*v(77)-a(4, 4)*v(99)
      v(106) = a(4, 1)*v(103)+a(4, 6)*v(76)-a(4, 4)*v(98)
      v(104) = a(3, 4)*v(102)-a(3, 2)*v(106)+a(3, 1)*v(109)-a(3, 6)*v(85)
      v(107) = a(3, 4)*v(105)-a(3, 3)*v(106)+a(3, 1)*v(110)-a(3, 6)*v(89)
      v(111) = a(3, 4)*v(108)-a(3, 3)*v(109)+a(3, 2)*v(110)-a(3, 6)*v(93)
      v(550) = -(a(2, 4)*v(101))+a(2, 3)*v(104)-a(2, 2)*v(107)+a(2, 1)*v(111)+a(2, 6)*v(79)
      v(112) = -(a(5, 6)*a(6, 5))+a(5, 5)*a(6, 6)
      v(120) = -(a(4, 5)*v(103))+a(4, 4)*v(112)+a(4, 6)*v(87)
      v(117) = -(a(4, 5)*v(100))+a(4, 3)*v(112)+a(4, 6)*v(83)
      v(116) = a(4, 2)*v(112)+a(4, 6)*v(82)-a(4, 5)*v(99)
      v(114) = a(4, 1)*v(112)+a(4, 6)*v(81)-a(4, 5)*v(98)
      v(113) = a(3, 5)*v(102)-a(3, 2)*v(114)+a(3, 1)*v(116)-a(3, 6)*v(86)
      v(115) = a(3, 5)*v(105)-a(3, 3)*v(114)+a(3, 1)*v(117)-a(3, 6)*v(90)
      v(118) = a(3, 5)*v(108)-a(3, 3)*v(116)+a(3, 2)*v(117)-a(3, 6)*v(94)
      v(504) = -(a(2, 5)*v(101))+a(2, 3)*v(113)-a(2, 2)*v(115)+a(2, 1)*v(118)+a(2, 6)*v(84)
      v(119) = a(3, 5)*v(106)-a(3, 4)*v(114)+a(3, 1)*v(120)-a(3, 6)*v(91)
      v(121) = a(3, 5)*v(109)-a(3, 4)*v(116)+a(3, 2)*v(120)-a(3, 6)*v(95)
      v(415) = -(a(2, 5)*v(104))+a(2, 4)*v(113)-a(2, 2)*v(119)+a(2, 1)*v(121)+a(2, 6)*v(88)
      v(122) = a(3, 5)*v(110)-a(3, 4)*v(117)+a(3, 3)*v(120)-a(3, 6)*v(96)
      v(314) = -(a(2, 5)*v(107))+a(2, 4)*v(115)-a(2, 3)*v(119)+a(2, 1)*v(122)+a(2, 6)*v(92)
      v(124) = -(a(2, 5)*v(111))+a(2, 4)*v(118)-a(2, 3)*v(121)+a(2, 2)*v(122)+a(2, 6)*v(97)
      v(123) = a(1, 1)*v(124)-a(1, 2)*v(314)+a(1, 3)*v(415)-a(1, 4)*v(504)+a(1, 5)*v(550)-&
      &a(1, 6)*v(602)
      v(126) = a(3, 5)*v(659)
      v(127) = a(1, 5)*v(659)
      v(128) = a(3, 4)*v(684)
      v(129) = a(1, 4)*v(684)
      v(130) = a(5, 3)*v(689)
      v(131) = a(3, 5)*v(660)
      v(132) = a(3, 5)*v(665)
      v(133) = a(1, 5)*v(665)
      v(134) = a(5, 4)*v(685)
      v(135) = a(4, 5)*v(686)
      v(136) = a(5, 4)*v(690)
      v(137) = a(3, 5)*v(686)
      v(138) = a(3, 4)*v(687)
      v(139) = a(1, 4)*v(687)
      v(140) = a(3, 3)*v(688)
      v(692) = -v(126)+v(128)+v(132)-v(134)-v(138)+v(140)
      v(141) = a(1, 3)*v(688)
      v(693) = v(127)-v(129)-v(133)+v(135)+v(139)-v(141)
      v(142) = a(5, 5)*v(635)
      v(734) = v(136)-v(142)
      v(143) = a(5, 5)*v(691)
      v(694) = -v(130)+v(131)-v(137)+v(143)+v(734)
      v(144) = a(4, 3)*v(689)
      v(145) = a(3, 5)*v(710)
      v(146) = a(4, 4)*v(690)
      v(147) = a(3, 5)*v(711)
      v(148) = a(4, 5)*v(635)
      v(149) = a(4, 5)*v(691)
      v(695) = v(144)-v(145)-v(146)+v(147)+v(148)-v(149)
      v(756) = v(146)+v(695)
      v(605) = a(1, 1)*v(692)+a(3, 1)*v(693)+a(4, 1)*v(694)+a(5, 1)*v(695)
      v(604) = a(1, 2)*v(692)+a(3, 2)*v(693)+a(4, 2)*v(694)+a(5, 2)*v(695)
      v(316) = -(a(1, 6)*v(692))-a(3, 6)*v(693)-a(4, 6)*v(694)-a(5, 6)*v(695)
      v(150) = a(1, 6)*v(650)
      v(151) = a(3, 6)*v(650)
      v(152) = a(3, 4)*v(658)
      v(153) = a(3, 6)*v(651)
      v(154) = a(3, 4)*v(696)
      v(155) = a(1, 4)*v(696)
      v(156) = a(1, 6)*v(666)
      v(157) = a(3, 6)*v(666)
      v(158) = a(5, 4)*v(697)
      v(159) = a(5, 4)*v(699)
      v(842) = -v(153)+v(159)
      v(160) = a(3, 2)*v(698)
      v(161) = a(1, 2)*v(698)
      v(162) = a(1, 6)*v(700)
      v(163) = a(3, 6)*v(701)
      v(164) = a(4, 4)*v(697)
      v(165) = a(4, 4)*v(699)
      v(166) = a(4, 6)*v(630)
      v(167) = a(4, 6)*v(703)
      v(856) = v(165)-v(167)
      v(704) = -v(162)+v(163)+v(164)-v(165)-v(166)+v(167)
      v(168) = a(5, 6)*v(700)
      v(169) = a(5, 6)*v(701)
      v(170) = a(3, 2)*v(702)
      v(705) = v(151)-v(154)-v(157)+v(160)+v(168)-v(170)
      v(171) = a(1, 2)*v(702)
      v(706) = -v(150)+v(155)+v(156)-v(161)-v(169)+v(171)
      v(172) = a(5, 6)*v(630)
      v(173) = a(5, 6)*v(703)
      v(707) = v(152)-v(158)+v(172)-v(173)+v(842)
      v(552) = a(5, 3)*v(704)+a(1, 3)*v(705)+a(3, 3)*v(706)+a(4, 3)*v(707)
      v(417) = -(a(5, 5)*v(704))-a(1, 5)*v(705)-a(3, 5)*v(706)-a(4, 5)*v(707)
      v(175) = a(2, 5)*v(659)
      v(176) = -(a(2, 6)*v(659))
      v(181) = a(5, 3)*v(655)
      v(186) = a(2, 5)*v(660)
      v(187) = a(2, 5)*v(665)
      v(188) = a(2, 6)*v(665)
      v(719) = v(176)+v(188)
      v(189) = a(4, 5)*v(708)
      v(190) = -(a(5, 4)*v(634))
      v(720) = v(181)+v(190)
      v(191) = a(1, 5)*v(708)
      v(192) = a(5, 4)*v(709)
      v(197) = -(a(2, 6)*v(710))
      v(198) = a(2, 3)*v(688)
      v(713) = -v(175)+v(187)-v(189)+v(198)
      v(199) = a(2, 6)*v(711)
      v(721) = v(197)+v(199)
      v(200) = a(5, 5)*v(712)
      v(714) = v(186)+v(191)-v(192)-v(200)
      v(209) = a(2, 5)*v(710)
      v(210) = a(4, 4)*v(726)
      v(211) = a(4, 4)*v(709)
      v(212) = a(4, 5)*v(712)
      v(715) = -v(209)-v(210)+v(211)+v(212)
      v(610) = -(a(2, 4)*v(680))+a(2, 2)*v(693)+a(1, 2)*v(713)+a(4, 2)*v(714)+a(5, 2)*v(715)
      v(343) = -(a(2, 4)*v(682))-a(1, 6)*v(713)-a(4, 6)*v(714)-a(5, 6)*v(715)+a(1, 5)*v(719)+&
      &a(4, 5)*v(720)+a(5, 5)*v(721)
      v(217) = a(1, 6)*v(716)
      v(218) = a(4, 6)*v(716)
      v(219) = a(1, 6)*v(717)
      v(220) = a(4, 6)*v(718)
      v(722) = v(219)-v(220)
      v(221) = a(5, 6)*v(717)
      v(723) = v(218)-v(221)
      v(222) = a(5, 6)*v(718)
      v(744) = -v(217)+v(222)
      v(557) = a(2, 4)*v(681)+a(2, 3)*v(706)+a(1, 2)*v(719)+a(4, 2)*v(720)+a(5, 2)*v(721)+ &
               a(5, 3)*v(722)+a(1, 3)*v(723)+a(4, 3)*v(744)
      v(444) = a(2, 4)*v(683)-a(2, 5)*v(706)-a(5, 5)*v(722)-a(1, 5)*v(723)+a(4, 4)*v(741)+ &
      a(4, 2)*v(745)+a(4, 5)*(v(217)-v(222)+v(751)&
      &)
      v(224) = a(2, 5)*v(724)
      v(225) = -(a(2, 6)*v(724))
      v(226) = a(2, 4)*v(725)
      v(227) = a(2, 6)*v(725)
      v(228) = a(5, 3)*v(729)
      v(229) = -(a(3, 6)*v(660))
      v(230) = a(5, 4)*v(730)
      v(234) = a(5, 4)*v(640)
      v(235) = a(3, 5)*v(634)
      v(816) = v(235)+v(282)+v(408)
      v(236) = a(3, 6)*v(726)
      v(768) = v(235)+v(236)+v(282)
      v(237) = a(3, 6)*v(686)
      v(743) = v(229)+v(237)
      v(238) = a(3, 3)*v(728)
      v(242) = a(3, 4)*v(727)
      v(731) = -v(224)+v(226)+v(230)-v(234)-v(238)+v(242)
      v(243) = a(3, 4)*v(634)
      v(244) = a(3, 6)*v(727)
      v(740) = v(227)+v(244)
      v(245) = a(1, 3)*v(728)
      v(732) = -v(228)+v(245)+v(714)
      v(246) = a(3, 3)*v(729)
      v(247) = a(1, 4)*v(730)
      v(248) = a(2, 3)*v(689)
      v(249) = a(2, 5)*v(691)
      v(250) = a(1, 4)*v(640)
      v(251) = a(1, 3)*v(753)
      v(733) = v(246)-v(247)-v(248)+v(249)+v(250)-v(251)
      v(617) = -(a(2, 1)*v(694))+a(1, 1)*v(731)+a(3, 1)*v(732)+a(5, 1)*v(733)
      v(616) = -(a(2, 2)*v(694))+a(1, 2)*v(731)+a(3, 2)*v(732)+a(5, 2)*v(733)
      v(368) = a(1, 5)*v(225)+a(5, 4)*(-v(235)-v(236))+a(5, 5)*v(243)+a(3, 6)*(v(228)-v(245))- &
      a(1, 6)*v(731)-a(5, 6)*v(733)+a(2, 6)*v&
      &(734)+a(1, 4)*v(740)+a(2, 5)*v(743)
      v(252) = a(2, 5)*v(658)
      v(253) = a(2, 4)*v(658)
      v(254) = a(2, 4)*v(735)
      v(255) = -(a(2, 5)*v(735))
      v(256) = -(a(2, 5)*v(697))
      v(257) = a(3, 5)*v(736)
      v(258) = -(a(1, 5)*v(737))
      v(259) = a(2, 5)*v(699)
      v(746) = v(256)+v(257)+v(258)+v(259)
      v(769) = v(303)+v(746)
      v(260) = a(2, 4)*v(697)
      v(261) = a(3, 4)*v(736)
      v(262) = a(5, 5)*v(737)
      v(263) = a(2, 4)*v(699)
      v(747) = -v(260)+v(261)+v(263)
      v(264) = a(2, 4)*v(738)
      v(265) = a(2, 5)*v(738)
      v(266) = a(3, 4)*v(739)
      v(748) = -v(254)+v(264)-v(266)
      v(267) = -(a(2, 5)*v(742))
      v(749) = v(252)+v(267)+v(741)
      v(268) = -(a(3, 5)*v(739))
      v(750) = v(255)+v(262)+v(265)+v(268)
      v(527) = -(a(1, 2)*v(740))+a(3, 3)*v(749)+a(1, 3)*v(750)+a(5, 6)*v(757)+a(5, 2)*v(768)+ &
               a(5, 3)*v(769)+a(5, 5)*v(770)
      v(269) = a(2, 4)*v(742)
      v(828) = v(269)+v(751)
      v(579) = a(1, 2)*v(225)+a(5, 2)*v(243)-a(2, 3)*v(707)+a(3, 2)*v(720)+a(2, 2)*v(743)+a(3, 3)* &
      (v(231)-v(239)+v(253)-v(269)+v(744)&
      &)+a(5, 3)*v(747)+a(1, 3)*v(748)
      v(468) = a(3, 2)*v(745)+a(5, 4)*v(746)-a(5, 5)*v(747)-a(1, 5)*v(748)+a(3, 4)*v(749)+a(1, 4)* &
               v(750)+a(3, 5)*(-v(253)+v(828))
      v(271) = a(2, 5)*v(752)
      v(272) = -(a(2, 6)*v(752))
      v(273) = a(4, 3)*v(753)
      v(274) = a(2, 6)*a(3, 5)*a(4, 3)
      v(275) = a(4, 3)*v(729)
      v(276) = -(a(3, 6)*v(710))
      v(277) = a(4, 4)*v(730)
      v(279) = a(3, 3)*v(754)
      v(284) = -(a(3, 5)*v(754))
      v(286) = a(3, 6)*v(711)
      v(287) = a(2, 4)*v(685)
      v(288) = -(a(2, 6)*v(635))
      v(803) = v(243)+v(288)
      v(289) = a(2, 3)*v(755)
      v(758) = -v(271)+v(273)+v(277)-v(287)+v(289)
      v(290) = a(2, 6)*v(755)
      v(773) = v(284)+v(290)
      v(820) = v(407)+v(773)
      v(291) = a(2, 3)*v(767)
      v(292) = a(4, 5)*v(760)
      v(761) = v(275)-v(292)
      v(759) = v(209)-v(211)-v(212)-v(275)+v(292)
      v(611) = a(2, 1)*v(693)+a(1, 1)*v(713)+a(4, 1)*v(732)+a(5, 1)*(v(715)+v(761))+a(2, 4)*v(778)
      v(413) = a(4, 1)*v(733)+a(2, 1)*v(756)+a(1, 1)*v(758)+a(3, 1)*v(759)+a(4, 4)*v(802)
      v(312) = a(4, 2)*v(733)+a(2, 2)*v(756)-a(4, 4)*v(757)+a(1, 2)*v(758)+a(3, 2)*v(759)
      v(293) = -(a(3, 3)*v(644))
      v(764) = v(272)+v(279)+v(293)
      v(294) = a(4, 6)*v(730)
      v(815) = -v(274)-v(291)-v(294)+v(403)
      v(295) = a(4, 6)*v(689)
      v(296) = -(a(3, 4)*v(671))
      v(765) = v(276)+v(286)+v(296)
      v(297) = a(4, 6)*v(760)
      v(812) = -v(297)+v(721)
      v(308) = -(a(4, 6)*v(250))+a(4, 4)*(-v(236)-v(282))+a(4, 5)*v(288)+a(1, 4)*(v(274)+v(291)+ &
      v(294))+a(2, 3)*v(295)+a(3, 5)*v(297)&
      &-a(1, 6)*v(758)+a(3, 6)*v(761)+a(1, 5)*v(764)+a(2, 5)*v(765)+a(1, 3)*v(773)
      v(298) = a(2, 5)*v(762)
      v(299) = -(a(2, 6)*v(678))
      v(771) = v(298)+v(299)
      v(300) = a(2, 4)*v(762)
      v(301) = a(2, 4)*v(766)
      v(311) = a(3, 2)*(-v(197)+v(297))+a(3, 3)*(-v(219)+v(220)+v(300))-a(1, 3)*v(301)+a(2, 3)*(-v(164)+ &
      v(704))+a(4, 3)*v(747)+a(4, 4&
      &)*v(763)+a(1, 2)*v(764)+a(2, 2)*v(765)+a(4, 2)*v(803)
      v(302) = -(a(2, 5)*v(766))
      v(305) = a(2, 2)*v(767)
      v(306) = a(3, 2)*v(787)
      v(772) = v(302)+v(305)+v(306)
      v(310) = a(2, 2)*v(341)+a(4, 6)*v(757)+a(4, 2)*v(768)+a(4, 3)*v(769)+a(4, 5)*v(770)+a(3, 3)*v(771)+ &
               a(1, 3)*v(772)+a(1, 2)*v(815)
      v(309) = -(a(2, 5)*v(167))+a(2, 2)*v(295)+a(3, 5)*(-v(220)-v(300))+a(1, 5)*v(301)+a(4, 2)*v(371)+ &
      a(3, 2)*v(405)-a(2, 6)*v(421)-a&
      &(4, 5)*v(747)+a(4, 4)*v(769)+a(3, 4)*v(771)+a(1, 4)*v(772)+a(1, 2)*v(820)
      v(317) = a(4, 4)*v(649)
      v(318) = -(a(1, 5)*v(774))
      v(319) = a(3, 4)*v(649)
      v(320) = a(4, 5)*v(774)
      v(321) = a(1, 5)*v(775)
      v(791) = v(321)+v(346)+v(355)+v(360)+v(362)+v(366)
      v(322) = a(1, 4)*v(775)
      v(323) = a(1, 6)*v(667)
      v(324) = a(1, 5)*v(777)
      v(325) = a(1, 6)*v(776)
      v(326) = a(5, 4)*v(642)
      v(327) = a(4, 6)*v(776)
      v(328) = a(1, 1)*v(698)
      v(329) = a(1, 6)*v(779)
      v(330) = -(a(5, 5)*v(777))
      v(781) = v(320)+v(330)
      v(507) = a(4, 3)*v(318)+a(5, 3)*v(324)+a(5, 1)*v(341)+a(3, 1)*v(682)-a(3, 6)*v(778)+ &
               a(1, 3)*v(781)+a(3, 5)*v(790)+a(3, 3)*v(791)
      v(331) = a(1, 6)*v(780)
      v(332) = a(4, 4)*v(642)
      v(333) = a(4, 6)*v(627)
      v(334) = a(4, 6)*v(638)
      v(782) = -v(329)+v(331)-v(332)-v(333)+v(334)
      v(335) = a(5, 6)*v(779)
      v(336) = a(1, 4)*v(675)
      v(337) = a(5, 6)*v(780)
      v(783) = v(327)+v(335)-v(337)
      v(338) = a(4, 4)*v(674)
      v(784) = -v(317)+v(322)+v(323)-v(328)-v(336)+v(338)
      v(339) = a(5, 6)*v(627)
      v(340) = a(5, 6)*v(638)
      v(785) = v(319)-v(325)+v(326)+v(339)-v(340)
      v(553) = -(a(4, 1)*v(743))+a(5, 1)*v(765)+a(5, 3)*v(782)+a(1, 3)*v(783)+a(3, 3)*v(784)+a(4, 3)*v(785)
      v(418) = a(4, 4)*v(318)+a(3, 4)*v(321)+a(5, 4)*v(324)+a(1, 4)*v(781)-a(5, 5)*v(782)- &
               a(1, 5)*v(783)-a(3, 5)*v(784)-a(4, 5)*v(785)
      v(345) = -(a(1, 5)*v(786))
      v(801) = v(345)+v(383)
      v(347) = a(4, 5)*v(786)
      v(348) = -(a(5, 1)*v(787))
      v(350) = a(1, 5)*v(789)
      v(351) = a(1, 6)*v(788)
      v(353) = a(4, 6)*v(788)
      v(356) = -(a(5, 5)*v(789))
      v(794) = v(347)+v(348)+v(356)
      v(512) = a(4, 3)*v(345)+a(5, 3)*v(350)+a(2, 1)*v(682)-a(2, 6)*v(778)+a(2, 5)*v(790)+&
      &a(2, 3)*v(791)+a(1, 3)*v(794)
      v(357) = a(1, 6)*v(792)
      v(359) = a(4, 6)*v(793)
      v(795) = v(357)-v(359)
      v(363) = a(5, 6)*v(792)
      v(796) = v(353)-v(363)
      v(365) = a(5, 6)*v(793)
      v(804) = -v(351)+v(365)
      v(558) = a(1, 1)*v(719)+a(4, 1)*v(720)+a(2, 3)*v(784)+a(2, 4)*v(790)+a(5, 3)*v(795)+a(1, 3)*v(796)+ &
               a(4, 3)*v(804)+a(5, 1)*v(812)
      v(445) = a(2, 5)*(v(317)-v(323)+v(328)+v(336)-v(338))+a(5, 4)*v(350)+ &
      a(4, 5)*(v(351)-v(365)-v(377))+a(2, 4)*v(791)+a(1, 4)*v&
      &(794)-a(5, 5)*v(795)-a(1, 5)*v(796)+a(4, 4)*v(801)
      v(369) = a(2, 5)*v(649)
      v(370) = a(2, 4)*v(649)
      v(372) = a(2, 4)*v(774)
      v(373) = a(3, 6)*v(645)
      v(811) = -v(319)+v(325)-v(326)-v(339)+v(340)+v(373)
      v(374) = -(a(2, 5)*v(797))
      v(375) = a(1, 5)*v(798)
      v(376) = a(3, 5)*v(799)
      v(378) = -(a(1, 5)*v(643))
      v(379) = a(2, 5)*v(642)
      v(805) = v(374)+v(375)+v(376)+v(378)+v(379)
      v(380) = a(2, 4)*v(797)
      v(381) = -(a(5, 5)*v(798))
      v(382) = a(3, 4)*v(799)
      v(384) = a(5, 5)*v(643)
      v(385) = a(2, 4)*v(642)
      v(806) = -v(380)+v(382)+v(385)
      v(386) = a(5, 6)*v(626)
      v(387) = a(3, 1)*v(800)
      v(388) = a(5, 6)*v(639)
      v(807) = -v(372)+v(386)-v(388)
      v(389) = -(a(1, 1)*v(800))
      v(808) = v(369)+v(389)+v(801)
      v(390) = -(a(5, 6)*v(843))
      v(809) = v(381)+v(384)+v(387)+v(390)
      v(528) = -(a(2, 3)*v(318))-a(1, 1)*v(740)-a(5, 6)*v(802)+a(5, 3)*v(805)+a(3, 3)*v(808)+&
      &a(1, 3)*v(809)+a(5, 5)*v(814)+a(5, 1)*v&
      &(816)
      v(391) = a(5, 6)*v(654)
      v(829) = v(370)+v(377)-v(391)+v(804)
      v(580) = a(1, 1)*v(225)+a(3, 1)*v(720)+a(2, 1)*v(743)+a(5, 1)*v(803)+a(5, 3)*v(806)+a(1, 3)*v(807)+&
      &a(2, 3)*v(811)+a(3, 3)*v(829)
      v(469) = a(5, 1)*v(371)-a(2, 5)*v(373)+a(3, 5)*(-v(370)-v(377)+v(391))+a(5, 4)*v(805)-a(5, 5)*v(806)-a(1, 5)*v(807)+a(3, 4)*v&
      &(808)+a(1, 4)*v(809)
      v(393) = a(2, 5)*v(810)
      v(817) = -v(350)+v(393)
      v(394) = a(2, 4)*v(810)
      v(857) = -v(394)+v(795)
      v(818) = v(359)+v(394)
      v(395) = a(2, 4)*v(777)
      v(396) = a(1, 4)*v(777)
      v(813) = v(396)+v(782)
      v(554) = -(a(4, 1)*v(159))+a(1, 2)*v(783)+a(3, 2)*v(784)-a(4, 2)*v(811)+a(5, 2)*v(813)+a(5, 1)*v(856)
      v(412) = -(a(1, 3)*v(395))+a(1, 1)*v(764)+a(2, 1)*v(765)+a(4, 1)*v(803)+a(4, 3)*v(806)-a(3, 1)*v(812)+&
      &a(2, 3)*(-v(331)+v(813))+a&
      &(4, 4)*v(814)+a(3, 3)*v(818)
      v(402) = -(a(4, 5)*v(798))
      v(404) = a(4, 5)*v(643)
      v(406) = a(3, 1)*v(787)
      v(819) = v(402)+v(404)+v(406)
      v(411) = a(2, 3)*v(324)+a(2, 1)*v(341)-a(4, 6)*v(802)+a(4, 3)*v(805)+a(4, 5)*v(814)+a(1, 1)*v(815)+&
      &a(4, 1)*v(816)+a(3, 3)*v(817)&
      &+a(1, 3)*v(819)
      v(410) = a(2, 1)*v(295)+a(4, 1)*v(371)+a(1, 5)*v(395)+a(2, 5)*(-v(334)-v(396))+a(3, 1)*v(405)+&
      &a(4, 4)*v(805)-a(4, 5)*v(806)+a(3&
      &, 4)*v(817)-a(3, 5)*v(818)+a(1, 4)*v(819)+a(1, 1)*v(820)
      v(419) = a(3, 5)*v(648)
      v(420) = a(1, 5)*v(648)
      v(422) = -(a(4, 5)*v(647))
      v(423) = a(5, 1)*v(633)
      v(424) = -(a(3, 5)*v(647))
      v(425) = a(3, 5)*v(653)
      v(426) = a(1, 5)*v(653)
      v(427) = a(3, 1)*v(821)
      v(428) = a(1, 1)*v(821)
      v(429) = a(3, 1)*v(656)
      v(430) = a(3, 5)*v(652)
      v(431) = a(3, 2)*v(822)
      v(432) = a(1, 2)*v(822)
      v(433) = a(3, 1)*v(672)
      v(823) = -v(419)+v(425)-v(427)-v(431)+v(433)
      v(434) = a(4, 2)*v(669)
      v(824) = v(420)-v(422)-v(426)+v(428)+v(432)-v(434)
      v(435) = a(3, 1)*v(670)
      v(436) = a(5, 5)*v(631)
      v(847) = v(429)-v(435)+v(436)
      v(825) = -v(423)+v(424)-v(430)+v(847)
      v(437) = a(4, 1)*v(633)
      v(438) = a(3, 5)*v(831)
      v(439) = a(3, 1)*v(678)
      v(440) = a(3, 5)*v(832)
      v(851) = v(437)-v(438)-v(439)+v(440)
      v(441) = a(3, 1)*v(676)
      v(442) = a(4, 5)*v(631)
      v(826) = v(441)-v(442)+v(851)
      v(606) = a(5, 1)*v(421)+a(1, 4)*v(823)+a(3, 4)*v(824)+a(4, 4)*v(825)+a(5, 4)*v(826)
      v(508) = a(3, 2)*v(346)-a(1, 6)*v(823)-a(3, 6)*v(824)-a(4, 6)*v(825)-a(5, 6)*v(826)
      v(446) = a(2, 5)*v(648)
      v(447) = -(a(2, 6)*v(648))
      v(448) = a(4, 5)*v(827)
      v(449) = -(a(2, 6)*v(647))
      v(450) = a(1, 5)*v(827)
      v(451) = -(a(2, 5)*v(647))
      v(452) = a(2, 5)*v(653)
      v(453) = a(2, 1)*v(821)
      v(454) = -(a(2, 6)*v(652))
      v(837) = v(449)+v(454)
      v(559) = a(1, 4)*v(447)+a(2, 2)*v(784)+a(1, 2)*v(796)+a(4, 1)*v(828)+a(4, 2)*v(829)-&
      &a(4, 6)*v(830)+a(4, 4)*v(837)+a(5, 2)*v(857)
      v(455) = a(2, 1)*v(656)
      v(456) = a(2, 5)*v(652)
      v(457) = a(2, 2)*v(822)
      v(458) = a(2, 1)*v(672)
      v(834) = v(448)-v(453)-v(457)+v(458)
      v(838) = -v(446)+v(452)+v(834)
      v(459) = a(2, 1)*v(670)
      v(460) = a(2, 2)*v(669)
      v(835) = -v(450)+v(455)-v(459)+v(460)
      v(839) = v(451)-v(456)+v(835)
      v(461) = a(4, 1)*v(844)
      v(462) = a(2, 5)*v(831)
      v(463) = a(2, 1)*v(678)
      v(464) = a(2, 5)*v(832)
      v(465) = a(2, 1)*v(676)
      v(466) = a(4, 5)*v(846)
      v(836) = v(461)-v(463)+v(465)-v(466)
      v(840) = -v(462)+v(464)+v(836)
      v(613) = a(2, 3)*v(824)+a(1, 3)*v(834)+a(4, 3)*v(835)+a(5, 3)*v(836)+a(2, 5)*v(868)
      v(612) = a(2, 4)*v(824)+a(1, 4)*v(838)+a(4, 4)*v(839)+a(5, 4)*v(840)
      v(513) = a(2, 6)*(v(426)-v(432)+v(434))+a(1, 5)*v(447)+a(4, 5)*v(837)-a(1, 6)*v(838)-a(4, 6)*v(839)-a(5, 6)*v(840)
      v(470) = a(2, 5)*v(841)
      v(471) = -(a(2, 6)*v(841))
      v(581) = a(1, 1)*v(254)-a(5, 1)*v(263)+a(1, 4)*v(471)+a(3, 1)*v(751)+a(5, 2)*(-&
      &v(385)+v(806))+a(1, 2)*(v(372)+v(807))+a(2, 2)*v&
      &(811)+a(3, 2)*v(829)+a(3, 4)*v(837)+a(2, 1)*v(842)
      v(472) = a(3, 5)*v(827)
      v(474) = a(5, 2)*v(845)
      v(475) = a(5, 2)*v(843)
      v(477) = a(5, 5)*v(628)
      v(478) = a(5, 5)*v(632)
      v(867) = v(474)-v(475)-v(477)+v(478)
      v(848) = -v(470)+v(472)+v(867)
      v(479) = a(3, 1)*v(844)
      v(480) = a(1, 2)*v(845)
      v(481) = a(1, 5)*v(632)
      v(482) = a(2, 5)*v(631)
      v(483) = a(1, 2)*v(843)
      v(484) = a(3, 5)*v(846)
      v(849) = v(479)-v(480)-v(481)+v(482)+v(483)-v(484)
      v(618) = a(2, 4)*(v(423)-v(429)+v(435)-v(436))-a(3, 5)*v(830)+a(3, 4)*v(839)+a(1, 4)*v(848)+a(5, 4)*v(849)
      v(529) = a(1, 5)*v(471)+a(3, 5)*v(837)-a(3, 6)*v(839)+a(2, 6)*v(847)-a(1, 6)*v(848)-a(5, 6)*v(849)
      v(486) = a(2, 5)*v(850)
      v(487) = -(a(2, 6)*v(850))
      v(488) = a(2, 2)*a(3, 5)*a(4, 1)
      v(489) = a(1, 2)*v(789)
      v(490) = a(4, 2)*v(845)
      v(491) = a(4, 2)*v(798)
      v(860) = v(487)+v(491)
      v(492) = a(4, 2)*v(843)
      v(853) = -v(486)+v(488)+v(490)-v(492)
      v(548) = a(3, 3)*(-v(461)+v(462)+v(463)-v(464))+a(4, 3)*v(849)+a(2, 3)*v(851)+a(4, 5)*v(852)+a(1, 3)*v(853)
      v(493) = -(a(2, 6)*v(832))
      v(854) = -v(489)-v(493)
      v(560) = a(1, 3)*v(447)+a(2, 6)*v(519)+a(4, 6)*v(530)+a(2, 1)*v(681)-a(2, 2)*v(790)+&
      &a(4, 3)*v(837)+a(5, 3)*v(854)+a(2, 3)*v(875)
      v(494) = a(4, 5)*v(628)
      v(861) = -v(494)+v(853)
      v(502) = a(2, 1)*v(421)+a(2, 4)*v(826)-a(3, 4)*v(840)+a(4, 4)*v(849)+a(1, 4)*v(861)
      v(495) = -(a(1, 2)*v(798))
      v(496) = a(2, 6)*v(631)
      v(858) = v(495)+v(496)
      v(862) = v(531)+v(858)
      v(874) = v(537)+v(538)+v(539)+v(862)
      v(497) = a(1, 2)*a(3, 1)*a(4, 6)
      v(498) = -(a(4, 6)*v(631))
      v(859) = v(497)+v(498)
      v(547) = a(1, 3)*v(487)+a(3, 6)*v(586)-a(3, 3)*v(854)+a(4, 6)*v(855)+a(2, 3)*v(859)+&
      &a(4, 1)*v(872)+a(4, 2)*v(873)+a(4, 3)*v(874)
      v(501) = -(a(1, 2)*v(395))+a(4, 2)*v(806)+a(2, 2)*v(813)-a(3, 4)*v(854)+&
      &a(2, 1)*(-v(163)+v(856))-a(3, 2)*v(857)+a(4, 4)*v(858)+a&
      &(2, 4)*v(859)+a(1, 4)*v(860)
      v(500) = a(4, 6)*(-v(479)+v(481)-v(483)+v(484))+a(3, 6)*v(840)-a(3, 5)*&
      &v(854)+a(2, 5)*v(859)+a(1, 5)*v(860)-a(1, 6)*v(861)+a(4&
      &, 5)*v(862)
      v(509) = a(5, 1)*v(863)
      v(620) = -(a(2, 4)*v(509))+a(3, 4)*v(530)+a(3, 3)*v(830)+a(5, 4)*v(852)+a(5, 1)*v(864)+a(5, 2)*v(865)+a(5, 3)*v(866)
      v(619) = -(a(2, 5)*v(509))+a(3, 5)*v(530)-a(2, 3)*v(825)+a(3, 3)*v(839)+a(5, 3)*v(849)+a(1, 3)*v(867)
      v(608) = a(4, 4)*v(509)+a(3, 4)*v(868)+a(3, 3)*v(869)+a(3, 1)*v(870)+a(3, 2)*v(871)
      v(607) = a(4, 5)*v(509)+a(1, 3)*v(823)+a(3, 3)*v(824)+a(4, 3)*v(825)+a(5, 3)*v(826)
      v(582) = -(a(2, 6)*v(509))+a(3, 6)*v(530)+a(3, 3)*v(837)+a(5, 6)*v(852)+a(5, 1)*v(872)+a(5, 2)*v(873)+a(5, 3)*v(874)
      v(555) = a(3, 2)*(-v(344)-v(349)-v(361)-v(364))+a(4, 6)*v(509)+a(3, 1)*(-v(183)+v(681))+a(5, 3)*v(859)+a(3, 6)*v(868)+a(3, 3)*v&
      &(875)
      ai(1, 1) = v(124)/v(123)
      ai(1, 2) = (-(a(6, 2)*v(316))+a(6, 3)*v(417)-a(6, 4)*v(506)+a(6, 5)*v(552)-a(6, 6)*v(604))/v(123)
      ai(1, 3) = (a(6, 2)*v(343)-a(6, 3)*v(444)+a(6, 4)*v(511)-a(6, 5)*v(557)+a(6, 6)*v(610))/v(123)
      ai(1, 4) = (-(a(6, 2)*v(368))+a(6, 3)*v(468)-a(6, 4)*v(527)+a(6, 5)*v(579)-a(6, 6)*v(616))/v(123)
      ai(1, 5) = (a(6, 2)*v(308)-a(6, 3)*v(309)+a(6, 4)*v(310)-a(6, 5)*v(311)+a(6, 6)*v(312))/v(123)
      ai(1, 6) = (-(a(5, 2)*v(308))+a(5, 3)*v(309)-a(5, 4)*v(310)+a(5, 5)*v(311)-a(5, 6)*v(312))/v(123)
      ai(2, 1) = -(v(314)/v(123))
      ai(2, 2) = (a(6, 1)*v(316)-a(6, 3)*v(418)+a(6, 4)*v(507)-a(6, 5)*v(553)+a(6, 6)*v(605))/v(123)
      ai(2, 3) = (-(a(6, 1)*v(343))+a(6, 3)*v(445)-a(6, 4)*v(512)+a(6, 5)*v(558)-a(6, 6)*v(611))/v(123)
      ai(2, 4) = (a(6, 1)*v(368)-a(6, 3)*v(469)+a(6, 4)*v(528)-a(6, 5)*v(580)+a(6, 6)*v(617))/v(123)
      ai(2, 5) = (-(a(6, 1)*v(308))+a(6, 3)*v(410)-a(6, 4)*v(411)+a(6, 5)*v(412)-a(6, 6)*v(413))/v(123)
      ai(2, 6) = (a(5, 1)*v(308)-a(5, 3)*v(410)+a(5, 4)*v(411)-a(5, 5)*v(412)+a(5, 6)*v(413))/v(123)
      ai(3, 1) = v(415)/v(123)
      ai(3, 2) = (-(a(6, 1)*v(417))+a(6, 2)*v(418)-a(6, 4)*v(508)+a(6, 5)*v(554)-a(6, 6)*v(606))/v(123)
      ai(3, 3) = (a(6, 1)*v(444)-a(6, 2)*v(445)+a(6, 4)*v(513)-a(6, 5)*v(559)+a(6, 6)*v(612))/v(123)
      ai(3, 4) = (-(a(6, 1)*v(468))+a(6, 2)*v(469)-a(6, 4)*v(529)+a(6, 5)*v(581)-a(6, 6)*v(618))/v(123)
      ai(3, 5) = (a(6, 1)*v(309)-a(6, 2)*v(410)+a(6, 4)*v(500)-a(6, 5)*v(501)+a(6, 6)*v(502))/v(123)
      ai(3, 6) = (-(a(5, 1)*v(309))+a(5, 2)*v(410)-a(5, 4)*v(500)+a(5, 5)*v(501)-a(5, 6)*v(502))/v(123)
      ai(4, 1) = -(v(504)/v(123))
      ai(4, 2) = (a(6, 1)*v(506)-a(6, 2)*v(507)+a(6, 3)*v(508)-a(6, 5)*v(555)+a(6, 6)*v(607))/v(123)
      ai(4, 3) = (-(a(6, 1)*v(511))+a(6, 2)*v(512)-a(6, 3)*v(513)+a(6, 5)*v(560)-a(6, 6)*v(613))/v(123)
      ai(4, 4) = (a(6, 1)*v(527)-a(6, 2)*v(528)+a(6, 3)*v(529)-a(6, 5)*v(582)+a(6, 6)*v(619))/v(123)
      ai(4, 5) = (-(a(6, 1)*v(310))+a(6, 2)*v(411)-a(6, 3)*v(500)+a(6, 5)*v(547)-a(6, 6)*v(548))/v(123)
      ai(4, 6) = (a(5, 1)*v(310)-a(5, 2)*v(411)+a(5, 3)*v(500)-a(5, 5)*v(547)+a(5, 6)*v(548))/v(123)
      ai(5, 1) = v(550)/v(123)
      ai(5, 2) = (-(a(6, 1)*v(552))+a(6, 2)*v(553)-a(6, 3)*v(554)+a(6, 4)*v(555)-a(6, 6)*v(608))/v(123)
      ai(5, 3) = (a(6, 1)*v(557)-a(6, 2)*v(558)+a(6, 3)*v(559)-a(6, 4)*v(560)+a(6, 6)*v(614))/v(123)
      ai(5, 4) = (-(a(6, 1)*v(579))+a(6, 2)*v(580)-a(6, 3)*v(581)+a(6, 4)*v(582)-a(6, 6)*v(620))/v(123)
      ai(5, 5) = (a(6, 1)*v(311)-a(6, 2)*v(412)+a(6, 3)*v(501)-a(6, 4)*v(547)+a(6, 6)*v(600))/v(123)
      ai(5, 6) = (-(a(5, 1)*v(311))+a(5, 2)*v(412)-a(5, 3)*v(501)+a(5, 4)*v(547)-a(5, 6)*v(600))/v(123)
      ai(6, 1) = -(v(602)/v(123))
      ai(6, 2) = (a(6, 1)*v(604)-a(6, 2)*v(605)+a(6, 3)*v(606)-a(6, 4)*v(607)+a(6, 5)*v(608))/v(123)
      ai(6, 3) = (-(a(6, 1)*v(610))+a(6, 2)*v(611)-a(6, 3)*v(612)+a(6, 4)*v(613)-a(6, 5)*v(614))/v(123)
      ai(6, 4) = (a(6, 1)*v(616)-a(6, 2)*v(617)+a(6, 3)*v(618)-a(6, 4)*v(619)+a(6, 5)*v(620))/v(123)
      ai(6, 5) = (-(a(6, 1)*v(312))+a(6, 2)*v(413)-a(6, 3)*v(502)+a(6, 4)*v(548)-a(6, 5)*v(600))/v(123)
      ai(6, 6) = (a(5, 1)*v(312)-a(5, 2)*v(413)+a(5, 3)*v(502)-a(5, 4)*v(548)+a(5, 5)*v(600))/v(123)
      det = v(123)
      RETURN
    END IF
    CALL vectcopy(n*n, adecomp, a)
    CALL matrixludecomposition(n, adecomp, kpiv, ierr)
    CALL matrixdeterminantfromlu(n, adecomp, kpiv, det)
    IF (ierr .NE. 0) THEN
      WRITE (*, *) "error in decomposition"
      STOP
    END IF
    DO j = 1, n
      DO j1 = 1, j-1
        ai(j1, j) = 0.d00
      END DO
      ai(j, j) = 1.0d00
      DO j1 = j+1, n
        ai(j1, j) = 0.0d00
      END DO
      CALL matrixlusubstitution(n, adecomp, kpiv, ai(1, j))
    END DO
  END SUBROUTINE matrixinverse
!> inverts a symmetric matrix
  SUBROUTINE matrixinversesymmetric(n, det, a, ai)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(5001)::v
    REAL(8), DIMENSION(n, *)::a, ai
    IF (n .EQ. 1) THEN
      det = a(1, 1)
      ai(1, 1) = 1.0d00/det
      RETURN
    END IF
    IF (n .EQ. 2) THEN
      v(9) = -a(1, 2)**2+a(1, 1)*a(2, 2)
      v(11) = -(a(1, 2)/v(9))
      ai(1, 1) = a(2, 2)/v(9)
      ai(1, 2) = v(11)
      ai(2, 1) = v(11)
      ai(2, 2) = a(1, 1)/v(9)
      det = v(9)
      RETURN
    END IF
    IF (n .EQ. 3) THEN
      v(24) = -(a(1, 3)*a(2, 2))+a(1, 2)*a(2, 3)
      v(22) = -(a(1, 3)*a(2, 3))+a(1, 2)*a(3, 3)
      v(20) = -a(2, 3)**2+a(2, 2)*a(3, 3)
      v(19) = a(1, 1)*v(20)-a(1, 2)*v(22)+a(1, 3)*v(24)
      v(23) = -(v(22)/v(19))
      v(25) = v(24)/v(19)
      v(27) = (a(1, 2)*a(1, 3)-a(1, 1)*a(2, 3))/v(19)
      ai(1, 1) = v(20)/v(19)
      ai(1, 2) = v(23)
      ai(1, 3) = v(25)
      ai(2, 1) = v(23)
      ai(2, 2) = (-a(1, 3)**2+a(1, 1)*a(3, 3))/v(19)
      ai(2, 3) = v(27)
      ai(3, 1) = v(25)
      ai(3, 2) = v(27)
      ai(3, 3) = (-a(1, 2)**2+a(1, 1)*a(2, 2))/v(19)
      det = v(19)
      RETURN
    END IF
    IF (n .EQ. 4) THEN
      v(68) = a(1, 2)*a(2, 3)
      v(66) = -(a(1, 4)*a(2, 3))
      v(65) = a(1, 2)*a(1, 3)-a(1, 1)*a(2, 3)
      v(64) = -(a(1, 2)*a(1, 4))+a(1, 1)*a(2, 4)
      v(63) = a(1, 4)*a(2, 4)
      v(62) = a(1, 4)*a(3, 4)
      v(61) = 2d0*a(1, 3)
      v(55) = a(1, 2)**2
      v(67) = a(1, 1)*a(2, 2)-v(55)
      v(52) = a(1, 3)**2
      v(50) = a(1, 4)**2
      v(48) = a(3, 4)**2
      v(33) = a(1, 3)*a(2, 4)+v(66)
      v(34) = -(a(1, 4)*a(3, 3))+a(1, 3)*a(3, 4)
      v(35) = -(a(2, 4)*a(3, 3))+a(2, 3)*a(3, 4)
      v(46) = a(2, 3)*v(33)-a(2, 2)*v(34)+a(1, 2)*v(35)
      v(36) = a(1, 3)*a(4, 4)-v(62)
      v(37) = -(a(2, 4)*a(3, 4))+a(2, 3)*a(4, 4)
      v(44) = a(2, 4)*v(33)-a(2, 2)*v(36)+a(1, 2)*v(37)
      v(38) = a(3, 3)*a(4, 4)-v(48)
      v(42) = a(2, 4)*v(34)-a(2, 3)*v(36)+a(1, 2)*v(38)
      v(40) = a(2, 4)*v(35)-a(2, 3)*v(37)+a(2, 2)*v(38)
      v(39) = a(1, 1)*v(40)-a(1, 2)*v(42)+a(1, 3)*v(44)-a(1, 4)*v(46)
      ai(1, 1) = v(40)/v(39)
      ai(1, 2) = -(v(42)/v(39))
      ai(1, 3) = v(44)/v(39)
      ai(1, 4) = -(v(46)/v(39))
      ai(2, 2) = (-(a(1, 1)*v(48))-a(3, 3)*v(50)+a(4, 4)*(a(1, 1)*a(3, 3)-v(52))+v(61)*v(62))/v(39)
      ai(2, 3) = (a(2, 3)*v(50)-a(1, 3)*v(63)+a(3, 4)*v(64)+a(4, 4)*v(65))/v(39)
      ai(2, 4) = (a(2, 4)*v(52)-a(3, 3)*v(64)-a(3, 4)*v(65)+a(1, 3)*v(66))/v(39)
      ai(3, 3) = (-(a(1, 1)*a(2, 4)**2)-a(2, 2)*v(50)+2d0*a(1, 2)*v(63)+a(4, 4)*v(67))/v(39)
      ai(3, 4) = (-(a(2, 4)*v(65))-a(3, 4)*v(67)+a(1, 4)*(a(1, 3)*a(2, 2)-v(68)))/v(39)
      ai(4, 4) = (-(a(1, 1)*a(2, 3)**2)-a(2, 2)*v(52)+a(3, 3)*v(67)+v(61)*v(68))/v(39)
      DO i01 = 2, 4
        DO i02 = 1, i01-1
          ai(i01, i02) = ai(i02, i01)
        END DO
      END DO
      det = v(39)
      RETURN
    END IF
    IF (n .EQ. 5) THEN
      v(180) = 2d0*a(1, 4)
      v(174) = 2d0*a(1, 3)
      v(173) = a(1, 5)*a(4, 5)
      v(172) = a(1, 4)*a(5, 5)
      v(171) = a(1, 1)*a(5, 5)
      v(170) = a(1, 2)*a(5, 5)
      v(169) = -(a(3, 5)*a(4, 4))
      v(168) = a(1, 2)*a(4, 5)
      v(167) = a(1, 1)*a(4, 5)
      v(166) = a(1, 3)*a(4, 5)
      v(165) = a(1, 4)*a(3, 5)
      v(164) = a(1, 1)*a(3, 5)
      v(163) = a(1, 2)*a(1, 5)
      v(162) = a(1, 1)*a(3, 4)
      v(161) = a(1, 1)*a(3, 3)
      v(160) = a(1, 4)*a(2, 5)
      v(159) = a(1, 3)*a(2, 5)
      v(158) = a(1, 1)*a(2, 5)
      v(157) = a(1, 2)*a(2, 5)
      v(156) = a(1, 4)*a(2, 4)
      v(155) = a(1, 2)*a(2, 4)
      v(154) = a(1, 3)*a(1, 5)
      v(153) = a(1, 5)*a(2, 3)
      v(152) = a(1, 4)*a(2, 2)
      v(147) = a(1, 3)*v(152)
      v(140) = a(1, 5)*v(152)
      v(130) = a(2, 2)*v(154)
      v(131) = a(1, 2)*v(153)
      v(122) = a(1, 4)*v(153)
      v(109) = a(2, 3)*v(154)
      v(146) = a(1, 3)*v(155)
      v(129) = a(1, 5)*v(155)
      v(128) = a(1, 3)*v(156)
      v(127) = a(1, 5)*v(156)
      v(98) = a(2, 4)*v(154)
      v(142) = a(2, 4)*v(158)
      v(141) = a(1, 4)*v(157)
      v(138) = -(a(1, 3)*v(157))
      v(132) = a(2, 3)*v(158)
      v(99) = a(1, 4)*v(159)
      v(96) = a(1, 5)*v(159)
      v(95) = a(1, 5)*v(160)
      v(52) = -(a(1, 5)*a(2, 4))+v(160)
      v(143) = a(2, 2)*v(161)
      v(111) = a(2, 5)*v(161)
      v(110) = a(3, 3)*v(163)
      v(135) = a(2, 2)*v(162)
      v(113) = a(2, 3)*v(162)
      v(100) = a(3, 4)*v(163)
      v(177) = v(100)+v(98)+v(99)
      v(124) = -(a(2, 4)*v(164))
      v(123) = a(1, 2)*v(165)
      v(184) = v(123)+v(124)
      v(101) = a(2, 5)*v(164)
      v(97) = a(1, 5)*v(165)
      v(57) = -(a(2, 5)*a(3, 4))+a(2, 4)*a(3, 5)
      v(54) = -(a(1, 5)*a(3, 4))+v(165)
      v(133) = a(4, 4)*v(164)
      v(112) = a(1, 2)*v(169)
      v(134) = a(1, 4)*v(166)
      v(125) = a(1, 2)*v(166)
      v(185) = v(122)+v(125)+v(184)
      v(117) = -(a(1, 4)*v(168))
      v(116) = a(2, 4)*v(167)
      v(103) = -(a(3, 5)*v(167))
      v(102) = a(3, 5)*v(168)
      v(92) = a(4, 5)*v(162)
      v(90) = a(4, 5)**2
      v(59) = a(3, 4)*a(4, 5)+v(169)
      v(58) = -(a(2, 5)*a(4, 4))+a(2, 4)*a(4, 5)
      v(55) = -(a(1, 5)*a(4, 4))+a(1, 4)*a(4, 5)
      v(107) = a(1, 3)*v(170)
      v(106) = a(2, 4)*v(171)
      v(105) = a(1, 4)*v(170)
      v(190) = -v(105)+v(106)
      v(104) = -(a(1, 3)*v(172))
      v(178) = v(103)+v(104)+v(97)
      v(93) = a(4, 4)*v(171)
      v(68) = a(4, 4)*a(5, 5)-v(90)
      v(65) = -(a(3, 5)*a(4, 5))+a(3, 4)*a(5, 5)
      v(64) = -(a(2, 5)*a(4, 5))+a(2, 4)*a(5, 5)
      v(62) = v(172)-v(173)
      v(51) = a(3, 3)*v(52)-a(2, 3)*v(54)+a(1, 3)*v(57)
      v(53) = a(3, 4)*v(52)-a(2, 3)*v(55)+a(1, 3)*v(58)
      v(56) = a(3, 4)*v(54)-a(3, 3)*v(55)+a(1, 3)*v(59)
      v(60) = a(3, 4)*v(57)-a(3, 3)*v(58)+a(2, 3)*v(59)
      v(80) = -(a(2, 4)*v(51))+a(2, 3)*v(53)-a(2, 2)*v(56)+a(1, 2)*v(60)
      v(61) = a(3, 5)*v(52)-a(2, 3)*v(62)+a(1, 3)*v(64)
      v(63) = a(3, 5)*v(54)-a(3, 3)*v(62)+a(1, 3)*v(65)
      v(66) = a(3, 5)*v(57)-a(3, 3)*v(64)+a(2, 3)*v(65)
      v(78) = -(a(2, 5)*v(51))+a(2, 3)*v(61)-a(2, 2)*v(63)+a(1, 2)*v(66)
      v(67) = a(3, 5)*v(55)-a(3, 4)*v(62)+a(1, 3)*v(68)
      v(69) = a(3, 5)*v(58)-a(3, 4)*v(64)+a(2, 3)*v(68)
      v(76) = -(a(2, 5)*v(53))+a(2, 4)*v(61)-a(2, 2)*v(67)+a(1, 2)*v(69)
      v(70) = a(3, 5)*v(59)-a(3, 4)*v(65)+a(3, 3)*v(68)
      v(74) = -(a(2, 5)*v(56))+a(2, 4)*v(63)-a(2, 3)*v(67)+a(1, 2)*v(70)
      v(72) = -(a(2, 5)*v(60))+a(2, 4)*v(66)-a(2, 3)*v(69)+a(2, 2)*v(70)
      v(71) = a(1, 1)*v(72)-a(1, 2)*v(74)+a(1, 3)*v(76)-a(1, 4)*v(78)+a(1, 5)*v(80)
      v(82) = a(1, 5)**2
      v(191) = a(2, 2)*v(82)
      v(181) = -v(107)-a(2, 3)*v(82)+v(96)
      v(179) = v(190)-a(2, 4)*v(82)+v(95)
      v(83) = a(3, 4)**2
      v(84) = a(3, 4)*v(180)
      v(85) = a(1, 4)**2
      v(186) = v(116)+v(117)-v(127)+a(2, 5)*v(85)
      v(176) = a(3, 5)*v(85)
      v(187) = -v(133)-v(134)+v(176)
      v(86) = a(3, 5)**2
      v(87) = a(3, 5)*v(174)
      v(88) = 2d0*v(173)
      v(175) = -(a(4, 4)*v(82))-a(5, 5)*v(85)+a(1, 4)*v(88)-a(1, 1)*v(90)+v(93)
      v(89) = a(1, 3)**2
      v(197) = a(2, 2)*v(89)
      v(183) = v(113)+a(2, 4)*v(89)
      v(182) = v(109)-v(110)+v(111)-a(2, 5)*v(89)
      v(115) = a(2, 4)**2
      v(118) = 2d0*a(1, 5)*a(2, 5)
      v(192) = a(1, 2)*v(118)
      v(119) = a(2, 5)**2
      v(193) = -(a(1, 1)*v(119))
      v(188) = v(192)+v(193)
      v(120) = a(1, 2)**2
      v(198) = a(3, 3)*v(120)
      v(195) = -(a(1, 1)*a(2, 2))+v(120)
      v(194) = a(3, 5)*v(120)
      v(189) = a(5, 5)*v(120)
      v(137) = a(2, 3)**2
      v(199) = a(1, 1)*v(137)
      v(196) = -v(143)+v(197)+v(198)+v(199)
      v(145) = a(2, 3)*v(174)
      ai(1, 1) = v(72)/v(71)
      ai(1, 2) = -(v(74)/v(71))
      ai(1, 3) = v(76)/v(71)
      ai(1, 4) = -(v(78)/v(71))
      ai(1, 5) = v(80)/v(71)
      ai(2, 2) = (a(3, 3)*v(175)+v(82)*v(83)+(-(a(1, 1)*a(4, 4))+v(85))*v(86)-v(55)*v(87)-&
      &a(1, 3)*a(3, 4)*v(88)+a(5, 5)*(-(a(1, 1)*v(83&
      &))+a(1, 3)*v(84)-a(4, 4)*v(89))+v(89)*v(90)+a(3, 5)*(-(a(1, 5)*v(84))+2d0*v(92)))/v(71)
      ai(2, 3) = (a(1, 4)*v(102)+a(1, 5)*v(112)-a(2, 3)*v(175)+a(4, 5)*v(177)+a(2, 4)*v(178)+&
      &a(3, 4)*v(179)-a(1, 2)*a(1, 3)*v(90)+a(2, 5&
      &)*(-v(176)-v(92))+a(4, 4)*(v(101)+v(107)-v(96)))/v(71)
      ai(2, 4) = (a(1, 3)*v(102)+a(2, 3)*v(178)-a(3, 3)*v(179)+a(3, 4)*(-v(101)+v(181))+&
      &a(4, 5)*v(182)+a(5, 5)*v(183)+(-(a(1, 2)*a(1, 4)&
      &)+a(1, 1)*a(2, 4))*v(86)+a(3, 5)*(v(177)-3d0*v(98)))/v(71)
      ai(2, 5) = (a(3, 5)*v(128)-a(4, 4)*v(182)-a(4, 5)*v(183)+a(3, 3)*v(186)-a(2, 3)*v(187)+(v(158)-v(163))*v(83)+a(1, 3)*(v(112)-a(2&
      &, 5)*v(84))+a(3, 4)*(v(185)+v(98)))/v(71)
      ai(3, 3) = (2d0*a(2, 5)*(v(116)+v(117))+a(2, 2)*v(175)+a(4, 4)*(v(188)-v(189))+v(115)*(-v(171)+v(82))+v(119)*v(85)+a(2, 4)*&
      &(2d0*v(105)-a(1, 4)*v(118)-a(1, 2)*v(88))+v(120)*v(90))/v(71)
      ai(3, 4) = (-(a(1, 3)*a(1, 4)*v(119))+a(3, 5)*v(129)-a(5, 5)*v(135)-a(2, 2)*v(178)+a(2, 4)*v(181)+a(2, 5)*v(185)+a(2, 3)*v(190)+a&
      &(3, 4)*(v(189)+v(191)-v(192)-v(193))+a(4, 5)*(-v(130)+v(131)-v(132)-v(194)))/v(71)
      ai(3, 5) = (a(2, 4)*((-2d0)*v(123)+v(125))+a(2, 5)*v(128)+a(4, 5)*v(135)+a(3, 4)*(-(a(4, 5)*v(120))+v(129)-v(140)+v(141)-v(142)&
      &)+v(115)*(-v(154)+v(164))-a(2, 3)*v(186)+a(2, 2)*v(187)+a(4, 4)*(v(130)-v(131)+v(132)+v(138)+v(194)))/v(71)
      ai(4, 4) = (a(2, 3)*(2d0*v(101)+2d0*v(107)-a(1, 3)*v(118))+a(3, 5)*((-2d0)*v(131)+2d0*v(138))+a(3, 3)*(v(188)-v(191))-a(5, 5)*v&
      &(196)+v(137)*v(82)+v(195)*v(86)+a(1, 5)*a(2, 2)*v(87)+v(119)*v(89))/v(71)
      ai(4, 5) = (-(a(2, 4)*v(110))-a(1, 4)*a(1, 5)*v(137)+a(3, 4)*(-v(130)-v(138))+a(3, 3)*(v(140)-v(141)+v(142))+a(3, 5)*(-(a(3, 4)*v&
      &(120))+v(135)+v(146)-v(147))-a(2, 5)*v(183)+a(2, 3)*((-2d0)*v(125)+v(177)+v(184))+a(4, 5)*v(196))/v(71)
      ai(5, 5) = (a(2, 4)*(2d0*v(113)-a(1, 4)*v(145))+a(3, 4)*((-2d0)*v(146)+2d0*v(147))+a(4, 4)*(v(143)+a(1, 2)*v(145)-v(197)-v(198)&
      &-v(199))+v(195)*v(83)-a(1, 2)*a(2, 3)*v(84)+v(137)*v(85)+&
      &a(3, 3)*(-(a(1, 1)*v(115))+v(155)*v(180)-a(2, 2)*v(85))+v(115)*v(89&
      &))/v(71)
      DO i01 = 2, 5
        DO i02 = 1, i01-1
          ai(i01, i02) = ai(i02, i01)
        END DO
      END DO
      det = v(71)
      RETURN
    END IF
    IF (n .EQ. 6) THEN
      v(682) = 2d0*a(3, 6)
      v(680) = a(1, 2)*a(2, 5)
      v(661) = a(1, 1)*a(3, 5)
      v(629) = 2d0*a(3, 4)
      v(628) = a(1, 6)*a(2, 4)
      v(624) = a(2, 2)*a(3, 4)
      v(619) = a(1, 4)*a(1, 5)
      v(618) = a(1, 3)*a(1, 6)
      v(613) = a(2, 3)*a(2, 5)
      v(612) = a(2, 5)*a(4, 6)
      v(611) = a(2, 2)*a(5, 5)
      v(610) = a(2, 3)*a(2, 6)
      v(609) = a(1, 5)*a(2, 2)
      v(608) = a(1, 2)*a(1, 6)
      v(607) = a(3, 5)*a(5, 6)
      v(603) = 2d0*a(4, 6)
      v(600) = a(3, 6)*a(4, 5)
      v(594) = a(3, 5)*a(4, 6)
      v(593) = a(1, 3)*a(1, 4)
      v(592) = a(1, 4)*a(1, 6)
      v(586) = a(1, 1)*a(2, 3)
      v(585) = a(1, 2)*a(3, 4)
      v(584) = a(1, 5)*a(2, 3)
      v(583) = a(1, 5)*a(3, 3)
      v(582) = a(1, 1)*a(5, 6)
      v(581) = a(1, 1)*a(4, 6)
      v(580) = a(2, 6)*a(5, 5)
      v(579) = a(2, 6)*a(4, 5)
      v(578) = a(1, 6)*a(3, 4)
      v(577) = a(1, 1)*a(2, 4)
      v(576) = a(1, 2)*a(1, 4)
      v(575) = a(1, 3)*a(2, 4)
      v(574) = a(1, 3)*a(4, 5)
      v(573) = -(a(1, 4)*a(3, 5))
      v(572) = a(1, 3)*a(1, 5)
      v(571) = a(1, 5)*a(4, 6)
      v(570) = a(3, 5)*a(4, 4)
      v(569) = a(1, 5)*a(3, 4)
      v(568) = a(3, 6)*a(5, 6)
      v(567) = a(1, 6)*a(5, 6)
      v(565) = a(2, 5)*a(5, 6)
      v(564) = -(a(2, 6)*a(4, 4))
      v(563) = a(2, 4)*a(2, 5)
      v(561) = a(5, 5)*a(6, 6)
      v(559) = a(2, 2)*a(3, 3)
      v(558) = a(1, 2)*a(1, 3)
      v(557) = 2d0*a(1, 5)
      v(555) = a(4, 5)*a(5, 6)
      v(554) = a(1, 6)*a(5, 5)
      v(553) = a(2, 5)*a(3, 6)
      v(552) = -(a(2, 6)*a(3, 5))
      v(551) = -(a(1, 6)*a(3, 5))
      v(550) = a(1, 5)*a(2, 6)
      v(549) = a(1, 1)*a(6, 6)
      v(548) = a(1, 3)*a(6, 6)
      v(547) = a(1, 4)*a(6, 6)
      v(546) = 2d0*a(1, 2)
      v(545) = a(4, 4)*a(5, 5)
      v(544) = a(1, 4)*a(4, 6)
      v(543) = a(1, 1)*a(3, 3)
      v(542) = a(1, 1)*a(3, 6)
      v(541) = a(1, 2)*a(2, 4)
      v(540) = 2d0*a(4, 5)
      v(539) = 2d0*a(2, 5)
      v(538) = a(1, 3)*a(4, 6)
      v(537) = a(1, 6)*a(4, 5)
      v(536) = a(1, 2)*a(4, 5)
      v(535) = a(1, 6)*a(4, 4)
      v(534) = a(2, 2)*a(4, 4)
      v(533) = a(1, 3)/2d0
      v(532) = a(2, 4)*a(3, 6)
      v(531) = a(1, 5)*a(3, 6)
      v(530) = -(a(1, 1)*a(3, 4))
      v(529) = a(1, 1)*a(2, 2)
      v(528) = 2d0*a(3, 5)
      v(527) = 2d0*a(1, 3)
      v(526) = a(1, 4)*a(3, 4)
      v(525) = 2d0*a(2, 4)
      v(524) = a(1, 2)*a(3, 3)
      v(523) = 2d0*a(1, 4)
      v(522) = a(1, 1)*a(2, 5)
      v(521) = a(1, 2)*a(1, 5)
      v(520) = a(1, 6)*a(2, 5)
      v(519) = 2d0*a(2, 3)
      v(466) = a(2, 2)*v(523)
      v(498) = a(1, 2)*v(519)
      v(432) = a(1, 3)*v(519)
      v(378) = a(2, 3)*v(521)
      v(463) = -(a(2, 3)*v(575))
      v(431) = a(1, 2)*v(525)
      v(373) = -(a(1, 4)*v(431))
      v(405) = a(2, 3)*v(520)
      v(385) = a(2, 4)*v(522)
      v(362) = a(1, 5)*v(520)
      v(310) = a(1, 5)*v(539)
      v(665) = -(a(1, 2)*v(310))
      v(222) = v(310)*v(533)
      v(469) = a(2, 6)*v(521)
      v(438) = a(2, 6)*v(527)
      v(396) = a(2, 3)*v(550)
      v(341) = -(a(2, 6)*v(522))
      v(459) = (a(3, 3)*v(431))/2d0
      v(458) = a(3, 3)*v(563)
      v(457) = -(a(3, 3)*v(523))
      v(424) = a(2, 6)*v(524)
      v(422) = a(3, 3)*v(529)
      v(415) = a(1, 6)*v(524)
      v(287) = a(3, 3)*v(521)
      v(501) = a(1, 3)*v(526)
      v(704) = 2d0*a(2, 2)*v(501)
      v(499) = a(3, 4)*v(525)
      v(465) = a(2, 6)*v(526)
      v(462) = -(a(2, 3)*v(499))
      v(439) = a(3, 4)*v(527)
      v(342) = a(2, 6)*v(530)
      v(268) = a(1, 4)*v(439)
      v(504) = a(2, 2)*v(528)
      v(398) = (a(1, 6)*v(504))/2d0
      v(387) = -(a(1, 4)*v(504))/2d0
      v(340) = a(3, 5)*v(521)
      v(334) = a(2, 4)*v(551)
      v(290) = a(3, 5)*v(558)
      v(595) = -v(287)+v(290)
      v(274) = a(3, 5)*v(342)
      v(229) = a(3, 5)*v(522)
      v(146) = a(1, 4)*v(528)
      v(483) = a(3, 6)*v(529)
      v(475) = a(1, 3)*v(531)
      v(460) = a(1, 4)*v(532)
      v(437) = a(3, 6)*v(546)
      v(436) = a(3, 6)*v(530)
      v(420) = -(a(2, 4)*v(437))/2d0
      v(399) = (a(2, 5)*v(437))/2d0
      v(386) = -(a(2, 2)*v(531))
      v(683) = v(386)+v(396)+v(399)
      v(345) = a(1, 5)*v(532)
      v(295) = v(437)*v(533)
      v(276) = -(a(1, 4)*v(553))
      v(492) = -(a(3, 3)*v(534))
      v(694) = v(462)+v(492)
      v(474) = a(3, 6)*v(534)
      v(468) = a(1, 2)*v(564)
      v(456) = a(3, 3)*v(535)
      v(383) = -(a(4, 4)*v(529))
      v(322) = -(a(1, 2)*v(535))
      v(288) = a(4, 4)*v(543)
      v(597) = v(268)+v(288)
      v(154) = a(4, 4)*v(528)
      v(464) = -(a(2, 3)*v(536))
      v(423) = -(a(2, 2)*v(537))
      v(347) = a(2, 5)*v(536)
      v(316) = a(2, 4)*v(537)
      v(314) = a(4, 5)*v(523)
      v(309) = a(4, 5)*v(525)
      v(250) = a(4, 5)*v(542)
      v(237) = -(a(4, 5)*v(521))
      v(225) = a(4, 5)*v(552)
      v(231) = a(1, 1)*v(225)
      v(142) = a(3, 4)*v(540)
      v(473) = a(4, 6)*v(530)
      v(467) = a(2, 5)*v(538)
      v(444) = a(3, 6)*v(538)
      v(442) = -(a(2, 2)*v(538))
      v(380) = a(4, 6)*v(541)
      v(319) = a(4, 6)*v(539)
      v(203) = a(1, 5)*v(544)
      v(157) = a(4, 6)*v(540)
      v(151) = a(4, 6)*v(528)
      v(360) = a(5, 5)*v(529)
      v(344) = a(5, 5)*v(541)
      v(262) = a(5, 5)*v(542)
      v(251) = a(1, 3)*v(554)
      v(244) = -(a(5, 5)*v(543))
      v(195) = a(1, 1)*v(545)
      v(184) = a(5, 5)*v(544)
      v(174) = a(1, 6)*v(545)
      v(404) = a(5, 6)*v(539)
      v(402) = a(5, 6)*v(546)
      v(311) = a(5, 6)*v(557)
      v(255) = v(311)*v(533)
      v(660) = v(251)-v(255)
      v(219) = (a(3, 4)*v(311))/2d0
      v(256) = a(1, 2)*v(219)
      v(179) = a(1, 4)*v(555)
      v(173) = (a(4, 4)*v(311))/2d0
      v(643) = v(173)-v(174)+v(184)
      v(168) = a(5, 6)**2
      v(452) = -(a(6, 6)*v(529))
      v(450) = -(a(6, 6)*v(543))
      v(449) = -(a(6, 6)*v(559))
      v(441) = a(3, 4)*v(548)
      v(433) = -(a(2, 4)*v(547))
      v(430) = a(3, 3)*v(547)
      v(401) = a(1, 5)*v(548)
      v(394) = a(6, 6)*v(521)
      v(331) = -(a(6, 6)*v(545))
      v(330) = -(a(5, 5)*v(549))
      v(324) = a(4, 4)*v(549)
      v(73) = -v(520)+v(550)
      v(74) = v(531)+v(551)
      v(75) = v(552)+v(553)
      v(80) = a(3, 4)*v(73)-a(2, 4)*v(74)+a(1, 4)*v(75)
      v(76) = -v(537)+v(571)
      v(77) = -v(579)+v(612)
      v(85) = a(4, 4)*v(73)-a(2, 4)*v(76)+a(1, 4)*v(77)
      v(78) = v(594)-v(600)
      v(93) = a(4, 4)*v(75)-a(3, 4)*v(77)+a(2, 4)*v(78)
      v(89) = a(4, 4)*v(74)-a(3, 4)*v(76)+a(1, 4)*v(78)
      v(79) = -(a(3, 4)*v(80))+a(3, 3)*v(85)-a(2, 3)*v(89)+a(1, 3)*v(93)
      v(81) = v(311)/2d0-v(554)
      v(82) = v(565)-v(580)
      v(86) = a(4, 5)*v(73)-a(2, 4)*v(81)+a(1, 4)*v(82)
      v(83) = -(a(3, 6)*a(5, 5))+v(607)
      v(94) = a(4, 5)*v(75)-a(3, 4)*v(82)+a(2, 4)*v(83)
      v(90) = a(4, 5)*v(74)-a(3, 4)*v(81)+a(1, 4)*v(83)
      v(84) = -(a(3, 5)*v(80))+a(3, 3)*v(86)-a(2, 3)*v(90)+a(1, 3)*v(94)
      v(87) = -(a(4, 6)*a(5, 5))+v(555)
      v(96) = a(4, 5)*v(78)-a(4, 4)*v(83)+a(3, 4)*v(87)
      v(95) = a(4, 5)*v(77)-a(4, 4)*v(82)+a(2, 4)*v(87)
      v(91) = a(4, 5)*v(76)-a(4, 4)*v(81)+a(1, 4)*v(87)
      v(88) = -(a(3, 5)*v(85))+a(3, 4)*v(86)-a(2, 3)*v(91)+a(1, 3)*v(95)
      v(92) = -(a(3, 5)*v(89))+a(3, 4)*v(90)-a(3, 3)*v(91)+a(1, 3)*v(96)
      v(97) = -(a(3, 5)*v(93))+a(3, 4)*v(94)-a(3, 3)*v(95)+a(2, 3)*v(96)
      v(134) = a(2, 5)*v(79)-a(2, 4)*v(84)+a(2, 3)*v(88)-a(2, 2)*v(92)+a(1, 2)*v(97)
      v(98) = a(1, 5)*a(6, 6)-v(567)
      v(99) = -(a(2, 6)*a(5, 6))+a(2, 5)*a(6, 6)
      v(102) = a(4, 6)*v(73)-a(2, 4)*v(98)+a(1, 4)*v(99)
      v(100) = a(3, 5)*a(6, 6)-v(568)
      v(108) = a(2, 4)*v(100)+a(4, 6)*v(75)-a(3, 4)*v(99)
      v(105) = a(1, 4)*v(100)+a(4, 6)*v(74)-a(3, 4)*v(98)
      v(101) = a(3, 3)*v(102)-a(2, 3)*v(105)+a(1, 3)*v(108)-a(3, 6)*v(80)
      v(103) = -(a(4, 6)*a(5, 6))+a(4, 5)*a(6, 6)
      v(110) = -(a(4, 4)*v(100))+a(3, 4)*v(103)+a(4, 6)*v(78)
      v(109) = a(2, 4)*v(103)+a(4, 6)*v(77)-a(4, 4)*v(99)
      v(106) = a(1, 4)*v(103)+a(4, 6)*v(76)-a(4, 4)*v(98)
      v(104) = a(3, 4)*v(102)-a(2, 3)*v(106)+a(1, 3)*v(109)-a(3, 6)*v(85)
      v(107) = a(3, 4)*v(105)-a(3, 3)*v(106)+a(1, 3)*v(110)-a(3, 6)*v(89)
      v(111) = a(3, 4)*v(108)-a(3, 3)*v(109)+a(2, 3)*v(110)-a(3, 6)*v(93)
      v(132) = -(a(2, 4)*v(101))+a(2, 3)*v(104)-a(2, 2)*v(107)+a(1, 2)*v(111)+a(2, 6)*v(79)
      v(112) = -v(168)+v(561)
      v(120) = -(a(4, 5)*v(103))+a(4, 4)*v(112)+a(4, 6)*v(87)
      v(117) = -(a(4, 5)*v(100))+a(3, 4)*v(112)+a(4, 6)*v(83)
      v(116) = a(2, 4)*v(112)+a(4, 6)*v(82)-a(4, 5)*v(99)
      v(114) = a(1, 4)*v(112)+a(4, 6)*v(81)-a(4, 5)*v(98)
      v(113) = a(3, 5)*v(102)-a(2, 3)*v(114)+a(1, 3)*v(116)-a(3, 6)*v(86)
      v(115) = a(3, 5)*v(105)-a(3, 3)*v(114)+a(1, 3)*v(117)-a(3, 6)*v(90)
      v(118) = a(3, 5)*v(108)-a(3, 3)*v(116)+a(2, 3)*v(117)-a(3, 6)*v(94)
      v(130) = -(a(2, 5)*v(101))+a(2, 3)*v(113)-a(2, 2)*v(115)+a(1, 2)*v(118)+a(2, 6)*v(84)
      v(119) = a(3, 5)*v(106)-a(3, 4)*v(114)+a(1, 3)*v(120)-a(3, 6)*v(91)
      v(121) = a(3, 5)*v(109)-a(3, 4)*v(116)+a(2, 3)*v(120)-a(3, 6)*v(95)
      v(128) = -(a(2, 5)*v(104))+a(2, 4)*v(113)-a(2, 2)*v(119)+a(1, 2)*v(121)+a(2, 6)*v(88)
      v(122) = a(3, 5)*v(110)-a(3, 4)*v(117)+a(3, 3)*v(120)-a(3, 6)*v(96)
      v(126) = -(a(2, 5)*v(107))+a(2, 4)*v(115)-a(2, 3)*v(119)+a(1, 2)*v(122)+a(2, 6)*v(92)
      v(124) = -(a(2, 5)*v(111))+a(2, 4)*v(118)-a(2, 3)*v(121)+a(2, 2)*v(122)+a(2, 6)*v(97)
      v(123) = a(1, 1)*v(124)-a(1, 2)*v(126)+a(1, 3)*v(128)-a(1, 4)*v(130)+a(1, 5)*v(132)-a(1, 6)*v(134)
      v(136) = a(1, 6)**2
      v(411) = -(v(136)*v(611))
      v(137) = a(3, 5)**2
      v(658) = a(1, 1)*v(137)
      v(635) = v(244)+v(658)
      v(496) = -(v(137)*v(576))
      v(138) = a(1, 5)*v(154)
      v(139) = a(1, 5)**2
      v(678) = -(a(2, 3)*v(139))
      v(675) = a(2, 2)*v(139)
      v(662) = v(222)+v(678)
      v(659) = a(3, 3)*v(139)
      v(664) = v(635)+v(659)
      v(637) = a(3, 6)*v(139)
      v(638) = v(255)+v(262)-v(637)
      v(633) = a(4, 4)*v(139)
      v(556) = a(3, 4)*v(139)
      v(500) = a(2, 4)*v(556)
      v(327) = -(a(6, 6)*v(633))
      v(258) = -(a(2, 6)*v(556))
      v(140) = a(3, 6)**2
      v(141) = a(3, 5)*v(142)
      v(143) = a(4, 5)*v(557)
      v(144) = a(3, 6)*v(578)
      v(145) = a(4, 5)*v(146)
      v(630) = -v(138)+v(145)
      v(147) = -(a(1, 5)*v(523))
      v(641) = a(4, 5)*v(147)
      v(148) = a(4, 5)**2
      v(644) = a(1, 1)*v(148)
      v(645) = v(195)-v(633)-v(644)
      v(510) = v(148)*v(558)
      v(328) = -(v(148)*v(549))
      v(149) = a(3, 6)*v(527)
      v(150) = a(3, 4)*v(151)
      v(152) = -(a(1, 6)*v(523))
      v(315) = v(152)*v(555)
      v(153) = a(3, 4)*v(603)
      v(155) = a(3, 6)*v(528)
      v(156) = a(3, 3)*v(157)
      v(631) = v(150)-v(156)
      v(158) = a(3, 5)*v(527)
      v(636) = -(a(1, 5)*v(158))
      v(679) = v(635)+v(636)
      v(159) = a(4, 6)**2
      v(160) = a(3, 4)**2
      v(589) = -(a(1, 1)*v(160))
      v(503) = v(160)*v(521)
      v(470) = a(2, 6)*v(589)
      v(161) = a(5, 5)*v(523)
      v(162) = a(1, 4)**2
      v(646) = a(5, 5)*v(162)
      v(634) = -v(195)+v(633)+v(641)+v(644)+v(646)
      v(599) = a(3, 6)*v(162)
      v(590) = -(a(3, 3)*v(162))
      v(560) = a(2, 3)*v(162)
      v(512) = v(162)*v(559)
      v(477) = a(2, 2)*v(599)
      v(476) = a(2, 5)*v(560)
      v(472) = a(2, 6)*v(560)
      v(461) = a(2, 6)*v(590)
      v(329) = -(v(162)*v(561))
      v(642) = v(136)*v(148)+v(139)*v(159)+v(162)*v(168)+2d0*a(1, 6)*v(173)+v(315)+v(327)+v(328)+v(329)
      v(163) = a(3, 3)*v(545)
      v(164) = a(1, 3)**2
      v(639) = -(a(4, 4)*v(164))
      v(687) = v(589)+v(590)+v(597)+v(639)
      v(640) = -v(288)-v(589)-v(639)
      v(632) = v(139)*v(160)+v(137)*v(162)+v(148)*v(164)
      v(621) = -(a(2, 2)*v(164))
      v(566) = a(2, 6)*v(164)
      v(562) = a(4, 5)*v(164)
      v(513) = v(164)*v(534)
      v(494) = a(2, 4)*v(562)
      v(488) = -(a(2, 2)*v(562))
      v(486) = v(164)*v(563)
      v(480) = v(164)*v(564)
      v(453) = a(6, 6)*v(621)
      v(434) = a(2, 4)*v(566)
      v(409) = v(164)*v(565)
      v(281) = -(a(4, 5)*v(566))
      v(165) = a(1, 6)*v(311)
      v(166) = a(1, 3)*v(567)
      v(167) = a(1, 1)*v(568)
      v(170) = a(2, 4)*v(569)
      v(171) = -(a(1, 4)*v(569))
      v(172) = a(2, 4)*v(573)
      v(175) = a(1, 5)*v(535)
      v(176) = a(4, 4)*v(572)
      v(177) = a(1, 2)*v(570)
      v(178) = -(a(1, 1)*v(570))
      v(587) = a(3, 5)*v(162)+v(176)+v(178)
      v(180) = a(1, 4)*v(537)
      v(181) = -(a(1, 4)*v(574))
      v(182) = -(a(3, 4)*v(536))
      v(652) = v(172)+v(177)+v(182)
      v(183) = -(a(4, 5)*v(530))
      v(653) = v(171)+v(181)+v(183)+v(587)
      v(185) = a(2, 3)*v(571)
      v(186) = -(a(2, 4)*v(572))
      v(187) = a(1, 2)*v(573)
      v(188) = a(3, 5)*v(577)
      v(654) = v(187)+v(188)
      v(588) = v(186)+v(188)
      v(591) = v(187)+v(588)
      v(293) = a(4, 6)*v(591)+a(1, 6)*v(652)+a(2, 6)*v(653)
      v(189) = a(4, 6)*v(574)
      v(190) = a(4, 5)*v(581)
      v(191) = a(1, 4)*v(575)
      v(192) = a(3, 4)*v(576)
      v(193) = -(a(3, 4)*v(577))
      v(649) = v(191)+v(193)
      v(194) = -(a(4, 4)*v(558))
      v(615) = v(192)+v(194)
      v(650) = v(615)+v(649)
      v(651) = -v(560)+v(650)
      v(479) = v(194)*v(519)
      v(196) = a(4, 4)*v(582)
      v(197) = a(2, 5)*v(578)
      v(220) = a(1, 5)*v(197)
      v(198) = a(2, 5)*v(573)
      v(199) = -(a(1, 4)*v(552))
      v(272) = a(1, 3)*v(199)
      v(227) = a(1, 5)*v(199)
      v(200) = -(a(1, 5)*v(537))
      v(201) = a(1, 5)*v(579)
      v(259) = a(1, 3)*v(201)
      v(202) = a(3, 5)*v(537)
      v(260) = a(1, 2)*v(202)
      v(205) = a(1, 4)*v(554)
      v(206) = a(1, 3)*v(580)
      v(226) = -(a(1, 4)*v(206))
      v(207) = a(3, 4)*v(554)
      v(261) = -(a(1, 2)*v(207))
      v(208) = a(3, 4)*v(580)
      v(230) = a(1, 1)*v(208)
      v(210) = -(a(5, 5)*v(581))
      v(211) = -(a(5, 6)*v(619))
      v(212) = a(1, 3)*v(565)
      v(263) = a(1, 4)*v(212)
      v(213) = a(3, 4)*v(565)
      v(264) = -(a(1, 1)*v(213))
      v(648) = v(220)+v(226)+v(227)+v(230)+v(231)+v(256)+v(258)+v(259)+v(260)+v(261)+v(263)+v(264)
      v(215) = a(4, 5)*v(582)
      v(666) = v(200)+v(205)+v(215)
      v(647) = a(4, 6)*v(139)+v(210)+v(211)+v(666)
      v(217) = a(2, 4)*v(583)
      v(218) = -(a(1, 4)*v(583))
      v(267) = a(2, 6)*v(218)
      v(221) = -(a(1, 6)*v(584))
      v(223) = a(3, 4)*v(550)
      v(672) = v(199)-v(223)
      v(270) = a(1, 3)*v(223)
      v(224) = a(3, 5)*v(575)
      v(656) = v(217)-v(224)
      v(228) = a(3, 5)*v(585)
      v(273) = a(1, 6)*v(228)
      v(232) = a(1, 4)*v(584)
      v(677) = -v(232)+v(588)
      v(275) = a(3, 6)*v(232)
      v(233) = -(a(1, 3)*v(532))
      v(695) = a(1, 4)*v(233)+v(461)+v(470)+v(480)
      v(234) = a(3, 6)*v(576)
      v(688) = a(2, 3)*v(234)
      v(236) = a(2, 3)*v(574)
      v(663) = -v(228)-v(236)
      v(280) = a(1, 6)*v(236)
      v(282) = -(a(4, 5)*v(415))
      v(239) = a(4, 5)*v(543)
      v(283) = a(2, 6)*v(239)
      v(240) = a(1, 3)*v(536)
      v(284) = a(3, 6)*v(240)
      v(241) = -(a(4, 5)*v(586))
      v(285) = a(3, 6)*v(241)
      v(655) = v(267)+v(270)+v(272)+v(273)+v(274)+v(275)+v(280)+v(281)+v(282)+v(283)+v(284)+v(285)
      v(242) = -(a(2, 3)*v(593))
      v(243) = a(1, 4)*v(524)
      v(245) = -(a(5, 6)*v(543))
      v(246) = -(a(1, 3)*v(585))
      v(625) = v(243)+v(246)
      v(247) = a(3, 4)*v(586)
      v(596) = v(242)+v(247)+v(625)
      v(657) = a(2, 4)*v(164)+v(596)
      v(303) = a(1, 4)*(-v(224)-v(228))+a(2, 4)*(-v(218)-v(239))+v(494)+v(503)+a(2, 3)*v(587)+a(4, 4)*v(595)+a(4, 5)*v(596)+a(3, 4)*v&
      &(677)+a(2, 5)*v(687)
      v(248) = -(a(1, 3)*v(520))
      v(367) = a(5, 5)*v(234)+a(3, 6)*v(237)+a(4, 5)*v(248)+a(2, 5)*v(250)+a(1, 5)*(v(276)+v(334))+a(5, 6)*v(591)+a(2, 4)*(v(251)-v&
      &(262)+v(637))+v(648)
      v(249) = a(1, 3)*v(553)
      v(252) = a(3, 3)*v(554)
      v(266) = a(3, 3)*v(592)
      v(269) = -(a(1, 3)*v(578))
      v(271) = -(a(2, 3)*v(592))
      v(277) = a(2, 5)*v(593)
      v(278) = a(3, 4)*v(521)
      v(670) = v(277)+v(278)
      v(279) = a(3, 4)*v(522)
      v(601) = -v(277)-v(278)+v(279)
      v(289) = -(a(4, 6)*v(543))
      v(598) = v(266)+v(269)+v(289)
      v(292) = a(2, 3)*v(594)
      v(300) = -(a(1, 3)*v(185))+a(3, 5)*v(271)+a(1, 1)*v(292)-a(4, 6)*v(595)+a(2, 5)*(a(4, 6)*v(164)+v(598))+a(3, 6)*v(601)+v(655)
      v(294) = a(1, 3)*v(535)
      v(692) = -v(294)+v(473)
      v(296) = -(a(4, 4)*v(531))
      v(297) = a(4, 4)*v(542)
      v(602) = v(294)-v(297)+v(599)
      v(302) = -(a(3, 6)*v(193))+a(3, 4)*(-v(234)+v(271))+a(4, 4)*v(295)+a(3, 3)*v(322)+a(2, 6)*v(597)+a(2, 4)*v(598)+a(2, 3)*v(602)+v&
      &(160)*v(608)+a(4, 6)*v(657)+v(695)
      v(298) = a(1, 4)*v(600)
      v(301) = -(a(2, 4)*v(250))+v(293)+a(1, 2)*(v(296)+v(298))+a(1, 3)*v(316)+a(1, 4)*(v(197)+v(345))-a(4, 6)*v(601)-a(2, 5)*v(602)
      v(305) = a(2, 5)**2
      v(676) = a(1, 1)*v(305)
      v(502) = v(305)*v(593)
      v(306) = a(4, 4)*v(310)
      v(307) = a(2, 6)**2
      v(308) = a(2, 5)*v(309)
      v(705) = a(1, 1)*v(308)
      v(312) = a(2, 6)*v(525)
      v(313) = a(2, 5)*v(314)
      v(317) = -(a(2, 6)*v(546))
      v(318) = a(2, 4)*v(319)
      v(320) = -(a(1, 6)*v(603))
      v(321) = a(2, 2)*v(603)
      v(323) = a(2, 4)**2
      v(701) = v(164)*v(323)
      v(505) = -(v(323)*v(572))
      v(478) = v(323)*v(542)
      v(471) = v(323)*v(618)
      v(325) = a(1, 2)**2
      v(702) = v(160)*v(325)
      v(667) = a(5, 5)*v(325)
      v(622) = -(a(3, 3)*v(325))
      v(606) = a(3, 4)*v(325)
      v(605) = a(4, 4)*v(325)
      v(669) = a(1, 1)*v(323)+v(373)+v(383)+v(605)
      v(668) = a(2, 2)*v(162)+v(669)
      v(604) = a(4, 5)*v(325)
      v(514) = a(3, 3)*v(605)
      v(509) = a(3, 5)*v(604)
      v(490) = -(a(3, 3)*v(604))
      v(487) = a(3, 5)*v(606)
      v(484) = a(3, 6)*v(604)
      v(481) = a(3, 6)*v(605)
      v(454) = a(6, 6)*v(622)
      v(446) = a(3, 6)*v(606)
      v(408) = -(v(325)*v(607))
      v(326) = a(2, 6)*v(539)
      v(333) = a(2, 4)*v(584)
      v(674) = -v(333)+v(387)
      v(337) = -(a(1, 4)*v(610))
      v(623) = v(337)+v(420)
      v(338) = -(a(1, 6)*v(609))
      v(339) = a(2, 5)*v(608)
      v(343) = a(1, 4)*v(609)
      v(352) = a(2, 2)*v(574)
      v(353) = -(a(2, 3)*v(537))
      v(354) = a(1, 1)*v(610)
      v(696) = a(4, 4)*v(354)
      v(355) = -(a(4, 5)*v(529))
      v(614) = v(343)+v(355)+v(385)
      v(369) = -(a(2, 5)*v(234))-a(2, 6)*v(240)+a(2, 4)*v(248)+a(1, 5)*v(337)+a(1, 6)*v(352)+a(1, 2)*(-v(345)+v(353))+a(4, 5)*v(354)+v&
      &(484)+a(3, 6)*v(614)
      v(356) = -(a(2, 2)*v(593))
      v(357) = a(2, 3)*v(576)
      v(358) = a(5, 6)*v(541)
      v(359) = -(a(2, 3)*v(577))
      v(691) = v(357)+v(359)
      v(626) = -v(356)-v(357)-v(359)
      v(616) = v(356)-v(606)+v(691)
      v(361) = a(5, 6)*v(529)
      v(671) = v(338)+v(339)+v(361)
      v(363) = a(2, 3)*v(554)
      v(364) = a(3, 6)*v(611)
      v(366) = a(2, 3)*v(565)
      v(673) = v(364)+v(366)
      v(370) = a(1, 4)*v(613)
      v(372) = -(a(2, 2)*v(592))
      v(681) = a(3, 3)*v(372)
      v(374) = a(1, 6)*v(541)
      v(377) = a(2, 2)*v(571)
      v(381) = a(1, 2)*v(612)
      v(698) = -v(377)+v(381)
      v(627) = v(377)-v(381)+v(423)
      v(390) = a(2, 6)*(-v(177)-v(182))+a(2, 2)*v(298)+a(3, 5)*v(380)+a(4, 5)*v(623)+a(3, 4)*v(627)+a(2, 4)*(v(197)+v(276)-v(353)+v&
      &(672))+a(4, 6)*(v(370)+v(674))+a(4, 4)*(v(398)-v(405)+v(683))+v(323)*v(74)
      v(382) = a(1, 1)*v(613)
      v(392) = a(2, 2)*v(176)+a(1, 4)*v(333)+a(4, 4)*(-v(378)+v(382))-v(476)+v(505)-a(3, 4)*v(614)+a(2, 5)*v(615)+a(4, 5)*v(616)+a(3&
      &, 5)*v(668)+a(2, 4)*(v(240)+v(670))
      v(384) = a(4, 6)*v(529)
      v(617) = v(372)+v(374)+v(384)
      v(391) = a(2, 4)*((-2d0)*v(234)-v(271))+a(2, 3)*v(322)+a(1, 3)*v(380)-v(471)-v(472)+v(477)+v(478)+v(481)+a(2, 2)*(-v(599)+v&
      &(602))+a(4, 6)*v(616)+a(3, 4)*v(617)+a(2, 6)*v(650)+v(696)
      v(389) = a(1, 2)*(v(185)+v(199))+v(369)+a(1, 6)*v(370)-a(4, 6)*v(382)-a(2, 6)*v(588)+a(3, 5)*(-(a(4, 6)*v(325))+v(617))+a(1, 3&
      &)*v(698)
      v(395) = a(3, 3)*v(539)
      v(397) = -(a(3, 5)*v(519))
      v(400) = a(1, 6)*v(528)
      v(403) = -(a(1, 3)*v(552))
      v(406) = 2d0*v(618)
      v(686) = -(a(3, 6)*v(406))
      v(407) = a(2, 6)*v(519)
      v(410) = a(2, 3)**2
      v(703) = v(162)*v(410)
      v(693) = -v(479)+v(701)+v(702)+v(703)
      v(684) = -(a(1, 1)*v(410))
      v(620) = a(4, 5)*v(410)
      v(495) = v(410)*v(619)
      v(489) = -(a(1, 1)*v(620))
      v(700) = v(486)+v(487)+v(488)+v(489)+v(490)
      v(485) = v(410)*v(571)
      v(482) = a(1, 6)*v(620)
      v(435) = v(410)*v(592)
      v(412) = a(2, 5)*v(519)
      v(413) = a(1, 2)*v(432)
      v(685) = -v(413)-v(422)-v(621)-v(622)-v(684)
      v(416) = -(a(2, 4)*a(2, 6)*a(3, 3))
      v(699) = a(1, 5)*v(416)
      v(689) = a(1, 1)*v(416)
      v(697) = a(2, 4)*v(415)+v(689)
      v(417) = -(a(2, 3)*v(578))
      v(690) = a(1, 2)*v(417)
      v(418) = -(a(2, 3)*v(628))
      v(427) = a(2, 6)*v(246)+a(3, 4)*v(354)+a(1, 4)*v(424)+v(434)+v(435)+a(2, 2)*(-v(269)-v(289)+v(436))+v(446)+a(1, 3)*(v(418)+v&
      &(623))+a(3, 6)*v(626)+v(681)+a(4, 6)*(v(413)+v(621)+v(622)+v(684))+v(690)+v(697)
      v(419) = a(3, 5)*v(624)
      v(421) = a(1, 5)*v(624)
      v(706) = v(333)-v(421)
      v(428) = -(a(2, 3)*v(278))+a(3, 4)*v(382)+a(3, 3)*(-v(343)-v(385))-a(1, 1)*v(419)+a(1, 3)*(-v(333)-v(370)+v(421))+a(4, 5)*(v&
      &(413)+v(422))+v(495)+a(2, 5)*v(625)+a(3, 5)*v(626)+a(1, 2)*v(656)+v(700)
      v(426) = a(1, 2)*v(292)+a(3, 4)*v(399)+a(2, 4)*(-v(249)+v(403))+a(2, 5)*v(417)+a(4, 5)*v(424)+a(3, 5)*(v(418)+v(442))+a(1, 6)*(v&
      &(419)+v(458))+a(2, 3)*(v(223)+v(467))+v(482)-v(485)+a(3, 3)*v(627)+a(2, 6)*v(663)+v(699)+a(3, 6)*(v(352)+v(464)+v(706))
      v(440) = a(2, 6)*v(628)
      v(443) = a(3, 6)*v(523)
      v(445) = a(3, 6)*v(519)
      v(447) = a(3, 6)*v(629)
      v(448) = -(a(2, 6)*v(603))
      v(451) = -(a(6, 6)*v(629))
      v(491) = a(5, 6)*v(519)
      v(497) = a(2, 5)*v(523)
      v(506) = a(2, 5)*v(528)
      v(507) = a(2, 5)*v(558)
      v(508) = -(a(4, 5)*v(519))
      v(511) = -(a(5, 5)*v(519))
      ai(1, 1) = v(124)/v(123)
      ai(1, 2) = -(v(126)/v(123))
      ai(1, 3) = v(128)/v(123)
      ai(1, 4) = -(v(130)/v(123))
      ai(1, 5) = v(132)/v(123)
      ai(1, 6) = -(v(134)/v(123))
      ai(2, 2) = (v(143)*v(144)+a(4, 6)*v(137)*v(152)-v(144)*v(161)+v(136)*(a(4, 4)*v(137)-v(141)+a(5, 5)*v(160)-v(163))-v(160)*v &
      & (165)+v(142)*v(166)-v(154)*v(166)-v(142)*v(167)+v(154)*v(167)+v(149)*(-v(173)+v(174)+v(179)-v(184))+v(155)*v(203)-v(151) &
      & *v(250)-v(137)*v(324)+v(160)*v(330)+v(164)*v(331)-v(142)*v(401)+v(143)*v(430)+v(161)*v(441)+v(143)*v(444)+a(5, 6)*(a(3, 4)*(-(a(3, 6)*v(147))-a(3, 5)*v(152))-v(155)*v(162)-v(157)*v(164)+v(151)*v(593)-a(1, 1)*v(631))+a(1, 6)*(-(v(148)*v(149)) &
      &+a(4, 5)*a(4, 6)*v(158)+a(3, 6)*v(630)+a(1, 5)*v(631))+a(6, 6)*(a(1, 1)*(v(141)+v(163))-v(146)*v(569)-a(1, 3)*v(630)+v(632))+v &
      &(140)*v(634)+v(153)*(-v(251)+v(638))+v(168)*(-v(268)+v(640))+a(3, 3)*(a(4, 6)*(a(5, 6)*v(147)-a(5, 5)*v(152))+v(642))+v(159 &
      &)*(a(5, 5)*v(164)+v(679)))/v(123)
      ai(2, 3) = (a(3, 6)*(a(2, 5)*(a(5, 6)*v(162)+v(175)-v(180)+v(190)-v(196)-v(203))+a(4, 6)*v(237)+v(201)*v(523)+a(1, 2)*(a(1, 6)*v &
      &(148)-v(179)+v(643))+a(2, 6)*(v(645)-v(646))+a(2, 4)*v(647))-a(4, 6)*(a(1, 6)*v(198)+a(4, 6)*(a(1, 2)*(-(a(1, 5)*a(3, 5))+a(1, 3 &
      &)*a(5, 5))-v(222)+v(229))-a(5, 6)*v(240)+a(2, 3)*v(647)+v(648))+a(5, 6)*(a(1, 6)*v(170)+a(1, 4)*v(185)+a(1, 2)*v(189)+a(2, 3)*(&
      &-v(175)+v(180)-v(190)+v(196))+v(293)+a(5, 6)*v(651))-a(6, 6)*(v(500)+v(510)+a(2, 3)*v(645)+a(5, 5)*v(651)+a(1, 5)*v(652)+a(2 &
      &, 5)*v(653)+a(4, 5)*(-(a(2, 3)*v(147))+v(186)+v(654)))-a(1, 6)*v(88))/v(123)
      ai(2, 4) = (-(a(3, 6)*v(367))-a(5, 6)*(a(3, 4)*v(221)+a(1, 5)*v(233)+a(2, 4)*v(245)+a(3, 5)*(-v(234)+a(1, 1)*v(532))+v(655)+a(1, 6 &
      &)*v(656)+a(5, 6)*v(657))+a(4, 6)*(a(2, 5)*v(245)+a(3, 5)*(v(221)+v(248))-a(1, 5)*v(249)+a(1, 2)*(a(1, 6)*v(137)-v(252))+a(5, 5 &
      &)*v(295)+a(3, 6)*(v(229)-v(340))+a(3, 3)*v(362)+v(409)-a(5, 6)*v(595)+a(2, 6)*(a(5, 5)*(-v(164)+v(543))-v(636)-v(658)-v(659) &
      &)+a(2, 3)*(a(3, 6)*(-(a(1, 1)*a(5, 5))+v(139))+v(660)+a(5, 6)*v(661)))+a(6, 6)*(-(a(1, 3)*v(198))+a(3, 3)*v(237)+a(3, 5)*(2d0*v &
      &(186)+v(232)+v(240)+v(241))+v(496)+a(2, 5)*(v(218)+v(239)-v(562))+a(5, 5)*v(657)+a(3, 4)*(-v(229)+v(662))-a(1, 5)*v(663)+a &
      &(2, 4)*v(664))+a(1, 6)*v(84))/v(123)
      ai(2, 5) = (-(a(4, 6)*v(300))+a(3, 6)*v(301)+a(5, 6)*v(302)-a(6, 6)*v(303)-a(1, 6)*v(79))/v(123)
      ai(2, 6) = (a(4, 5)*v(300)-a(3, 5)*v(301)-a(5, 5)*v(302)+a(5, 6)*v(303)+a(1, 5)*v(79))/v(123)
      ai(3, 3) = (a(1, 2)*a(2, 6)*a(4, 6)*v(143)-v(136)*v(308)+a(5, 5)*v(136)*v(323)+a(1, 6)*(a(2, 6)*(-v(306)+v(313))+v(148)*v(317)+a&
      &(1, 5)*v(318)-v(311)*v(323))+v(305)*(a(4, 4)*v(136)+a(1, 1)*v(159)+a(1, 4)*v(320)-v(324))+v(196)*v(326)+v(323)*v(330)+v(325&
      &)*v(331)+v(157)*v(341)+v(320)*v(344)-v(320)*v(347)-v(159)*v(360)+v(311)*v(380)-v(309)*v(394)+v(316)*v(402)+v(322)*v(404&
      &)+a(4, 4)*v(411)+v(310)*v(433)+a(5, 6)*(-(a(1, 1)*v(318))+a(1, 2)*(a(2, 6)*v(314)+a(1, 4)*v(319))-v(157)*v(325)-v(162)*v(326)&
      &-v(152)*v(563))-a(2, 6)*v(147)*v(612)+v(307)*v(634)+a(2, 2)*((-2d0)*a(5, 6)*v(203)+a(5, 5)*v(324)-a(6, 6)*v(641)+v(642))+v&
      &(317)*v(643)-v(312)*v(647)+v(159)*v(665)+v(321)*v(666)+v(159)*v(667)+v(168)*v(669)+a(6, 6)*(v(162)*v(305)+a(1, 2)*(a(2, 4&
      &)*v(161)+v(306)-v(313))+v(139)*v(323)+v(148)*v(325)+v(705)))/v(123)
      ai(3, 4) = (a(2, 6)*v(367)+a(5, 6)*(a(1, 6)*v(333)+a(2, 5)*v(342)+a(1, 3)*v(358)+v(369)+a(5, 6)*v(616)+a(2, 6)*v(670)+a(3, 4)*v&
      &(671))-a(1, 6)*(a(2, 3)*v(201)+a(2, 2)*(v(202)-v(207)+v(219))+a(1, 2)*(v(208)-v(213)+v(225))+a(5, 5)*v(337)+a(3, 6)*(-(a(1, 4&
      &)*v(305))-v(344)+v(347))+a(3, 5)*v(358)+a(2, 4)*v(363)+a(4, 5)*v(386)+v(305)*v(578)+a(2, 5)*(v(334)+v(345)+v(353)+v(672))+a&
      &(1, 4)*v(673)+a(5, 6)*v(674))-a(4, 6)*(a(5, 5)*v(354)+a(2, 3)*v(362)+a(1, 2)*(-v(206)+v(212)-v(363))+a(5, 6)*v(378)+v(408)-v&
      &(305)*v(618)+a(2, 2)*v(660)+a(2, 6)*(v(340)+v(662))+a(3, 5)*(v(341)+v(671))-a(1, 1)*v(673)+a(3, 6)*(v(665)+v(667)+v(675)+v&
      &(676)))-a(6, 6)*(a(1, 2)*v(198)+a(2, 3)*v(237)+a(1, 3)*v(344)+a(1, 5)*v(352)+v(502)+v(509)+a(3, 5)*(-v(385)+v(614))+a(5, 5)*v&
      &(616)+a(3, 4)*(v(360)-v(675)-v(676))+a(2, 5)*(-v(240)-v(241)+2d0*v(278)+v(677))+a(2, 4)*(-v(340)-v(678))))/v(123)
      ai(3, 5) = (-(a(2, 6)*v(301))+a(4, 6)*v(389)+a(1, 6)*v(390)-a(5, 6)*v(391)+a(6, 6)*v(392))/v(123)
      ai(3, 6) = (a(2, 5)*v(301)-a(4, 5)*v(389)-a(1, 5)*v(390)+a(5, 5)*v(391)-a(5, 6)*v(392))/v(123)
      ai(4, 4) = (a(2, 2)*v(136)*v(137)-v(252)*v(317)-v(245)*v(326)+v(155)*v(341)-v(140)*v(360)+v(155)*v(361)+v(394)*v(395)+a(2, 5&
      &)*v(136)*v(397)+v(394)*v(397)+v(402)*v(403)+v(364)*v(406)-2d0*a(2, 6)*v(409)+v(330)*v(410)+a(3, 3)*v(411)+(v(166)-v(167)&
      &-v(401))*v(412)-v(404)*v(415)-v(311)*v(424)-v(363)*v(437)-v(363)*v(438)+v(139)*v(449)+v(137)*v(452)+a(5, 5)*(v(164)*v&
      &(307)+a(1, 3)*a(3, 6)*v(317)+v(140)*v(325)+v(136)*v(410)+v(453)+v(454))+v(155)*v(469)+v(326)*v(475)+v(401)*v(504)+2d0*v&
      &(405)*v(531)+a(1, 6)*(v(137)*v(317)-v(311)*v(410)+v(403)*v(539)-v(395)*v(550))+v(407)*v(638)+v(140)*v(665)+v(140)*v(675)&
      &+v(307)*(v(659)+v(679))+a(6, 6)*(-(a(2, 2)*v(244))+v(164)*v(305)+v(137)*v(325)+v(139)*v(410)+a(5, 5)*v(413)+v(382)*v(528)&
      &-v(158)*v(680))+v(408)*v(682)+a(5, 6)*((-2d0)*a(3, 3)*v(338)-v(398)*v(527)+v(397)*(a(1, 1)*a(2, 6)-v(608))+v(149)*(-v(609)&
      &+v(680))+v(378)*v(682))+v(400)*v(683)+v(168)*v(685)+v(305)*(a(3, 3)*v(136)+a(1, 1)*v(140)+v(450)+v(686)))/v(123)
      ai(4, 5) = (a(2, 6)*v(300)-a(3, 6)*v(389)-a(1, 6)*v(426)+a(5, 6)*v(427)-a(6, 6)*v(428))/v(123)
      ai(4, 6) = (-(a(2, 5)*v(300))+a(3, 5)*v(389)+a(1, 5)*v(426)-a(5, 5)*v(427)+a(5, 6)*v(428))/v(123)
      ai(5, 5) = (a(1, 6)*v(160)*v(317)+v(269)*v(321)-v(324)*v(410)+v(430)*v(431)+v(432)*v(433)+v(312)*v(436)+v(439)*v(440)-v(418&
      &)*v(443)+v(442)*v(443)+v(431)*v(444)-a(2, 6)*v(162)*v(445)+v(322)*v(445)+v(162)*v(449)+v(323)*v(450)+v(160)*v(452)+a(4, 4&
      &)*v(453)+a(4, 4)*v(454)-v(317)*v(456)+v(440)*v(457)+v(320)*v(459)+v(438)*v(460)+v(320)*v(463)+v(437)*v(465)+v(441)*v(466&
      &)+v(149)*v(468)+v(406)*v(474)+v(153)*v(483)-a(2, 6)*v(417)*v(523)+v(448)*(-v(247)+v(596))+v(447)*(-v(384)+v(617))+v(140&
      &)*v(668)+v(159)*v(685)+v(323)*v(686)-v(307)*v(687)+a(4, 6)*((-2d0)*v(434)-2d0*v(435)-2d0*v(446)-2d0*v(681)+v(359)*v(682)&
      &+2d0*v(688)-2d0*v(689)-2d0*v(690))+v(451)*v(691)+v(407)*(v(297)+v(692))+a(6, 6)*(a(2, 2)*v(288)+v(246)*v(525)+v(693))+v&
      &(136)*(a(2, 2)*v(160)+a(3, 3)*v(323)+a(4, 4)*v(410)+v(694)))/v(123)
      ai(5, 6) = (-(a(2, 4)*v(270))-a(2, 4)*v(280)-a(2, 4)*v(281)-v(245)*v(323)+v(160)*v(338)+v(160)*v(339)+v(160)*v(361)-v(175)*v&
      &(410)+v(196)*v(410)-v(172)*v(437)+v(358)*v(439)+v(358)*v(457)+a(2, 3)*(a(2, 6)*v(176)+a(1, 6)*(2d0*v(170)+v(177))-a(4, 4)*v&
      &(248)-a(1, 2)*v(296)-a(1, 5)*v(465))+a(3, 3)*(-(a(2, 2)*v(180))+a(2, 2)*v(190)-a(1, 5)*a(1, 6)*v(323)-a(4, 4)*v(339)+a(1, 5)*v&
      &(468))+v(160)*v(469)+v(323)*v(475)+a(3, 6)*(-(a(1, 2)*v(170))-a(2, 2)*v(178)+a(2, 4)*(-v(232)+v(279))-a(4, 4)*v(382)+v(476))&
      &+v(191)*v(491)+v(192)*v(491)+v(189)*v(498)+v(326)*v(501)-v(474)*v(572)+v(456)*v(609)+a(3, 4)*(a(1, 3)*v(377)+a(1, 6)*v(464&
      &)+a(1, 2)*(-v(185)-v(467))+v(484)+a(2, 6)*v(654))+a(2, 5)*(a(2, 6)*v(288)+a(1, 4)*v(417)+a(2, 4)*(-v(289)+v(598))-a(3, 6)*v&
      &(615)+v(695))+a(3, 5)*(-(a(2, 6)*v(194))-a(3, 4)*v(374)+a(1, 4)*v(418)+v(471)+v(472)-v(477)-v(478)-v(481)+a(2, 2)*v(692)-v&
      &(696))+a(4, 5)*(a(1, 2)*v(233)-a(2, 2)*v(269)+a(1, 3)*v(337)+a(3, 4)*(v(354)-v(483))+a(2, 6)*v(625)+a(3, 6)*(v(357)+v(626))-v&
      &(688)+v(697))+a(1, 4)*(-(a(2, 4)*v(403))+a(1, 6)*v(419)+a(3, 6)*v(421)+v(482)+v(485)+a(3, 3)*v(698)-v(699))+a(4, 6)*(a(1, 2)*v&
      &(217)-a(2, 4)*v(290)-a(1, 3)*v(387)-a(1, 1)*v(458)+a(1, 5)*v(463)+a(2, 5)*(v(596)-v(625))-a(3, 5)*v(691)+v(700))+a(5, 6)*(v&
      &(479)+v(512)+v(513)+v(514)+a(1, 1)*v(694)-v(701)-v(702)-v(703)-v(704)))/v(123)
      ai(6, 6) = (-(v(160)*v(360))-v(154)*v(378)+v(137)*v(383)+(-v(176)-v(178)-v(181))*v(412)-v(148)*v(422)-v(344)*v(439)+v(139&
      &)*v(492)+v(222)*v(499)+v(176)*v(504)+v(193)*v(506)-v(154)*v(507)+v(142)*(v(378)-v(382)+v(507))+v(186)*v(508)+v(188)*v&
      &(508)+v(191)*v(511)+v(193)*v(511)-v(500)*v(519)-v(510)*v(519)+a(4, 4)*v(395)*v(521)+a(1, 5)*v(228)*v(525)+v(496)*v(525)+v&
      &(505)*v(528)-v(494)*v(539)-v(503)*v(539)+a(4, 5)*(a(1, 2)*(a(2, 4)*v(158)-2d0*v(217))-2d0*v(495)-v(243)*v(539))-v(506)*v&
      &(560)+v(497)*(-v(217)+v(224)+v(228)+a(2, 3)*v(569))+v(137)*v(605)-v(148)*v(622)-v(502)*v(629)-v(509)*v(629)+v(305)*(-v&
      &(590)+v(640))-v(410)*v(645)+a(2, 2)*(v(632)+a(3, 3)*(v(195)-v(641))+v(142)*(-v(572)+v(661)))+v(323)*v(664)+a(5, 5)*(-v(512&
      &)-v(513)-v(514)+v(243)*v(525)-v(498)*v(526)+v(693)+v(704))+a(3, 3)*v(705)+v(146)*(-v(352)-v(464)+v(706)))/v(123)
      DO i01 = 2, 6
        DO i02 = 1, i01-1
          ai(i01, i02) = ai(i02, i01)
        END DO
      END DO
      det = v(123)
      RETURN
    END IF
    CALL matrixinverse(n, det, a, ai)
  END SUBROUTINE matrixinversesymmetric
!> identity matrix components
  REAL(8) FUNCTION matrixkronecker(i, j)
!*** uma comparacao+uma atribuiã§ã‚ã£o
    IF (i .NE. j) THEN
      matrixkronecker = 0.0d00
    ELSE
      matrixkronecker = 1.0d00
    END IF
  END FUNCTION matrixkronecker
!> lu decomposition for a matrix
  SUBROUTINE matrixludecomposition(n, a, kpiv, ierr)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a
    INTEGER, DIMENSION(*)::kpiv
    INTEGER::ierr
    REAL(8), PARAMETER::small = 2.0d-16
    ierr = 0
    DO k = 1, n-1
      igash = k-1
      l = vectindexmaxabs(n-igash, a(k, k))+igash
      kpiv(k) = l
      IF (abs(a(l, k)) .LE. small) THEN
        ierr = k
        RETURN
      END IF
!*** swaps rows l and k for column k
      rtemp = a(l, k)
      a(l, k) = a(k, k)
      a(k, k) = rtemp
!*** divides by pivot
      a(k+1:n, k) = -a(k+1:n, k)/rtemp
      DO j = k+1, n
        rtemp = a(l, j)
        a(l, j) = a(k, j)
        a(k, j) = rtemp
        a(k+1:n, j) = a(k+1:n, j)+rtemp*a(k+1:n, k)
      END DO
    END DO
    kpiv(n) = n
    IF (abs(a(n, n)) .LE. epsmach()) ierr = n
  END SUBROUTINE matrixludecomposition
!> lu substitution for a matrix
  SUBROUTINE matrixlusubstitution(n, a, kpiv, b)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a
    REAL(8), DIMENSION(*)::b
    INTEGER, DIMENSION(*)::kpiv
    DO k = 1, n-1
      l = kpiv(k)
      t = b(l)
      b(l) = b(k)
      b(k) = t
      CALL vectaxpy(n-k, b(k+1), t, a(k+1, k), b(k+1))
    END DO
    CALL matrixtriangularsolve(n, a, b, 1)
  END SUBROUTINE matrixlusubstitution
!> matrix product 3x3 matrices
!> BULLETIN OF THE
!> AMERICAN MATHEMATICAL SOCIETY
!> Volume 82, Number 1, January 1976
!> A NONCOMMUTATIVE ALGORITHM
!> FOR MULTIPLYING 3 x 3 MATRICES
!> USING 23 MULTIPLICATIONS
!> BY JULIAN D. LADERMAN
  SUBROUTINE matrixmatrixproduct33(c, a, b)
    IMPLICIT NONE
    REAL(8) v(78), c(3, 3), a(3, 3), b(3, 3)
    v(71) = b(2, 3)+b(3, 1)-b(3, 3)
    v(70) = a(2, 2)+a(2, 3)
    v(69) = (-b(3, 1)+b(3, 3))*v(70)
    v(68) = b(2, 2)+b(3, 1)-b(3, 2)
    v(67) = a(3, 2)+a(3, 3)
    v(66) = (-b(3, 1)+b(3, 2))*v(67)
    v(65) = a(1, 3)*b(3, 1)
    v(64) = -b(1, 1)+b(2, 1)-b(2, 2)-b(2, 3)-b(3, 1)
    v(63) = (-a(1, 1)+a(3, 1))*(b(1, 3)-b(2, 3))
    v(62) = b(1, 1)-b(1, 3)+b(2, 3)
    v(61) = a(3, 1)+a(3, 2)
    v(60) = -a(1, 1)+a(2, 1)+a(2, 2)
    v(59) = -b(1, 2)+b(2, 2)
    v(58) = (a(1, 1)-a(2, 1))*v(59)
    v(57) = (-b(1, 1)+b(1, 3))*v(61)
    v(56) = a(1, 1)*b(1, 1)
    v(55) = a(1, 1)+a(1, 2)+a(1, 3)-a(2, 2)-a(3, 2)
    v(35) = (b(1, 1)+v(59))*v(60)
    v(37) = v(35)+v(56)+(-b(1, 1)+b(1, 2))*(a(1, 1)+v(60))
    v(44) = (-a(1, 1)+v(61))*v(62)
    v(51) = v(44)+v(56)+v(63)
    v(45) = (-a(1, 3)+v(67))*v(68)
    v(72) = v(45)+v(65)
    v(48) = (a(1, 3)-a(3, 3))*(-b(3, 1)+v(68))+v(72)
    v(41) = (-a(1, 3)+v(70))*v(71)
    v(40) = (a(1, 3)-a(2, 3))*(-b(3, 1)+v(71))
    v(34) = v(41)+v(56)+v(65)
    v(73) = v(34)+v(40)
    c(1, 1) = a(1, 2)*b(2, 1)+v(34)-v(41)
    c(1, 2) = v(37)+b(2, 2)*(-a(2, 1)-a(3, 3)+v(55))+v(66)+v(72)
    c(1, 3) = v(34)+v(44)+b(2, 3)*(-a(2, 3)-a(3, 1)+v(55))+v(57)+v(69)
    c(2, 1) = v(35)+v(58)+a(2, 2)*(b(1, 2)+b(3, 3)+v(64))+v(73)
    c(2, 2) = a(2, 3)*b(3, 2)+v(37)+v(58)
    c(2, 3) = a(2, 1)*b(1, 3)-v(56)+v(69)+v(73)
    c(3, 1) = v(48)+v(51)+a(3, 2)*(b(1, 3)+b(3, 2)+v(64))
    c(3, 2) = a(3, 1)*b(1, 2)+v(48)+v(66)
    c(3, 3) = a(3, 3)*b(3, 3)+v(51)+v(57)
  END SUBROUTINE matrixmatrixproduct33
!> matrix product c=a.b^T 3x3 matrices
  SUBROUTINE matrixmatrixproduct33t(c, a, b)
    IMPLICIT NONE
    REAL(8) v(78), c(3, 3), a(3, 3), b(3, 3)
    v(71) = b(1, 3)+b(3, 2)-b(3, 3)
    v(70) = a(2, 2)+a(2, 3)
    v(69) = (-b(1, 3)+b(3, 3))*v(70)
    v(68) = -b(1, 1)+b(1, 2)-b(1, 3)-b(2, 2)-b(3, 2)
    v(67) = (-a(1, 1)+a(3, 1))*(b(3, 1)-b(3, 2))
    v(66) = b(1, 1)-b(3, 1)+b(3, 2)
    v(65) = a(3, 1)+a(3, 2)
    v(64) = (-b(1, 1)+b(3, 1))*v(65)
    v(63) = b(1, 3)+b(2, 2)-b(2, 3)
    v(62) = a(3, 2)+a(3, 3)
    v(61) = (-b(1, 3)+b(2, 3))*v(62)
    v(60) = -a(1, 1)+a(2, 1)+a(2, 2)
    v(59) = -b(2, 1)+b(2, 2)
    v(58) = (a(1, 1)-a(2, 1))*v(59)
    v(57) = a(1, 3)*b(1, 3)
    v(56) = a(1, 1)*b(1, 1)
    v(55) = a(1, 1)+a(1, 2)+a(1, 3)-a(2, 2)-a(3, 2)
    v(35) = (b(1, 1)+v(59))*v(60)
    v(37) = v(35)+v(56)+(-b(1, 1)+b(2, 1))*(a(1, 1)+v(60))
    v(44) = (-a(1, 3)+v(62))*v(63)
    v(72) = v(44)+v(57)
    v(48) = (a(1, 3)-a(3, 3))*(-b(1, 3)+v(63))+v(72)
    v(46) = (-a(1, 1)+v(65))*v(66)
    v(51) = v(46)+v(56)+v(67)
    v(41) = (-a(1, 3)+v(70))*v(71)
    v(40) = (a(1, 3)-a(2, 3))*(-b(1, 3)+v(71))
    v(34) = v(41)+v(56)+v(57)
    v(73) = v(34)+v(40)
    c(1, 1) = a(1, 2)*b(1, 2)+v(34)-v(41)
    c(1, 2) = v(37)+b(2, 2)*(-a(2, 1)-a(3, 3)+v(55))+v(61)+v(72)
    c(1, 3) = v(34)+v(46)+b(3, 2)*(-a(2, 3)-a(3, 1)+v(55))+v(64)+v(69)
    c(2, 1) = v(35)+v(58)+a(2, 2)*(b(2, 1)+b(3, 3)+v(68))+v(73)
    c(2, 2) = a(2, 3)*b(2, 3)+v(37)+v(58)
    c(2, 3) = a(2, 1)*b(3, 1)-v(56)+v(69)+v(73)
    c(3, 1) = v(48)+v(51)+a(3, 2)*(b(2, 3)+b(3, 1)+v(68))
    c(3, 2) = a(3, 1)*b(2, 1)+v(48)+v(61)
    c(3, 3) = a(3, 3)*b(3, 3)+v(51)+v(64)
  END SUBROUTINE matrixmatrixproduct33t
!> matrix product c=a^T.b 3x3 matrices
  SUBROUTINE matrixmatrixproduct3t3(c, a, b)
    IMPLICIT NONE
    REAL(8) v(78), c(3, 3), a(3, 3), b(3, 3)
    v(71) = b(2, 3)+b(3, 1)-b(3, 3)
    v(70) = a(2, 2)+a(3, 2)
    v(69) = (-b(3, 1)+b(3, 3))*v(70)
    v(68) = b(2, 2)+b(3, 1)-b(3, 2)
    v(67) = a(2, 3)+a(3, 3)
    v(66) = (-b(3, 1)+b(3, 2))*v(67)
    v(65) = a(3, 1)*b(3, 1)
    v(64) = -b(1, 1)+b(2, 1)-b(2, 2)-b(2, 3)-b(3, 1)
    v(63) = (-a(1, 1)+a(1, 3))*(b(1, 3)-b(2, 3))
    v(62) = b(1, 1)-b(1, 3)+b(2, 3)
    v(61) = a(1, 3)+a(2, 3)
    v(60) = -a(1, 1)+a(1, 2)+a(2, 2)
    v(59) = -b(1, 2)+b(2, 2)
    v(58) = (a(1, 1)-a(1, 2))*v(59)
    v(57) = (-b(1, 1)+b(1, 3))*v(61)
    v(56) = a(1, 1)*b(1, 1)
    v(55) = a(1, 1)+a(2, 1)-a(2, 2)-a(2, 3)+a(3, 1)
    v(35) = (b(1, 1)+v(59))*v(60)
    v(37) = v(35)+v(56)+(-b(1, 1)+b(1, 2))*(a(1, 1)+v(60))
    v(44) = (-a(1, 1)+v(61))*v(62)
    v(51) = v(44)+v(56)+v(63)
    v(45) = (-a(3, 1)+v(67))*v(68)
    v(72) = v(45)+v(65)
    v(48) = (a(3, 1)-a(3, 3))*(-b(3, 1)+v(68))+v(72)
    v(41) = (-a(3, 1)+v(70))*v(71)
    v(40) = (a(3, 1)-a(3, 2))*(-b(3, 1)+v(71))
    v(34) = v(41)+v(56)+v(65)
    v(73) = v(34)+v(40)
    c(1, 1) = a(2, 1)*b(2, 1)+v(34)-v(41)
    c(1, 2) = v(37)+b(2, 2)*(-a(1, 2)-a(3, 3)+v(55))+v(66)+v(72)
    c(1, 3) = v(34)+v(44)+b(2, 3)*(-a(1, 3)-a(3, 2)+v(55))+v(57)+v(69)
    c(2, 1) = v(35)+v(58)+a(2, 2)*(b(1, 2)+b(3, 3)+v(64))+v(73)
    c(2, 2) = a(3, 2)*b(3, 2)+v(37)+v(58)
    c(2, 3) = a(1, 2)*b(1, 3)-v(56)+v(69)+v(73)
    c(3, 1) = v(48)+v(51)+a(2, 3)*(b(1, 3)+b(3, 2)+v(64))
    c(3, 2) = a(1, 3)*b(1, 2)+v(48)+v(66)
    c(3, 3) = a(3, 3)*b(3, 3)+v(51)+v(57)
  END SUBROUTINE matrixmatrixproduct3t3
!> matrix product c=a^T.b^T 3x3 matrices
  SUBROUTINE matrixmatrixproduct3t3t(c, a, b)
    IMPLICIT NONE
    REAL(8) v(78), c(3, 3), a(3, 3), b(3, 3)
    v(71) = b(1, 3)+b(3, 2)-b(3, 3)
    v(70) = a(2, 2)+a(3, 2)
    v(69) = (-b(1, 3)+b(3, 3))*v(70)
    v(68) = -b(1, 1)+b(1, 2)-b(1, 3)-b(2, 2)-b(3, 2)
    v(67) = (-a(1, 1)+a(1, 3))*(b(3, 1)-b(3, 2))
    v(66) = b(1, 1)-b(3, 1)+b(3, 2)
    v(65) = a(1, 3)+a(2, 3)
    v(64) = (-b(1, 1)+b(3, 1))*v(65)
    v(63) = b(1, 3)+b(2, 2)-b(2, 3)
    v(62) = a(2, 3)+a(3, 3)
    v(61) = (-b(1, 3)+b(2, 3))*v(62)
    v(60) = -a(1, 1)+a(1, 2)+a(2, 2)
    v(59) = -b(2, 1)+b(2, 2)
    v(58) = (a(1, 1)-a(1, 2))*v(59)
    v(57) = a(3, 1)*b(1, 3)
    v(56) = a(1, 1)*b(1, 1)
    v(55) = a(1, 1)+a(2, 1)-a(2, 2)-a(2, 3)+a(3, 1)
    v(35) = (b(1, 1)+v(59))*v(60)
    v(37) = v(35)+v(56)+(-b(1, 1)+b(2, 1))*(a(1, 1)+v(60))
    v(44) = (-a(3, 1)+v(62))*v(63)
    v(72) = v(44)+v(57)
    v(48) = (a(3, 1)-a(3, 3))*(-b(1, 3)+v(63))+v(72)
    v(46) = (-a(1, 1)+v(65))*v(66)
    v(51) = v(46)+v(56)+v(67)
    v(41) = (-a(3, 1)+v(70))*v(71)
    v(40) = (a(3, 1)-a(3, 2))*(-b(1, 3)+v(71))
    v(34) = v(41)+v(56)+v(57)
    v(73) = v(34)+v(40)
    c(1, 1) = a(2, 1)*b(1, 2)+v(34)-v(41)
    c(1, 2) = v(37)+b(2, 2)*(-a(1, 2)-a(3, 3)+v(55))+v(61)+v(72)
    c(1, 3) = v(34)+v(46)+b(3, 2)*(-a(1, 3)-a(3, 2)+v(55))+v(64)+v(69)
    c(2, 1) = v(35)+v(58)+a(2, 2)*(b(2, 1)+b(3, 3)+v(68))+v(73)
    c(2, 2) = a(3, 2)*b(2, 3)+v(37)+v(58)
    c(2, 3) = a(1, 2)*b(3, 1)-v(56)+v(69)+v(73)
    c(3, 1) = v(48)+v(51)+a(2, 3)*(b(2, 3)+b(3, 1)+v(68))
    c(3, 2) = a(1, 3)*b(2, 1)+v(48)+v(61)
    c(3, 3) = a(3, 3)*b(3, 3)+v(51)+v(64)
  END SUBROUTINE matrixmatrixproduct3t3t
!> matrix matrix product, general
  SUBROUTINE matrixmatrixproduct(m, r, n, a, b, c, ktyp)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::r
    REAL(8), DIMENSION(m, *)::a
    REAL(8), DIMENSION(r, *)::b
    REAL(8), DIMENSION(m, *)::c
    ityp = abs(ktyp)
!*** check for problems
    IF (loc(a(1, 1)) .EQ. loc(c(1, 1))) STOP "error in memory addresses calling matrixmatrixproduct(ac)"
    IF (loc(b(1, 1)) .EQ. loc(c(1, 1))) STOP "error in memory addresses calling matrixmatrixproduct(bc)"
!*** quick check
    IF (ityp .GE. 2) THEN
      IF (m .NE. r .OR. m .NE. n) STOP "illegal operation in matrixmatrixproduct-square matrices only for ityp>1"
    END IF
    IF (ktyp .GE. 0) CALL vectsetconst(n*m, c, 0.0d00)
    SELECT CASE (ityp)
    CASE (0:1)
!$omp parallel do private(i, k) shared(a, b, c, m, n, r)
      DO j = 1, n
        DO k = 1, r
          DO i = 1, m
            c(i, j) = c(i, j)+a(i, k)*b(k, j)
          END DO
        END DO
      END DO
!$omp end parallel do
    CASE (2)
!$omp parallel do collapse(2) private(k) shared(a, b, c, m, n, r)
      DO j = 1, n
        DO i = 1, m
          DO k = 1, r
            c(i, j) = c(i, j)+a(k, i)*b(k, j)
          END DO
        END DO
      END DO
!$omp end parallel do
    CASE (3)
!$omp parallel do private(i, j) shared(a, b, c, m, n, r)
      DO k = 1, r
        DO j = 1, n
          DO i = 1, m
            c(i, j) = c(i, j)+a(i, k)*b(j, k)
          END DO
        END DO
      END DO
!$omp end parallel do
    CASE (4)
!$omp parallel do private(i, k) shared(a, b, c, m, n, r)
      DO j = 1, n
        DO k = 1, r
          DO i = 1, m
            c(i, j) = c(i, j)+a(k, i)*b(j, k)
          END DO
        END DO
      END DO
!$omp end parallel do
    CASE default
      STOP "illegal request in matrixmatrixproduct"
    END SELECT
  END SUBROUTINE matrixmatrixproduct
!> frobenius norm of a matrix
  REAL(8) FUNCTION matrixnormfrobenius(n, a)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a
    r = vectdot(n*n, a, a)
    matrixnormfrobenius = sqrt(r)
  END FUNCTION matrixnormfrobenius
!> power of a matrix
  SUBROUTINE matrixpower(x, nx, n)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(nx, *)::x
    REAL(8), DIMENSION(nx, nx)::xtemp, ytemp
    IF (n .LT. 0) STOP "wrong call to matrixpower"
    IF (n .GT. 0) THEN
      CALL vectcopy(nx*nx, xtemp, x)
      DO i = 1, n-1
        CALL matrixmatrixproduct(nx, nx, nx, xtemp, x, ytemp, 0)
        xtemp = ytemp
      END DO
      CALL vectcopy(nx*nx, x, ytemp)
    ELSE
      DO i = 1, nx
        DO j = 1, nx
          x(j, i) = matrixkronecker(j, i)
        END DO
      END DO
    END IF
  END SUBROUTINE matrixpower
!> prints a matrix
  SUBROUTINE matrixprint(mtx)
    REAL(8), DIMENSION(:, :)::mtx
    m = size(mtx, 1)
    n = size(mtx, 2)
    DO i = 1, m
      DO j = 1, n
        WRITE (*, "(a,2i6,e15.6)") "row, column, value", i, j, mtx(i, j)
      END DO
    END DO
  END SUBROUTINE matrixprint
!> generates random orthogonal matrix
  SUBROUTINE matrixgenerateorthogonal(r)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), INTENT(out) :: r(3, 3)
    REAL(8) :: alpha, beta, gamma
    REAL(8) :: ca, sa, cb, sb, cg, sg
    REAL::r1, r2, r3
    CALL random_seed(iseed)
    CALL random_number(r1)
    CALL random_number(r2)
    CALL random_number(r3)
! generate random euler angles
    alpha = 2.0*3.141592653589793*r1
    beta = 2.0*3.141592653589793*r2
    gamma = 2.0*3.141592653589793*r3
! compute sines and cosines of the angles
    ca = cos(alpha)
    sa = sin(alpha)
    cb = cos(beta)
    sb = sin(beta)
    cg = cos(gamma)
    sg = sin(gamma)
! construct the rotation matrix
    r(1, 1) = ca*cb
    r(1, 2) = ca*sb*sg-sa*cg
    r(1, 3) = ca*sb*cg+sa*sg
    r(2, 1) = sa*cb
    r(2, 2) = sa*sb*sg+ca*cg
    r(2, 3) = sa*sb*cg-ca*sg
    r(3, 1) = -sb
    r(3, 2) = cb*sg
    r(3, 3) = cb*cg
  END SUBROUTINE matrixgenerateorthogonal
!> solve a linear system (overwrites b)
  SUBROUTINE matrixsolvesystem(n, a, b)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::b
    REAL(8), DIMENSION(n, *)::a
    INTEGER, DIMENSION(n)::kpiv
    CALL matrixludecomposition(n, a, kpiv, ierr)
    CALL matrixlusubstitution(n, a, kpiv, b)
  END SUBROUTINE matrixsolvesystem
!> obtains a symmmetric version of matrix mat
  SUBROUTINE matrixsymmetrize(n, mat, mats)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::mat, mats
    DO j = 1, n
      DO i = 1, n
        mats(i, j) = 0.5d00*(mat(i, j)+mat(j, i))
      END DO
    END DO
  END SUBROUTINE matrixsymmetrize
!> performs a rank-one udpdate on a
  SUBROUTINE matrixtensorupdate(n, a, x, y)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a
    REAL(8), DIMENSION(*)::x, y
    DO j = 1, n
      ytemp = y(j)
      DO i = 1, n
        xtemp = x(i)
        a(i, j) = xtemp*ytemp+a(i, j)
      END DO
    END DO
  END SUBROUTINE matrixtensorupdate
!> transposes a matrix (use sparingly)
  SUBROUTINE matrixtranspose(n, a, at)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::a, at
    DO i = 1, n
      DO j = i+1, n
        at(i, j) = a(j, i)
      END DO
      DO j = 1, i-1
        at(i, j) = a(j, i)
      END DO
      at(i, i) = a(i, i)
    END DO
  END SUBROUTINE matrixtranspose
!> solve a triangular dense system
  SUBROUTINE matrixtriangularsolve(n, ul, b, ityp)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n, *)::ul
    REAL(8), DIMENSION(*)::b
    SELECT CASE (ityp)
!*** back (u) - the second to do
    CASE (1) ! ub=b
      DO j = n, 2, -1
        b(j) = b(j)/ul(j, j)
        rtemp = -b(j)
!*** b(1:j-1)=b(1:j-1)-b(j)*ul(1:j-1,j)
        CALL vectaxpy(j-1, b, rtemp, ul(1, j), b)
      END DO
      b(1) = b(1)/ul(1, 1)
!*** forward (l) - the first to do
    CASE (2)
      DO j = 1, n-1
        b(j) = b(j)/ul(j, j)
        rtemp = -b(j)
!*** b(j+1:n)=b(j+1:n)-b(j)*ul(j+1:n,j)
        CALL vectaxpy(n-j, b(j+1), rtemp, ul(j+1, j), b(j+1))
      END DO
      b(n) = b(n)/ul(n, n)
    END SELECT
  END SUBROUTINE matrixtriangularsolve
!> performs a matrix times vector product dense
  SUBROUTINE matrixvectorproduct(m, n, a, x, z)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(m, *)::a
    REAL(8), DIMENSION(*)::x
    REAL(8), DIMENSION(*)::z
    CALL vectsetconst(m, z)
    DO j = 1, n
      CALL vectaxpy(m, z(1), x(j), a(1, j), z(1))
    END DO
  END SUBROUTINE matrixvectorproduct
!> performs a matrix times vector product dense
  SUBROUTINE matrixtranspvectorproduct(m, n, a, x, z)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(m, *)::a
    REAL(8), DIMENSION(*)::x
    REAL(8), DIMENSION(*)::z
    DO i = 1, m
      z(i) = vectdot(n, x, a(1, i))
    END DO
  END SUBROUTINE matrixtranspvectorproduct
!> moving least squares 2d determines the kernel for two nodes k and l
  SUBROUTINE movingleastsquares2d(nk, nl, f, s, ds, kernel)
    IMPLICIT NONE
    REAL(8) v(67), nk(2), nl(2), f(2, 2), s(3), ds(3, 3), kernel(2, 2)
    v(53) = nk(2)*(nl(2)*s(2)+nl(1)*s(3))+nk(1)*(nl(1)*s(1)+nl(2)*s(3))
    v(52) = f(2, 2)*nk(1)+f(2, 1)*nk(2)
    v(51) = f(1, 2)*nk(1)+f(1, 1)*nk(2)
    v(50) = f(2, 2)*nk(2)
    v(49) = f(1, 2)*nk(2)
    v(48) = f(2, 1)*nk(1)
    v(47) = f(1, 1)*nk(1)
    v(46) = f(2, 2)*nl(1)+f(2, 1)*nl(2)
    v(45) = f(1, 2)*nl(1)+f(1, 1)*nl(2)
    v(44) = f(2, 2)*nl(2)
    v(60) = ds(2, 2)*v(44)
    v(59) = ds(1, 2)*v(44)+ds(1, 3)*v(46)
    v(43) = f(1, 2)*nl(2)
    v(54) = ds(1, 2)*v(43)+ds(1, 3)*v(45)
    v(42) = f(2, 1)*nl(1)
    v(62) = ds(3, 1)*v(42)+ds(3, 2)*v(44)+ds(3, 3)*v(46)
    v(61) = ds(2, 1)*v(42)+ds(2, 3)*v(46)
    v(58) = ds(1, 1)*v(42)
    v(41) = f(1, 1)*nl(1)
    v(56) = ds(3, 1)*v(41)+ds(3, 2)*v(43)+ds(3, 3)*v(45)
    v(55) = ds(2, 1)*v(41)+ds(2, 3)*v(45)
    v(38) = v(47)*v(58)
    v(39) = v(49)*v(60)
    v(57) = v(38)+v(39)
    kernel(1, 1) = v(53)+v(47)*(ds(1, 1)*v(41)+v(54))+v(49)*(ds(2, 2)*v(43)+v(55))+v(51)*v(56)
    kernel(1, 2) = v(57)+v(47)*v(59)+v(49)*v(61)+v(51)*v(62)
    kernel(2, 1) = v(48)*v(54)+v(50)*v(55)+v(52)*v(56)+v(57)
    kernel(2, 2) = v(53)+v(48)*(v(58)+v(59))+v(50)*(v(60)+v(61))+v(52)*v(62)
  END SUBROUTINE movingleastsquares2d
!> moving least squares 2d determines the nucleus for a given node k
  SUBROUTINE movingleastsquares2dforce(nk, f, s, nucleus)
    IMPLICIT NONE
    REAL(8) v(27), nk(2), f(2, 2), s(3), nucleus(2)
    v(22) = nk(2)*s(2)
    v(21) = nk(1)*s(1)
    nucleus(1) = (f(1, 2)*nk(1)+f(1, 1)*nk(2))*s(3)+f(1, 1)*v(21)+f(1, 2)*v(22)
    nucleus(2) = (f(2, 2)*nk(1)+f(2, 1)*nk(2))*s(3)+f(2, 1)*v(21)+f(2, 2)*v(22)
  END SUBROUTINE movingleastsquares2dforce
!> moving least squares 3d determines the kernel for two nodes k and l
  SUBROUTINE movingleastsquares3d(nk, nl, f, s, ds, kernel)
    IMPLICIT NONE
    REAL(8) v(173), nk(3), nl(3), f(3, 3), s(6), ds(6, 6), kernel(3, 3)
    v(144) = nk(1)*(nl(1)*s(1)+nl(2)*s(4)+nl(3)*s(5))+nk(3)*(nl(3)*s(3)+nl(1)*s(5)+nl(2)*s(6))+nk(2)*(nl(2)*s(2)+nl(1)*s(4)&
    &+nl(3)*s(6))
    v(143) = f(3, 3)*nk(2)+f(3, 2)*nk(3)
    v(142) = f(2, 3)*nk(2)+f(2, 2)*nk(3)
    v(141) = f(1, 3)*nk(2)+f(1, 2)*nk(3)
    v(140) = f(3, 3)*nk(1)+f(3, 1)*nk(3)
    v(139) = f(2, 3)*nk(1)+f(2, 1)*nk(3)
    v(138) = f(1, 3)*nk(1)+f(1, 1)*nk(3)
    v(137) = f(3, 2)*nk(1)+f(3, 1)*nk(2)
    v(136) = f(2, 2)*nk(1)+f(2, 1)*nk(2)
    v(135) = f(1, 2)*nk(1)+f(1, 1)*nk(2)
    v(134) = f(3, 3)*nk(3)
    v(133) = f(2, 3)*nk(3)
    v(132) = f(1, 3)*nk(3)
    v(131) = f(3, 2)*nk(2)
    v(130) = f(2, 2)*nk(2)
    v(129) = f(1, 2)*nk(2)
    v(128) = f(3, 1)*nk(1)
    v(127) = f(2, 1)*nk(1)
    v(126) = f(1, 1)*nk(1)
    v(125) = f(3, 3)*nl(2)+f(3, 2)*nl(3)
    v(124) = f(2, 3)*nl(2)+f(2, 2)*nl(3)
    v(123) = f(1, 3)*nl(2)+f(1, 2)*nl(3)
    v(122) = f(3, 3)*nl(1)+f(3, 1)*nl(3)
    v(121) = f(2, 3)*nl(1)+f(2, 1)*nl(3)
    v(120) = f(1, 3)*nl(1)+f(1, 1)*nl(3)
    v(119) = f(3, 2)*nl(1)+f(3, 1)*nl(2)
    v(118) = f(2, 2)*nl(1)+f(2, 1)*nl(2)
    v(117) = f(1, 2)*nl(1)+f(1, 1)*nl(2)
    v(116) = f(3, 3)*nl(3)
    v(115) = f(2, 3)*nl(3)
    v(168) = ds(3, 3)*v(115)
    v(114) = f(1, 3)*nl(3)
    v(147) = ds(3, 3)*v(114)
    v(113) = f(3, 2)*nl(2)
    v(160) = ds(1, 2)*v(113)+ds(1, 3)*v(116)+ds(1, 4)*v(119)+ds(1, 5)*v(122)+ds(1, 6)*v(125)
    v(112) = f(2, 2)*nl(2)
    v(167) = ds(2, 2)*v(112)
    v(154) = ds(1, 2)*v(112)+ds(1, 3)*v(115)+ds(1, 4)*v(118)+ds(1, 5)*v(121)+ds(1, 6)*v(124)
    v(111) = f(1, 2)*nl(2)
    v(148) = ds(1, 2)*v(111)+ds(1, 3)*v(114)+ds(1, 4)*v(117)+ds(1, 5)*v(120)+ds(1, 6)*v(123)
    v(146) = ds(2, 2)*v(111)
    v(110) = f(3, 1)*nl(1)
    v(165) = ds(6, 1)*v(110)+ds(6, 2)*v(113)+ds(6, 3)*v(116)+ds(6, 4)*v(119)+ds(6, 5)*v(122)+ds(6, 6)*v(125)
    v(164) = ds(5, 1)*v(110)+ds(5, 2)*v(113)+ds(5, 3)*v(116)+ds(5, 4)*v(119)+ds(5, 5)*v(122)+ds(5, 6)*v(125)
    v(163) = ds(4, 1)*v(110)+ds(4, 2)*v(113)+ds(4, 3)*v(116)+ds(4, 4)*v(119)+ds(4, 5)*v(122)+ds(4, 6)*v(125)
    v(162) = ds(3, 1)*v(110)+ds(3, 2)*v(113)+ds(3, 4)*v(119)+ds(3, 5)*v(122)+ds(3, 6)*v(125)
    v(161) = ds(2, 1)*v(110)+ds(2, 3)*v(116)+ds(2, 4)*v(119)+ds(2, 5)*v(122)+ds(2, 6)*v(125)
    v(109) = f(2, 1)*nl(1)
    v(166) = ds(1, 1)*v(109)
    v(159) = ds(6, 1)*v(109)+ds(6, 2)*v(112)+ds(6, 3)*v(115)+ds(6, 4)*v(118)+ds(6, 5)*v(121)+ds(6, 6)*v(124)
    v(158) = ds(5, 1)*v(109)+ds(5, 2)*v(112)+ds(5, 3)*v(115)+ds(5, 4)*v(118)+ds(5, 5)*v(121)+ds(5, 6)*v(124)
    v(157) = ds(4, 1)*v(109)+ds(4, 2)*v(112)+ds(4, 3)*v(115)+ds(4, 4)*v(118)+ds(4, 5)*v(121)+ds(4, 6)*v(124)
    v(156) = ds(3, 1)*v(109)+ds(3, 2)*v(112)+ds(3, 4)*v(118)+ds(3, 5)*v(121)+ds(3, 6)*v(124)
    v(155) = ds(2, 1)*v(109)+ds(2, 3)*v(115)+ds(2, 4)*v(118)+ds(2, 5)*v(121)+ds(2, 6)*v(124)
    v(108) = f(1, 1)*nl(1)
    v(153) = ds(6, 1)*v(108)+ds(6, 2)*v(111)+ds(6, 3)*v(114)+ds(6, 4)*v(117)+ds(6, 5)*v(120)+ds(6, 6)*v(123)
    v(152) = ds(5, 1)*v(108)+ds(5, 2)*v(111)+ds(5, 3)*v(114)+ds(5, 4)*v(117)+ds(5, 5)*v(120)+ds(5, 6)*v(123)
    v(151) = ds(4, 1)*v(108)+ds(4, 2)*v(111)+ds(4, 3)*v(114)+ds(4, 4)*v(117)+ds(4, 5)*v(120)+ds(4, 6)*v(123)
    v(150) = ds(3, 1)*v(108)+ds(3, 2)*v(111)+ds(3, 4)*v(117)+ds(3, 5)*v(120)+ds(3, 6)*v(123)
    v(149) = ds(2, 1)*v(108)+ds(2, 3)*v(114)+ds(2, 4)*v(117)+ds(2, 5)*v(120)+ds(2, 6)*v(123)
    v(145) = ds(1, 1)*v(108)
    v(104) = v(127)*v(145)+v(130)*v(146)+v(133)*v(147)
    v(105) = v(128)*v(145)+v(131)*v(146)+v(134)*v(147)
    v(106) = v(128)*v(166)+v(131)*v(167)+v(134)*v(168)
    kernel(1, 1) = v(144)+v(126)*(v(145)+v(148))+v(129)*(v(146)+v(149))+v(132)*(v(147)+v(150))+v(135)*v(151)+v(138)*v(152)+v&
    &(141)*v(153)
    kernel(1, 2) = v(104)+v(126)*v(154)+v(129)*v(155)+v(132)*v(156)+v(135)*v(157)+v(138)*v(158)+v(141)*v(159)
    kernel(1, 3) = v(105)+v(126)*v(160)+v(129)*v(161)+v(132)*v(162)+v(135)*v(163)+v(138)*v(164)+v(141)*v(165)
    kernel(2, 1) = v(104)+v(127)*v(148)+v(130)*v(149)+v(133)*v(150)+v(136)*v(151)+v(139)*v(152)+v(142)*v(153)
    kernel(2, 2) = v(144)+v(136)*v(157)+v(139)*v(158)+v(142)*v(159)+v(127)*(v(154)+v(166))+v(130)*(v(155)+v(167))+v(133)*(v&
    &(156)+v(168))
    kernel(2, 3) = v(106)+v(127)*v(160)+v(130)*v(161)+v(133)*v(162)+v(136)*v(163)+v(139)*v(164)+v(142)*v(165)
    kernel(3, 1) = v(105)+v(128)*v(148)+v(131)*v(149)+v(134)*v(150)+v(137)*v(151)+v(140)*v(152)+v(143)*v(153)
    kernel(3, 2) = v(106)+v(128)*v(154)+v(131)*v(155)+v(134)*v(156)+v(137)*v(157)+v(140)*v(158)+v(143)*v(159)
    kernel(3, 3) = v(144)+v(128)*(ds(1, 1)*v(110)+v(160))+v(131)*(ds(2, 2)*v(113)+v(161))+v(134)*(ds(3, 3)*v(116)+v(162))+v(137&
    &)*v(163)+v(140)*v(164)+v(143)*v(165)
  END SUBROUTINE movingleastsquares3d
!> moving least squares 3d determines the nucleus
  SUBROUTINE movingleastsquares3dforce(nk, f, s, nucleus)
    IMPLICIT NONE
    REAL(8) v(51), nk(3), f(3, 3), s(6), nucleus(3)
    v(46) = nk(3)*s(3)
    v(45) = nk(2)*s(2)
    v(44) = nk(1)*s(1)
    nucleus(1) = (f(1, 2)*nk(1)+f(1, 1)*nk(2))*s(4)+(f(1, 3)*nk(1)+f(1, 1)*nk(3))*s(5)+(f(1, 3)*nk(2)+f(1, 2)*nk(3))*s(6)+f(1, 1)*v&
    &(44)+f(1, 2)*v(45)+f(1, 3)*v(46)
    nucleus(2) = (f(2, 2)*nk(1)+f(2, 1)*nk(2))*s(4)+(f(2, 3)*nk(1)+f(2, 1)*nk(3))*s(5)+(f(2, 3)*nk(2)+f(2, 2)*nk(3))*s(6)+f(2, 1)*v&
    &(44)+f(2, 2)*v(45)+f(2, 3)*v(46)
    nucleus(3) = (f(3, 2)*nk(1)+f(3, 1)*nk(2))*s(4)+(f(3, 3)*nk(1)+f(3, 1)*nk(3))*s(5)+(f(3, 3)*nk(2)+f(3, 2)*nk(3))*s(6)+f(3, 1)*v&
    &(44)+f(3, 2)*v(45)+f(3, 3)*v(46)
  END SUBROUTINE movingleastsquares3dforce
!> moving least squares the Cauchy Green tensor
!> beware, even for undeformed configurations, the deformation gradient can be far from the identity
  SUBROUTINE movingleastsquarescauchygreen(ndi, n, xtot, xdef, dff, f, cauchygreen)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(ndi, n)::xtot, xdef
    REAL(8), DIMENSION(ndi, ndi)::c, f, fo
    REAL(8), DIMENSION(ndi, n)::dff
    REAL(8), DIMENSION(ndi*(ndi+1)/2)::cauchygreen
!-------------------------
!*** deformation gradient
!*** before correction
!-------------------------
    f = 0.0d00
    fo = 0.0d00
    DO in = 1, n
      DO jd = 1, ndi
        DO id = 1, ndi
          fo(id, jd) = fo(id, jd)+dff(jd, in)*xtot(id, in)
          f(id, jd) = f(id, jd)+dff(jd, in)*xdef(id, in)
        END DO
      END DO
    END DO
!------------------------------
!*** right cauchy-green tensor
!------------------------------
    CALL matrixmatrixproduct(ndi, ndi, ndi, f, f, c, 2)
    DO id = 1, ndi*(ndi+1)/2
      CALL voigttomatrix(i1, i2, id, ndi)
      cauchygreen(id) = c(i1, i2)
    END DO
  END SUBROUTINE movingleastsquarescauchygreen
!> moving least squares 2d, the second derivative of c
!> for two nodes
  SUBROUTINE movingleastsquaresclassicaldc22d(nk, nl, d2c)
    IMPLICIT NONE
    REAL(8) v(28), nk(2), nl(2), d2c(3, 2, 2)
    v(23) = nk(2)*nl(1)+nk(1)*nl(2)
    v(22) = 2d0*nk(2)*nl(2)
    v(21) = 2d0*nk(1)*nl(1)
    d2c(1, 1, 1) = v(21)
    d2c(1, 1, 2) = 0d0
    d2c(1, 2, 1) = 0d0
    d2c(1, 2, 2) = v(21)
    d2c(2, 1, 1) = v(22)
    d2c(2, 1, 2) = 0d0
    d2c(2, 2, 1) = 0d0
    d2c(2, 2, 2) = v(22)
    d2c(3, 1, 1) = v(23)
    d2c(3, 1, 2) = 0d0
    d2c(3, 2, 1) = 0d0
    d2c(3, 2, 2) = v(23)
  END SUBROUTINE movingleastsquaresclassicaldc22d
!> moving least squares 2d, the first derivative of c
  SUBROUTINE movingleastsquaresclassicaldc2d(nk, f, dc)
    IMPLICIT NONE
    REAL(8) v(26), nk(2), f(2, 2), dc(3, 2)
    v(21) = 2d0*nk(2)
    v(20) = 2d0*nk(1)
    dc(1, 1) = f(1, 1)*v(20)
    dc(1, 2) = f(2, 1)*v(20)
    dc(2, 1) = f(1, 2)*v(21)
    dc(2, 2) = f(2, 2)*v(21)
    dc(3, 1) = f(1, 2)*nk(1)+f(1, 1)*nk(2)
    dc(3, 2) = f(2, 2)*nk(1)+f(2, 1)*nk(2)
  END SUBROUTINE movingleastsquaresclassicaldc2d
!> moving least squares 3d, the second derivative of c
  SUBROUTINE movingleastsquaresclassicaldc2(nk, nl, d2c)
    IMPLICIT NONE
    REAL(8) v(78), nk(3), nl(3), d2c(6, 3, 3)
    v(73) = nk(3)*nl(2)+nk(2)*nl(3)
    v(72) = nk(3)*nl(1)+nk(1)*nl(3)
    v(71) = nk(2)*nl(1)+nk(1)*nl(2)
    v(70) = 2d0*nk(3)*nl(3)
    v(69) = 2d0*nk(2)*nl(2)
    v(68) = 2d0*nk(1)*nl(1)
    d2c(1, 1, 1) = v(68)
    d2c(1, 1, 2) = 0d0
    d2c(1, 1, 3) = 0d0
    d2c(1, 2, 1) = 0d0
    d2c(1, 2, 2) = v(68)
    d2c(1, 2, 3) = 0d0
    d2c(1, 3, 1) = 0d0
    d2c(1, 3, 2) = 0d0
    d2c(1, 3, 3) = v(68)
    d2c(2, 1, 1) = v(69)
    d2c(2, 1, 2) = 0d0
    d2c(2, 1, 3) = 0d0
    d2c(2, 2, 1) = 0d0
    d2c(2, 2, 2) = v(69)
    d2c(2, 2, 3) = 0d0
    d2c(2, 3, 1) = 0d0
    d2c(2, 3, 2) = 0d0
    d2c(2, 3, 3) = v(69)
    d2c(3, 1, 1) = v(70)
    d2c(3, 1, 2) = 0d0
    d2c(3, 1, 3) = 0d0
    d2c(3, 2, 1) = 0d0
    d2c(3, 2, 2) = v(70)
    d2c(3, 2, 3) = 0d0
    d2c(3, 3, 1) = 0d0
    d2c(3, 3, 2) = 0d0
    d2c(3, 3, 3) = v(70)
    d2c(4, 1, 1) = v(71)
    d2c(4, 1, 2) = 0d0
    d2c(4, 1, 3) = 0d0
    d2c(4, 2, 1) = 0d0
    d2c(4, 2, 2) = v(71)
    d2c(4, 2, 3) = 0d0
    d2c(4, 3, 1) = 0d0
    d2c(4, 3, 2) = 0d0
    d2c(4, 3, 3) = v(71)
    d2c(5, 1, 1) = v(72)
    d2c(5, 1, 2) = 0d0
    d2c(5, 1, 3) = 0d0
    d2c(5, 2, 1) = 0d0
    d2c(5, 2, 2) = v(72)
    d2c(5, 2, 3) = 0d0
    d2c(5, 3, 1) = 0d0
    d2c(5, 3, 2) = 0d0
    d2c(5, 3, 3) = v(72)
    d2c(6, 1, 1) = v(73)
    d2c(6, 1, 2) = 0d0
    d2c(6, 1, 3) = 0d0
    d2c(6, 2, 1) = 0d0
    d2c(6, 2, 2) = v(73)
    d2c(6, 2, 3) = 0d0
    d2c(6, 3, 1) = 0d0
    d2c(6, 3, 2) = 0d0
    d2c(6, 3, 3) = v(73)
  END SUBROUTINE movingleastsquaresclassicaldc2
!> moving least squares, derivative of c for a node k
  SUBROUTINE movingleastsquaresclassicaldc(nk, f, dc)
    IMPLICIT NONE
    REAL(8) v(57), nk(3), f(3, 3), dc(6, 3)
    v(52) = 2d0*nk(3)
    v(51) = 2d0*nk(2)
    v(50) = 2d0*nk(1)
    dc(1, 1) = f(1, 1)*v(50)
    dc(1, 2) = f(2, 1)*v(50)
    dc(1, 3) = f(3, 1)*v(50)
    dc(2, 1) = f(1, 2)*v(51)
    dc(2, 2) = f(2, 2)*v(51)
    dc(2, 3) = f(3, 2)*v(51)
    dc(3, 1) = f(1, 3)*v(52)
    dc(3, 2) = f(2, 3)*v(52)
    dc(3, 3) = f(3, 3)*v(52)
    dc(4, 1) = f(1, 2)*nk(1)+f(1, 1)*nk(2)
    dc(4, 2) = f(2, 2)*nk(1)+f(2, 1)*nk(2)
    dc(4, 3) = f(3, 2)*nk(1)+f(3, 1)*nk(2)
    dc(5, 1) = f(1, 3)*nk(1)+f(1, 1)*nk(3)
    dc(5, 2) = f(2, 3)*nk(1)+f(2, 1)*nk(3)
    dc(5, 3) = f(3, 3)*nk(1)+f(3, 1)*nk(3)
    dc(6, 1) = f(1, 3)*nk(2)+f(1, 2)*nk(3)
    dc(6, 2) = f(2, 3)*nk(2)+f(2, 2)*nk(3)
    dc(6, 3) = f(3, 3)*nk(2)+f(3, 2)*nk(3)
  END SUBROUTINE movingleastsquaresclassicaldc
!> moving least squares, fbar cs 2d
  SUBROUTINE movingleastsquarescombinedfbar02d(c, cb, cs)
    IMPLICIT NONE
    REAL(8) v(20), c(3), cb(3), cs(3)
    v(15) = sqrt((cb(1)*cb(2)-cb(3)**2)/(c(1)*c(2)-c(3)**2))
    cs(1) = c(1)*v(15)
    cs(2) = c(2)*v(15)
    cs(3) = c(3)*v(15)
  END SUBROUTINE movingleastsquarescombinedfbar02d
!> moving least squares fbar cs 3d
  SUBROUTINE movingleastsquarescombinedfbar0(c, cb, cs)
    IMPLICIT NONE
    REAL(8) v(32), c(6), cb(6), cs(6)
    v(27) = ((cb(3)*cb(4)**2+cb(5)*(cb(2)*cb(5)-2d0*cb(4)*cb(6))+cb(1)*(-(cb(2)*cb(3))+cb(6)**2))/(c(3)*c(4)**2+c(5)*(c(2)*c&
    &(5)-2d0*c(4)*c(6))+c(1)*(-(c(2)*c(3))+c(6)**2)))**0.3333333333333333d0
    cs(1) = c(1)*v(27)
    cs(2) = c(2)*v(27)
    cs(3) = c(3)*v(27)
    cs(4) = c(4)*v(27)
    cs(5) = c(5)*v(27)
    cs(6) = c(6)*v(27)
  END SUBROUTINE movingleastsquarescombinedfbar0
!> moving least squares fbar dcb and dcs
  SUBROUTINE movingleastsquarescombinedfbar12d(c, cb, dc, dcb, dcs)
    IMPLICIT NONE
    REAL(8) v(62), c(3), cb(3), dc(3, 2), dcb(3, 2), dcs(3, 2)
    v(57) = c(1)*dc(2, 2)
    v(56) = c(2)*dc(1, 2)
    v(55) = cb(1)*dcb(2, 2)
    v(54) = cb(2)*dcb(1, 2)
    v(53) = (-2d0)*c(3)
    v(51) = (-2d0)*cb(3)
    v(49) = c(2)*dc(1, 1)+c(1)*dc(2, 1)+dc(3, 1)*v(53)
    v(48) = cb(2)*dcb(1, 1)+cb(1)*dcb(2, 1)+dcb(3, 1)*v(51)
    v(47) = cb(1)*cb(2)-cb(3)**2
    v(46) = c(1)*c(2)-c(3)**2
    v(36) = 1d0/v(46)**2
    v(52) = -(v(36)*v(47))
    v(31) = v(47)/v(46)
    v(35) = 1d0/sqrt(v(31))
    v(50) = v(35)/2d0
    v(28) = sqrt(v(31))
    v(34) = v(50)*(v(48)/v(46)+v(49)*v(52))
    v(38) = v(50)*((dcb(3, 2)*v(51)+v(54)+v(55))/v(46)+v(52)*(dc(3, 2)*v(53)+v(56)+v(57)))
    dcs(1, 1) = dc(1, 1)*v(28)+c(1)*v(34)
    dcs(1, 2) = dc(1, 2)*v(28)+c(1)*v(38)
    dcs(2, 1) = dc(2, 1)*v(28)+c(2)*v(34)
    dcs(2, 2) = dc(2, 2)*v(28)+c(2)*v(38)
    dcs(3, 1) = dc(3, 1)*v(28)+c(3)*v(34)
    dcs(3, 2) = dc(3, 2)*v(28)+c(3)*v(38)
  END SUBROUTINE movingleastsquarescombinedfbar12d
!> moving least squares fbar dcb and dcs
  SUBROUTINE movingleastsquarescombinedfbar1(c, cb, dc, dcb, dcs)
    IMPLICIT NONE
    REAL(8) v(166), c(6), cb(6), dc(6, 3), dcb(6, 3), dcs(6, 3)
    v(161) = cb(2)*dcb(3, 3)
    v(160) = cb(3)*dcb(2, 3)
    v(159) = -(cb(2)*dcb(5, 3))
    v(158) = cb(6)*dcb(4, 3)
    v(157) = c(2)*dc(3, 3)
    v(156) = c(3)*dc(2, 3)
    v(155) = -(c(2)*dc(5, 3))
    v(154) = c(6)*dc(4, 3)
    v(153) = 2d0*dcb(6, 3)
    v(152) = 2d0*dc(6, 3)
    v(151) = cb(2)*dcb(3, 2)
    v(150) = cb(3)*dcb(2, 2)
    v(149) = -(cb(2)*dcb(5, 2))
    v(148) = cb(6)*dcb(4, 2)
    v(147) = c(2)*dc(3, 2)
    v(146) = c(3)*dc(2, 2)
    v(145) = -(c(2)*dc(5, 2))
    v(144) = c(6)*dc(4, 2)
    v(143) = 2d0*dcb(6, 2)
    v(142) = cb(4)**2
    v(141) = cb(5)**2
    v(140) = 2d0*dc(6, 2)
    v(139) = c(4)**2
    v(138) = c(5)**2
    v(135) = cb(2)*dcb(3, 1)
    v(134) = cb(3)*dcb(2, 1)
    v(133) = -(cb(2)*dcb(5, 1))
    v(132) = cb(6)*dcb(4, 1)
    v(131) = -(dcb(3, 1)*v(142))
    v(130) = -(dcb(2, 1)*v(141))
    v(129) = c(2)*dc(3, 1)
    v(128) = c(3)*dc(2, 1)
    v(127) = -(c(2)*dc(5, 1))
    v(126) = c(6)*dc(4, 1)
    v(125) = -(dc(3, 1)*v(139))
    v(124) = -(dc(2, 1)*v(138))
    v(123) = 2d0*dcb(6, 1)
    v(122) = cb(4)*cb(6)
    v(121) = cb(3)*cb(4)
    v(120) = 2d0*dc(6, 1)
    v(119) = c(4)*c(6)
    v(118) = c(3)*c(4)
    v(117) = cb(2)*cb(3)-cb(6)**2
    v(116) = -(cb(5)*cb(6))+v(121)
    v(115) = -(cb(2)*cb(5))+v(122)
    v(114) = c(2)*c(3)-c(6)**2
    v(113) = -(c(5)*c(6))+v(118)
    v(112) = -(c(2)*c(5))+v(119)
    v(78) = c(5)*v(112)-c(4)*v(113)+c(1)*v(114)
    v(88) = 1d0/v(78)**2
    v(79) = cb(5)*v(115)-cb(4)*v(116)+cb(1)*v(117)
    v(137) = -(v(79)*v(88))
    v(77) = v(79)/v(78)
    v(87) = 1d0/v(77)**0.6666666666666666d0
    v(136) = v(87)/3d0
    v(71) = v(77)**0.3333333333333333d0
    v(86) = v(136)*((dc(5, 1)*v(112)-dc(4, 1)*v(113)+dc(1, 1)*v(114)-dc(4, 1)*v(118)+dc(5, 1)*v(119)+v(124)+v(125)+c(5)*(c(4)*v&
    &(120)+v(126)+v(127))+c(1)*(-(c(6)*v(120))+v(128)+v(129)))*v(137)+(dcb(5, 1)*v(115)-dcb(4, 1)*v(116)+dcb(1, 1)*v(117)-dcb(4&
    &, 1)*v(121)+dcb(5, 1)*v(122)+v(130)+v(131)+cb(5)*(cb(4)*v(123)+v(132)+v(133))+cb(1)*(-(cb(6)*v(123))+v(134)+v(135)))/v(78&
    &))
    v(90) = v(136)*(v(137)*(dc(5, 2)*v(112)-dc(4, 2)*v(113)+dc(1, 2)*v(114)-dc(4, 2)*v(118)+dc(5, 2)*v(119)-dc(2, 2)*v(138)-dc(3, 2&
    &)*v(139)+c(5)*(c(4)*v(140)+v(144)+v(145))+c(1)*(-(c(6)*v(140))+v(146)+v(147)))+(dcb(5, 2)*v(115)-dcb(4, 2)*v(116)+dcb(1, 2&
    &)*v(117)-dcb(4, 2)*v(121)+dcb(5, 2)*v(122)-dcb(2, 2)*v(141)-dcb(3, 2)*v(142)+cb(5)*(cb(4)*v(143)+v(148)+v(149))+cb(1)*(-(cb&
    &(6)*v(143))+v(150)+v(151)))/v(78))
    v(92) = v(136)*(v(137)*(dc(5, 3)*v(112)-dc(4, 3)*v(113)+dc(1, 3)*v(114)-dc(4, 3)*v(118)+dc(5, 3)*v(119)-dc(2, 3)*v(138)-dc(3, 3&
    &)*v(139)+c(5)*(c(4)*v(152)+v(154)+v(155))+c(1)*(-(c(6)*v(152))+v(156)+v(157)))+(dcb(5, 3)*v(115)-dcb(4, 3)*v(116)+dcb(1, 3&
    &)*v(117)-dcb(4, 3)*v(121)+dcb(5, 3)*v(122)-dcb(2, 3)*v(141)-dcb(3, 3)*v(142)+cb(5)*(cb(4)*v(153)+v(158)+v(159))+cb(1)*(-(cb&
    &(6)*v(153))+v(160)+v(161)))/v(78))
    dcs(1, 1) = dc(1, 1)*v(71)+c(1)*v(86)
    dcs(1, 2) = dc(1, 2)*v(71)+c(1)*v(90)
    dcs(1, 3) = dc(1, 3)*v(71)+c(1)*v(92)
    dcs(2, 1) = dc(2, 1)*v(71)+c(2)*v(86)
    dcs(2, 2) = dc(2, 2)*v(71)+c(2)*v(90)
    dcs(2, 3) = dc(2, 3)*v(71)+c(2)*v(92)
    dcs(3, 1) = dc(3, 1)*v(71)+c(3)*v(86)
    dcs(3, 2) = dc(3, 2)*v(71)+c(3)*v(90)
    dcs(3, 3) = dc(3, 3)*v(71)+c(3)*v(92)
    dcs(4, 1) = dc(4, 1)*v(71)+c(4)*v(86)
    dcs(4, 2) = dc(4, 2)*v(71)+c(4)*v(90)
    dcs(4, 3) = dc(4, 3)*v(71)+c(4)*v(92)
    dcs(5, 1) = dc(5, 1)*v(71)+c(5)*v(86)
    dcs(5, 2) = dc(5, 2)*v(71)+c(5)*v(90)
    dcs(5, 3) = dc(5, 3)*v(71)+c(5)*v(92)
    dcs(6, 1) = dc(6, 1)*v(71)+c(6)*v(86)
    dcs(6, 2) = dc(6, 2)*v(71)+c(6)*v(90)
    dcs(6, 3) = dc(6, 3)*v(71)+c(6)*v(92)
  END SUBROUTINE movingleastsquarescombinedfbar1
!> mls fbar combine the first derivatives
  SUBROUTINE movingleastsquarescombinedfbar22d(c1, c0, dck1, dcl1, dck0, dcl0, d2c1, d2c0, d2c)
    IMPLICIT NONE
    REAL(8) v(202), c1(3), c0(3), dck1(3, 2), dcl1(3, 2), dck0(3, 2), dcl0(3, 2), d2c1(3, 2, 2), d2c0(3, 2, 2), d2c(3, 2, 2&
    &)
    v(197) = dck1(1, 1)*dcl1(2, 2)
    v(196) = dck1(2, 1)*dcl1(1, 2)
    v(195) = dck0(1, 1)*dcl0(2, 2)
    v(194) = dck0(2, 1)*dcl0(1, 2)
    v(193) = c0(1)*d2c0(2, 1, 2)
    v(192) = c0(2)*d2c0(1, 1, 2)
    v(191) = (-2d0)*dcl1(3, 2)
    v(188) = (-2d0)*dcl0(3, 2)
    v(187) = dck1(3, 2)*v(191)
    v(186) = dck1(1, 2)*dcl1(2, 2)
    v(185) = dck1(2, 2)*dcl1(1, 2)
    v(184) = dck0(3, 2)*v(188)
    v(183) = dck0(1, 2)*dcl0(2, 2)
    v(182) = dck0(2, 2)*dcl0(1, 2)
    v(181) = c0(1)*d2c0(2, 2, 2)
    v(180) = c0(2)*d2c0(1, 2, 2)
    v(178) = dck1(1, 1)*dcl1(2, 1)
    v(177) = dck1(2, 1)*dcl1(1, 1)
    v(176) = dck0(1, 1)*dcl0(2, 1)
    v(175) = dck0(2, 1)*dcl0(1, 1)
    v(174) = c0(1)*d2c0(2, 1, 1)
    v(173) = c0(2)*d2c0(1, 1, 1)
    v(172) = (-2d0)*dcl1(3, 1)
    v(169) = (-2d0)*dcl0(3, 1)
    v(168) = dck1(3, 2)*v(172)
    v(167) = dck1(1, 2)*dcl1(2, 1)
    v(166) = dck1(2, 2)*dcl1(1, 1)
    v(165) = dck0(3, 2)*v(169)
    v(164) = dck0(1, 2)*dcl0(2, 1)
    v(163) = dck0(2, 2)*dcl0(1, 1)
    v(162) = c0(1)*d2c0(2, 2, 1)
    v(161) = c0(2)*d2c0(1, 2, 1)
    v(160) = c0(1)*dcl0(2, 2)
    v(159) = c0(2)*dcl0(1, 2)
    v(158) = c0(1)*dcl0(2, 1)
    v(157) = c0(2)*dcl0(1, 1)
    v(155) = c1(1)*dcl1(2, 2)
    v(154) = c1(2)*dcl1(1, 2)
    v(153) = c1(1)*dcl1(2, 1)
    v(152) = c1(2)*dcl1(1, 1)
    v(150) = d2c1(3, 2, 2)
    v(149) = d2c1(3, 2, 1)
    v(148) = d2c1(3, 1, 2)
    v(147) = d2c1(3, 1, 1)
    v(146) = d2c1(2, 2, 2)
    v(145) = d2c1(2, 2, 1)
    v(144) = d2c1(2, 1, 2)
    v(143) = d2c1(2, 1, 1)
    v(142) = d2c1(1, 2, 2)
    v(141) = d2c1(1, 2, 1)
    v(140) = d2c1(1, 1, 2)
    v(139) = d2c1(1, 1, 1)
    v(137) = c0(1)*dck0(2, 2)
    v(136) = c0(2)*dck0(1, 2)
    v(135) = (-2d0)*c0(3)
    v(134) = c0(2)*dck0(1, 1)+c0(1)*dck0(2, 1)+dck0(3, 1)*v(135)
    v(133) = c1(1)*dck1(2, 2)
    v(132) = c1(2)*dck1(1, 2)
    v(131) = (-2d0)*c1(3)
    v(130) = c1(2)*dck1(1, 1)+c1(1)*dck1(2, 1)+dck1(3, 1)*v(131)
    v(129) = c0(1)*c0(2)-c0(3)**2
    v(128) = c1(1)*c1(2)-c1(3)**2
    v(94) = 1d0/v(128)**3
    v(156) = (-2d0)*v(94)
    v(80) = 1d0/v(128)**2
    v(138) = -(v(129)*v(80))
    v(75) = v(129)/v(128)
    v(101) = 1d0/v(75)**0.15d1
    v(179) = (-0.5d0)*v(101)
    v(79) = 1d0/sqrt(v(75))
    v(151) = v(79)/2d0
    v(72) = sqrt(v(75))
    v(111) = dck1(3, 2)*v(131)+v(132)+v(133)
    v(107) = v(134)/v(128)+v(130)*v(138)
    v(110) = dck0(3, 2)*v(135)+v(136)+v(137)
    v(112) = v(110)/v(128)+v(111)*v(138)
    v(78) = v(107)*v(151)
    v(82) = v(112)*v(151)
    v(89) = dcl1(3, 1)*v(131)+v(152)+v(153)
    v(90) = dcl1(3, 2)*v(131)+v(154)+v(155)
    v(91) = -(v(80)*v(89))
    v(92) = -(v(80)*v(90))
    v(93) = v(156)*v(89)
    v(171) = -(v(129)*v(93))
    v(95) = v(156)*v(90)
    v(190) = -(v(129)*v(95))
    v(96) = dcl0(3, 1)*v(135)+v(157)+v(158)
    v(170) = -(v(80)*v(96))
    v(97) = dcl0(3, 2)*v(135)+v(159)+v(160)
    v(189) = -(v(80)*v(97))
    v(98) = v(129)*v(91)+v(96)/v(128)
    v(99) = v(129)*v(92)+v(97)/v(128)
    v(100) = v(179)*v(98)
    v(113) = (v(100)*v(112)+v(79)*((d2c0(3, 2, 1)*v(135)+v(161)+v(162)+v(163)+v(164)+v(165))/v(128)+v(138)*(c1(2)*v(141)+c1(1&
    &)*v(145)+v(131)*v(149)+v(166)+v(167)+v(168))+v(111)*v(170)+v(111)*v(171)+v(110)*v(91)))/2d0
    v(108) = (v(100)*v(107)+v(79)*(v(130)*v(170)+v(130)*v(171)+(d2c0(3, 1, 1)*v(135)+dck0(3, 1)*v(169)+v(173)+v(174)+v(175)+v&
    &(176))/v(128)+v(138)*(c1(2)*v(139)+c1(1)*v(143)+v(131)*v(147)+dck1(3, 1)*v(172)+v(177)+v(178))+v(134)*v(91)))/2d0
    v(102) = v(179)*v(99)
    v(114) = (v(102)*v(112)+v(79)*((d2c0(3, 2, 2)*v(135)+v(180)+v(181)+v(182)+v(183)+v(184))/v(128)+v(138)*(c1(2)*v(142)+c1(1&
    &)*v(146)+v(131)*v(150)+v(185)+v(186)+v(187))+v(111)*v(189)+v(111)*v(190)+v(110)*v(92)))/2d0
    v(109) = (v(102)*v(107)+v(79)*(v(130)*v(189)+v(130)*v(190)+(d2c0(3, 1, 2)*v(135)+dck0(3, 1)*v(188)+v(192)+v(193)+v(194)+v&
    &(195))/v(128)+v(138)*(c1(2)*v(140)+c1(1)*v(144)+v(131)*v(148)+dck1(3, 1)*v(191)+v(196)+v(197))+v(134)*v(92)))/2d0
    v(103) = v(151)*v(98)
    v(104) = v(151)*v(99)
    d2c(1, 1, 1) = dck1(1, 1)*v(103)+c1(1)*v(108)+v(139)*v(72)+dcl1(1, 1)*v(78)
    d2c(1, 1, 2) = dck1(1, 1)*v(104)+c1(1)*v(109)+v(140)*v(72)+dcl1(1, 2)*v(78)
    d2c(1, 2, 1) = dck1(1, 2)*v(103)+c1(1)*v(113)+v(141)*v(72)+dcl1(1, 1)*v(82)
    d2c(1, 2, 2) = dck1(1, 2)*v(104)+c1(1)*v(114)+v(142)*v(72)+dcl1(1, 2)*v(82)
    d2c(2, 1, 1) = dck1(2, 1)*v(103)+c1(2)*v(108)+v(143)*v(72)+dcl1(2, 1)*v(78)
    d2c(2, 1, 2) = dck1(2, 1)*v(104)+c1(2)*v(109)+v(144)*v(72)+dcl1(2, 2)*v(78)
    d2c(2, 2, 1) = dck1(2, 2)*v(103)+c1(2)*v(113)+v(145)*v(72)+dcl1(2, 1)*v(82)
    d2c(2, 2, 2) = dck1(2, 2)*v(104)+c1(2)*v(114)+v(146)*v(72)+dcl1(2, 2)*v(82)
    d2c(3, 1, 1) = dck1(3, 1)*v(103)+c1(3)*v(108)+v(147)*v(72)+dcl1(3, 1)*v(78)
    d2c(3, 1, 2) = dck1(3, 1)*v(104)+c1(3)*v(109)+v(148)*v(72)+dcl1(3, 2)*v(78)
    d2c(3, 2, 1) = dck1(3, 2)*v(103)+c1(3)*v(113)+v(149)*v(72)+dcl1(3, 1)*v(82)
    d2c(3, 2, 2) = dck1(3, 2)*v(104)+c1(3)*v(114)+v(150)*v(72)+dcl1(3, 2)*v(82)
  END SUBROUTINE movingleastsquarescombinedfbar22d
!> mls fbar combine the second derivatives
  SUBROUTINE movingleastsquarescombinedfbar2(c1, c0, dck1, dcl1, dck0, dcl0, d2c1, d2c0, d2c)
    IMPLICIT NONE
    REAL(8) v(785), c1(6), c0(6), dck1(6, 3), dcl1(6, 3), dck0(6, 3), dcl0(6, 3), d2c1(6, 3, 3), d2c0(6, 3, 3), d2c(6, 3, 3&
    &)
    v(780) = dck0(2, 1)*dcl0(3, 3)
    v(779) = dck0(3, 1)*dcl0(2, 3)
    v(778) = -(dck0(5, 1)*dcl0(6, 3))
    v(777) = -(dck0(6, 1)*dcl0(5, 3))
    v(776) = dck0(3, 1)*dcl0(4, 3)
    v(775) = dck0(4, 1)*dcl0(3, 3)
    v(774) = dck0(4, 1)*dcl0(6, 3)
    v(773) = -(dck0(2, 1)*dcl0(5, 3))
    v(772) = dck0(6, 1)*dcl0(4, 3)
    v(771) = -(dck0(5, 1)*dcl0(2, 3))
    v(770) = dck1(2, 1)*dcl1(3, 3)
    v(769) = dck1(3, 1)*dcl1(2, 3)
    v(768) = -(dck1(5, 1)*dcl1(6, 3))
    v(767) = -(dck1(6, 1)*dcl1(5, 3))
    v(766) = dck1(3, 1)*dcl1(4, 3)
    v(765) = dck1(4, 1)*dcl1(3, 3)
    v(764) = dck1(4, 1)*dcl1(6, 3)
    v(763) = -(dck1(2, 1)*dcl1(5, 3))
    v(762) = dck1(6, 1)*dcl1(4, 3)
    v(761) = -(dck1(5, 1)*dcl1(2, 3))
    v(760) = dck0(2, 2)*dcl0(3, 3)
    v(759) = dck0(3, 2)*dcl0(2, 3)
    v(758) = -(dck0(5, 2)*dcl0(6, 3))
    v(757) = -(dck0(6, 2)*dcl0(5, 3))
    v(756) = dck0(3, 2)*dcl0(4, 3)
    v(755) = dck0(4, 2)*dcl0(3, 3)
    v(754) = dck0(4, 2)*dcl0(6, 3)
    v(753) = -(dck0(2, 2)*dcl0(5, 3))
    v(752) = dck0(6, 2)*dcl0(4, 3)
    v(751) = -(dck0(5, 2)*dcl0(2, 3))
    v(750) = dck1(2, 2)*dcl1(3, 3)
    v(749) = dck1(3, 2)*dcl1(2, 3)
    v(748) = -(dck1(5, 2)*dcl1(6, 3))
    v(747) = -(dck1(6, 2)*dcl1(5, 3))
    v(746) = dck1(3, 2)*dcl1(4, 3)
    v(745) = dck1(4, 2)*dcl1(3, 3)
    v(744) = dck1(4, 2)*dcl1(6, 3)
    v(743) = -(dck1(2, 2)*dcl1(5, 3))
    v(742) = dck1(6, 2)*dcl1(4, 3)
    v(741) = -(dck1(5, 2)*dcl1(2, 3))
    v(740) = (-2d0)*dcl0(6, 3)
    v(739) = (-2d0)*dcl1(6, 3)
    v(736) = dck0(6, 3)*v(740)
    v(735) = dck0(2, 3)*dcl0(3, 3)
    v(734) = dck0(3, 3)*dcl0(2, 3)
    v(733) = -(dck0(5, 3)*dcl0(6, 3))
    v(732) = -(dck0(6, 3)*dcl0(5, 3))
    v(731) = dck0(3, 3)*dcl0(4, 3)
    v(730) = dck0(4, 3)*dcl0(3, 3)
    v(729) = dck0(4, 3)*dcl0(6, 3)
    v(728) = -(dck0(2, 3)*dcl0(5, 3))
    v(727) = dck0(6, 3)*dcl0(4, 3)
    v(726) = -(dck0(5, 3)*dcl0(2, 3))
    v(725) = dck1(6, 3)*v(739)
    v(724) = dck1(2, 3)*dcl1(3, 3)
    v(723) = dck1(3, 3)*dcl1(2, 3)
    v(722) = -(dck1(5, 3)*dcl1(6, 3))
    v(721) = -(dck1(6, 3)*dcl1(5, 3))
    v(720) = dck1(3, 3)*dcl1(4, 3)
    v(719) = dck1(4, 3)*dcl1(3, 3)
    v(718) = dck1(4, 3)*dcl1(6, 3)
    v(717) = -(dck1(2, 3)*dcl1(5, 3))
    v(716) = dck1(6, 3)*dcl1(4, 3)
    v(715) = -(dck1(5, 3)*dcl1(2, 3))
    v(714) = dck0(2, 1)*dcl0(3, 2)
    v(713) = dck0(3, 1)*dcl0(2, 2)
    v(712) = -(dck0(5, 1)*dcl0(6, 2))
    v(711) = -(dck0(6, 1)*dcl0(5, 2))
    v(710) = dck0(3, 1)*dcl0(4, 2)
    v(709) = dck0(4, 1)*dcl0(3, 2)
    v(708) = dck0(4, 1)*dcl0(6, 2)
    v(707) = -(dck0(2, 1)*dcl0(5, 2))
    v(706) = dck0(6, 1)*dcl0(4, 2)
    v(705) = -(dck0(5, 1)*dcl0(2, 2))
    v(704) = dck1(2, 1)*dcl1(3, 2)
    v(703) = dck1(3, 1)*dcl1(2, 2)
    v(702) = -(dck1(5, 1)*dcl1(6, 2))
    v(701) = -(dck1(6, 1)*dcl1(5, 2))
    v(700) = dck1(3, 1)*dcl1(4, 2)
    v(699) = dck1(4, 1)*dcl1(3, 2)
    v(698) = dck1(4, 1)*dcl1(6, 2)
    v(697) = -(dck1(2, 1)*dcl1(5, 2))
    v(696) = dck1(6, 1)*dcl1(4, 2)
    v(695) = -(dck1(5, 1)*dcl1(2, 2))
    v(694) = dck0(2, 2)*dcl0(3, 2)
    v(693) = dck0(3, 2)*dcl0(2, 2)
    v(692) = -(dck0(5, 2)*dcl0(6, 2))
    v(691) = -(dck0(6, 2)*dcl0(5, 2))
    v(690) = dck0(3, 2)*dcl0(4, 2)
    v(689) = dck0(4, 2)*dcl0(3, 2)
    v(688) = dck0(4, 2)*dcl0(6, 2)
    v(687) = -(dck0(2, 2)*dcl0(5, 2))
    v(686) = dck0(6, 2)*dcl0(4, 2)
    v(685) = -(dck0(5, 2)*dcl0(2, 2))
    v(684) = dck1(2, 2)*dcl1(3, 2)
    v(683) = dck1(3, 2)*dcl1(2, 2)
    v(682) = -(dck1(5, 2)*dcl1(6, 2))
    v(681) = -(dck1(6, 2)*dcl1(5, 2))
    v(680) = dck1(3, 2)*dcl1(4, 2)
    v(679) = dck1(4, 2)*dcl1(3, 2)
    v(678) = dck1(4, 2)*dcl1(6, 2)
    v(677) = -(dck1(2, 2)*dcl1(5, 2))
    v(676) = dck1(6, 2)*dcl1(4, 2)
    v(675) = -(dck1(5, 2)*dcl1(2, 2))
    v(674) = (-2d0)*dcl0(6, 2)
    v(673) = (-2d0)*dcl1(6, 2)
    v(670) = dck0(6, 3)*v(674)
    v(669) = dck0(2, 3)*dcl0(3, 2)
    v(668) = dck0(3, 3)*dcl0(2, 2)
    v(667) = -(dck0(5, 3)*dcl0(6, 2))
    v(666) = -(dck0(6, 3)*dcl0(5, 2))
    v(665) = dck0(3, 3)*dcl0(4, 2)
    v(664) = dck0(4, 3)*dcl0(3, 2)
    v(663) = dck0(4, 3)*dcl0(6, 2)
    v(662) = -(dck0(2, 3)*dcl0(5, 2))
    v(661) = dck0(6, 3)*dcl0(4, 2)
    v(660) = -(dck0(5, 3)*dcl0(2, 2))
    v(659) = dck1(6, 3)*v(673)
    v(658) = dck1(2, 3)*dcl1(3, 2)
    v(657) = dck1(3, 3)*dcl1(2, 2)
    v(656) = -(dck1(5, 3)*dcl1(6, 2))
    v(655) = -(dck1(6, 3)*dcl1(5, 2))
    v(654) = dck1(3, 3)*dcl1(4, 2)
    v(653) = dck1(4, 3)*dcl1(3, 2)
    v(652) = dck1(4, 3)*dcl1(6, 2)
    v(651) = -(dck1(2, 3)*dcl1(5, 2))
    v(650) = dck1(6, 3)*dcl1(4, 2)
    v(649) = -(dck1(5, 3)*dcl1(2, 2))
    v(647) = dck0(2, 1)*dcl0(3, 1)
    v(646) = dck0(3, 1)*dcl0(2, 1)
    v(645) = -(dck0(5, 1)*dcl0(6, 1))
    v(644) = -(dck0(6, 1)*dcl0(5, 1))
    v(643) = dck0(3, 1)*dcl0(4, 1)
    v(642) = dck0(4, 1)*dcl0(3, 1)
    v(641) = dck0(4, 1)*dcl0(6, 1)
    v(640) = -(dck0(2, 1)*dcl0(5, 1))
    v(639) = dck0(6, 1)*dcl0(4, 1)
    v(638) = -(dck0(5, 1)*dcl0(2, 1))
    v(637) = dck1(2, 1)*dcl1(3, 1)
    v(636) = dck1(3, 1)*dcl1(2, 1)
    v(635) = -(dck1(5, 1)*dcl1(6, 1))
    v(634) = -(dck1(6, 1)*dcl1(5, 1))
    v(633) = dck1(3, 1)*dcl1(4, 1)
    v(632) = dck1(4, 1)*dcl1(3, 1)
    v(631) = dck1(4, 1)*dcl1(6, 1)
    v(630) = -(dck1(2, 1)*dcl1(5, 1))
    v(629) = dck1(6, 1)*dcl1(4, 1)
    v(628) = -(dck1(5, 1)*dcl1(2, 1))
    v(627) = dck0(2, 2)*dcl0(3, 1)
    v(626) = dck0(3, 2)*dcl0(2, 1)
    v(625) = -(dck0(5, 2)*dcl0(6, 1))
    v(624) = -(dck0(6, 2)*dcl0(5, 1))
    v(623) = dck0(3, 2)*dcl0(4, 1)
    v(622) = dck0(4, 2)*dcl0(3, 1)
    v(621) = dck0(4, 2)*dcl0(6, 1)
    v(620) = -(dck0(2, 2)*dcl0(5, 1))
    v(619) = dck0(6, 2)*dcl0(4, 1)
    v(618) = -(dck0(5, 2)*dcl0(2, 1))
    v(617) = dck1(2, 2)*dcl1(3, 1)
    v(616) = dck1(3, 2)*dcl1(2, 1)
    v(615) = -(dck1(5, 2)*dcl1(6, 1))
    v(614) = -(dck1(6, 2)*dcl1(5, 1))
    v(613) = dck1(3, 2)*dcl1(4, 1)
    v(612) = dck1(4, 2)*dcl1(3, 1)
    v(611) = dck1(4, 2)*dcl1(6, 1)
    v(610) = -(dck1(2, 2)*dcl1(5, 1))
    v(609) = dck1(6, 2)*dcl1(4, 1)
    v(608) = -(dck1(5, 2)*dcl1(2, 1))
    v(607) = (-2d0)*dcl0(6, 1)
    v(606) = (-2d0)*dcl1(6, 1)
    v(603) = dck0(6, 3)*v(607)
    v(602) = dck0(2, 3)*dcl0(3, 1)
    v(601) = dck0(3, 3)*dcl0(2, 1)
    v(600) = -(dck0(5, 3)*dcl0(6, 1))
    v(599) = -(dck0(6, 3)*dcl0(5, 1))
    v(598) = dck0(3, 3)*dcl0(4, 1)
    v(597) = dck0(4, 3)*dcl0(3, 1)
    v(596) = dck0(4, 3)*dcl0(6, 1)
    v(595) = -(dck0(2, 3)*dcl0(5, 1))
    v(594) = dck0(6, 3)*dcl0(4, 1)
    v(593) = -(dck0(5, 3)*dcl0(2, 1))
    v(592) = dck1(6, 3)*v(606)
    v(591) = dck1(2, 3)*dcl1(3, 1)
    v(590) = dck1(3, 3)*dcl1(2, 1)
    v(589) = -(dck1(5, 3)*dcl1(6, 1))
    v(588) = -(dck1(6, 3)*dcl1(5, 1))
    v(587) = dck1(3, 3)*dcl1(4, 1)
    v(586) = dck1(4, 3)*dcl1(3, 1)
    v(585) = dck1(4, 3)*dcl1(6, 1)
    v(584) = -(dck1(2, 3)*dcl1(5, 1))
    v(583) = dck1(6, 3)*dcl1(4, 1)
    v(582) = -(dck1(5, 3)*dcl1(2, 1))
    v(581) = c0(2)*dcl0(3, 3)
    v(580) = c0(3)*dcl0(2, 3)
    v(579) = c0(2)*dcl0(3, 2)
    v(578) = c0(3)*dcl0(2, 2)
    v(577) = c0(2)*dcl0(3, 1)
    v(576) = c0(3)*dcl0(2, 1)
    v(575) = c0(4)*dcl0(3, 3)+c0(3)*dcl0(4, 3)-c0(6)*dcl0(5, 3)-c0(5)*dcl0(6, 3)
    v(574) = c0(4)*dcl0(3, 2)+c0(3)*dcl0(4, 2)-c0(6)*dcl0(5, 2)-c0(5)*dcl0(6, 2)
    v(573) = c0(4)*dcl0(3, 1)+c0(3)*dcl0(4, 1)-c0(6)*dcl0(5, 1)-c0(5)*dcl0(6, 1)
    v(572) = -(c0(5)*dcl0(2, 3))+c0(6)*dcl0(4, 3)-c0(2)*dcl0(5, 3)+c0(4)*dcl0(6, 3)
    v(571) = -(c0(5)*dcl0(2, 2))+c0(6)*dcl0(4, 2)-c0(2)*dcl0(5, 2)+c0(4)*dcl0(6, 2)
    v(570) = -(c0(5)*dcl0(2, 1))+c0(6)*dcl0(4, 1)-c0(2)*dcl0(5, 1)+c0(4)*dcl0(6, 1)
    v(568) = c1(2)*dcl1(3, 3)
    v(567) = c1(3)*dcl1(2, 3)
    v(566) = c1(2)*dcl1(3, 2)
    v(565) = c1(3)*dcl1(2, 2)
    v(564) = c1(2)*dcl1(3, 1)
    v(563) = c1(3)*dcl1(2, 1)
    v(562) = c1(4)*dcl1(3, 3)+c1(3)*dcl1(4, 3)-c1(6)*dcl1(5, 3)-c1(5)*dcl1(6, 3)
    v(561) = c1(4)*dcl1(3, 2)+c1(3)*dcl1(4, 2)-c1(6)*dcl1(5, 2)-c1(5)*dcl1(6, 2)
    v(560) = c1(4)*dcl1(3, 1)+c1(3)*dcl1(4, 1)-c1(6)*dcl1(5, 1)-c1(5)*dcl1(6, 1)
    v(559) = -(c1(5)*dcl1(2, 3))+c1(6)*dcl1(4, 3)-c1(2)*dcl1(5, 3)+c1(4)*dcl1(6, 3)
    v(558) = -(c1(5)*dcl1(2, 2))+c1(6)*dcl1(4, 2)-c1(2)*dcl1(5, 2)+c1(4)*dcl1(6, 2)
    v(557) = -(c1(5)*dcl1(2, 1))+c1(6)*dcl1(4, 1)-c1(2)*dcl1(5, 1)+c1(4)*dcl1(6, 1)
    v(555) = d2c0(6, 3, 3)
    v(554) = d2c0(6, 3, 2)
    v(553) = d2c0(6, 3, 1)
    v(552) = d2c0(6, 2, 3)
    v(551) = d2c0(6, 2, 2)
    v(550) = d2c0(6, 2, 1)
    v(549) = d2c0(6, 1, 3)
    v(548) = d2c0(6, 1, 2)
    v(547) = d2c0(6, 1, 1)
    v(546) = d2c0(5, 3, 3)
    v(545) = d2c0(5, 3, 2)
    v(544) = d2c0(5, 3, 1)
    v(543) = d2c0(5, 2, 3)
    v(542) = d2c0(5, 2, 2)
    v(541) = d2c0(5, 2, 1)
    v(540) = d2c0(5, 1, 3)
    v(539) = d2c0(5, 1, 2)
    v(538) = d2c0(5, 1, 1)
    v(537) = d2c0(4, 3, 3)
    v(536) = d2c0(4, 3, 2)
    v(535) = d2c0(4, 3, 1)
    v(534) = d2c0(4, 2, 3)
    v(533) = d2c0(4, 2, 2)
    v(532) = d2c0(4, 2, 1)
    v(531) = d2c0(4, 1, 3)
    v(530) = d2c0(4, 1, 2)
    v(529) = d2c0(4, 1, 1)
    v(528) = d2c0(3, 3, 3)
    v(527) = d2c0(3, 3, 2)
    v(526) = d2c0(3, 3, 1)
    v(525) = d2c0(3, 2, 3)
    v(524) = d2c0(3, 2, 2)
    v(523) = d2c0(3, 2, 1)
    v(522) = d2c0(3, 1, 3)
    v(521) = d2c0(3, 1, 2)
    v(520) = d2c0(3, 1, 1)
    v(519) = d2c0(2, 3, 3)
    v(518) = d2c0(2, 3, 2)
    v(517) = d2c0(2, 3, 1)
    v(516) = d2c0(2, 2, 3)
    v(515) = d2c0(2, 2, 2)
    v(514) = d2c0(2, 2, 1)
    v(513) = d2c0(2, 1, 3)
    v(512) = d2c0(2, 1, 2)
    v(511) = d2c0(2, 1, 1)
    v(510) = d2c1(6, 3, 3)
    v(509) = d2c1(6, 3, 2)
    v(508) = d2c1(6, 3, 1)
    v(507) = d2c1(6, 2, 3)
    v(506) = d2c1(6, 2, 2)
    v(505) = d2c1(6, 2, 1)
    v(504) = d2c1(6, 1, 3)
    v(503) = d2c1(6, 1, 2)
    v(502) = d2c1(6, 1, 1)
    v(501) = d2c1(5, 3, 3)
    v(500) = d2c1(5, 3, 2)
    v(499) = d2c1(5, 3, 1)
    v(498) = d2c1(5, 2, 3)
    v(497) = d2c1(5, 2, 2)
    v(496) = d2c1(5, 2, 1)
    v(495) = d2c1(5, 1, 3)
    v(494) = d2c1(5, 1, 2)
    v(493) = d2c1(5, 1, 1)
    v(492) = d2c1(4, 3, 3)
    v(491) = d2c1(4, 3, 2)
    v(490) = d2c1(4, 3, 1)
    v(489) = d2c1(4, 2, 3)
    v(488) = d2c1(4, 2, 2)
    v(487) = d2c1(4, 2, 1)
    v(486) = d2c1(4, 1, 3)
    v(485) = d2c1(4, 1, 2)
    v(484) = d2c1(4, 1, 1)
    v(483) = d2c1(3, 3, 3)
    v(482) = d2c1(3, 3, 2)
    v(481) = d2c1(3, 3, 1)
    v(480) = d2c1(3, 2, 3)
    v(479) = d2c1(3, 2, 2)
    v(478) = d2c1(3, 2, 1)
    v(477) = d2c1(3, 1, 3)
    v(476) = d2c1(3, 1, 2)
    v(475) = d2c1(3, 1, 1)
    v(474) = d2c1(2, 3, 3)
    v(473) = d2c1(2, 3, 2)
    v(472) = d2c1(2, 3, 1)
    v(471) = d2c1(2, 2, 3)
    v(470) = d2c1(2, 2, 2)
    v(469) = d2c1(2, 2, 1)
    v(468) = d2c1(2, 1, 3)
    v(467) = d2c1(2, 1, 2)
    v(466) = d2c1(2, 1, 1)
    v(465) = d2c1(1, 3, 3)
    v(464) = d2c1(1, 3, 2)
    v(463) = d2c1(1, 3, 1)
    v(462) = d2c1(1, 2, 3)
    v(461) = d2c1(1, 2, 2)
    v(460) = d2c1(1, 2, 1)
    v(459) = d2c1(1, 1, 3)
    v(458) = d2c1(1, 1, 2)
    v(457) = d2c1(1, 1, 1)
    v(456) = c0(2)*dck0(3, 3)
    v(455) = c0(3)*dck0(2, 3)
    v(454) = c0(4)*dck0(3, 3)+c0(3)*dck0(4, 3)-c0(6)*dck0(5, 3)-c0(5)*dck0(6, 3)
    v(453) = -(c0(5)*dck0(2, 3))+c0(6)*dck0(4, 3)-c0(2)*dck0(5, 3)+c0(4)*dck0(6, 3)
    v(451) = c0(2)*dck0(3, 2)
    v(450) = c0(3)*dck0(2, 2)
    v(449) = (-2d0)*c0(6)
    v(448) = c0(4)*dck0(3, 2)+c0(3)*dck0(4, 2)-c0(6)*dck0(5, 2)-c0(5)*dck0(6, 2)
    v(447) = -(c0(5)*dck0(2, 2))+c0(6)*dck0(4, 2)-c0(2)*dck0(5, 2)+c0(4)*dck0(6, 2)
    v(446) = c0(3)*dck0(2, 1)+c0(2)*dck0(3, 1)+dck0(6, 1)*v(449)
    v(445) = c0(4)*dck0(3, 1)+c0(3)*dck0(4, 1)-c0(6)*dck0(5, 1)-c0(5)*dck0(6, 1)
    v(444) = -(c0(5)*dck0(2, 1))+c0(6)*dck0(4, 1)-c0(2)*dck0(5, 1)+c0(4)*dck0(6, 1)
    v(443) = c1(2)*dck1(3, 3)
    v(442) = c1(3)*dck1(2, 3)
    v(441) = c1(4)*dck1(3, 3)+c1(3)*dck1(4, 3)-c1(6)*dck1(5, 3)-c1(5)*dck1(6, 3)
    v(440) = -(c1(5)*dck1(2, 3))+c1(6)*dck1(4, 3)-c1(2)*dck1(5, 3)+c1(4)*dck1(6, 3)
    v(439) = c1(2)*dck1(3, 2)
    v(438) = c1(3)*dck1(2, 2)
    v(437) = (-2d0)*c1(6)
    v(436) = c1(4)*dck1(3, 2)+c1(3)*dck1(4, 2)-c1(6)*dck1(5, 2)-c1(5)*dck1(6, 2)
    v(435) = -(c1(5)*dck1(2, 2))+c1(6)*dck1(4, 2)-c1(2)*dck1(5, 2)+c1(4)*dck1(6, 2)
    v(434) = c1(3)*dck1(2, 1)+c1(2)*dck1(3, 1)+dck1(6, 1)*v(437)
    v(433) = c1(4)*dck1(3, 1)+c1(3)*dck1(4, 1)-c1(6)*dck1(5, 1)-c1(5)*dck1(6, 1)
    v(432) = -(c1(5)*dck1(2, 1))+c1(6)*dck1(4, 1)-c1(2)*dck1(5, 1)+c1(4)*dck1(6, 1)
    v(431) = c0(2)*c0(3)-c0(6)**2
    v(430) = c0(3)*c0(4)-c0(5)*c0(6)
    v(429) = -(c0(2)*c0(5))+c0(4)*c0(6)
    v(428) = c1(2)*c1(3)-c1(6)**2
    v(427) = c1(3)*c1(4)-c1(5)*c1(6)
    v(426) = -(c1(2)*c1(5))+c1(4)*c1(6)
    v(261) = c1(5)*v(426)-c1(4)*v(427)+c1(1)*v(428)
    v(310) = 1d0/v(261)**3
    v(569) = (-2d0)*v(310)
    v(271) = 1d0/v(261)**2
    v(262) = c0(5)*v(429)-c0(4)*v(430)+c0(1)*v(431)
    v(452) = -(v(262)*v(271))
    v(260) = v(262)/v(261)
    v(329) = 1d0/v(260)**0.16666666666666669d1
    v(648) = (-2d0/3d0)*v(329)
    v(270) = 1d0/v(260)**0.6666666666666666d0
    v(556) = v(270)/3d0
    v(254) = v(260)**0.3333333333333333d0
    v(339) = dck1(5, 1)*v(426)-dck1(4, 1)*v(427)+dck1(1, 1)*v(428)+c1(5)*v(432)-c1(4)*v(433)+c1(1)*v(434)
    v(347) = dck1(6, 2)*v(437)+v(438)+v(439)
    v(351) = c1(1)*v(347)+dck1(5, 2)*v(426)-dck1(4, 2)*v(427)+dck1(1, 2)*v(428)+c1(5)*v(435)-c1(4)*v(436)
    v(359) = dck1(6, 3)*v(437)+v(442)+v(443)
    v(363) = c1(1)*v(359)+dck1(5, 3)*v(426)-dck1(4, 3)*v(427)+dck1(1, 3)*v(428)+c1(5)*v(440)-c1(4)*v(441)
    v(338) = dck0(5, 1)*v(429)-dck0(4, 1)*v(430)+dck0(1, 1)*v(431)+c0(5)*v(444)-c0(4)*v(445)+c0(1)*v(446)
    v(343) = v(338)/v(261)+v(339)*v(452)
    v(352) = dck0(6, 2)*v(449)+v(450)+v(451)
    v(350) = c0(1)*v(352)+dck0(5, 2)*v(429)-dck0(4, 2)*v(430)+dck0(1, 2)*v(431)+c0(5)*v(447)-c0(4)*v(448)
    v(355) = v(350)/v(261)+v(351)*v(452)
    v(364) = dck0(6, 3)*v(449)+v(455)+v(456)
    v(362) = c0(1)*v(364)+dck0(5, 3)*v(429)-dck0(4, 3)*v(430)+dck0(1, 3)*v(431)+c0(5)*v(453)-c0(4)*v(454)
    v(367) = v(362)/v(261)+v(363)*v(452)
    v(269) = v(343)*v(556)
    v(273) = v(355)*v(556)
    v(275) = v(367)*v(556)
    v(300) = dcl1(6, 1)*v(437)+v(563)+v(564)
    v(301) = dcl1(6, 2)*v(437)+v(565)+v(566)
    v(302) = dcl1(6, 3)*v(437)+v(567)+v(568)
    v(303) = c1(1)*v(300)+dcl1(5, 1)*v(426)-dcl1(4, 1)*v(427)+dcl1(1, 1)*v(428)+c1(5)*v(557)-c1(4)*v(560)
    v(304) = c1(1)*v(301)+dcl1(5, 2)*v(426)-dcl1(4, 2)*v(427)+dcl1(1, 2)*v(428)+c1(5)*v(558)-c1(4)*v(561)
    v(305) = c1(1)*v(302)+dcl1(5, 3)*v(426)-dcl1(4, 3)*v(427)+dcl1(1, 3)*v(428)+c1(5)*v(559)-c1(4)*v(562)
    v(306) = -(v(271)*v(303))
    v(307) = -(v(271)*v(304))
    v(308) = -(v(271)*v(305))
    v(309) = v(303)*v(569)
    v(604) = -(v(262)*v(309))
    v(311) = v(304)*v(569)
    v(671) = -(v(262)*v(311))
    v(312) = v(305)*v(569)
    v(737) = -(v(262)*v(312))
    v(319) = dcl0(6, 1)*v(449)+v(576)+v(577)
    v(320) = dcl0(6, 2)*v(449)+v(578)+v(579)
    v(321) = dcl0(6, 3)*v(449)+v(580)+v(581)
    v(322) = c0(1)*v(319)+dcl0(5, 1)*v(429)-dcl0(4, 1)*v(430)+dcl0(1, 1)*v(431)+c0(5)*v(570)-c0(4)*v(573)
    v(605) = -(v(271)*v(322))
    v(323) = c0(1)*v(320)+dcl0(5, 2)*v(429)-dcl0(4, 2)*v(430)+dcl0(1, 2)*v(431)+c0(5)*v(571)-c0(4)*v(574)
    v(672) = -(v(271)*v(323))
    v(324) = c0(1)*v(321)+dcl0(5, 3)*v(429)-dcl0(4, 3)*v(430)+dcl0(1, 3)*v(431)+c0(5)*v(572)-c0(4)*v(575)
    v(738) = -(v(271)*v(324))
    v(325) = v(262)*v(306)+v(322)/v(261)
    v(326) = v(262)*v(307)+v(323)/v(261)
    v(327) = v(262)*v(308)+v(324)/v(261)
    v(328) = v(325)*v(648)
    v(368) = (v(328)*v(367)+v(270)*(v(306)*v(362)+v(452)*(dck1(1, 3)*v(300)+dcl1(1, 1)*v(359)+dcl1(5, 1)*v(440)-dcl1(4, 1)*v(441)&
    &+v(428)*v(463)-v(427)*v(490)+v(426)*v(499)+dck1(5, 3)*v(557)-dck1(4, 3)*v(560)+c1(5)*(-(c1(5)*v(472))+c1(6)*v(490)-c1(2&
    &)*v(499)+c1(4)*v(508)+v(582)+v(583)+v(584)+v(585))-c1(4)*(c1(4)*v(481)+c1(3)*v(490)-c1(6)*v(499)-c1(5)*v(508)+v(586)+v&
    &(587)+v(588)+v(589))+c1(1)*(c1(3)*v(472)+c1(2)*v(481)+v(437)*v(508)+v(590)+v(591)+v(592)))+(dck0(1, 3)*v(319)+dcl0(1, 1&
    &)*v(364)+d2c0(1, 3, 1)*v(431)+dcl0(5, 1)*v(453)-dcl0(4, 1)*v(454)-v(430)*v(535)+v(429)*v(544)+dck0(5, 3)*v(570)-dck0(4, 3)*v&
    &(573)+c0(5)*(-(c0(5)*v(517))+c0(6)*v(535)-c0(2)*v(544)+c0(4)*v(553)+v(593)+v(594)+v(595)+v(596))-c0(4)*(c0(4)*v(526)+c0&
    &(3)*v(535)-c0(6)*v(544)-c0(5)*v(553)+v(597)+v(598)+v(599)+v(600))+c0(1)*(c0(3)*v(517)+c0(2)*v(526)+v(449)*v(553)+v(601)&
    &+v(602)+v(603)))/v(261)+v(363)*v(604)+v(363)*v(605)))/3d0
    v(356) = (v(328)*v(355)+v(270)*(v(306)*v(350)+v(351)*v(604)+v(351)*v(605)+v(452)*(dck1(1, 2)*v(300)+dcl1(1, 1)*v(347)+dcl1&
    &(5, 1)*v(435)-dcl1(4, 1)*v(436)+v(428)*v(460)-v(427)*v(487)+v(426)*v(496)+dck1(5, 2)*v(557)-dck1(4, 2)*v(560)+c1(5)*(-(c1(5&
    &)*v(469))+c1(6)*v(487)-c1(2)*v(496)+c1(4)*v(505)+v(608)+v(609)+v(610)+v(611))-c1(4)*(c1(4)*v(478)+c1(3)*v(487)-c1(6)*v&
    &(496)-c1(5)*v(505)+v(612)+v(613)+v(614)+v(615))+c1(1)*(c1(3)*v(469)+c1(2)*v(478)+v(437)*v(505)+dck1(6, 2)*v(606)+v(616)&
    &+v(617)))+(dck0(1, 2)*v(319)+dcl0(1, 1)*v(352)+d2c0(1, 2, 1)*v(431)+dcl0(5, 1)*v(447)-dcl0(4, 1)*v(448)-v(430)*v(532)+v(429&
    &)*v(541)+dck0(5, 2)*v(570)-dck0(4, 2)*v(573)+c0(5)*(-(c0(5)*v(514))+c0(6)*v(532)-c0(2)*v(541)+c0(4)*v(550)+v(618)+v(619)&
    &+v(620)+v(621))-c0(4)*(c0(4)*v(523)+c0(3)*v(532)-c0(6)*v(541)-c0(5)*v(550)+v(622)+v(623)+v(624)+v(625))+c0(1)*(c0(3)*v&
    &(514)+c0(2)*v(523)+v(449)*v(550)+dck0(6, 2)*v(607)+v(626)+v(627)))/v(261)))/3d0
    v(344) = (v(328)*v(343)+v(270)*(v(306)*v(338)+v(339)*v(604)+v(339)*v(605)+v(452)*(dck1(1, 1)*v(300)+dcl1(5, 1)*v(432)-dcl1&
    &(4, 1)*v(433)+dcl1(1, 1)*v(434)+v(428)*v(457)-v(427)*v(484)+v(426)*v(493)+dck1(5, 1)*v(557)-dck1(4, 1)*v(560)+c1(5)*(-(c1(5&
    &)*v(466))+c1(6)*v(484)-c1(2)*v(493)+c1(4)*v(502)+v(628)+v(629)+v(630)+v(631))-c1(4)*(c1(4)*v(475)+c1(3)*v(484)-c1(6)*v&
    &(493)-c1(5)*v(502)+v(632)+v(633)+v(634)+v(635))+c1(1)*(c1(3)*v(466)+c1(2)*v(475)+v(437)*v(502)+dck1(6, 1)*v(606)+v(636)&
    &+v(637)))+(dck0(1, 1)*v(319)+d2c0(1, 1, 1)*v(431)+dcl0(5, 1)*v(444)-dcl0(4, 1)*v(445)+dcl0(1, 1)*v(446)-v(430)*v(529)+v(429&
    &)*v(538)+dck0(5, 1)*v(570)-dck0(4, 1)*v(573)+c0(5)*(-(c0(5)*v(511))+c0(6)*v(529)-c0(2)*v(538)+c0(4)*v(547)+v(638)+v(639)&
    &+v(640)+v(641))-c0(4)*(c0(4)*v(520)+c0(3)*v(529)-c0(6)*v(538)-c0(5)*v(547)+v(642)+v(643)+v(644)+v(645))+c0(1)*(c0(3)*v&
    &(511)+c0(2)*v(520)+v(449)*v(547)+dck0(6, 1)*v(607)+v(646)+v(647)))/v(261)))/3d0
    v(330) = v(326)*v(648)
    v(369) = (v(330)*v(367)+v(270)*(v(307)*v(362)+v(452)*(dck1(1, 3)*v(301)+dcl1(1, 2)*v(359)+dcl1(5, 2)*v(440)-dcl1(4, 2)*v(441)&
    &+v(428)*v(464)-v(427)*v(491)+v(426)*v(500)+dck1(5, 3)*v(558)-dck1(4, 3)*v(561)+c1(5)*(-(c1(5)*v(473))+c1(6)*v(491)-c1(2&
    &)*v(500)+c1(4)*v(509)+v(649)+v(650)+v(651)+v(652))-c1(4)*(c1(4)*v(482)+c1(3)*v(491)-c1(6)*v(500)-c1(5)*v(509)+v(653)+v&
    &(654)+v(655)+v(656))+c1(1)*(c1(3)*v(473)+c1(2)*v(482)+v(437)*v(509)+v(657)+v(658)+v(659)))+(dck0(1, 3)*v(320)+dcl0(1, 2&
    &)*v(364)+d2c0(1, 3, 2)*v(431)+dcl0(5, 2)*v(453)-dcl0(4, 2)*v(454)-v(430)*v(536)+v(429)*v(545)+dck0(5, 3)*v(571)-dck0(4, 3)*v&
    &(574)+c0(5)*(-(c0(5)*v(518))+c0(6)*v(536)-c0(2)*v(545)+c0(4)*v(554)+v(660)+v(661)+v(662)+v(663))-c0(4)*(c0(4)*v(527)+c0&
    &(3)*v(536)-c0(6)*v(545)-c0(5)*v(554)+v(664)+v(665)+v(666)+v(667))+c0(1)*(c0(3)*v(518)+c0(2)*v(527)+v(449)*v(554)+v(668)&
    &+v(669)+v(670)))/v(261)+v(363)*v(671)+v(363)*v(672)))/3d0
    v(357) = (v(330)*v(355)+v(270)*(v(307)*v(350)+v(351)*v(671)+v(351)*v(672)+v(452)*(dck1(1, 2)*v(301)+dcl1(1, 2)*v(347)+dcl1&
    &(5, 2)*v(435)-dcl1(4, 2)*v(436)+v(428)*v(461)-v(427)*v(488)+v(426)*v(497)+dck1(5, 2)*v(558)-dck1(4, 2)*v(561)+c1(5)*(-(c1(5&
    &)*v(470))+c1(6)*v(488)-c1(2)*v(497)+c1(4)*v(506)+v(675)+v(676)+v(677)+v(678))-c1(4)*(c1(4)*v(479)+c1(3)*v(488)-c1(6)*v&
    &(497)-c1(5)*v(506)+v(679)+v(680)+v(681)+v(682))+c1(1)*(c1(3)*v(470)+c1(2)*v(479)+v(437)*v(506)+dck1(6, 2)*v(673)+v(683)&
    &+v(684)))+(dck0(1, 2)*v(320)+dcl0(1, 2)*v(352)+d2c0(1, 2, 2)*v(431)+dcl0(5, 2)*v(447)-dcl0(4, 2)*v(448)-v(430)*v(533)+v(429&
    &)*v(542)+dck0(5, 2)*v(571)-dck0(4, 2)*v(574)+c0(5)*(-(c0(5)*v(515))+c0(6)*v(533)-c0(2)*v(542)+c0(4)*v(551)+v(685)+v(686)&
    &+v(687)+v(688))-c0(4)*(c0(4)*v(524)+c0(3)*v(533)-c0(6)*v(542)-c0(5)*v(551)+v(689)+v(690)+v(691)+v(692))+c0(1)*(c0(3)*v&
    &(515)+c0(2)*v(524)+v(449)*v(551)+dck0(6, 2)*v(674)+v(693)+v(694)))/v(261)))/3d0
    v(345) = (v(330)*v(343)+v(270)*(v(307)*v(338)+v(339)*v(671)+v(339)*v(672)+v(452)*(dck1(1, 1)*v(301)+dcl1(5, 2)*v(432)-dcl1&
    &(4, 2)*v(433)+dcl1(1, 2)*v(434)+v(428)*v(458)-v(427)*v(485)+v(426)*v(494)+dck1(5, 1)*v(558)-dck1(4, 1)*v(561)+c1(5)*(-(c1(5&
    &)*v(467))+c1(6)*v(485)-c1(2)*v(494)+c1(4)*v(503)+v(695)+v(696)+v(697)+v(698))-c1(4)*(c1(4)*v(476)+c1(3)*v(485)-c1(6)*v&
    &(494)-c1(5)*v(503)+v(699)+v(700)+v(701)+v(702))+c1(1)*(c1(3)*v(467)+c1(2)*v(476)+v(437)*v(503)+dck1(6, 1)*v(673)+v(703)&
    &+v(704)))+(dck0(1, 1)*v(320)+d2c0(1, 1, 2)*v(431)+dcl0(5, 2)*v(444)-dcl0(4, 2)*v(445)+dcl0(1, 2)*v(446)-v(430)*v(530)+v(429&
    &)*v(539)+dck0(5, 1)*v(571)-dck0(4, 1)*v(574)+c0(5)*(-(c0(5)*v(512))+c0(6)*v(530)-c0(2)*v(539)+c0(4)*v(548)+v(705)+v(706)&
    &+v(707)+v(708))-c0(4)*(c0(4)*v(521)+c0(3)*v(530)-c0(6)*v(539)-c0(5)*v(548)+v(709)+v(710)+v(711)+v(712))+c0(1)*(c0(3)*v&
    &(512)+c0(2)*v(521)+v(449)*v(548)+dck0(6, 1)*v(674)+v(713)+v(714)))/v(261)))/3d0
    v(331) = v(327)*v(648)
    v(370) = (v(331)*v(367)+v(270)*(v(308)*v(362)+v(452)*(dck1(1, 3)*v(302)+dcl1(1, 3)*v(359)+dcl1(5, 3)*v(440)-dcl1(4, 3)*v(441)&
    &+v(428)*v(465)-v(427)*v(492)+v(426)*v(501)+dck1(5, 3)*v(559)-dck1(4, 3)*v(562)+c1(5)*(-(c1(5)*v(474))+c1(6)*v(492)-c1(2&
    &)*v(501)+c1(4)*v(510)+v(715)+v(716)+v(717)+v(718))-c1(4)*(c1(4)*v(483)+c1(3)*v(492)-c1(6)*v(501)-c1(5)*v(510)+v(719)+v&
    &(720)+v(721)+v(722))+c1(1)*(c1(3)*v(474)+c1(2)*v(483)+v(437)*v(510)+v(723)+v(724)+v(725)))+(dck0(1, 3)*v(321)+dcl0(1, 3&
    &)*v(364)+d2c0(1, 3, 3)*v(431)+dcl0(5, 3)*v(453)-dcl0(4, 3)*v(454)-v(430)*v(537)+v(429)*v(546)+dck0(5, 3)*v(572)-dck0(4, 3)*v&
    &(575)+c0(5)*(-(c0(5)*v(519))+c0(6)*v(537)-c0(2)*v(546)+c0(4)*v(555)+v(726)+v(727)+v(728)+v(729))-c0(4)*(c0(4)*v(528)+c0&
    &(3)*v(537)-c0(6)*v(546)-c0(5)*v(555)+v(730)+v(731)+v(732)+v(733))+c0(1)*(c0(3)*v(519)+c0(2)*v(528)+v(449)*v(555)+v(734)&
    &+v(735)+v(736)))/v(261)+v(363)*v(737)+v(363)*v(738)))/3d0
    v(358) = (v(331)*v(355)+v(270)*(v(308)*v(350)+v(351)*v(737)+v(351)*v(738)+v(452)*(dck1(1, 2)*v(302)+dcl1(1, 3)*v(347)+dcl1&
    &(5, 3)*v(435)-dcl1(4, 3)*v(436)+v(428)*v(462)-v(427)*v(489)+v(426)*v(498)+dck1(5, 2)*v(559)-dck1(4, 2)*v(562)+c1(5)*(-(c1(5&
    &)*v(471))+c1(6)*v(489)-c1(2)*v(498)+c1(4)*v(507)+v(741)+v(742)+v(743)+v(744))-c1(4)*(c1(4)*v(480)+c1(3)*v(489)-c1(6)*v&
    &(498)-c1(5)*v(507)+v(745)+v(746)+v(747)+v(748))+c1(1)*(c1(3)*v(471)+c1(2)*v(480)+v(437)*v(507)+dck1(6, 2)*v(739)+v(749)&
    &+v(750)))+(dck0(1, 2)*v(321)+dcl0(1, 3)*v(352)+d2c0(1, 2, 3)*v(431)+dcl0(5, 3)*v(447)-dcl0(4, 3)*v(448)-v(430)*v(534)+v(429&
    &)*v(543)+dck0(5, 2)*v(572)-dck0(4, 2)*v(575)+c0(5)*(-(c0(5)*v(516))+c0(6)*v(534)-c0(2)*v(543)+c0(4)*v(552)+v(751)+v(752)&
    &+v(753)+v(754))-c0(4)*(c0(4)*v(525)+c0(3)*v(534)-c0(6)*v(543)-c0(5)*v(552)+v(755)+v(756)+v(757)+v(758))+c0(1)*(c0(3)*v&
    &(516)+c0(2)*v(525)+v(449)*v(552)+dck0(6, 2)*v(740)+v(759)+v(760)))/v(261)))/3d0
    v(346) = (v(331)*v(343)+v(270)*(v(308)*v(338)+v(339)*v(737)+v(339)*v(738)+v(452)*(dck1(1, 1)*v(302)+dcl1(5, 3)*v(432)-dcl1&
    &(4, 3)*v(433)+dcl1(1, 3)*v(434)+v(428)*v(459)-v(427)*v(486)+v(426)*v(495)+dck1(5, 1)*v(559)-dck1(4, 1)*v(562)+c1(5)*(-(c1(5&
    &)*v(468))+c1(6)*v(486)-c1(2)*v(495)+c1(4)*v(504)+v(761)+v(762)+v(763)+v(764))-c1(4)*(c1(4)*v(477)+c1(3)*v(486)-c1(6)*v&
    &(495)-c1(5)*v(504)+v(765)+v(766)+v(767)+v(768))+c1(1)*(c1(3)*v(468)+c1(2)*v(477)+v(437)*v(504)+dck1(6, 1)*v(739)+v(769)&
    &+v(770)))+(dck0(1, 1)*v(321)+d2c0(1, 1, 3)*v(431)+dcl0(5, 3)*v(444)-dcl0(4, 3)*v(445)+dcl0(1, 3)*v(446)-v(430)*v(531)+v(429&
    &)*v(540)+dck0(5, 1)*v(572)-dck0(4, 1)*v(575)+c0(5)*(-(c0(5)*v(513))+c0(6)*v(531)-c0(2)*v(540)+c0(4)*v(549)+v(771)+v(772)&
    &+v(773)+v(774))-c0(4)*(c0(4)*v(522)+c0(3)*v(531)-c0(6)*v(540)-c0(5)*v(549)+v(775)+v(776)+v(777)+v(778))+c0(1)*(c0(3)*v&
    &(513)+c0(2)*v(522)+v(449)*v(549)+dck0(6, 1)*v(740)+v(779)+v(780)))/v(261)))/3d0
    v(332) = v(325)*v(556)
    v(333) = v(326)*v(556)
    v(334) = v(327)*v(556)
    d2c(1, 1, 1) = dcl1(1, 1)*v(269)+dck1(1, 1)*v(332)+c1(1)*v(344)+v(254)*v(457)
    d2c(1, 1, 2) = dcl1(1, 2)*v(269)+dck1(1, 1)*v(333)+c1(1)*v(345)+v(254)*v(458)
    d2c(1, 1, 3) = dcl1(1, 3)*v(269)+dck1(1, 1)*v(334)+c1(1)*v(346)+v(254)*v(459)
    d2c(1, 2, 1) = dcl1(1, 1)*v(273)+dck1(1, 2)*v(332)+c1(1)*v(356)+v(254)*v(460)
    d2c(1, 2, 2) = dcl1(1, 2)*v(273)+dck1(1, 2)*v(333)+c1(1)*v(357)+v(254)*v(461)
    d2c(1, 2, 3) = dcl1(1, 3)*v(273)+dck1(1, 2)*v(334)+c1(1)*v(358)+v(254)*v(462)
    d2c(1, 3, 1) = dcl1(1, 1)*v(275)+dck1(1, 3)*v(332)+c1(1)*v(368)+v(254)*v(463)
    d2c(1, 3, 2) = dcl1(1, 2)*v(275)+dck1(1, 3)*v(333)+c1(1)*v(369)+v(254)*v(464)
    d2c(1, 3, 3) = dcl1(1, 3)*v(275)+dck1(1, 3)*v(334)+c1(1)*v(370)+v(254)*v(465)
    d2c(2, 1, 1) = dcl1(2, 1)*v(269)+dck1(2, 1)*v(332)+c1(2)*v(344)+v(254)*v(466)
    d2c(2, 1, 2) = dcl1(2, 2)*v(269)+dck1(2, 1)*v(333)+c1(2)*v(345)+v(254)*v(467)
    d2c(2, 1, 3) = dcl1(2, 3)*v(269)+dck1(2, 1)*v(334)+c1(2)*v(346)+v(254)*v(468)
    d2c(2, 2, 1) = dcl1(2, 1)*v(273)+dck1(2, 2)*v(332)+c1(2)*v(356)+v(254)*v(469)
    d2c(2, 2, 2) = dcl1(2, 2)*v(273)+dck1(2, 2)*v(333)+c1(2)*v(357)+v(254)*v(470)
    d2c(2, 2, 3) = dcl1(2, 3)*v(273)+dck1(2, 2)*v(334)+c1(2)*v(358)+v(254)*v(471)
    d2c(2, 3, 1) = dcl1(2, 1)*v(275)+dck1(2, 3)*v(332)+c1(2)*v(368)+v(254)*v(472)
    d2c(2, 3, 2) = dcl1(2, 2)*v(275)+dck1(2, 3)*v(333)+c1(2)*v(369)+v(254)*v(473)
    d2c(2, 3, 3) = dcl1(2, 3)*v(275)+dck1(2, 3)*v(334)+c1(2)*v(370)+v(254)*v(474)
    d2c(3, 1, 1) = dcl1(3, 1)*v(269)+dck1(3, 1)*v(332)+c1(3)*v(344)+v(254)*v(475)
    d2c(3, 1, 2) = dcl1(3, 2)*v(269)+dck1(3, 1)*v(333)+c1(3)*v(345)+v(254)*v(476)
    d2c(3, 1, 3) = dcl1(3, 3)*v(269)+dck1(3, 1)*v(334)+c1(3)*v(346)+v(254)*v(477)
    d2c(3, 2, 1) = dcl1(3, 1)*v(273)+dck1(3, 2)*v(332)+c1(3)*v(356)+v(254)*v(478)
    d2c(3, 2, 2) = dcl1(3, 2)*v(273)+dck1(3, 2)*v(333)+c1(3)*v(357)+v(254)*v(479)
    d2c(3, 2, 3) = dcl1(3, 3)*v(273)+dck1(3, 2)*v(334)+c1(3)*v(358)+v(254)*v(480)
    d2c(3, 3, 1) = dcl1(3, 1)*v(275)+dck1(3, 3)*v(332)+c1(3)*v(368)+v(254)*v(481)
    d2c(3, 3, 2) = dcl1(3, 2)*v(275)+dck1(3, 3)*v(333)+c1(3)*v(369)+v(254)*v(482)
    d2c(3, 3, 3) = dcl1(3, 3)*v(275)+dck1(3, 3)*v(334)+c1(3)*v(370)+v(254)*v(483)
    d2c(4, 1, 1) = dcl1(4, 1)*v(269)+dck1(4, 1)*v(332)+c1(4)*v(344)+v(254)*v(484)
    d2c(4, 1, 2) = dcl1(4, 2)*v(269)+dck1(4, 1)*v(333)+c1(4)*v(345)+v(254)*v(485)
    d2c(4, 1, 3) = dcl1(4, 3)*v(269)+dck1(4, 1)*v(334)+c1(4)*v(346)+v(254)*v(486)
    d2c(4, 2, 1) = dcl1(4, 1)*v(273)+dck1(4, 2)*v(332)+c1(4)*v(356)+v(254)*v(487)
    d2c(4, 2, 2) = dcl1(4, 2)*v(273)+dck1(4, 2)*v(333)+c1(4)*v(357)+v(254)*v(488)
    d2c(4, 2, 3) = dcl1(4, 3)*v(273)+dck1(4, 2)*v(334)+c1(4)*v(358)+v(254)*v(489)
    d2c(4, 3, 1) = dcl1(4, 1)*v(275)+dck1(4, 3)*v(332)+c1(4)*v(368)+v(254)*v(490)
    d2c(4, 3, 2) = dcl1(4, 2)*v(275)+dck1(4, 3)*v(333)+c1(4)*v(369)+v(254)*v(491)
    d2c(4, 3, 3) = dcl1(4, 3)*v(275)+dck1(4, 3)*v(334)+c1(4)*v(370)+v(254)*v(492)
    d2c(5, 1, 1) = dcl1(5, 1)*v(269)+dck1(5, 1)*v(332)+c1(5)*v(344)+v(254)*v(493)
    d2c(5, 1, 2) = dcl1(5, 2)*v(269)+dck1(5, 1)*v(333)+c1(5)*v(345)+v(254)*v(494)
    d2c(5, 1, 3) = dcl1(5, 3)*v(269)+dck1(5, 1)*v(334)+c1(5)*v(346)+v(254)*v(495)
    d2c(5, 2, 1) = dcl1(5, 1)*v(273)+dck1(5, 2)*v(332)+c1(5)*v(356)+v(254)*v(496)
    d2c(5, 2, 2) = dcl1(5, 2)*v(273)+dck1(5, 2)*v(333)+c1(5)*v(357)+v(254)*v(497)
    d2c(5, 2, 3) = dcl1(5, 3)*v(273)+dck1(5, 2)*v(334)+c1(5)*v(358)+v(254)*v(498)
    d2c(5, 3, 1) = dcl1(5, 1)*v(275)+dck1(5, 3)*v(332)+c1(5)*v(368)+v(254)*v(499)
    d2c(5, 3, 2) = dcl1(5, 2)*v(275)+dck1(5, 3)*v(333)+c1(5)*v(369)+v(254)*v(500)
    d2c(5, 3, 3) = dcl1(5, 3)*v(275)+dck1(5, 3)*v(334)+c1(5)*v(370)+v(254)*v(501)
    d2c(6, 1, 1) = dcl1(6, 1)*v(269)+dck1(6, 1)*v(332)+c1(6)*v(344)+v(254)*v(502)
    d2c(6, 1, 2) = dcl1(6, 2)*v(269)+dck1(6, 1)*v(333)+c1(6)*v(345)+v(254)*v(503)
    d2c(6, 1, 3) = dcl1(6, 3)*v(269)+dck1(6, 1)*v(334)+c1(6)*v(346)+v(254)*v(504)
    d2c(6, 2, 1) = dcl1(6, 1)*v(273)+dck1(6, 2)*v(332)+c1(6)*v(356)+v(254)*v(505)
    d2c(6, 2, 2) = dcl1(6, 2)*v(273)+dck1(6, 2)*v(333)+c1(6)*v(357)+v(254)*v(506)
    d2c(6, 2, 3) = dcl1(6, 3)*v(273)+dck1(6, 2)*v(334)+c1(6)*v(358)+v(254)*v(507)
    d2c(6, 3, 1) = dcl1(6, 1)*v(275)+dck1(6, 3)*v(332)+c1(6)*v(368)+v(254)*v(508)
    d2c(6, 3, 2) = dcl1(6, 2)*v(275)+dck1(6, 3)*v(333)+c1(6)*v(369)+v(254)*v(509)
    d2c(6, 3, 3) = dcl1(6, 3)*v(275)+dck1(6, 3)*v(334)+c1(6)*v(370)+v(254)*v(510)
  END SUBROUTINE movingleastsquarescombinedfbar2
!> moving least squares determines u2
  SUBROUTINE movingleastsquaresdetermu2(m, n, w, p, u2)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::m, n
    REAL(8), DIMENSION(n)::w
    REAL(8), DIMENSION(m, n)::b, u1, u2
    REAL(8), DIMENSION(m, n)::p
    REAL(8), DIMENSION(n, m)::at
    REAL(8), DIMENSION(m, m)::r
    DO im = 1, m
      DO in = 1, n
        at(in, im) = sqrt(w(in))*p(im, in)
      END DO
    END DO
    DO in = 1, n
      DO im = 1, m
        b(im, in) = p(im, in)*w(in)
      END DO
    END DO
    CALL movingleastsquaresnaiveqr(n, m, at, r)
    CALL movingleastsquaresgenmatrixtriangularsolve(.FALSE., n, m, r, b, u1)
    CALL movingleastsquaresgenmatrixtriangularsolve(.TRUE., n, m, r, u1, u2)
  END SUBROUTINE movingleastsquaresdetermu2
!> moving least squares 2d fbar kernel
  SUBROUTINE movingleastsquaresfbar2d(dck, dcl, d2c, s, ds, kernel)
    IMPLICIT NONE
    REAL(8) v(86), dck(3, 2), dcl(3, 2), d2c(3, 2, 2), s(3), ds(3, 3), kernel(2, 2)
    v(75) = s(2)/2d0
    v(74) = s(1)/2d0
    v(73) = ds(3, 2)/2d0
    v(72) = ds(3, 1)/2d0
    v(71) = dcl(3, 2)/2d0
    v(70) = dcl(2, 2)/4d0
    v(69) = dcl(1, 2)/4d0
    v(68) = ds(1, 3)*v(71)
    v(67) = ds(1, 2)*v(70)
    v(66) = ds(1, 1)*v(69)
    v(81) = v(66)+v(67)+v(68)
    v(65) = dcl(2, 1)*v(73)
    v(64) = dcl(1, 1)*v(72)
    v(78) = dcl(3, 1)*ds(3, 3)+v(64)+v(65)
    v(63) = dcl(3, 1)/2d0
    v(62) = dcl(2, 1)/4d0
    v(61) = dcl(1, 1)/4d0
    v(60) = ds(1, 3)*v(63)
    v(59) = ds(1, 2)*v(62)
    v(58) = ds(1, 1)*v(61)
    v(77) = v(58)+v(59)+v(60)
    v(44) = ds(2, 1)*v(61)
    v(45) = ds(2, 2)*v(62)
    v(46) = ds(2, 3)*v(63)
    v(76) = v(44)+v(45)+v(46)
    v(52) = ds(2, 1)*v(69)
    v(53) = ds(2, 2)*v(70)
    v(54) = ds(2, 3)*v(71)
    v(79) = v(52)+v(53)+v(54)
    v(55) = dcl(1, 2)*v(72)
    v(56) = dcl(2, 2)*v(73)
    v(80) = dcl(3, 2)*ds(3, 3)+v(55)+v(56)
    kernel(1, 1) = d2c(3, 1, 1)*s(3)+d2c(1, 1, 1)*v(74)+d2c(2, 1, 1)*v(75)+dck(2, 1)*v(76)+dck(1, 1)*v(77)+dck(3, 1)*v(78)
    kernel(1, 2) = d2c(3, 1, 2)*s(3)+d2c(1, 1, 2)*v(74)+d2c(2, 1, 2)*v(75)+dck(2, 1)*v(79)+dck(3, 1)*v(80)+dck(1, 1)*v(81)
    kernel(2, 1) = d2c(3, 2, 1)*s(3)+d2c(1, 2, 1)*v(74)+d2c(2, 2, 1)*v(75)+dck(2, 2)*v(76)+dck(1, 2)*v(77)+dck(3, 2)*v(78)
    kernel(2, 2) = d2c(3, 2, 2)*s(3)+d2c(1, 2, 2)*v(74)+d2c(2, 2, 2)*v(75)+dck(2, 2)*v(79)+dck(3, 2)*v(80)+dck(1, 2)*v(81)
  END SUBROUTINE movingleastsquaresfbar2d
!> moving least squares 3d fbar kernel
  SUBROUTINE movingleastsquaresfbar(dck, dcl, d2c, s, ds, kernel)
    IMPLICIT NONE
    REAL(8) v(303), dck(6, 3), dcl(6, 3), d2c(6, 3, 3), s(6), ds(6, 6), kernel(3, 3)
    v(280) = s(3)/2d0
    v(279) = s(2)/2d0
    v(278) = s(1)/2d0
    v(277) = dcl(3, 3)/2d0
    v(276) = dcl(2, 3)/2d0
    v(275) = dcl(1, 3)/2d0
    v(274) = ds(4, 3)*v(277)
    v(273) = ds(4, 2)*v(276)
    v(272) = ds(4, 1)*v(275)
    v(298) = dcl(4, 3)*ds(4, 4)+dcl(5, 3)*ds(4, 5)+dcl(6, 3)*ds(4, 6)+v(272)+v(273)+v(274)
    v(271) = dcl(6, 3)/2d0
    v(270) = dcl(5, 3)/2d0
    v(269) = dcl(4, 3)/2d0
    v(268) = dcl(3, 3)/4d0
    v(267) = dcl(2, 3)/4d0
    v(266) = dcl(1, 3)/4d0
    v(265) = ds(1, 6)*v(271)
    v(264) = ds(1, 5)*v(270)
    v(263) = ds(1, 4)*v(269)
    v(262) = ds(1, 3)*v(268)
    v(261) = ds(1, 2)*v(267)
    v(260) = ds(1, 1)*v(266)
    v(297) = v(260)+v(261)+v(262)+v(263)+v(264)+v(265)
    v(259) = dcl(3, 2)/2d0
    v(258) = dcl(2, 2)/2d0
    v(257) = dcl(1, 2)/2d0
    v(256) = ds(4, 3)*v(259)
    v(255) = ds(4, 2)*v(258)
    v(254) = ds(4, 1)*v(257)
    v(292) = dcl(4, 2)*ds(4, 4)+dcl(5, 2)*ds(4, 5)+dcl(6, 2)*ds(4, 6)+v(254)+v(255)+v(256)
    v(253) = dcl(6, 2)/2d0
    v(252) = dcl(5, 2)/2d0
    v(251) = dcl(4, 2)/2d0
    v(250) = dcl(3, 2)/4d0
    v(249) = dcl(2, 2)/4d0
    v(248) = dcl(1, 2)/4d0
    v(247) = ds(1, 6)*v(253)
    v(246) = ds(1, 5)*v(252)
    v(245) = ds(1, 4)*v(251)
    v(244) = ds(1, 3)*v(250)
    v(243) = ds(1, 2)*v(249)
    v(242) = ds(1, 1)*v(248)
    v(291) = v(242)+v(243)+v(244)+v(245)+v(246)+v(247)
    v(241) = dcl(3, 1)/2d0
    v(240) = dcl(2, 1)/2d0
    v(239) = dcl(1, 1)/2d0
    v(238) = ds(4, 3)*v(241)
    v(237) = ds(4, 2)*v(240)
    v(236) = ds(4, 1)*v(239)
    v(286) = dcl(4, 1)*ds(4, 4)+dcl(5, 1)*ds(4, 5)+dcl(6, 1)*ds(4, 6)+v(236)+v(237)+v(238)
    v(235) = dcl(6, 1)/2d0
    v(234) = dcl(5, 1)/2d0
    v(233) = dcl(4, 1)/2d0
    v(232) = dcl(3, 1)/4d0
    v(231) = dcl(2, 1)/4d0
    v(230) = dcl(1, 1)/4d0
    v(229) = ds(1, 6)*v(235)
    v(228) = ds(1, 5)*v(234)
    v(227) = ds(1, 4)*v(233)
    v(226) = ds(1, 3)*v(232)
    v(225) = ds(1, 2)*v(231)
    v(224) = ds(1, 1)*v(230)
    v(285) = v(224)+v(225)+v(226)+v(227)+v(228)+v(229)
    v(148) = ds(2, 1)*v(230)
    v(149) = ds(2, 2)*v(231)
    v(150) = ds(2, 3)*v(232)
    v(151) = ds(2, 4)*v(233)
    v(152) = ds(2, 5)*v(234)
    v(153) = ds(2, 6)*v(235)
    v(281) = v(148)+v(149)+v(150)+v(151)+v(152)+v(153)
    v(154) = ds(3, 1)*v(230)
    v(155) = ds(3, 2)*v(231)
    v(156) = ds(3, 3)*v(232)
    v(157) = ds(3, 4)*v(233)
    v(158) = ds(3, 5)*v(234)
    v(159) = ds(3, 6)*v(235)
    v(282) = v(154)+v(155)+v(156)+v(157)+v(158)+v(159)
    v(163) = ds(5, 1)*v(239)
    v(164) = ds(5, 2)*v(240)
    v(165) = ds(5, 3)*v(241)
    v(283) = dcl(4, 1)*ds(5, 4)+dcl(5, 1)*ds(5, 5)+dcl(6, 1)*ds(5, 6)+v(163)+v(164)+v(165)
    v(166) = ds(6, 1)*v(239)
    v(167) = ds(6, 2)*v(240)
    v(168) = ds(6, 3)*v(241)
    v(284) = dcl(4, 1)*ds(6, 4)+dcl(5, 1)*ds(6, 5)+dcl(6, 1)*ds(6, 6)+v(166)+v(167)+v(168)
    v(175) = ds(2, 1)*v(248)
    v(176) = ds(2, 2)*v(249)
    v(177) = ds(2, 3)*v(250)
    v(178) = ds(2, 4)*v(251)
    v(179) = ds(2, 5)*v(252)
    v(180) = ds(2, 6)*v(253)
    v(287) = v(175)+v(176)+v(177)+v(178)+v(179)+v(180)
    v(181) = ds(3, 1)*v(248)
    v(182) = ds(3, 2)*v(249)
    v(183) = ds(3, 3)*v(250)
    v(184) = ds(3, 4)*v(251)
    v(185) = ds(3, 5)*v(252)
    v(186) = ds(3, 6)*v(253)
    v(288) = v(181)+v(182)+v(183)+v(184)+v(185)+v(186)
    v(190) = ds(5, 1)*v(257)
    v(191) = ds(5, 2)*v(258)
    v(192) = ds(5, 3)*v(259)
    v(289) = dcl(4, 2)*ds(5, 4)+dcl(5, 2)*ds(5, 5)+dcl(6, 2)*ds(5, 6)+v(190)+v(191)+v(192)
    v(193) = ds(6, 1)*v(257)
    v(194) = ds(6, 2)*v(258)
    v(195) = ds(6, 3)*v(259)
    v(290) = dcl(4, 2)*ds(6, 4)+dcl(5, 2)*ds(6, 5)+dcl(6, 2)*ds(6, 6)+v(193)+v(194)+v(195)
    v(202) = ds(2, 1)*v(266)
    v(203) = ds(2, 2)*v(267)
    v(204) = ds(2, 3)*v(268)
    v(205) = ds(2, 4)*v(269)
    v(206) = ds(2, 5)*v(270)
    v(207) = ds(2, 6)*v(271)
    v(293) = v(202)+v(203)+v(204)+v(205)+v(206)+v(207)
    v(208) = ds(3, 1)*v(266)
    v(209) = ds(3, 2)*v(267)
    v(210) = ds(3, 3)*v(268)
    v(211) = ds(3, 4)*v(269)
    v(212) = ds(3, 5)*v(270)
    v(213) = ds(3, 6)*v(271)
    v(294) = v(208)+v(209)+v(210)+v(211)+v(212)+v(213)
    v(217) = ds(5, 1)*v(275)
    v(218) = ds(5, 2)*v(276)
    v(219) = ds(5, 3)*v(277)
    v(295) = dcl(4, 3)*ds(5, 4)+dcl(5, 3)*ds(5, 5)+dcl(6, 3)*ds(5, 6)+v(217)+v(218)+v(219)
    v(220) = ds(6, 1)*v(275)
    v(221) = ds(6, 2)*v(276)
    v(222) = ds(6, 3)*v(277)
    v(296) = dcl(4, 3)*ds(6, 4)+dcl(5, 3)*ds(6, 5)+dcl(6, 3)*ds(6, 6)+v(220)+v(221)+v(222)
    kernel(1, 1) = d2c(4, 1, 1)*s(4)+d2c(5, 1, 1)*s(5)+d2c(6, 1, 1)*s(6)+d2c(1, 1, 1)*v(278)+d2c(2, 1, 1)*v(279)+d2c(3, 1, 1)*v(280)+dck(2&
    &, 1)*v(281)+dck(3, 1)*v(282)+dck(5, 1)*v(283)+dck(6, 1)*v(284)+dck(1, 1)*v(285)+dck(4, 1)*v(286)
    kernel(1, 2) = d2c(4, 1, 2)*s(4)+d2c(5, 1, 2)*s(5)+d2c(6, 1, 2)*s(6)+d2c(1, 1, 2)*v(278)+d2c(2, 1, 2)*v(279)+d2c(3, 1, 2)*v(280)+dck(2&
    &, 1)*v(287)+dck(3, 1)*v(288)+dck(5, 1)*v(289)+dck(6, 1)*v(290)+dck(1, 1)*v(291)+dck(4, 1)*v(292)
    kernel(1, 3) = d2c(4, 1, 3)*s(4)+d2c(5, 1, 3)*s(5)+d2c(6, 1, 3)*s(6)+d2c(1, 1, 3)*v(278)+d2c(2, 1, 3)*v(279)+d2c(3, 1, 3)*v(280)+dck(2&
    &, 1)*v(293)+dck(3, 1)*v(294)+dck(5, 1)*v(295)+dck(6, 1)*v(296)+dck(1, 1)*v(297)+dck(4, 1)*v(298)
    kernel(2, 1) = d2c(4, 2, 1)*s(4)+d2c(5, 2, 1)*s(5)+d2c(6, 2, 1)*s(6)+d2c(1, 2, 1)*v(278)+d2c(2, 2, 1)*v(279)+d2c(3, 2, 1)*v(280)+dck(2&
    &, 2)*v(281)+dck(3, 2)*v(282)+dck(5, 2)*v(283)+dck(6, 2)*v(284)+dck(1, 2)*v(285)+dck(4, 2)*v(286)
    kernel(2, 2) = d2c(4, 2, 2)*s(4)+d2c(5, 2, 2)*s(5)+d2c(6, 2, 2)*s(6)+d2c(1, 2, 2)*v(278)+d2c(2, 2, 2)*v(279)+d2c(3, 2, 2)*v(280)+dck(2&
    &, 2)*v(287)+dck(3, 2)*v(288)+dck(5, 2)*v(289)+dck(6, 2)*v(290)+dck(1, 2)*v(291)+dck(4, 2)*v(292)
    kernel(2, 3) = d2c(4, 2, 3)*s(4)+d2c(5, 2, 3)*s(5)+d2c(6, 2, 3)*s(6)+d2c(1, 2, 3)*v(278)+d2c(2, 2, 3)*v(279)+d2c(3, 2, 3)*v(280)+dck(2&
    &, 2)*v(293)+dck(3, 2)*v(294)+dck(5, 2)*v(295)+dck(6, 2)*v(296)+dck(1, 2)*v(297)+dck(4, 2)*v(298)
    kernel(3, 1) = d2c(4, 3, 1)*s(4)+d2c(5, 3, 1)*s(5)+d2c(6, 3, 1)*s(6)+d2c(1, 3, 1)*v(278)+d2c(2, 3, 1)*v(279)+d2c(3, 3, 1)*v(280)+dck(2&
    &, 3)*v(281)+dck(3, 3)*v(282)+dck(5, 3)*v(283)+dck(6, 3)*v(284)+dck(1, 3)*v(285)+dck(4, 3)*v(286)
    kernel(3, 2) = d2c(4, 3, 2)*s(4)+d2c(5, 3, 2)*s(5)+d2c(6, 3, 2)*s(6)+d2c(1, 3, 2)*v(278)+d2c(2, 3, 2)*v(279)+d2c(3, 3, 2)*v(280)+dck(2&
    &, 3)*v(287)+dck(3, 3)*v(288)+dck(5, 3)*v(289)+dck(6, 3)*v(290)+dck(1, 3)*v(291)+dck(4, 3)*v(292)
    kernel(3, 3) = d2c(4, 3, 3)*s(4)+d2c(5, 3, 3)*s(5)+d2c(6, 3, 3)*s(6)+d2c(1, 3, 3)*v(278)+d2c(2, 3, 3)*v(279)+d2c(3, 3, 3)*v(280)+dck(2&
    &, 3)*v(293)+dck(3, 3)*v(294)+dck(5, 3)*v(295)+dck(6, 3)*v(296)+dck(1, 3)*v(297)+dck(4, 3)*v(298)
  END SUBROUTINE movingleastsquaresfbar
!> moving least squares 2d fbar nucleus
  SUBROUTINE movingleastsquaresfbarforce2d(dck, s, nucleus)
    IMPLICIT NONE
    REAL(8) v(28), dck(3, 2), s(3), nucleus(2)
    v(23) = s(2)/2d0
    v(22) = s(1)/2d0
    nucleus(1) = dck(3, 1)*s(3)+dck(1, 1)*v(22)+dck(2, 1)*v(23)
    nucleus(2) = dck(3, 2)*s(3)+dck(1, 2)*v(22)+dck(2, 2)*v(23)
  END SUBROUTINE movingleastsquaresfbarforce2d
!> moving least squares 3d fbar kernel
  SUBROUTINE movingleastsquaresfbarforce(dck, s, nucleus)
    IMPLICIT NONE
    REAL(8) v(54), dck(6, 3), s(6), nucleus(3)
    v(49) = s(3)/2d0
    v(48) = s(2)/2d0
    v(47) = s(1)/2d0
    nucleus(1) = dck(4, 1)*s(4)+dck(5, 1)*s(5)+dck(6, 1)*s(6)+dck(1, 1)*v(47)+dck(2, 1)*v(48)+dck(3, 1)*v(49)
    nucleus(2) = dck(4, 2)*s(4)+dck(5, 2)*s(5)+dck(6, 2)*s(6)+dck(1, 2)*v(47)+dck(2, 2)*v(48)+dck(3, 2)*v(49)
    nucleus(3) = dck(4, 3)*s(4)+dck(5, 3)*s(5)+dck(6, 3)*s(6)+dck(1, 3)*v(47)+dck(2, 3)*v(48)+dck(3, 3)*v(49)
  END SUBROUTINE movingleastsquaresfbarforce
!> performs a triangular solve for the moving least squares
  SUBROUTINE movingleastsquaresgenmatrixtriangularsolve(upper, nrhs, n, r, b, x)
    IMPLICIT REAL(8) (a-h, o-z)
    LOGICAL::upper
    INTEGER::n
    REAL(8), DIMENSION(n, n)::r
    REAL(8), DIMENSION(n, nrhs)::b, x
    IF (upper) THEN
!-------------------------------------------
!*** solve r.x=b where r is upper triangular
!-------------------------------------------
      DO ir = 1, nrhs
        DO id = n, 1, -1
          x(id, ir) = b(id, ir)
          DO jd = id+1, n
            x(id, ir) = x(id, ir)-r(id, jd)*x(jd, ir)
          END DO
          x(id, ir) = x(id, ir)/r(id, id)
        END DO
      END DO
    ELSE
!---------------------------------------------
!*** solve r^t.x=b where r is upper triangular
!---------------------------------------------
      DO ir = 1, nrhs
        DO id = 1, n
          x(id, ir) = b(id, ir)
          DO jd = 1, id-1
            x(id, ir) = x(id, ir)-r(jd, id)*x(jd, ir)
          END DO
          x(id, ir) = x(id, ir)/r(id, id)
        END DO
      END DO
    END IF
  END SUBROUTINE movingleastsquaresgenmatrixtriangularsolve
!> moving least squares gets nodes
!> n<0 makes ranking
  SUBROUTINE movingleastsquaresgetsnnodes(rmax, x, ndi, ntot, xtot, n, listn)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8)::rmax
    REAL(8), DIMENSION(ndi, ntot)::xtot
    REAL(8), DIMENSION(ndi)::x
    REAL(8), DIMENSION(ntot)::ranking
    INTEGER, DIMENSION(ntot)::per
    INTEGER::ndi
    INTEGER, DIMENSION(:), ALLOCATABLE::listn
    IF (n .LE. 0) THEN
      CALL movingleastsquaresgetsnodes(rmax, x, ndi, ntot, xtot, n, listn)
    ELSE
      DO ino = 1, ntot
        ranking(ino) = vectnorm2(ndi, x(1:ndi)-xtot(1:ndi, ino))
      END DO
      CALL vectsortpermutation(ntot, ranking, per)
      ALLOCATE (listn(n))
      DO i = 1, n
        listn(i) = per(i)
      END DO
    END IF
  END SUBROUTINE movingleastsquaresgetsnnodes
!> moving least squares gets nodes
  SUBROUTINE movingleastsquaresgetsnodes(rmax, x, ndi, ntot, xtot, n, listn)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8)::rmax
    REAL(8), DIMENSION(ndi, ntot)::xtot
    REAL(8), DIMENSION(ndi)::x
    INTEGER::ndi
    INTEGER, DIMENSION(:), ALLOCATABLE::listn
    n = 0
    DO itot = 1, ntot
      IF (vectnorm2(ndi, xtot(1:ndi, itot)-x(1:ndi)) .LE. rmax) THEN
        n = n+1
      END IF
    END DO
    ALLOCATE (listn(n))
!--------------------------------------
!*** now inserts the nodes in the list
!--------------------------------------
    n = 0
    DO itot = 1, ntot
      IF (vectnorm2(ndi, xtot(1:ndi, itot)-x(1:ndi)) .LE. rmax) THEN
        n = n+1
        listn(n) = itot
      END IF
    END DO
  END SUBROUTINE movingleastsquaresgetsnodes
!> moving least squares qr factorization naive
  SUBROUTINE movingleastsquaresnaiveqr(n, m, at, r)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::n, m
    REAL(8), DIMENSION(n)::temp
    REAL(8), DIMENSION(n, m)::e
    REAL(8), DIMENSION(n, m)::a
    REAL(8), DIMENSION(m, m)::r
    REAL(8), DIMENSION(n, m)::at
!-------------------
!*** sets r to zero
!-------------------
    r = 0.0d00
    DO im = 1, m
      e(1:n, im) = at(1:n, im)
      DO jm = 1, im-1
        CALL movingleastsquaresprojqr(n, e(1:n, jm), at(1:n, im), temp(1:n))
        e(1:n, im) = e(1:n, im)-temp
      END DO
      CALL vectnormalize(n, e(1:n, im))
    END DO
    DO im = 1, m
      a(1:n, im) = 0.0d00
      DO jm = 1, im
        a(1:n, im) = a(1:n, im)+vectdot(n, e(1:n, jm), at(1:n, im))*e(1:n, jm)
      END DO
    END DO
    DO im = 1, m
      DO jm = im, m
        r(im, jm) = vectdot(n, e(1:n, im), a(1:n, jm))
      END DO
    END DO
  END SUBROUTINE movingleastsquaresnaiveqr
!> moving least squares cubic basis
  SUBROUTINE movingleastsquarespolyncubbase(ndi, dtemp, x, xbar, polyn, dpolyn, dpolyn2, m)
    IMPLICIT REAL(8) (a-h, o-z)
!*** check this parameter for updates (mpol)
    REAL(8)::d, dtemp
    REAL(8), DIMENSION(ndi)::x, xbar
    REAL(8), DIMENSION(*)::polyn
    REAL(8), DIMENSION(ndi, *)::dpolyn
    REAL(8), DIMENSION(ndi, ndi, *)::dpolyn2
    d = dtemp
    SELECT CASE (ndi)
    CASE (1)
      m = 4
      CALL movingleastsquarespolynquad1d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE (2)
      m = 10
      CALL movingleastsquarespolynquad2d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE (3)
      m = 20
      CALL movingleastsquarespolynquad3d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE default
      STOP "case not found in movingleastsquarespolyncubbase"
    END SELECT
  END SUBROUTINE movingleastsquarespolyncubbase
!> moving least squares linear basis
  SUBROUTINE movingleastsquarespolynlinbase(ndi, dtemp, x, xbar, polyn, dpolyn, dpolyn2, m)
    IMPLICIT REAL(8) (a-h, o-z)
!*** check this parameter for updates (mpol)
    REAL(8)::d, dtemp
    REAL(8), DIMENSION(ndi)::x, xbar
    REAL(8), DIMENSION(*)::polyn
    REAL(8), DIMENSION(ndi, *)::dpolyn
    REAL(8), DIMENSION(ndi, ndi, *)::dpolyn2
    d = dtemp
    SELECT CASE (ndi)
    CASE (1)
      m = 2
      CALL movingleastsquarespolynquad1d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE (2)
      m = 3
      CALL movingleastsquarespolynquad2d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE (3)
      m = 4
      CALL movingleastsquarespolynquad3d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE default
      STOP "case not found in movingleastsquarespolynlinbase"
    END SELECT
  END SUBROUTINE movingleastsquarespolynlinbase
!> moving least squares quadratic basis
  SUBROUTINE movingleastsquarespolynqbase(ndi, dtemp, x, xbar, polyn, dpolyn, dpolyn2, m)
    IMPLICIT REAL(8) (a-h, o-z)
!*** check this parameter for updates (mpol)
    REAL(8)::d, dtemp
    REAL(8), DIMENSION(ndi)::x, xbar
    REAL(8), DIMENSION(*)::polyn
    REAL(8), DIMENSION(ndi, *)::dpolyn
    REAL(8), DIMENSION(ndi, ndi, *)::dpolyn2
    d = dtemp
    SELECT CASE (ndi)
    CASE (1)
      m = 3
      CALL movingleastsquarespolynquad1d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE (2)
      m = 6
      CALL movingleastsquarespolynquad2d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE (3)
      m = 10
      CALL movingleastsquarespolynquad3d(d, x, xbar, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m))
    CASE default
      STOP "case not found in movingleastsquarespolynqbase"
    END SELECT
  END SUBROUTINE movingleastsquarespolynqbase
!> moving least squares 1d quadratic
  SUBROUTINE movingleastsquarespolynquad1d(d, x, xbar, polyn, dpolyn, dpolyn2)
    IMPLICIT NONE
    REAL(8) v(40), d, x(1), xbar(1), polyn(*), dpolyn(1, *), dpolyn2(1, 1, *)
    v(34) = 1d0/d
    v(33) = x(1)-xbar(1)
    v(32) = 1d0/d**2
    v(35) = 2d0*v(32)
    v(31) = 1d0/d**3
    v(23) = (v(33)*v(33))
    polyn(1) = 1d0
    polyn(2) = v(33)*v(34)
    polyn(3) = v(23)*v(32)
    polyn(4) = v(31)*v(33)**3
    dpolyn(1, 1) = 0d0
    dpolyn(1, 2) = v(34)
    dpolyn(1, 3) = v(33)*v(35)
    dpolyn(1, 4) = 3d0*v(23)*v(31)
    dpolyn2(1, 1, 1) = 0d0
    dpolyn2(1, 1, 2) = 0d0
    dpolyn2(1, 1, 3) = v(35)
    dpolyn2(1, 1, 4) = 6d0*v(31)*v(33)
  END SUBROUTINE movingleastsquarespolynquad1d
!> moving least squares 2d quadratic
  SUBROUTINE movingleastsquarespolynquad2d(d, x, xbar, polyn, dpolyn, dpolyn2)
    IMPLICIT NONE
    REAL(8) v(124), d, x(2), xbar(2), polyn(*), dpolyn(2, *), dpolyn2(2, 2, *)
    v(115) = 1d0/d
    v(113) = x(2)-xbar(2)
    v(112) = x(1)-xbar(1)
    v(111) = 1d0/d**2
    v(116) = v(111)*v(113)
    v(110) = 1d0/d**3
    v(119) = 3d0*v(110)
    v(114) = v(110)*v(113)
    v(91) = 2d0*v(112)
    v(87) = (v(112)*v(112))
    v(117) = v(110)*v(87)
    v(97) = 2d0*v(113)
    v(89) = (v(113)*v(113))
    v(118) = v(110)*v(89)
    v(95) = v(114)*v(91)
    v(102) = 2d0*v(111)
    v(104) = 2d0*v(114)
    v(105) = v(110)*v(91)
    polyn(1) = 1d0
    polyn(2) = v(112)*v(115)
    polyn(3) = v(113)*v(115)
    polyn(4) = v(111)*v(87)
    polyn(5) = v(111)*v(89)
    polyn(6) = v(112)*v(116)
    polyn(7) = v(110)*v(112)**3
    polyn(8) = v(110)*v(113)**3
    polyn(9) = v(114)*v(87)
    polyn(10) = v(112)*v(118)
    dpolyn(1, 1) = 0d0
    dpolyn(1, 2) = v(115)
    dpolyn(1, 3) = 0d0
    dpolyn(1, 4) = v(111)*v(91)
    dpolyn(1, 5) = 0d0
    dpolyn(1, 6) = v(116)
    dpolyn(1, 7) = 3d0*v(117)
    dpolyn(1, 8) = 0d0
    dpolyn(1, 9) = v(95)
    dpolyn(1, 10) = v(118)
    dpolyn(2, 1) = 0d0
    dpolyn(2, 2) = 0d0
    dpolyn(2, 3) = v(115)
    dpolyn(2, 4) = 0d0
    dpolyn(2, 5) = v(111)*v(97)
    dpolyn(2, 6) = v(111)*v(112)
    dpolyn(2, 7) = 0d0
    dpolyn(2, 8) = 3d0*v(118)
    dpolyn(2, 9) = v(117)
    dpolyn(2, 10) = v(95)
    dpolyn2(1, 1, 1) = 0d0
    dpolyn2(1, 1, 2) = 0d0
    dpolyn2(1, 1, 3) = 0d0
    dpolyn2(1, 1, 4) = v(102)
    dpolyn2(1, 1, 5) = 0d0
    dpolyn2(1, 1, 6) = 0d0
    dpolyn2(1, 1, 7) = v(119)*v(91)
    dpolyn2(1, 1, 8) = 0d0
    dpolyn2(1, 1, 9) = v(104)
    dpolyn2(1, 1, 10) = 0d0
    dpolyn2(1, 2, 1) = 0d0
    dpolyn2(1, 2, 2) = 0d0
    dpolyn2(1, 2, 3) = 0d0
    dpolyn2(1, 2, 4) = 0d0
    dpolyn2(1, 2, 5) = 0d0
    dpolyn2(1, 2, 6) = v(111)
    dpolyn2(1, 2, 7) = 0d0
    dpolyn2(1, 2, 8) = 0d0
    dpolyn2(1, 2, 9) = v(105)
    dpolyn2(1, 2, 10) = v(104)
    dpolyn2(2, 1, 1) = 0d0
    dpolyn2(2, 1, 2) = 0d0
    dpolyn2(2, 1, 3) = 0d0
    dpolyn2(2, 1, 4) = 0d0
    dpolyn2(2, 1, 5) = 0d0
    dpolyn2(2, 1, 6) = v(111)
    dpolyn2(2, 1, 7) = 0d0
    dpolyn2(2, 1, 8) = 0d0
    dpolyn2(2, 1, 9) = v(105)
    dpolyn2(2, 1, 10) = v(104)
    dpolyn2(2, 2, 1) = 0d0
    dpolyn2(2, 2, 2) = 0d0
    dpolyn2(2, 2, 3) = 0d0
    dpolyn2(2, 2, 4) = 0d0
    dpolyn2(2, 2, 5) = v(102)
    dpolyn2(2, 2, 6) = 0d0
    dpolyn2(2, 2, 7) = 0d0
    dpolyn2(2, 2, 8) = v(119)*v(97)
    dpolyn2(2, 2, 9) = 0d0
    dpolyn2(2, 2, 10) = v(105)
  END SUBROUTINE movingleastsquarespolynquad2d
!> moving least squares 3d quadratic
  SUBROUTINE movingleastsquarespolynquad3d(d, x, xbar, polyn, dpolyn, dpolyn2)
    IMPLICIT NONE
    REAL(8) v(340), d, x(3), xbar(3), polyn(*), dpolyn(3, *), dpolyn2(3, 3, *)
    v(334) = 1d0/d
    v(333) = x(3)-xbar(3)
    v(332) = x(2)-xbar(2)
    v(331) = x(1)-xbar(1)
    v(330) = 1d0/d**2
    v(329) = 1d0/d**3
    v(335) = 3d0*v(329)
    v(295) = 2d0*v(331)
    v(286) = (v(331)*v(331))
    v(316) = v(329)*v(332)
    v(305) = 2d0*v(332)
    v(289) = (v(332)*v(332))
    v(313) = 2d0*v(333)
    v(309) = v(329)*v(333)
    v(300) = v(309)*v(332)
    v(292) = (v(333)*v(333))
    v(297) = v(330)*v(332)
    v(298) = v(330)*v(333)
    v(301) = v(295)*v(316)
    v(302) = v(295)*v(309)
    v(303) = v(289)*v(329)
    v(304) = v(292)*v(329)
    v(307) = v(330)*v(331)
    v(311) = v(286)*v(329)
    v(312) = v(305)*v(309)
    v(318) = 2d0*v(330)
    v(320) = v(305)*v(329)
    v(321) = 2d0*v(309)
    v(322) = v(295)*v(329)
    v(324) = v(329)*v(331)
    polyn(1) = 1d0
    polyn(2) = v(331)*v(334)
    polyn(3) = v(332)*v(334)
    polyn(4) = v(333)*v(334)
    polyn(5) = v(286)*v(330)
    polyn(6) = v(289)*v(330)
    polyn(7) = v(292)*v(330)
    polyn(8) = v(307)*v(332)
    polyn(9) = v(307)*v(333)
    polyn(10) = v(298)*v(332)
    polyn(11) = v(329)*v(331)**3
    polyn(12) = v(329)*v(332)**3
    polyn(13) = v(329)*v(333)**3
    polyn(14) = v(300)*v(331)
    polyn(15) = v(311)*v(332)
    polyn(16) = v(311)*v(333)
    polyn(17) = v(289)*v(324)
    polyn(18) = v(303)*v(333)
    polyn(19) = v(292)*v(324)
    polyn(20) = v(304)*v(332)
    dpolyn(1, 1) = 0d0
    dpolyn(1, 2) = v(334)
    dpolyn(1, 3) = 0d0
    dpolyn(1, 4) = 0d0
    dpolyn(1, 5) = v(295)*v(330)
    dpolyn(1, 6) = 0d0
    dpolyn(1, 7) = 0d0
    dpolyn(1, 8) = v(297)
    dpolyn(1, 9) = v(298)
    dpolyn(1, 10) = 0d0
    dpolyn(1, 11) = 3d0*v(311)
    dpolyn(1, 12) = 0d0
    dpolyn(1, 13) = 0d0
    dpolyn(1, 14) = v(300)
    dpolyn(1, 15) = v(301)
    dpolyn(1, 16) = v(302)
    dpolyn(1, 17) = v(303)
    dpolyn(1, 18) = 0d0
    dpolyn(1, 19) = v(304)
    dpolyn(1, 20) = 0d0
    dpolyn(2, 1) = 0d0
    dpolyn(2, 2) = 0d0
    dpolyn(2, 3) = v(334)
    dpolyn(2, 4) = 0d0
    dpolyn(2, 5) = 0d0
    dpolyn(2, 6) = v(305)*v(330)
    dpolyn(2, 7) = 0d0
    dpolyn(2, 8) = v(307)
    dpolyn(2, 9) = 0d0
    dpolyn(2, 10) = v(298)
    dpolyn(2, 11) = 0d0
    dpolyn(2, 12) = 3d0*v(303)
    dpolyn(2, 13) = 0d0
    dpolyn(2, 14) = v(309)*v(331)
    dpolyn(2, 15) = v(311)
    dpolyn(2, 16) = 0d0
    dpolyn(2, 17) = v(301)
    dpolyn(2, 18) = v(312)
    dpolyn(2, 19) = 0d0
    dpolyn(2, 20) = v(304)
    dpolyn(3, 1) = 0d0
    dpolyn(3, 2) = 0d0
    dpolyn(3, 3) = 0d0
    dpolyn(3, 4) = v(334)
    dpolyn(3, 5) = 0d0
    dpolyn(3, 6) = 0d0
    dpolyn(3, 7) = v(313)*v(330)
    dpolyn(3, 8) = 0d0
    dpolyn(3, 9) = v(307)
    dpolyn(3, 10) = v(297)
    dpolyn(3, 11) = 0d0
    dpolyn(3, 12) = 0d0
    dpolyn(3, 13) = 3d0*v(304)
    dpolyn(3, 14) = v(316)*v(331)
    dpolyn(3, 15) = 0d0
    dpolyn(3, 16) = v(311)
    dpolyn(3, 17) = 0d0
    dpolyn(3, 18) = v(303)
    dpolyn(3, 19) = v(302)
    dpolyn(3, 20) = v(312)
    dpolyn2(1, 1, 1) = 0d0
    dpolyn2(1, 1, 2) = 0d0
    dpolyn2(1, 1, 3) = 0d0
    dpolyn2(1, 1, 4) = 0d0
    dpolyn2(1, 1, 5) = v(318)
    dpolyn2(1, 1, 6) = 0d0
    dpolyn2(1, 1, 7) = 0d0
    dpolyn2(1, 1, 8) = 0d0
    dpolyn2(1, 1, 9) = 0d0
    dpolyn2(1, 1, 10) = 0d0
    dpolyn2(1, 1, 11) = v(295)*v(335)
    dpolyn2(1, 1, 12) = 0d0
    dpolyn2(1, 1, 13) = 0d0
    dpolyn2(1, 1, 14) = 0d0
    dpolyn2(1, 1, 15) = v(320)
    dpolyn2(1, 1, 16) = v(321)
    dpolyn2(1, 1, 17) = 0d0
    dpolyn2(1, 1, 18) = 0d0
    dpolyn2(1, 1, 19) = 0d0
    dpolyn2(1, 1, 20) = 0d0
    dpolyn2(1, 2, 1) = 0d0
    dpolyn2(1, 2, 2) = 0d0
    dpolyn2(1, 2, 3) = 0d0
    dpolyn2(1, 2, 4) = 0d0
    dpolyn2(1, 2, 5) = 0d0
    dpolyn2(1, 2, 6) = 0d0
    dpolyn2(1, 2, 7) = 0d0
    dpolyn2(1, 2, 8) = v(330)
    dpolyn2(1, 2, 9) = 0d0
    dpolyn2(1, 2, 10) = 0d0
    dpolyn2(1, 2, 11) = 0d0
    dpolyn2(1, 2, 12) = 0d0
    dpolyn2(1, 2, 13) = 0d0
    dpolyn2(1, 2, 14) = v(309)
    dpolyn2(1, 2, 15) = v(322)
    dpolyn2(1, 2, 16) = 0d0
    dpolyn2(1, 2, 17) = v(320)
    dpolyn2(1, 2, 18) = 0d0
    dpolyn2(1, 2, 19) = 0d0
    dpolyn2(1, 2, 20) = 0d0
    dpolyn2(1, 3, 1) = 0d0
    dpolyn2(1, 3, 2) = 0d0
    dpolyn2(1, 3, 3) = 0d0
    dpolyn2(1, 3, 4) = 0d0
    dpolyn2(1, 3, 5) = 0d0
    dpolyn2(1, 3, 6) = 0d0
    dpolyn2(1, 3, 7) = 0d0
    dpolyn2(1, 3, 8) = 0d0
    dpolyn2(1, 3, 9) = v(330)
    dpolyn2(1, 3, 10) = 0d0
    dpolyn2(1, 3, 11) = 0d0
    dpolyn2(1, 3, 12) = 0d0
    dpolyn2(1, 3, 13) = 0d0
    dpolyn2(1, 3, 14) = v(316)
    dpolyn2(1, 3, 15) = 0d0
    dpolyn2(1, 3, 16) = v(322)
    dpolyn2(1, 3, 17) = 0d0
    dpolyn2(1, 3, 18) = 0d0
    dpolyn2(1, 3, 19) = v(321)
    dpolyn2(1, 3, 20) = 0d0
    dpolyn2(2, 1, 1) = 0d0
    dpolyn2(2, 1, 2) = 0d0
    dpolyn2(2, 1, 3) = 0d0
    dpolyn2(2, 1, 4) = 0d0
    dpolyn2(2, 1, 5) = 0d0
    dpolyn2(2, 1, 6) = 0d0
    dpolyn2(2, 1, 7) = 0d0
    dpolyn2(2, 1, 8) = v(330)
    dpolyn2(2, 1, 9) = 0d0
    dpolyn2(2, 1, 10) = 0d0
    dpolyn2(2, 1, 11) = 0d0
    dpolyn2(2, 1, 12) = 0d0
    dpolyn2(2, 1, 13) = 0d0
    dpolyn2(2, 1, 14) = v(309)
    dpolyn2(2, 1, 15) = v(322)
    dpolyn2(2, 1, 16) = 0d0
    dpolyn2(2, 1, 17) = v(320)
    dpolyn2(2, 1, 18) = 0d0
    dpolyn2(2, 1, 19) = 0d0
    dpolyn2(2, 1, 20) = 0d0
    dpolyn2(2, 2, 1) = 0d0
    dpolyn2(2, 2, 2) = 0d0
    dpolyn2(2, 2, 3) = 0d0
    dpolyn2(2, 2, 4) = 0d0
    dpolyn2(2, 2, 5) = 0d0
    dpolyn2(2, 2, 6) = v(318)
    dpolyn2(2, 2, 7) = 0d0
    dpolyn2(2, 2, 8) = 0d0
    dpolyn2(2, 2, 9) = 0d0
    dpolyn2(2, 2, 10) = 0d0
    dpolyn2(2, 2, 11) = 0d0
    dpolyn2(2, 2, 12) = 3d0*v(320)
    dpolyn2(2, 2, 13) = 0d0
    dpolyn2(2, 2, 14) = 0d0
    dpolyn2(2, 2, 15) = 0d0
    dpolyn2(2, 2, 16) = 0d0
    dpolyn2(2, 2, 17) = v(322)
    dpolyn2(2, 2, 18) = v(321)
    dpolyn2(2, 2, 19) = 0d0
    dpolyn2(2, 2, 20) = 0d0
    dpolyn2(2, 3, 1) = 0d0
    dpolyn2(2, 3, 2) = 0d0
    dpolyn2(2, 3, 3) = 0d0
    dpolyn2(2, 3, 4) = 0d0
    dpolyn2(2, 3, 5) = 0d0
    dpolyn2(2, 3, 6) = 0d0
    dpolyn2(2, 3, 7) = 0d0
    dpolyn2(2, 3, 8) = 0d0
    dpolyn2(2, 3, 9) = 0d0
    dpolyn2(2, 3, 10) = v(330)
    dpolyn2(2, 3, 11) = 0d0
    dpolyn2(2, 3, 12) = 0d0
    dpolyn2(2, 3, 13) = 0d0
    dpolyn2(2, 3, 14) = v(324)
    dpolyn2(2, 3, 15) = 0d0
    dpolyn2(2, 3, 16) = 0d0
    dpolyn2(2, 3, 17) = 0d0
    dpolyn2(2, 3, 18) = v(320)
    dpolyn2(2, 3, 19) = 0d0
    dpolyn2(2, 3, 20) = v(321)
    dpolyn2(3, 1, 1) = 0d0
    dpolyn2(3, 1, 2) = 0d0
    dpolyn2(3, 1, 3) = 0d0
    dpolyn2(3, 1, 4) = 0d0
    dpolyn2(3, 1, 5) = 0d0
    dpolyn2(3, 1, 6) = 0d0
    dpolyn2(3, 1, 7) = 0d0
    dpolyn2(3, 1, 8) = 0d0
    dpolyn2(3, 1, 9) = v(330)
    dpolyn2(3, 1, 10) = 0d0
    dpolyn2(3, 1, 11) = 0d0
    dpolyn2(3, 1, 12) = 0d0
    dpolyn2(3, 1, 13) = 0d0
    dpolyn2(3, 1, 14) = v(316)
    dpolyn2(3, 1, 15) = 0d0
    dpolyn2(3, 1, 16) = v(322)
    dpolyn2(3, 1, 17) = 0d0
    dpolyn2(3, 1, 18) = 0d0
    dpolyn2(3, 1, 19) = v(321)
    dpolyn2(3, 1, 20) = 0d0
    dpolyn2(3, 2, 1) = 0d0
    dpolyn2(3, 2, 2) = 0d0
    dpolyn2(3, 2, 3) = 0d0
    dpolyn2(3, 2, 4) = 0d0
    dpolyn2(3, 2, 5) = 0d0
    dpolyn2(3, 2, 6) = 0d0
    dpolyn2(3, 2, 7) = 0d0
    dpolyn2(3, 2, 8) = 0d0
    dpolyn2(3, 2, 9) = 0d0
    dpolyn2(3, 2, 10) = v(330)
    dpolyn2(3, 2, 11) = 0d0
    dpolyn2(3, 2, 12) = 0d0
    dpolyn2(3, 2, 13) = 0d0
    dpolyn2(3, 2, 14) = v(324)
    dpolyn2(3, 2, 15) = 0d0
    dpolyn2(3, 2, 16) = 0d0
    dpolyn2(3, 2, 17) = 0d0
    dpolyn2(3, 2, 18) = v(320)
    dpolyn2(3, 2, 19) = 0d0
    dpolyn2(3, 2, 20) = v(321)
    dpolyn2(3, 3, 1) = 0d0
    dpolyn2(3, 3, 2) = 0d0
    dpolyn2(3, 3, 3) = 0d0
    dpolyn2(3, 3, 4) = 0d0
    dpolyn2(3, 3, 5) = 0d0
    dpolyn2(3, 3, 6) = 0d0
    dpolyn2(3, 3, 7) = v(318)
    dpolyn2(3, 3, 8) = 0d0
    dpolyn2(3, 3, 9) = 0d0
    dpolyn2(3, 3, 10) = 0d0
    dpolyn2(3, 3, 11) = 0d0
    dpolyn2(3, 3, 12) = 0d0
    dpolyn2(3, 3, 13) = v(313)*v(335)
    dpolyn2(3, 3, 14) = 0d0
    dpolyn2(3, 3, 15) = 0d0
    dpolyn2(3, 3, 16) = 0d0
    dpolyn2(3, 3, 17) = 0d0
    dpolyn2(3, 3, 18) = 0d0
    dpolyn2(3, 3, 19) = v(322)
    dpolyn2(3, 3, 20) = v(320)
  END SUBROUTINE movingleastsquarespolynquad3d
!> moving least squares projection qr
  SUBROUTINE movingleastsquaresprojqr(n, u, a, ua)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::n
    REAL(8), DIMENSION(n)::u, a, ua
    ua = u*vectdot(n, u, a)/vectdot(n, u, u)
  END SUBROUTINE movingleastsquaresprojqr
!> moving least squares shape functions and derivatives
  SUBROUTINE movingleastsquaressfder(ndi, n, m, polyn, dpolyn, dpolyn2, u2, ff, dff, dff2)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::m, n
    REAL(8), DIMENSION(m)::polyn
    REAL(8), DIMENSION(ndi, m)::dpolyn
    REAL(8), DIMENSION(ndi, ndi, m)::dpolyn2
    REAL(8), DIMENSION(m, n)::u2
    REAL(8), DIMENSION(n)::ff
    REAL(8), DIMENSION(ndi, n)::dff
    REAL(8), DIMENSION(ndi, ndi, n)::dff2
    DO in = 1, n
      ff(in) = vectdot(m, polyn(1:m), u2(1:m, in))
      DO id = 1, ndi
        dff(id, in) = vectdot(m, dpolyn(id, 1:m), u2(1:m, in))
        DO jd = 1, ndi
          dff2(id, jd, in) = vectdot(m, dpolyn2(id, jd, 1:m), u2(1:m, in))
        END DO
      END DO
    END DO
  END SUBROUTINE movingleastsquaressfder
!> moving least squares Green strain and derivative
  SUBROUTINE movingleastsquaresstrain2(ndi, grad, grad2, e, e2)
    IMPLICIT REAL(8) (a-h, o-z)
!----------------------------------------------------
    REAL(8), DIMENSION(ndi, ndi)::grad
    REAL(8), DIMENSION(ndi, ndi, ndi)::grad2
!----------------------------------------------------
    REAL(8), DIMENSION(ndi*(ndi+1)/2)::e
    REAL(8), DIMENSION(ndi*(ndi+1)/2, ndi)::e2
!----------------------------------------------------
    nvoigt = ndi*(ndi+1)/2
    DO ij = 1, nvoigt
      CALL voigttomatrix(i, j, ij, ndi)
!e
      e(ij) = 0.0d00
      DO k = 1, ndi
        e(ij) = e(ij)+0.5d00*grad(k, i)*grad(k, j)
      END DO
      e(ij) = e(ij)-0.5d00*matrixkronecker(i, j)
!e2
      DO l = 1, ndi
        e2(ij, l) = 0.0d00
        DO k = 1, ndi
          e2(ij, l) = e2(ij, l)+0.5d00*grad2(k, i, l)*grad(k, j)+0.5d00*grad(k, i)*grad2(k, j, l)
        END DO
      END DO
    END DO
  END SUBROUTINE movingleastsquaresstrain2
!> moving least squares just regular Green strain
  SUBROUTINE movingleastsquaresstrain(ndi, n, xdef, dff, f, strain)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(ndi, ndi)::c, f
    REAL(8), DIMENSION(ndi, n)::dff
    REAL(8), DIMENSION(ndi, n)::xdef
    REAL(8), DIMENSION(ndi*(ndi+1)/2)::strain
!-------------------------
!*** deformation gradient
!*** before correction
!-------------------------
    f = 0.0d00
    DO in = 1, n
      DO jd = 1, ndi
        DO id = 1, ndi
          f(id, jd) = f(id, jd)+dff(jd, in)*xdef(id, in)
        END DO
      END DO
    END DO
!------------------------------
!*** right cauchy-green tensor
!------------------------------
    CALL matrixmatrixproduct(ndi, ndi, ndi, f, f, c, 2)
    DO id = 1, ndi*(ndi+1)/2
      CALL voigttomatrix(i1, i2, id, ndi)
      strain(id) = 0.5d00*(c(i1, i2)-matrixkronecker(i1, i2))
    END DO
    DO id = ndi+1, ndi*(ndi+1)/2
      strain(id) = 2.0d00*strain(id)
    END DO
  END SUBROUTINE movingleastsquaresstrain
!> moving least squares: determines u2 at a given point
  SUBROUTINE movingleastsquaresu2atacoordinate(imeshless, d, tol, ndi, n, xbar, xn, x, u2)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8)::d, tol
    INTEGER, PARAMETER::mpol = 20
    REAL(8), DIMENSION(n)::w
    REAL(8), DIMENSION(mpol, n)::p
    REAL(8), DIMENSION(mpol, n)::u2
    REAL(8), DIMENSION(ndi, n)::xn
    REAL(8), DIMENSION(ndi)::x, xbar
    REAL(8), DIMENSION(3, mpol)::trash
    REAL(8), DIMENSION(3, 3, mpol)::trash2
!-------------------------------
!*** now we must define matrix p
!*** derivatives are discarded
!-------------------------------
    SELECT CASE (imeshless)
    CASE (-1)
      DO in = 1, n
        CALL movingleastsquarespolynlinbase(ndi, d, xn(1:ndi, in), xbar(1:ndi), p(1:mpol, in), trash(1:ndi, 1:mpol), trash2(1:ndi, 1:ndi, 1:mpol), m)
      END DO
    CASE (-2)
      DO in = 1, n
        CALL movingleastsquarespolynqbase(ndi, d, xn(1:ndi, in), xbar(1:ndi), p(1:mpol, in), trash(1:ndi, 1:mpol), trash2(1:ndi, 1:ndi, 1:mpol), m)
      END DO
    CASE (-3)
      DO in = 1, n
        CALL movingleastsquarespolyncubbase(ndi, d, xn(1:ndi, in), xbar(1:ndi), p(1:mpol, in), trash(1:ndi, 1:mpol), trash2(1:ndi, 1:ndi, 1:mpol), m)
      END DO
    CASE default
      STOP "wrong request to u2atacoordinate"
    END SELECT
!------------------------
!*** now we must define w
!------------------------
    DO in = 1, n
      CALL movingleastsquaresweight(ndi, d, tol, xn(1:ndi, in), x(1:ndi), w(in))
    END DO
!------------------------------
!*** finalize it with specifics
!------------------------------
    CALL movingleastsquaresdetermu2(m, n, w(1:n), p(1:m, 1:n), u2(1:m, 1:n))
  END SUBROUTINE movingleastsquaresu2atacoordinate
!> nesterov determines alpha step size
  REAL(8) FUNCTION nesterovalphadet(n, q0, q1, phi0, phi1, g0, g1)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n)::q0, q1, g0, g1
    alpha1 = vectdot(n, q1-q0, q1-q0)/(2.0d00*(phi0-phi1+vectdot(n, q1-q0, g1)))
    alpha2 = vectdot(n, q1-q0, q1-q0)/(6.0d00*(phi0-phi1)+4.0d00*vectdot(n, q1-q0, g1)+2.0d00*vectdot(n, q1-q0, g0))
    nesterovalphadet = max(0.0d00, alpha1, alpha2)
  END FUNCTION nesterovalphadet
!> nesterov determines beta
  REAL(8) FUNCTION nesterovbetadet(n, q0, q1, g0, g1)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), PARAMETER::small = 1.0d-20
    REAL(8), DIMENSION(n)::q0, q1, g0, g1
    t1 = vectdot(n, q1-q0, g1-g0)
    b1 = vectdot(n, g1-g0, g1-g0)
    t2 = vectdot(n, q1-q0, q1-q0)
    b2 = vectdot(n, q1-q0, g1-g0)
    IF (abs(b1) .GT. small) THEN
      beta1 = t1/b1
      IF (beta1 .LE. small) beta1 = 0.0d00
    ELSE
      beta1 = 0.0d00
    END IF
    IF (abs(b2) .GT. small) THEN
      beta2 = t2/b2
      IF (beta2 .LE. small) beta2 = 0.0d00
    ELSE
      beta2 = 0.0d00
    END IF
    nesterovbetadet = max(0.0d00, beta1, beta2)
  END FUNCTION nesterovbetadet
!> nesterov updates the solution
  SUBROUTINE nesterovupdatestep(n, q0, q1, q2, k, c, gphi, h2, deltamax)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n)::q0, q1, q2, gphi, delta
    IF (k .LE. 0) THEN
      delta = -h2*gphi
      IF (vectnormuniform(n, delta) .GT. deltamax) THEN
        delta = delta*deltamax/vectnormuniform(n, delta)
      END IF
      q2 = q1+delta
    ELSE
      const1 = (2.0d00*k-c)/(2.0d00*k+c)
      const2 = (2.0d00*k*h2)/(2.0d00*k+c)
      delta = const1*(q1-q0)-const2*gphi
      IF (vectnormuniform(n, delta) .GT. deltamax) THEN
        delta = delta*deltamax/vectnormuniform(n, delta)
      END IF
      q2 = q1+delta
    END IF
  END SUBROUTINE nesterovupdatestep
!> nesterov optimizer
  SUBROUTINE nesterovoptimizer(q, n, niter, h2init, proc, histphi, histgrad, histstep, tol, iter, deltamax)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n)::q, q0, q1, q2, gphi0, gphi1, gphi2
    REAL(8), DIMENSION(niter)::histphi, histgrad, histstep
    REAL(8), PARAMETER::c = 3.0d00
    LOGICAL::fixed
    EXTERNAL::proc
!------------------
!*** initial values
!------------------
    q0 = q
    q1 = q
    q2 = q
    k = 0
    phi1 = 0.0d00
    phi2 = 0.0d00
    gphi1 = 0.0d00
    gphi2 = 0.0d00
    DO iter = 0, niter-1
      phi0 = phi1
      phi1 = phi2
      gphi0 = gphi1
      gphi1 = gphi2
!-------------------------------------
!*** evaluates function and derivative
!-------------------------------------
      CALL proc(n, q2, phi2, gphi2)
      IF (iter .EQ. 0) THEN
        phi0 = phi2
        phi1 = phi2
        gphi0 = gphi2
        gphi1 = gphi2
      END IF
!-------------------------------------------------
!*** at this point it's all syncronized, 0 1 and 2
!-------------------------------------------------
!----------------------
!*** determines h2
!----------------------
      CALL nesterovsizeandrestart(k, iter, n, q0, q1, q2, phi0, phi1, phi2, gphi0, gphi1, gphi2, h2)
      IF (h2init .GT. 1.0d-20) THEN
        h2 = h2init
        fixed = .TRUE.
      END IF
!-----------------
!*** determines q2
!-----------------
      q0 = q1
      q1 = q2
      CALL nesterovupdatestep(n, q0, q1, q2, k, c, gphi2, h2, deltamax)
!--------------------------------
!*** update step between restarts
!--------------------------------
      k = k+1
!-------------------
!*** store functions
!-------------------
      histphi(iter+1) = phi2
      histgrad(iter+1) = vectnorm2(n, gphi2)
      histstep(iter+1) = h2
!-------------------------
!*** check for convergence
!-------------------------
      IF (vectnorm2(n, gphi2) .LE. tol) EXIT
    END DO
!--------------------
!*** return solution
!--------------------
    q = q2
  END SUBROUTINE nesterovoptimizer
!> determination of step sizes, restart, etc
  SUBROUTINE nesterovsizeandrestart(k, iter, n, q0, q1, q2, phi0, phi1, phi2, gphi0, gphi1, gphi2, h2)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), PARAMETER::small = 1.0d-20
    REAL(8)::phi0, phi1, phi2
    REAL(8), DIMENSION(n)::q0, q1, q2, gphi0, gphi1, gphi2, gtemp
    LOGICAL::c1, c2, c3, c4, c5, c6
    h2old = h2
    IF (iter .EQ. 0) THEN
      IF ((abs(phi2) .GT. small) .AND. (vectnormuniform(n, gphi2) .GT. small)) THEN
        rl = abs(phi2)/vectnormuniform(n, gphi2)
        h2 = rl/vectnormuniform(n, gphi2)
      ELSEIF (vectnormuniform(n, gphi2) .GT. small) THEN
        h2 = 1.0d00/vectnormuniform(n, gphi2)
      ELSE
        h2 = 1.0d00
      END IF
    ELSE
      IF (k .GE. 2) THEN
        gtemp = q2-q0
        c1 = vectdot(n, gphi0, gtemp) .GT. 0.0d00
        c2 = vectdot(n, gphi1, gtemp) .GT. 0.0d00
        c3 = vectdot(n, gphi2, gtemp) .GT. 0.0d00
        c4 = (phi2 .GT. 1.10d00*phi0) .OR. (phi2 .GT. 1.20d00*phi1) .OR. (phi1 .GT. 1.20d00*phi0)
        c5 = vectnorm2(n, gphi2) .GT. 1.1d00*vectnorm2(n, gphi0)
        c6 = vectnormuniform(n, gphi2) .GT. 1.1d00*vectnormuniform(n, gphi0)
        IF (c1 .OR. c2 .OR. c3 .OR. c4 .OR. c5 .OR. c6) THEN
          h2beta = nesterovbetadet(n, q1, q2, gphi1, gphi2)
          h2 = abs(h2beta)
          k = 1
        ELSE
          h2alpha = nesterovalphadet(n, q1, q2, phi1, phi2, gphi1, gphi2)
          h2 = abs(h2alpha)
        END IF
      END IF
    END IF
  END SUBROUTINE nesterovsizeandrestart
!> moving least squares weight function
  SUBROUTINE movingleastsquaresweight(ndi, d, tol, xi, x, w)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(ndi)::xi, x
!    REAL(8), PARAMETER::pi = 4.0d00*atan(1.0d00)
    s = vectnorm2(ndi, xi-x)
!!    wmax=1.0d00/(tol**2.0d00+tol**4.0d00)
!!    wbot=bottom*wmax
!!    w=max((1.0d00/((s*s)/(d*d)+tol*tol))-(1.0d00/(1.0d00+tol*tol)),wbot)
    w = (1.0d00/((s*s)/(d*d)+tol*tol))-(1.0d00/(1.0d00+tol*tol))
!!    w=max(exp(-s/(1.0d-6*d)),1.0d-15)
!!    w=sqrt(1.0d00/(tol*pi))*exp(-((s/d)**2)/tol)
!!    w=1.0d00/((s/d)+tol)-1.0d00/(1.0d00+tol)
!!    w=max(w,0.0d00)
    w = max(w, 1.0d-30)
!    w=1.0d00
!    w=1.0d00
!    w=(1.0d00/((s*s)/(d*d)+tol*tol))-(1.0d00/(1.0d00+tol*tol))
!    w=max(w,1.0d-20)
!    w=1.0d00
  END SUBROUTINE movingleastsquaresweight
!> radial basis functions: obtain shape functions, first order derivatives and second order derivatives
  SUBROUTINE radbasfuncallshapefunctions(imeshless, rcentroid, ndi, xg, xc, ntot, xtot, ff, dff, dff2)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::imeshless
    REAL(8)::rcentroid, rgauss
    REAL(8), DIMENSION(ndi)::xg, xc
    REAL(8), DIMENSION(ntot)::ff
    REAL(8), DIMENSION(ndi, ntot)::dff
    REAL(8), DIMENSION(ndi, ndi, ntot)::dff2
    REAL(8), DIMENSION(ndi, ntot)::xtot
    rgauss = rcentroid-vectnorm2(ndi, xg-xc)
    CALL radbasfuncallrequired(imeshless, rgauss, ndi, ntot, xtot, xg, xc, ff, dff, dff2)
  END SUBROUTINE radbasfuncallshapefunctions
!> radial basis functions second derivative of a
  SUBROUTINE radbasfuncc2(rmax, r, a, dadr, d2adr2)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8)::rmax, r, a, dadr, d2adr2
    a = 0.0d00
    dadr = 0.0d00
    d2adr2 = 0.0d00
    IF (r .LE. 0.99999d00*rmax) THEN
      a = ((1.0d00-(r/rmax))**4)*(1.0d00+4.0d00*(r/rmax))
      dadr = (20.0d00*r*(r-rmax)**3.0d00)/(rmax**5.0d00)
      d2adr2 = 20.0d00*((r-rmax)**2.0d00)*(4.0d00*r-rmax)/(rmax**5.0d00)
    END IF
  END SUBROUTINE radbasfuncc2
!> radial basis functions a, dadx, d2adx2
  SUBROUTINE radbasfuncone(rmax, ndi, x, xi, a, dadx, d2adx2)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8)::rmax, r, a, dadr, d2adr2
    REAL(8), DIMENSION(ndi)::x, xi
    REAL(8), DIMENSION(ndi)::dadx, drdx
    REAL(8), DIMENSION(ndi, ndi)::d2adx2, d2rdx2
    CALL radbasfuncradius(ndi, x, xi, r, drdx, d2rdx2)
    CALL radbasfuncc2(rmax, r, a, dadr, d2adr2)
    DO id = 1, ndi
      dadx(id) = dadr*drdx(id)
      DO jd = 1, ndi
        d2adx2(id, jd) = d2adr2*drdx(id)*drdx(jd)+dadr*d2rdx2(id, jd)
      END DO
    END DO
  END SUBROUTINE radbasfuncone
!> radial basis functions derivative of radius
  SUBROUTINE radbasfuncradius(ndi, x, xi, r, drdx, d2rdx2)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), PARAMETER::small = 1.0d-10
    REAL(8), DIMENSION(ndi)::x, xi
    REAL(8), DIMENSION(ndi)::drdx
    REAL(8), DIMENSION(ndi, ndi)::d2rdx2
    r = vectnorm2(ndi, x-xi)
    drdx = 0.0d00
    d2rdx2 = 0.0d00
    IF (r .GT. small) THEN
      DO id = 1, ndi
        drdx(id) = (x(id)-xi(id))/r
        DO jd = 1, ndi
          d2rdx2(id, jd) = matrixkronecker(id, jd)/r-(1.0d00/(r**3.0d00))*(x(id)-xi(id))*(x(jd)-xi(jd))
        END DO
      END DO
    END IF
  END SUBROUTINE radbasfuncradius
!> radial basis functions shape functions and derivatives
  SUBROUTINE radbasfuncshapefunctions(imeshless, rmax, ndi, n, xn, x, xc, ff, dff, dff2)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER, PARAMETER::mpol = 20
    REAL(8)::rmax
    REAL(8), DIMENSION(n, n)::amatrix, matrixinverserix, gmatrix
    REAL(8), DIMENSION(n, mpol)::bmatrix
    REAL(8), DIMENSION(mpol, n)::hmatrix
    REAL(8), DIMENSION(ndi, ndi, n)::dff2
    REAL(8), DIMENSION(ndi, n)::dff
    REAL(8), DIMENSION(n)::ff, a
    REAL(8), DIMENSION(ndi, n)::xn
    REAL(8), DIMENSION(ndi)::x, xc
    REAL(8), DIMENSION(mpol)::b
    REAL(8), DIMENSION(ndi, mpol)::dbdx
    REAL(8), DIMENSION(ndi, ndi, mpol)::dbdx2
    REAL(8), DIMENSION(mpol, mpol)::b1b, invb1b
    REAL(8), DIMENSION(ndi, n)::dadx
    REAL(8), DIMENSION(ndi, ndi, n)::d2adx2
    REAL(8), DIMENSION(mpol)::polyn
    REAL(8), DIMENSION(ndi, mpol)::dpolyn
    REAL(8), DIMENSION(ndi, ndi, mpol)::dpolyn2
!------------------
!*** forms amatrix
!------------------
    DO jn = 1, n
      DO in = 1, n
!-----------------------------------
!*** dadx and d2adx2 are trash here
!-----------------------------------
        CALL radbasfuncone(rmax, ndi, xn(1:ndi, in), xn(1:ndi, jn), amatrix(in, jn), dadx(1:ndi, in), d2adx2(1:ndi, 1:ndi, in))
      END DO
    END DO
!--------------------------
!*** matrixinverserix=amatrix^-1
!--------------------------
    CALL matrixinverse(n, deta, amatrix, matrixinverserix)
!----------------------------------
!*** forms bmatrix
!*** checks effect of dtemp and xc
!----------------------------------
    mp = 0
    IF (imeshless .NE. 0) THEN
      DO in = 1, n
        mp = 0
        SELECT CASE (imeshless)
        CASE (1)
          CALL movingleastsquarespolynlinbase(ndi, 1.0d00, xn(1:ndi, in), xc(1:ndi), polyn(1:mpol), dpolyn(1:ndi, 1:mpol), dpolyn2(1:ndi, 1:ndi, 1:mpol), mp)
        CASE (2)
          CALL movingleastsquarespolynqbase(ndi, 1.0d00, xn(1:ndi, in), xc(1:ndi), polyn(1:mpol), dpolyn(1:ndi, 1:mpol), dpolyn2(1:ndi, 1:ndi, 1:mpol), mp)
        CASE (3)
          CALL movingleastsquarespolyncubbase(ndi, 1.0d00, xn(1:ndi, in), xc(1:ndi), polyn(1:mpol), dpolyn(1:ndi, 1:mpol), dpolyn2(1:ndi, 1:ndi, 1:mpol), mp)
        END SELECT
        DO ip = 1, mp
          bmatrix(in, ip) = polyn(ip)
        END DO
      END DO
    END IF
!-----------------------
!*** forms big matrices
!-----------------------
    IF (imeshless .NE. 0) THEN
      DO jd = 1, mp
        DO id = 1, mp
          b1b(id, jd) = 0.0d00
          DO in = 1, n
            DO jn = 1, n
              b1b(id, jd) = b1b(id, jd)+bmatrix(in, id)*matrixinverserix(in, jn)*bmatrix(jn, jd)
            END DO
          END DO
        END DO
      END DO
    END IF
!-----------------------
!*** !!!above seems ok
!-----------------------
!----------------
!*** invert b1b
!*** into invb1b
!----------------
    IF (mp .GT. 0) THEN
      CALL matrixinverse(mp, det, b1b(1:mp, 1:mp), invb1b(1:mp, 1:mp))
!------------
!*** hmatrix
!------------
      DO in = 1, n
        DO id = 1, mp
          hmatrix(id, in) = 0.0d00
          DO jn = 1, n
            DO jd = 1, mp
              hmatrix(id, in) = hmatrix(id, in)+invb1b(id, jd)*bmatrix(jn, jd)*matrixinverserix(jn, in)
            END DO
          END DO
        END DO
      END DO
      DO jn = 1, n
        DO in = 1, n
          gmatrix(in, jn) = matrixinverserix(in, jn)
          DO id = 1, mp
            DO kn = 1, n
              gmatrix(in, jn) = gmatrix(in, jn)-matrixinverserix(in, kn)*bmatrix(kn, id)*hmatrix(id, jn)
            END DO
          END DO
        END DO
      END DO
    END IF
!----------------------------------
!*** forms vectors a and b
!*** checks effect of dtemp and xc
!----------------------------------
    DO in = 1, n
      CALL radbasfuncone(rmax, ndi, x(1:ndi), xn(1:ndi, in), a(in), dadx(1:ndi, in), d2adx2(1:ndi, 1:ndi, in))
      SELECT CASE (imeshless)
      CASE (1)
        CALL movingleastsquarespolynlinbase(ndi, 1.0d00, x(1:ndi), xc(1:ndi), b, dbdx(1:ndi, 1:mp), dbdx2(1:ndi, 1:ndi, 1:mp), mp)
      CASE (2)
        CALL movingleastsquarespolynqbase(ndi, 1.0d00, x(1:ndi), xc(1:ndi), b, dbdx(1:ndi, 1:mp), dbdx2(1:ndi, 1:ndi, 1:mp), mp)
      CASE (3)
        CALL movingleastsquarespolyncubbase(ndi, 1.0d00, x(1:ndi), xc(1:ndi), b, dbdx(1:ndi, 1:mp), dbdx2(1:ndi, 1:ndi, 1:mp), mp)
      END SELECT
    END DO
!------------------------------------------------------
!*** now determines rb shape functions and derivatives
!------------------------------------------------------
    IF (imeshless .EQ. 0) THEN
      DO in = 1, n
        ff(in) = vectdot(n, a(1:n), matrixinverserix(1:n, in))
        DO id = 1, ndi
          dff(id, in) = vectdot(n, dadx(id, 1:n), matrixinverserix(1:n, in))
          DO jd = 1, ndi
            dff2(id, jd, in) = vectdot(n, d2adx2(id, jd, 1:n), matrixinverserix(1:n, in))
          END DO
        END DO
      END DO
    ELSE
      DO in = 1, n
        ff(in) = vectdot(n, a(1:n), gmatrix(1:n, in))+vectdot(mp, b(1:mp), hmatrix(1:mp, in))
        DO id = 1, ndi
          dff(id, in) = vectdot(n, dadx(id, 1:n), gmatrix(1:n, in))+vectdot(mp, dbdx(id, 1:mp), hmatrix(1:mp, in))
          DO jd = 1, ndi
            dff2(id, jd, in) = vectdot(n, d2adx2(id, jd, 1:n), gmatrix(1:n, in))+vectdot(mp, dbdx2(id, jd, 1:mp), hmatrix(1:mp, in))
          END DO
        END DO
      END DO
    END IF
  END SUBROUTINE radbasfuncshapefunctions
!> radial basis functions and moving least squares: shape functions and derivatives
  SUBROUTINE radbasfuncallrequired(imeshless, rmax, ndi, ntot, xtot, x, xc, ff, dff, dff2)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER::imeshless
    INTEGER, PARAMETER::mpol = 20
    INTEGER, DIMENSION(:), ALLOCATABLE::listn
    REAL(8)::rmax
    REAL(8), DIMENSION(ntot)::ff, ffloc
    REAL(8), DIMENSION(ndi, ntot)::dff, dffloc
    REAL(8), DIMENSION(ndi, ndi, ntot)::dff2, dff2loc
    REAL(8), DIMENSION(ndi, ntot)::xtot, xn
    REAL(8), DIMENSION(ndi)::x, xc
    REAL(8), DIMENSION(mpol)::polyn
    REAL(8), DIMENSION(ndi, mpol)::dpolyn
    REAL(8), DIMENSION(ndi, ndi, mpol)::dpolyn2
    REAL(8), DIMENSION(mpol, ntot)::u2
!-----------------------
!*** gets closest nodes
!*** to x with corected
!*** radius above
!-----------------------
    CALL movingleastsquaresgetsnodes(rmax, x, ndi, ntot, xtot, n, listn)
    DO i = 1, n
      DO id = 1, ndi
        xn(id, i) = xtot(id, listn(i))
      END DO
    END DO
!-------------------------------
!*** shape function derivatives
!-------------------------------
    IF (imeshless .GE. 0) THEN
      CALL radbasfuncshapefunctions(imeshless, rmax, ndi, n, xn, x, xc, ffloc, dffloc, dff2loc)
    ELSE
!---------
!*** mls
!---------
      tol = 1.0d-2
      CALL movingleastsquaresu2atacoordinate(imeshless, rmax, tol, ndi, n, xc, xn, x, u2(1:mpol, 1:n))
      SELECT CASE (imeshless)
      CASE (-1)
        CALL movingleastsquarespolynlinbase(ndi, rmax, x(1:ndi), xc(1:ndi), polyn(1:mpol), dpolyn(1:ndi, 1:mpol), dpolyn2(1:ndi, 1:ndi, 1:mpol), m)
      CASE (-2)
        CALL movingleastsquarespolynqbase(ndi, rmax, x(1:ndi), xc(1:ndi), polyn(1:mpol), dpolyn(1:ndi, 1:mpol), dpolyn2(1:ndi, 1:ndi, 1:mpol), m)
      CASE (-3)
        CALL movingleastsquarespolyncubbase(ndi, rmax, x(1:ndi), xc(1:ndi), polyn(1:mpol), dpolyn(1:ndi, 1:mpol), dpolyn2(1:ndi, 1:ndi, 1:mpol), m)
      END SELECT
      CALL movingleastsquaressfder(ndi, n, m, polyn(1:m), dpolyn(1:ndi, 1:m), dpolyn2(1:ndi, 1:ndi, 1:m), u2(1:m, 1:n), ffloc, dffloc, dff2loc)
    END IF
    ff = 0.0d00
    dff = 0.0d00
    dff2 = 0.0d00
    DO i = 1, n
      ff(listn(i)) = ffloc(i)
      dff(1:ndi, listn(i)) = dffloc(1:ndi, i)
      dff2(1:ndi, 1:ndi, listn(i)) = dff2loc(1:ndi, 1:ndi, i)
    END DO
    DEALLOCATE (listn)
  END SUBROUTINE radbasfuncallrequired
!> scalar modular bucket number
  INTEGER FUNCTION scalarmodularbucket(n, i)
    itmp = int((i+1.0d00)/n)
    scalarmodularbucket = itmp
  END FUNCTION scalarmodularbucket
!> scalar modular position local
  INTEGER FUNCTION scalarmodularposition(n, i)
    itmp = mod(i, n)
    IF (itmp .EQ. 0) THEN
      scalarmodularposition = n
    ELSE
      scalarmodularposition = itmp
    END IF
    IF (scalarmodularposition .LE. 0) scalarmodularposition = scalarmodularposition+ceiling(-1.0d00*scalarmodularposition/n)*n
  END FUNCTION scalarmodularposition
!> swap two scalars
  SUBROUTINE scalarswap(n1, n2)
    INTEGER::n1, n2, ngash
    ngash = n1
    n1 = n2
    n2 = ngash
  END SUBROUTINE scalarswap
!> hyperbolic secant
  REAL(8) FUNCTION sech(x)
    IMPLICIT REAL(8) (a-h, o-z)
    sech = 2.0d00/(exp(x)+exp(-x))
  END FUNCTION sech
  !> rotates a 2nd order tensor in voigt notation
!> ityp="STRESS" for... stress
!> ityp="STRAIN" for... strain
!> ijob=1=> av=rt.av.r
!> ijob=2=> av=r.av.rt
  SUBROUTINE stressstrainrotate(ityp, ijob, ndi, nvoigt, r, av)
    IMPLICIT REAL(8) (a-h, o-z)
    CHARACTER(*)::ityp
    REAL(8), DIMENSION(ndi, ndi)::r
    REAL(8), DIMENSION(nvoigt, nvoigt)::rv
    REAL(8), DIMENSION(nvoigt)::av, bv
    CALL stressstrainrotation(ityp, ijob, ndi, nvoigt, r, rv)
    CALL vectcopy(nvoigt, bv, av)
    CALL matrixvectorproduct(nvoigt, nvoigt, rv, bv, av)
  END SUBROUTINE stressstrainrotate
!> rotates a 4nd order tensor in voigt notation
!> ijob=1=> rv=rt.rt.rv.r.r
!> ijob=2=> rv=r.r.rv.rt.rt
  SUBROUTINE stressstrainrotation(ityp, ijob, ndi, nvoigt, r, rv)
    IMPLICIT REAL(8) (a-h, o-z)
    CHARACTER(*)::ityp
    REAL(8), DIMENSION(ndi, ndi)::r, r2
    REAL(8), DIMENSION(nvoigt, nvoigt)::rv
    IF (ijob .EQ. 2) THEN
      CALL matrixtranspose(ndi, r, r2)
    ELSE
      CALL vectcopy(ndi*ndi, r2, r)
    END IF
    SELECT CASE (ityp)
    CASE default
      STOP "error in stressstrainrotation 1"
    CASE ("stress")
      SELECT CASE (nvoigt)
      CASE default
        STOP "error in stressstrainrotation 2"
      CASE (3)
        r11 = r2(1, 1)
        r22 = r2(2, 2)
        r12 = r2(1, 2)
        r21 = r2(2, 1)
        t1 = r11**2
        t2 = r21**2
        t5 = r12**2
        t6 = r22**2
        rv(1, 1) = t1
        rv(1, 2) = t2
        rv(1, 3) = 2.0d00*r21*r11
        rv(2, 1) = t5
        rv(2, 2) = t6
        rv(2, 3) = 2.0d00*r22*r12
        rv(3, 1) = r11*r12
        rv(3, 2) = r21*r22
        rv(3, 3) = r21*r12+r11*r22
      CASE (4)
        r11 = r2(1, 1)
        r12 = r2(1, 2)
        r13 = r2(1, 3)
        r21 = r2(2, 1)
        r22 = r2(2, 2)
        r23 = r2(2, 3)
        r31 = r2(3, 1)
        r32 = r2(3, 2)
        r33 = r2(3, 3)
        t1 = r11**2
        t2 = r21**2
        t3 = r31**2
        t10 = r12**2
        t11 = r22**2
        t12 = r32**2
        t19 = r13**2
        t20 = r23**2
        t21 = r33**2
        rv(1, 1) = t1
        rv(1, 2) = t2
        rv(1, 3) = t3
        rv(1, 4) = 2.0d00*r21*r11
        rv(2, 1) = t10
        rv(2, 2) = t11
        rv(2, 3) = t12
        rv(2, 4) = 2.0d00*r22*r12
        rv(3, 1) = t19
        rv(3, 2) = t20
        rv(3, 3) = t21
        rv(3, 4) = 2.0d00*r23*r13
        rv(4, 1) = r11*r12
        rv(4, 2) = r21*r22
        rv(4, 3) = r31*r32
        rv(4, 4) = r21*r12+r11*r22
      CASE (6)
        r11 = r2(1, 1)
        r12 = r2(1, 2)
        r13 = r2(1, 3)
        r21 = r2(2, 1)
        r22 = r2(2, 2)
        r23 = r2(2, 3)
        r31 = r2(3, 1)
        r32 = r2(3, 2)
        r33 = r2(3, 3)
        t1 = r11**2
        t2 = r21**2
        t3 = r31**2
        t10 = r12**2
        t11 = r22**2
        t12 = r32**2
        t19 = r13**2
        t20 = r23**2
        t21 = r33**2
        rv(1, 1) = t1
        rv(1, 2) = t2
        rv(1, 3) = t3
        rv(1, 4) = 2.0d00*r21*r11
        rv(1, 5) = 2.0d00*r31*r11
        rv(1, 6) = 2.0d00*r31*r21
        rv(2, 1) = t10
        rv(2, 2) = t11
        rv(2, 3) = t12
        rv(2, 4) = 2.0d00*r22*r12
        rv(2, 5) = 2.0d00*r32*r12
        rv(2, 6) = 2.0d00*r32*r22
        rv(3, 1) = t19
        rv(3, 2) = t20
        rv(3, 3) = t21
        rv(3, 4) = 2.0d00*r23*r13
        rv(3, 5) = 2.0d00*r33*r13
        rv(3, 6) = 2.0d00*r33*r23
        rv(4, 1) = r11*r12
        rv(4, 2) = r21*r22
        rv(4, 3) = r31*r32
        rv(4, 4) = r21*r12+r11*r22
        rv(4, 5) = r31*r12+r11*r32
        rv(4, 6) = r31*r22+r21*r32
        rv(5, 1) = r11*r13
        rv(5, 2) = r21*r23
        rv(5, 3) = r31*r33
        rv(5, 4) = r21*r13+r11*r23
        rv(5, 5) = r31*r13+r11*r33
        rv(5, 6) = r31*r23+r21*r33
        rv(6, 1) = r12*r13
        rv(6, 2) = r22*r23
        rv(6, 3) = r32*r33
        rv(6, 4) = r22*r13+r12*r23
        rv(6, 5) = r32*r13+r12*r33
        rv(6, 6) = r32*r23+r22*r33
      END SELECT
    CASE ("strain")
      SELECT CASE (nvoigt)
      CASE default
        STOP "error in stressstrainrotation 3"
      CASE (3)
        r11 = r2(1, 1)
        r22 = r2(2, 2)
        r12 = r2(1, 2)
        r21 = r2(2, 1)
        t1 = r11**2
        t2 = r21**2
        t5 = r12**2
        t6 = r22**2
        rv(1, 1) = t1
        rv(1, 2) = t2
        rv(1, 3) = r21*r11
        rv(2, 1) = t5
        rv(2, 2) = t6
        rv(2, 3) = r22*r12
        rv(3, 1) = 2.0d00*r11*r12
        rv(3, 2) = 2.0d00*r21*r22
        rv(3, 3) = r21*r12+r11*r22
      CASE (4)
        r11 = r2(1, 1)
        r12 = r2(1, 2)
        r13 = r2(1, 3)
        r21 = r2(2, 1)
        r22 = r2(2, 2)
        r23 = r2(2, 3)
        r31 = r2(3, 1)
        r32 = r2(3, 2)
        r33 = r2(3, 3)
        t1 = r11**2
        t2 = r21**2
        t3 = r31**2
        t7 = r12**2
        t8 = r22**2
        t9 = r32**2
        t13 = r13**2
        t14 = r23**2
        t15 = r33**2
        rv(1, 1) = t1
        rv(1, 2) = t2
        rv(1, 3) = t3
        rv(1, 4) = r21*r11
        rv(2, 1) = t7
        rv(2, 2) = t8
        rv(2, 3) = t9
        rv(2, 4) = r22*r12
        rv(3, 1) = t13
        rv(3, 2) = t14
        rv(3, 3) = t15
        rv(3, 4) = r23*r13
        rv(4, 1) = 2.0d00*r11*r12
        rv(4, 2) = 2.0d00*r21*r22
        rv(4, 3) = 2.0d00*r31*r32
        rv(4, 4) = r21*r12+r11*r22
      CASE (6)
        r11 = r2(1, 1)
        r12 = r2(1, 2)
        r13 = r2(1, 3)
        r21 = r2(2, 1)
        r22 = r2(2, 2)
        r23 = r2(2, 3)
        r31 = r2(3, 1)
        r32 = r2(3, 2)
        r33 = r2(3, 3)
        t1 = r11**2
        t2 = r21**2
        t3 = r31**2
        t7 = r12**2
        t8 = r22**2
        t9 = r32**2
        t13 = r13**2
        t14 = r23**2
        t15 = r33**2
        rv(1, 1) = t1
        rv(1, 2) = t2
        rv(1, 3) = t3
        rv(1, 4) = r21*r11
        rv(1, 5) = r31*r11
        rv(1, 6) = r31*r21
        rv(2, 1) = t7
        rv(2, 2) = t8
        rv(2, 3) = t9
        rv(2, 4) = r22*r12
        rv(2, 5) = r32*r12
        rv(2, 6) = r32*r22
        rv(3, 1) = t13
        rv(3, 2) = t14
        rv(3, 3) = t15
        rv(3, 4) = r23*r13
        rv(3, 5) = r33*r13
        rv(3, 6) = r33*r23
        rv(4, 1) = 2.0d00*r11*r12
        rv(4, 2) = 2.0d00*r21*r22
        rv(4, 3) = 2.0d00*r31*r32
        rv(4, 4) = r21*r12+r11*r22
        rv(4, 5) = r31*r12+r11*r32
        rv(4, 6) = r31*r22+r21*r32
        rv(5, 1) = 2.0d00*r11*r13
        rv(5, 2) = 2.0d00*r21*r23
        rv(5, 3) = 2.0d00*r31*r33
        rv(5, 4) = r21*r13+r11*r23
        rv(5, 5) = r31*r13+r11*r33
        rv(5, 6) = r31*r23+r21*r33
        rv(6, 1) = 2.0d00*r12*r13
        rv(6, 2) = 2.0d00*r22*r23
        rv(6, 3) = 2.0d00*r32*r33
        rv(6, 4) = r22*r13+r12*r23
        rv(6, 5) = r32*r13+r12*r33
        rv(6, 6) = r32*r23+r22*r33
      END SELECT
    END SELECT
  END SUBROUTINE stressstrainrotation
!> checks if two strings are the same
  LOGICAL FUNCTION stringcheckifsame(n, s1, s2)
    CHARACTER(*)::s1, s2
    stringcheckifsame = .FALSE.
    IF (len_trim(s1) .NE. len_trim(s2)) RETURN
    IF (s1(1:n) .EQ. s2(1:n)) stringcheckifsame = .TRUE.
  END FUNCTION stringcheckifsame
!> resize a string
  SUBROUTINE stringdynamic(n, l)
    CHARACTER(*), DIMENSION(:), ALLOCATABLE::l
    CHARACTER(lcharacter), DIMENSION(:), ALLOCATABLE::lt
    IF (n .LE. 0) THEN
      IF (allocated(l)) DEALLOCATE (l)
    ELSE
      IF (allocated(l)) THEN
        nl = size(l)
        IF (nl .LT. n) THEN
          ALLOCATE (lt(nl))
          DO i = 1, nl
            lt(i) = l(i)
          END DO
          CALL allocsafe(n*2, l)
          DO i = 1, nl
            l(i) = lt(i)
          END DO
          DEALLOCATE (lt)
        END IF
      ELSE
        CALL allocsafe(n*2, l)
      END IF
    END IF
  END SUBROUTINE stringdynamic
!> find a string in an array
  INTEGER FUNCTION stringfindinarray(m, n, sa, s)
    CHARACTER(*)::s
    CHARACTER(*), DIMENSION(*)::sa
    stringfindinarray = 0
    DO i = 1, m
      IF (stringcheckifsame(n, sa(i), s)) THEN
        stringfindinarray = i
        RETURN
      END IF
    END DO
  END FUNCTION stringfindinarray
!> insert a string in an array
  SUBROUTINE stringinsert(l, i, k)
    CHARACTER(*)::k
    CHARACTER(len(k))::ktemp
    CHARACTER(*), DIMENSION(:), ALLOCATABLE::l
    ktemp = k
    CALL stringdynamic(i, l)
    IF (i .GT. 0) l(i) = ktemp
  END SUBROUTINE stringinsert
!> lowercase a string
  SUBROUTINE stringlowercase(str)
    CHARACTER(*)::str
    jlen = len(str)
    DO i = 1, jlen
      iasc = ichar(str(i:i))
      IF ((iasc .GT. 64) .AND. (iasc .LT. 91)) THEN
        str(i:i) = char(iasc+32)
      END IF
    END DO
  END SUBROUTINE stringlowercase
!> read an integer from a string
  SUBROUTINE stringreadaninteger(text, l)
    CHARACTER(*)::text
    READ (text, *, iostat=ierr) l
    IF (ierr .NE. 0 .OR. text .EQ. " ") l = 0
  END SUBROUTINE stringreadaninteger
!> read a real from a string
  SUBROUTINE stringreadareal(text, r)
    CHARACTER(*)::text
    REAL(8)::r
    READ (text, *, iostat=ierr) r
    IF (ierr .NE. 0 .OR. text .EQ. " ") r = 0.0d00
  END SUBROUTINE stringreadareal
!> read a string from... a string
  SUBROUTINE stringreadastring(text, r)
    CHARACTER(*)::text, r
    READ (text, *, iostat=ierr) r
    IF (ierr .NE. 0 .OR. text .EQ. " ") r = ""
    CALL stringuppercase(r)
  END SUBROUTINE stringreadastring
!> read an integer from a string and removes it
  SUBROUTINE stringreadintegerandremove(text, l)
    CHARACTER(*)::text
    CALL stringreadaninteger(text, l)
    i = index(text, " ")
    IF (i .NE. 0) THEN
      text(1:) = text(i:)
      CALL stringremoveheadspaces(text)
    END IF
  END SUBROUTINE stringreadintegerandremove
!> read a double from a string and removes it
  SUBROUTINE stringreadrealandremove(text, r)
    CHARACTER(*)::text
    REAL(8)::r
    CALL stringreadareal(text, r)
    i = index(text, " ")
    IF (i .NE. 0) THEN
      text(1:) = text(i:)
      CALL stringremoveheadspaces(text)
    END IF
  END SUBROUTINE stringreadrealandremove
!> read a string from a string and removes it
  SUBROUTINE stringreadstringandremove(text, l)
    CHARACTER(*)::text, l
    CALL stringreadastring(text, l)
    i = index(text, " ")
    IF (i .NE. 0) THEN
      text(1:) = text(i:)
      CALL stringremoveheadspaces(text)
    END IF
    CALL stringremoveheadspaces(l)
    CALL stringremoveats(l)
  END SUBROUTINE stringreadstringandremove
!> remove all spaces from a string
  SUBROUTINE stringremoveallspaces(text)
    CHARACTER(*)::text
    DO
      i = index(trim(text), " ")
      IF (i .EQ. 0) EXIT
      text(i:) = text(i+1:)
    END DO
  END SUBROUTINE stringremoveallspaces
!> string remove all "@" at signs
  SUBROUTINE stringremoveats(text)
    CHARACTER(*)::text
    lm = len(text)
    ik = 1
    DO
      ik = ik+1
      IF (ik .EQ. lm) EXIT
      IF (text(1:1) .EQ. "@" .OR. text(1:1) .EQ. " ") THEN
        text(1:) = text(2:)
      ELSE
        EXIT
      END IF
    END DO
  END SUBROUTINE stringremoveats
!> string remove all commas
  SUBROUTINE stringremovecommas(text)
    CHARACTER(*)::text
    DO
      i = index(text, ",")
      IF (i .LE. 0) EXIT
      text(i:i) = " "
    END DO
    DO
      i = index(trim(text), "  ")
      IF (i .LE. 0) EXIT
      text(i:) = text(i+1:)
    END DO
  END SUBROUTINE stringremovecommas
!> string remove digits and commments
  SUBROUTINE stringremovedigitsandcomments(text)
    CHARACTER(*)::text
    CHARACTER(*), PARAMETER::numer = "0123456789.ee-*+ "
    DO
      i = verify(text, numer)
      IF (i .EQ. 0) EXIT
      text(i:i) = " "
    END DO
    DO
      i = index(trim(text), "  ")
      IF (i .EQ. 0) EXIT
      text(i:) = text(i+1:)
    END DO
  END SUBROUTINE stringremovedigitsandcomments
!> string remove all digits
  SUBROUTINE stringremovedigits(text)
    CHARACTER(*)::text
    CHARACTER(*), PARAMETER::numer = "0123456789"
    DO
      i = scan(text, numer)
      IF (i .EQ. 0) EXIT
      text(i:i) = " "
    END DO
    DO
      i = index(trim(text), "  ")
      IF (i .EQ. 0) EXIT
      text(i:) = text(i+1:)
    END DO
  END SUBROUTINE stringremovedigits
!> string remove double spaces
  SUBROUTINE stringremovedoublespaces(text)
    CHARACTER(*)::text
    DO
      i = index(trim(text), "  ")
      IF (i .EQ. 0) EXIT
      text(i:) = text(i+1:)
    END DO
  END SUBROUTINE stringremovedoublespaces
!> string remove a character at position i
  SUBROUTINE stringremove(nl, l, i)
    CHARACTER(*), DIMENSION(:), ALLOCATABLE::l
    IF (i .LE. nl) THEN
      l(i) = l(nl)
      l(nl) = ""
      nl = nl-1
    END IF
  END SUBROUTINE stringremove
!> string remove all spaces at the head
  SUBROUTINE stringremoveheadspaces(text)
    CHARACTER(*)::text
    lm = len(text)
    ik = 1
    DO
      ik = ik+1
      IF (ik .EQ. lm) EXIT
      IF (text(1:1) .EQ. " ") THEN
        text(1:) = text(2:)
      ELSE
        EXIT
      END IF
    END DO
  END SUBROUTINE stringremoveheadspaces
!> string replace non-printing by spaces
  SUBROUTINE stringreplacenonprinting(str) ! ascii < 33 ou ascii > 254
    CHARACTER(*)::str
    jlen = len(str)
    IF (jlen .GT. 1) THEN
      DO i = 1, jlen
        iasc = ichar(str(i:i))
        IF ((iasc .LE. 31) .OR. (iasc .GE. 255)) THEN
          str(i:i) = ' '
        END IF
      END DO
    END IF
  END SUBROUTINE stringreplacenonprinting
!> string turn to upper case
  SUBROUTINE stringuppercase(str)
    CHARACTER(*)::str
    jlen = len(str)
    DO i = 1, jlen
      iasc = ichar(str(i:i))
      IF ((iasc .GT. 96) .AND. (iasc .LT. 123)) THEN
        str(i:i) = char(iasc-32)
      END IF
    END DO
  END SUBROUTINE stringuppercase
!> chrono ijob=1 start, ijob=2 end
  SUBROUTINE timechrono(ijob, nseg)
    REAL, SAVE::tempo1 = 0.0
    REAL::tempo2
    SELECT CASE (ijob)
    CASE (1)
      CALL cpu_time(tempo1)
    CASE (2)
      CALL cpu_time(tempo2)
      nseg = nint(tempo2-tempo1)
    END SELECT
  END SUBROUTINE timechrono
!> z=ax+by
  SUBROUTINE vectaxpby(n, z, a, x, b, y)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*) :: x, y, z
    INTEGER :: i
    REAL(8) :: rtemp, stemp
!$OMP PARALLEL DO PRIVATE(i, rtemp, stemp)
    DO i = 1, n
      rtemp = x(i)
      stemp = y(i)
      z(i) = a*rtemp+b*stemp
    END DO
!$OMP END PARALLEL DO
  END SUBROUTINE vectaxpby
!> z=ax+y
  SUBROUTINE vectaxpy(n, z, a, x, y)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x, y, z
    IF (n .LE. 0) RETURN
    IF (abs(a) .LE. epsmach()) THEN
      CALL vectcopy(n, z, y)
    ELSE
      IF (abs(a-1.0d00) .GT. epsmach()) THEN
!$omp parallel
!$omp do schedule(auto)
        DO i = 1, n
          z(i) = a*x(i)+y(i)
        END DO
!$omp end do
!$omp end parallel
      ELSE
!$omp parallel
!$omp do schedule(auto)
        DO i = 1, n
          z(i) = x(i)+y(i)
        END DO
!$omp end do
!$omp end parallel
      END IF
    END IF
  END SUBROUTINE vectaxpy
!> binary search a vector
  INTEGER FUNCTION vectbinarysearch(n, l, u)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8)::u
    REAL(8), DIMENSION(*)::l
    vectbinarysearch = 0
    IF (n .GE. 2) THEN
      i = 1
      j = n
      DO
        k = i+((j-i)/2)
        IF (u .LT. l(k)) THEN
          j = k
        ELSE
          i = k
        END IF
        IF (i+1 .GE. j) EXIT
      END DO
      vectbinarysearch = i
    ELSEIF (n .EQ. 1) THEN
      vectbinarysearch = 1
    END IF
  END FUNCTION vectbinarysearch
!> copy a vector x=y
  SUBROUTINE vectcopy(n, x, y)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x, y
    x(1:n) = y(1:n)
  END SUBROUTINE vectcopy
!> vectdotuct
  REAL(8) FUNCTION vectdot(n, x, y)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x, y
    vectdot = 0.0d00
!$OMP PARALLEL DO DEFAULT(none) SHARED(x, y, n) PRIVATE(i) REDUCTION(+:vectdot)
    DO i = 1, n
      vectdot = vectdot+x(i)*y(i)
    END DO
!$OMP END PARALLEL DO
  END FUNCTION vectdot
!> resizes a vector
  SUBROUTINE vectdynamic(n, l)
    REAL(8), DIMENSION(:), ALLOCATABLE::l, lt
    IF (n .LE. 0) THEN
      IF (allocated(l)) DEALLOCATE (l)
    ELSE
      IF (allocated(l)) THEN
        nl = size(l)
        IF (nl .LT. n) THEN
          ALLOCATE (lt(nl))
          CALL vectcopy(nl, lt, l)
          CALL allocsafe(n*2, l)
          CALL vectcopy(nl, l, lt)
          DEALLOCATE (lt)
        END IF
      ELSE
        CALL allocsafe(n*2, l)
      END IF
    END IF
  END SUBROUTINE vectdynamic
!> checks if a vector has a nan
  LOGICAL FUNCTION vectifnan(n, v)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::v
    inan = 0
    DO i = 1, n
      IF (isnan(v(i))) THEN
        inan = 1
        EXIT
      END IF
    END DO
    IF (inan .EQ. 0) THEN
      vectifnan = .FALSE.
    ELSE
      vectifnan = .TRUE.
    END IF
  END FUNCTION vectifnan
!> checks if a vector is null
  FUNCTION vectifzero(n, v)
    IMPLICIT REAL(8) (a-h, o-z)
    LOGICAL::vectifzero
    REAL(8)::zero
    REAL(8), DIMENSION(*)::v
    vectifzero = .TRUE.
    zero = epsmach()
    DO i = 1, n
      IF (abs(v(i)) .GT. zero) THEN
        vectifzero = .FALSE.
        EXIT
      END IF
    END DO
  END FUNCTION vectifzero
!> determines the index with the maximum absolute value
  INTEGER FUNCTION vectindexmaxabs(n, x)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x
    r = abs(x(1))
    ii = 1
    DO i = 2, n
      rtemp = abs(x(i))
      IF (rtemp .GE. r) THEN
        ii = i
        r = rtemp
      END IF
    END DO
    vectindexmaxabs = ii
  END FUNCTION vectindexmaxabs
!> determines the index with the maximum value
  INTEGER FUNCTION vectindexmax(n, x)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n)::x
    r = x(1)
    ii = 1
    DO i = 2, n
      rtemp = x(i)
      IF (rtemp .LE. r) CYCLE
      ii = i
      r = rtemp
    END DO
    vectindexmax = ii
  END FUNCTION vectindexmax
!> determines the index with minimum value
  INTEGER FUNCTION vectindexmin(n, x)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x
    r = x(1)
    ii = 1
    DO i = 2, n
      rtemp = x(i)
      IF (r .LE. rtemp) CYCLE
      ii = i
      r = rtemp
    END DO
    vectindexmin = ii
  END FUNCTION vectindexmin
!> insert an element in a vector
  SUBROUTINE vectinsert(l, i, k)
    REAL(8)::k, ktemp
    REAL(8), DIMENSION(:), ALLOCATABLE::l
    ktemp = k
    CALL vectdynamic(i, l)
    IF (i .GT. 0) l(i) = ktemp
  END SUBROUTINE vectinsert
!> maximum absolute value for components
  REAL(8) FUNCTION vectmaxabs(n, x)
    REAL(8), DIMENSION(*)::x
    vectmaxabs = x(vectindexmax(n, x))
  END FUNCTION vectmaxabs
!> minimum value component
  REAL(8) FUNCTION vectminval(n, x)
    REAL(8), DIMENSION(*)::x
    vectminval = x(vectindexmin(n, x))
  END FUNCTION vectminval
!> norm2 of a vector
  REAL(8) FUNCTION vectnorm2(n, x)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x
    IF (n .LE. 0) THEN
      vectnorm2 = 0.0d00
    ELSE
      vectnorm2 = sqrt(vectdot(n, x, x))
    END IF
  END FUNCTION vectnorm2
!> normalize a vector
  SUBROUTINE vectnormalize(n, x)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x
    r = vectnorm2(n, x)
    IF (r .GT. 0.0d00) THEN
      r = 1.0d00/r
      CALL vectscale(n, x, r, x)
    END IF
  END SUBROUTINE vectnormalize
!> vector p-norm
  REAL(8) FUNCTION vectnormp(n, x, p)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x
    INTEGER::p
    vectnormp = 0.0d00
    IF (n .GT. 0) THEN
      a = vectnormuniform(n, x)
      IF (p .EQ. huge(p)) THEN
        vectnormp = a
        RETURN
      END IF
      IF (a .GT. 1.0d-35) THEN
        a1 = 1.0d00/a
      ELSE
        a1 = 0.0d00
      END IF
      DO i = 1, n
        vectnormp = vectnormp+abs(a1*x(i))**p
      END DO
      vectnormp = a*(vectnormp)**(1.0d00/p)
    END IF
  END FUNCTION vectnormp
!> vector infinity norm
  FUNCTION vectnormuniform(n, x)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x
    IF (n .LE. 0) THEN
      vectnormuniform = 0.0d00
    ELSE
      vectnormuniform = abs(x(vectindexmaxabs(n, x)))
    END IF
  END FUNCTION vectnormuniform
!> permute elements in a vector
  SUBROUTINE vectorpermute(n, iperm, lis, ijob)
    INTEGER, DIMENSION(*)::iperm
    REAL(8), DIMENSION(*)::lis
    REAL(8), DIMENSION(n)::list
    CALL vectcopy(n, list, lis)
    SELECT CASE (ijob)
    CASE (1)
      DO i = 1, n
        IF (iperm(i) .EQ. 0) STOP "zero in vectorpermute"
        lis(i) = list(iperm(i))
      END DO
    CASE (2)
      DO i = 1, n
        IF (iperm(i) .EQ. 0) STOP "zero in vectorpermute"
        lis(iperm(i)) = list(i)
      END DO
    END SELECT
  END SUBROUTINE vectorpermute
!> print a vector
  SUBROUTINE vectorprint(mtx)
    REAL(8), DIMENSION(:)::mtx
    m = size(mtx)
    DO i = 1, m
      WRITE (*, "(a,i4,e15.6)") "row, value", i, mtx(i)
    END DO
  END SUBROUTINE vectorprint
!> permute a vector
  SUBROUTINE vectpermute(n, arr, per)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::arr
    REAL(8), DIMENSION(:), ALLOCATABLE::tmp
    INTEGER, DIMENSION(*)::per
    CALL allocsafe(n, tmp)
    CALL vectcopy(n, tmp, arr)
    DO i = 1, n
      arr(i) = tmp(per(i))
    END DO
    CALL allocsafe(0, tmp)
  END SUBROUTINE vectpermute
!> scale a vector
  SUBROUTINE vectscale(n, y, a, x)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x, y
    IF (abs(a-1.0d00) .LE. 1.0d-20) THEN
      CALL vectcopy(n, y, x)
    ELSE
      DO i = 1, n
        y(i) = a*x(i)
      END DO
    END IF
  END SUBROUTINE vectscale
!> sets a vector to a given constant
  SUBROUTINE vectsetconst(n, x, r)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::x
    REAL(8), OPTIONAL::r
    IF (n .LE. 0) RETURN
    IF (present(r)) THEN
      DO i = 1, n
        x(i) = r
      END DO
    ELSE
      DO i = 1, n
        x(i) = 0.0d00
      END DO
    END IF
  END SUBROUTINE vectsetconst
!> sorts a vector
  SUBROUTINE vectsort(n, arr)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::arr
    INTEGER, DIMENSION(n)::per
    CALL vectsortpermutation(n, arr, per)
    CALL vectpermute(n, arr, per)
  END SUBROUTINE vectsort
!> calculates an ordering permutation with heapsort
  SUBROUTINE vectsortpermutation(n, arr, per)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(n)::arr
    INTEGER, DIMENSION(n)::per
    DO i = 1, n
      per(i) = i
    END DO
    DO i = n/2, 1, -1
      CALL listsd(i, n)
    END DO
    DO i = n, 2, -1
      l = per(i)
      per(i) = per(1)
      per(1) = l
      CALL listsd(1, i-1)
    END DO
  CONTAINS
    SUBROUTINE listsd(l, m)
      IMPLICIT REAL(8) (a-h, o-z)
      INTEGER::m
      ia = per(l)
      a = arr(ia)
      jold = l
      j = l+l
      DO
        IF (j .GT. m) EXIT
        IF (j .LT. m) THEN
          IF (arr(per(j)) < arr(per(j+1))) j = j+1
        END IF
        IF (a .GE. arr(per(j))) EXIT
        per(jold) = per(j)
        jold = j
        j = j+j
      END DO
      per(jold) = ia
    END SUBROUTINE listsd
  END SUBROUTINE vectsortpermutation
!> vector trim to size n
  SUBROUTINE vecttrim(n, l)
    REAL(8), DIMENSION(:), ALLOCATABLE::l, lt
    IF (n .LE. 0) THEN
      IF (allocated(l)) DEALLOCATE (l)
    ELSE
      IF (allocated(l)) THEN
        nl = size(l)
        ALLOCATE (lt(n))
        CALL vectcopy(n, lt, l)
        CALL allocsafe(n, l)
        CALL vectcopy(n, l, lt)
        DEALLOCATE (lt)
      ELSE
        CALL allocsafe(n, l)
      END IF
    END IF
  END SUBROUTINE vecttrim
!> obtain a voigt form from a matrix
  SUBROUTINE voigtfrommatrix(ic1, ic2, ic, ncd)
    IF (ic1 .EQ. ic2) THEN
      ic = ic1
    ELSE
      ic = ic1+ic2-2+ncd
    END IF
  END SUBROUTINE voigtfrommatrix
!> obtain a dimension from the number of voigt dimensions
  INTEGER FUNCTION voigtgetdimension(nvoigt)
    SELECT CASE (nvoigt)
    CASE (1)
      ndi = 1
    CASE (2)
      ndi = 1
    CASE (3)
      ndi = 2
    CASE (4)
      ndi = 3
    CASE (6)
      ndi = 3
    CASE default
      STOP "error in voigtgetdimension"
    END SELECT
    voigtgetdimension = ndi
  END FUNCTION voigtgetdimension
!> obtain matrix indices from the voigt form
  SUBROUTINE voigttomatrix(ic1, ic2, ic, ncd)
    IF (ic .GT. ncd) THEN
      SELECT CASE (ic-ncd)
      CASE (1)
        ic1 = 1
        ic2 = 2
      CASE (2)
        ic1 = 1
        ic2 = 3
      CASE (3)
        ic1 = 2
        ic2 = 3
      CASE default
        ic1 = 1000
        ic2 = 1000
      END SELECT
    ELSE
      ic1 = ic
      ic2 = ic
    END IF
  END SUBROUTINE voigttomatrix
END MODULE basfun
!> module bucketsort
MODULE bucketsort
  USE basfun
  SAVE
  PRIVATE
  PUBLIC::bucketcoordnode, bucketcoordpolygon, bucketnodenode, bucketpolypoly, bucketpolygonnode, bucketnodepolygon, bucketcreate, bucketss, bucketpca, wbucketpca
  TYPE bucketss
    INTEGER, DIMENSION(3)::nbd
    INTEGER, DIMENSION(:), ALLOCATABLE::bn, bp, k1, k2, l1, l2
    INTEGER::npoin = 0
    INTEGER::npoly = 0
    INTEGER::ntb = 0
    REAL(8), DIMENSION(3)::xmi, xma
    REAL(8), DIMENSION(3, 3)::vcp
  END TYPE bucketss
CONTAINS
!> given coordinates x, returns the list of NODES that surround the coordinates
  SUBROUTINE bucketcoordnode(bs, x, nn, ln)
    TYPE(bucketss)::bs
    REAL(8), DIMENSION(3)::x, xt
    INTEGER, DIMENSION(:), ALLOCATABLE::ln
    INTEGER, DIMENSION(27)::iviz
    DO j = 1, 3
      xt(j) = vectdot(3, bs%vcp(1:3, j), x(1:3))
    END DO
    CALL bucketdeterminebuckets(nviz, bucketglobalfromcoordinates(bs%xmi, bs%xma, xt, bs%nbd), iviz, bs%nbd)
    nn = 0
    DO i = 1, nviz
      ib = iviz(i)
      nn = bs%k1(ib+1)-bs%k1(ib)+nn
    END DO
    CALL allocsafe(nn, ln)
    k = 0
    DO j = 1, nviz
      ib = iviz(j)
      DO i = bs%k1(ib), bs%k1(ib+1)-1
        k = k+1
        ln(k) = bs%l1(i)
      END DO
    END DO
  END SUBROUTINE bucketcoordnode
!> given coordinates x, returns the list of POLYGONS/POLYHEDRA that surround the coordinates
  SUBROUTINE bucketcoordpolygon(bs, x, nn, ln)
    TYPE(bucketss)::bs
    REAL(8), DIMENSION(3)::x, xt
    INTEGER, DIMENSION(:), ALLOCATABLE::ln
    INTEGER, DIMENSION(27)::iviz
    DO j = 1, 3
      xt(j) = vectdot(3, bs%vcp(1:3, j), x(1:3))
    END DO
    CALL bucketdeterminebuckets(nviz, bucketglobalfromcoordinates(bs%xmi, bs%xma, xt, bs%nbd), iviz, bs%nbd)
    nn = 0
    DO i = 1, nviz
      ib = iviz(i)
      nn = bs%k2(ib+1)-bs%k2(ib)+nn
    END DO
    CALL allocsafe(nn, ln)
    k = 0
    DO j = 1, nviz
      ib = iviz(j)
      DO i = bs%k2(ib), bs%k2(ib+1)-1
        k = k+1
        ln(k) = bs%l2(i)
      END DO
    END DO
  END SUBROUTINE bucketcoordpolygon
!> given a node, returns the list of NODES that surround that node
  SUBROUTINE bucketnodenode(bs, in, nn, ln)
    TYPE(bucketss)::bs
    INTEGER, DIMENSION(:), ALLOCATABLE::ln
    INTEGER, DIMENSION(27)::iviz
    jb = bs%bn(in)
    CALL bucketdeterminebuckets(nviz, jb, iviz, bs%nbd)
    nn = 0
    DO i = 1, nviz
      ib = iviz(i)
      nn = bs%k1(ib+1)-bs%k1(ib)+nn
    END DO
    CALL allocsafe(nn, ln)
    k = 0
    DO j = 1, nviz
      ib = iviz(j)
      DO i = bs%k1(ib), bs%k1(ib+1)-1
        k = k+1
        ln(k) = bs%l1(i)
      END DO
    END DO
  END SUBROUTINE bucketnodenode
!> given a polygon/polyhedron, determines the list of POLYGONS/POLYHEDRA
!> that surround the polygon/polyhedron
  SUBROUTINE bucketpolypoly(bs, in, nn, ln)
    TYPE(bucketss)::bs
    INTEGER, DIMENSION(:), ALLOCATABLE::ln
    INTEGER, DIMENSION(27)::iviz
    jb = bs%bp(in)
    CALL bucketdeterminebuckets(nviz, jb, iviz, bs%nbd)
    nn = 0
    DO i = 1, nviz
      ib = iviz(i)
      nn = bs%k2(ib+1)-bs%k2(ib)+nn
    END DO
    CALL allocsafe(nn, ln)
    k = 0
    DO j = 1, nviz
      ib = iviz(j)
      DO i = bs%k2(ib), bs%k2(ib+1)-1
        k = k+1
        ln(k) = bs%l2(i)
      END DO
    END DO
  END SUBROUTINE bucketpolypoly
!> given a polygon/polyhedron determines the list of NODES that surround the polygon/polyhedron
  SUBROUTINE bucketpolygonnode(bs, in, nn, ln)
    TYPE(bucketss)::bs
    INTEGER, DIMENSION(:), ALLOCATABLE::ln
    INTEGER, DIMENSION(27)::iviz
    jb = bs%bp(in)
    CALL bucketdeterminebuckets(nviz, jb, iviz, bs%nbd)
    nn = 0
    DO i = 1, nviz
      ib = iviz(i)
      nn = bs%k1(ib+1)-bs%k1(ib)+nn
    END DO
    CALL allocsafe(nn, ln)
    k = 0
    DO j = 1, nviz
      ib = iviz(j)
      DO i = bs%k1(ib), bs%k1(ib+1)-1
        k = k+1
        ln(k) = bs%l1(i)
      END DO
    END DO
  END SUBROUTINE bucketpolygonnode
!> given a node determines the list of POLYGONS/POLYHEDRA that surround the polygon/polyhedron
  SUBROUTINE bucketnodepolygon(bs, in, nn, ln)
    TYPE(bucketss)::bs
    INTEGER, DIMENSION(:), ALLOCATABLE::ln
    INTEGER, DIMENSION(27)::iviz
    jb = bs%bn(in)
    CALL bucketdeterminebuckets(nviz, jb, iviz, bs%nbd)
    nn = 0
    DO i = 1, nviz
      ib = iviz(i)
      nn = bs%k2(ib+1)-bs%k2(ib)+nn
    END DO
    CALL allocsafe(nn, ln)
    k = 0
    DO j = 1, nviz
      ib = iviz(j)
      DO i = bs%k2(ib), bs%k2(ib+1)-1
        k = k+1
        ln(k) = bs%l2(i)
      END DO
    END DO
  END SUBROUTINE bucketnodepolygon
!> from a number of nodes nn/xc and a number of polyhedra np/ip/jp
!> creates a bucket data structure bs
  SUBROUTINE bucketcreate(bs, nn, np, ip, jp, xc)
    IMPLICIT REAL(8) (a-h, o-z)
    TYPE(bucketss)::bs
    REAL(8), DIMENSION(:, :), ALLOCATABLE::xcoo
    REAL(8), DIMENSION(:), ALLOCATABLE::xtemp
    INTEGER, DIMENSION(*)::ip, jp
    REAL(8), DIMENSION(3, *)::xc
    REAL(8), DIMENSION(3)::a, b, atr, dmm
    bs%npoin = nn
    bs%npoly = np
    CALL allocsafe(3, bs%npoin, xcoo)
    CALL allocsafe(bs%npoin, bs%bn)
    CALL allocsafe(bs%npoly, bs%bp)
    CALL bucketpca(bs%npoin, xc, bs%vcp)
    CALL allocsafe(BS%NPOIN, Xtemp)
    DO i = 1, bs%npoin
      DO j = 1, 3
        xcoo(j, i) = vectdot(3, bs%vcp(1:3, j), xc(1:3, i))
      END DO
    END DO
    CALL vectsetconst(3, dmm, 0.0d00)
    CALL vectsetconst(3, bs%xmi, 0.0d00)
    CALL vectsetconst(3, bs%xma, 0.0d00)
    DO i = 1, bs%npoly
      ist = ip(i)
      ifi = ip(i+1)
      nnp = ifi-ist
      DO k = 1, 3
        a(k) = -huge(1.0d00)
        b(k) = huge(1.0d00)
      END DO
      DO j = 1, nnp
        in = jp(j-1+ist)
        DO k = 1, 3
          a(k) = max(a(k), xcoo(k, in))
          b(k) = min(b(k), xcoo(k, in))
        END DO
      END DO
      DO k = 1, 3
        dmm(k) = max(dmm(k), a(k)-b(k))
      END DO
    END DO
    DO i = 1, 3
      DO in = 1, nn
        xtemp(in) = xcoo(i, in)
      END DO
      bs%xmi(i) = vectindexmin(nn, xtemp)
      bs%xma(i) = vectindexmax(nn, xtemp)
      atr(i) = bs%xma(i)-bs%xmi(i)
    END DO
    CALL bucketdetermineglobals(atr, dmm, bs%nbd, bs%ntb)
    DO i = 1, bs%npoin
      bs%bn(i) = bucketglobalfromcoordinates(bs%xmi, bs%xma, xcoo(1:3, i), bs%nbd)
    END DO
    DO i = 1, bs%npoly
      j = jp(ip(i))
      bs%bp(i) = bucketglobalfromcoordinates(bs%xmi, bs%xma, xcoo(1:3, j), bs%nbd)
    END DO
    CALL allocsafe(bs%npoin, bs%l1)
    CALL allocsafe(bs%npoly, bs%l2)
    CALL allocsafe(bs%ntb+1, bs%k1)
    CALL allocsafe(bs%ntb+1, bs%k2)
    CALL manymanymatrixtransposeoseindicesfromlist(bs%npoin, bs%ntb, bs%k1, bs%bn, bs%l1)
    CALL manymanymatrixtransposeoseindicesfromlist(bs%npoly, bs%ntb, bs%k2, bs%bp, bs%l2)
    CALL allocsafe(0, 0, xcoo)
    CALL allocsafe(0, xtemp)
  END SUBROUTINE bucketcreate
!> principal component analysis-determination
!> of eigenvectors vcp as a function of n nodes x
  SUBROUTINE bucketpca(n, x, vcp)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, *)::x
    REAL(8), DIMENSION(3)::temp, vlp
    INTEGER, DIMENSION(3)::per
    REAL(8), DIMENSION(3, 3)::vcp
    IF (n .LE. 0) RETURN
    rconst = 1.0d00/n
    CALL vectsetconst(3, vlp, 0.0d00)
    DO i = 1, n
      vlp = vlp+x(1:3, i)*rconst
    END DO
    CALL vectsetconst(9, vcp, 0.0d00)
    DO i = 1, n
      temp = x(1:3, i)-vlp
      CALL matrixtensorupdate(3, vcp, temp, temp)
    END DO
    CALL vectscale(9, vcp, rconst, vcp)
    CALL matrixeigensystem(vcp, vlp, vcp, 3, ierr)
    CALL vectsortpermutation(3, vlp, per)
    CALL vectpermute(3, vlp, per)
    DO i = 1, 3
      CALL vectpermute(3, vcp(i, 1:3), per)
    END DO
  END SUBROUTINE bucketpca
!> determines the buckets for a given global index indb
  SUBROUTINE bucketdeterminebuckets(nviz, indb, iviz, nbd)
    INTEGER, DIMENSION(3)::jnc, ind
    INTEGER, DIMENSION(*)::iviz, nbd
    CALL listsetconstant(27, iviz, 0)
    CALL bucketlocalindices(indb, nbd, ind)
    nviz = 0
    DO i = -1, 1
      jnc(1) = ind(1)+i
      IF (jnc(1) .GT. nbd(1) .OR. jnc(1) .LE. 0) CYCLE
      DO j = -1, 1
        jnc(2) = ind(2)+j
        IF (jnc(2) .GT. nbd(2) .OR. jnc(2) .LE. 0) CYCLE
        DO k = -1, 1
          jnc(3) = ind(3)+k
          IF (jnc(3) .GT. nbd(3) .OR. jnc(3) .LE. 0) CYCLE
          nviz = nviz+1
          iviz(nviz) = bucketglobalindex(nbd, jnc)
        END DO
      END DO
    END DO
  END SUBROUTINE bucketdeterminebuckets
!> determines a unique index from the set of
!> indices in each direction
  INTEGER FUNCTION bucketglobalindex(nbd, ind)
    INTEGER, DIMENSION(*)::ind, nbd
    bucketglobalindex = ind(1)+nbd(1)*(ind(2)-1)+nbd(1)*nbd(2)*(ind(3)-1)
  END FUNCTION bucketglobalindex
!> from a unique index, determines the buckets
!> in 3 directions
!> prof. timon rabzuck detected a bug in previous version
  SUBROUTINE bucketlocalindices(indt, nbd, ind)
    INTEGER, DIMENSION(*)::ind, nbd
    nx = nbd(1)
    ny = nbd(2)
    nxy = nx*ny
    ind(3) = int((indt-1)/nxy)+1
    jndt = indt-(ind(3)-1)*nxy
    ind(2) = int((jndt-1)/nx)+1
    ind(1) = mod(jndt-1, nx)+1
  END SUBROUTINE bucketlocalindices
!> obtain the bucket number from a given set
!> of coordinates
  INTEGER FUNCTION bucketglobalfromcoordinates(xmi, xma, xcu, nbd)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(*)::xmi, xma, xcu
    INTEGER, DIMENSION(*)::nbd
    INTEGER, DIMENSION(3)::pc
    DO i = 1, 3
      rtemp = xma(i)-xmi(i)
      IF (abs(rtemp) .GT. tiny(rtemp)) THEN
        nn = int(nbd(i)*(xcu(i)-xmi(i))/(xma(i)-xmi(i)))+1
      ELSE
        nn = 1
      END IF
      nn = min(max(1, nn), nbd(i))
      pc(i) = nn
    END DO
    bucketglobalfromcoordinates = bucketglobalindex(nbd, pc)
  END FUNCTION bucketglobalfromcoordinates
!> given the extension
!> and maximum dimensions in each direction
!> determines the number of buckets in each direction
!> and the total number of buckets
  SUBROUTINE bucketdetermineglobals(atr, dmm, nbd, ntb)
    IMPLICIT REAL(8) (a-h, o-z)
    INTEGER, PARAMETER::nbdm = 1000
    INTEGER, DIMENSION(*)::nbd
    REAL(8), DIMENSION(*)::atr, dmm
    REAL(8), DIMENSION(3)::dmmc
    imi = vectindexmin(3, dmm)
    tol = (1.0d-6*dmm(imi)+tiny(tol))
    DO i = 1, 3
      dmmc(i) = dmm(i)+tol
    END DO
    DO i = 1, 3
      nbd(i) = min(max(1, int(atr(i)/dmmc(i))), nbdm)
    END DO
    ntb = bucketglobalindex(nbd, nbd)
  END SUBROUTINE bucketdetermineglobals
END MODULE bucketsort
!> module linear, provides an interface to the sparse solver
!> and also assembling facilities
MODULE linear
  USE basfun
  SAVE
!-------------------
!*** assembler type
!-------------------
  TYPE::lineartype
!------------------
!*** symbolic flag
!------------------
    LOGICAL::symbo = .TRUE. ! symbolic starts with true
!------------------
!*** symbolic flag
!------------------
    LOGICAL::symm = .TRUE.  ! symbolic starts with true
!------------------------
!*** relevant dimensions
!------------------------
    INTEGER::ntgl = 0 ! maximum number of dof types per node
    INTEGER::ndof = 0 ! number of dof corresponding to the original assignment
    INTEGER::ntyp = 0 ! number of types, which is inferred from the input data
!-----------------------------------------------------------------------
!*** mapping between degrees-of-freedom and types
!*** types can be related to the original node / type of dof assignment
!-----------------------------------------------------------------------
    INTEGER, DIMENSION(:), ALLOCATABLE::dofoftype, typeofdof       ! dof of type, type of dof
!---------------------------------------
!*** elements and additive constituents
!---------------------------------------
    INTEGER::nel = 0                                               ! number of elements
    INTEGER, DIMENSION(:), ALLOCATABLE::ieldof, jeldof, jeldoftyp  ! element pointer,dof,type
!-----------------------------------------------
!*** global stiffness addressing by the cliques
!-----------------------------------------------
    INTEGER, DIMENSION(:), ALLOCATABLE::iclique, jclique           ! iclique and jclique
    REAL(8), DIMENSION(:), ALLOCATABLE::cliquematrix, cliquevector ! element matrix and vector
!---------
!*** mpcs
!---------
    INTEGER::nmp = 0                                     ! number of mpcs
    INTEGER, DIMENSION(:), ALLOCATABLE::mpctyp           ! slave dof,type
    REAL(8), DIMENSION(:), ALLOCATABLE::mpcvec           ! destination
!---------------------
!*** global stiffness
!---------------------
    INTEGER, DIMENSION(:), ALLOCATABLE::idofdof, jdofdof ! matrix pointer and destination row-by-row
    REAL(8), DIMENSION(:), ALLOCATABLE::matrix           ! matrix coefficients
!-----------------------------------------
!*** reduced right hand side and solution
!-----------------------------------------
    REAL(8), DIMENSION(:), ALLOCATABLE::force, soluc     ! force and solution
!--------------
!*** reactions
!--------------
    REAL(8), DIMENSION(:), ALLOCATABLE::react            ! reaction
  END TYPE lineartype
CONTAINS
!> starts assembling with an ass assembler
!> with nel elements and ntgl number of dofs PER node
  SUBROUTINE linearstart0(ass, nel, ntgl)
    TYPE(lineartype)::ass
!*** number of elements
!*** number of mpcs
!*** number of dofs
!*** number of dof types
!*** symbolic analysis
    ass%nel = nel
    ass%nmp = 0
    ass%ntgl = ntgl
    ass%symbo = .TRUE.
    ass%symm = .TRUE.
!*** allocates element stuff
    CALL allocsafe(ass%nel+1, ass%ieldof)
    CALL allocsafe(ass%nel+1, ass%iclique)
  END SUBROUTINE linearstart0
!> after the first stage of assembling (dimensions), calls linearend0
!> to close the assembling process
  SUBROUTINE linearend0(ass)
    IMPLICIT REAL(8) (a-h, o-z)
    TYPE(lineartype)::ass
    CALL manymanycreaterowpointersandallocate(ass%nel, ass%ieldof, ass%jeldoftyp)
    CALL manymanycreaterowpointersandallocatearray(ass%nel, ass%ieldof, ass%cliquevector)
    CALL manymanycreaterowpointersandallocatearray(ass%nel, ass%iclique, ass%cliquematrix)
  END SUBROUTINE linearend0
!> after the second stage of assembling (pointers), calls linearend1
  SUBROUTINE linearend1(ass)
    IMPLICIT REAL(8) (a-h, o-z)
    TYPE(lineartype)::ass
!--------------------------------------
!*** determines the number of types
!*** by going through the constituents
!--------------------------------------
    ass%ntyp = 0
    DO ktyp = 1, ass%ieldof(ass%nel+1)-1  ! elements
      ass%ntyp = max(ass%ntyp, ass%jeldoftyp(ktyp))
    END DO
!-------------------------------
!*** allocates dof destinations
!-------------------------------
    CALL allocsafe(ass%ieldof(ass%nel+1)-1, ass%jeldof) ! element destinations
    CALL allocsafe(ass%ntyp, ass%dofoftype)             ! given type, provides dof
!-----------------------------------
!*** determines the number of dofs
!*** and assigns dofs to elements
!*** (jeldof)
!*** finally, relates type with dof
!-----------------------------------
    ass%ndof = 0
!------------------------
!*** now elements
!*** assigns to elements
!*** (jeldof)
!------------------------
    DO iel = 1, ass%nel
      DO kedof = ass%ieldof(iel), ass%ieldof(iel+1)-1
        ityp = ass%jeldoftyp(kedof)
        idof = ass%dofoftype(ityp)
        IF (idof .EQ. 0) THEN
          ass%ndof = ass%ndof+1
          ass%dofoftype(ityp) = ass%ndof
          idof = ass%ndof
        END IF
        ass%jeldof(kedof) = idof
      END DO
    END DO
!--------------------------------------
!*** allocates typeofdof
!*** dofoftype may be over-dimensioned
!*** i.e. dofoftype may have zeros
!--------------------------------------
    CALL allocsafe(ass%ndof, ass%typeofdof)
!------------------------------------------------
!*** calculates the matrixtransposeosed relation
!------------------------------------------------
    DO ityp = 1, ass%ntyp
      IF (ass%dofoftype(ityp) .NE. 0) ass%typeofdof(ass%dofoftype(ityp)) = ityp
    END DO
    WRITE (*, *) "ass%ndof", ass%ndof
!-----------------------------------------------------------
!*** performs the symbolic assembling process and allocates
!*** the global stiffness
!*** manymanyfromcliquespart1 also removes duplicates
!*** it alters ieldof and jeldof
!-----------------------------------------------------------
    CALL manymanyfromcliquespart1(ass%nel, ass%ieldof, ass%jeldof, ass%iclique, ass%jclique, ass%ndof, ass%idofdof, ass%jdofdof)
!-----------------
!*** opens spaces
!-----------------
    CALL manymanyfromcliquespart2(ass%ndof, ass%idofdof, ass%matrix)
    CALL allocsafe(ass%ndof, ass%force)
    CALL allocsafe(ass%ndof, ass%soluc)
    CALL allocsafe(ass%ndof, ass%react)
    WRITE (*, *) "ass%ndof", ass%ndof
  END SUBROUTINE linearend1
!> starts the linear assembling
!> by resetting the mpcs
  SUBROUTINE linearstart1(ass)
    TYPE(lineartype)::ass
    ass%nmp = 0
  END SUBROUTINE linearstart1
!> assembling with mpcs
!> but does not solve
!> it calculates the reactions
  SUBROUTINE linearassemblingnumeric(ass)
    IMPLICIT REAL(8) (a-h, o-z)
    TYPE(lineartype)::ass
!-----------------------
!*** sets stuff to zero
!-----------------------
    CALL vectsetconst(ass%ndof, ass%force)
    CALL vectsetconst(ass%ndof, ass%soluc)
    CALL vectsetconst(ass%ndof, ass%react)
    CALL manymanyfromcliquespart3(ass%ndof, ass%nel, ass%ieldof, ass%jeldof, ass%iclique, ass%jclique, ass%cliquematrix, ass%idofdof, ass%matrix)
!-----------------------
!*** main element cycle
!-----------------------
    DO iel = 1, ass%nel
      nedof = ass%ieldof(iel+1)-ass%ieldof(iel)
      DO jedof = 1, nedof
        jtemp = ass%ieldof(iel)-1+jedof
        jdof = ass%jeldof(jtemp)
!---------------------------------------------------
!*** adds force to right-hand-side
!*** where the sign "-" is adopted to comply
!*** with convention cliquevector=internal-external
!---------------------------------------------------
        ass%force(jdof) = ass%force(jdof)-ass%cliquevector(jtemp)
        ass%react(jdof) = ass%react(jdof)-ass%cliquevector(jtemp)
      END DO
    END DO
!------------------------------------------------------------
!*** main mpc cycle
!*** sets stifness diagonal to 1 and out-of-diagonal to zero
!------------------------------------------------------------
    rmax = 0.0d00
    DO imp = 1, ass%nmp
      ityp = ass%mpctyp(imp)
      IF (ityp .LE. ass%ntyp) THEN
        idof = ass%dofoftype(ityp)
        IF (idof .GT. 0) THEN
          DO ik = ass%idofdof(idof), ass%idofdof(idof+1)-1
            IF (ass%jdofdof(ik) .EQ. idof) THEN
              rmax = max(rmax, abs(ass%matrix(ik)))
            END IF
          END DO
        END IF
      END IF
    END DO
    rmax = 1.0d4*rmax
    IF (abs(rmax) .LE. 1.0d-20) rmax = 1.0d9
!------------------------------------------------------------
!*** now alters force and stiffness to impose the constraint
!*** in a penalty-type fashion
!------------------------------------------------------------
    DO imp = 1, ass%nmp
      ityp = ass%mpctyp(imp)
      IF (ityp .LE. ass%ntyp) THEN
        idof = ass%dofoftype(ityp)
        IF (idof .GT. 0) THEN
          ass%force(idof) = ass%mpcvec(imp)*rmax
          DO ik = ass%idofdof(idof), ass%idofdof(idof+1)-1
            IF (ass%jdofdof(ik) .EQ. idof) THEN
              ass%matrix(ik) = rmax
            END IF
          END DO
        END IF
      END IF
    END DO
  END SUBROUTINE linearassemblingnumeric
!> linear solution
  SUBROUTINE linearsolver(ass, soluc, react, resid)
    IMPLICIT REAL(8) (a-h, o-z)
    TYPE(lineartype)::ass
    REAL(8), DIMENSION(:, :)::soluc, react
!--------------------------------------------------------------------
!*** now solves the problem using
!*** a sparse solver of the choice (given as an external subroutine)
!--------------------------------------------------------------------
    CALL manymanymklsolve(ass%symbo, ass%symm, 1, ass%ndof, ass%idofdof, ass%jdofdof, ass%matrix, ass%soluc, ass%force)
!--------------------------------------------------------------------------
!*** now resets the force values corresponding to the imposed dofs to zero
!--------------------------------------------------------------------------
    DO imp = 1, ass%nmp
      ityp = ass%mpctyp(imp)
      IF (ityp .LE. ass%ntyp) THEN
        idof = ass%dofoftype(ityp)
        IF (idof .GT. 0) ass%force(idof) = 0.0d00
      END IF
    END DO
!------------------------
!*** sets symbo to false
!------------------------
    ass%symbo = .FALSE.
!---------------------------------------------
!*** and calculates the euclidean norm of rhs
!---------------------------------------------
    resid = vectnorm2(ass%ndof, ass%force)
!-------------------------
!*** maps to the original
!-------------------------
    DO ityp = 1, ass%ntyp
      idof = ass%dofoftype(ityp)
      ino = 1+(ityp-1)/ass%ntgl
      igl = scalarmodularposition(ass%ntgl, ityp)
      IF (idof .GT. 0) THEN
        soluc(igl, ino) = ass%soluc(idof)
        react(igl, ino) = -ass%react(idof)
      ELSE
        soluc(igl, ino) = 0.0d00
        react(igl, ino) = 0.0d00
      END IF
    END DO
  END SUBROUTINE linearsolver
!> insert the number of dofs for a given element
  SUBROUTINE linearstorepart0(ass, iel, nedof)
    TYPE(lineartype)::ass
    ass%ieldof(iel) = nedof
    ass%iclique(iel) = nedof*nedof
  END SUBROUTINE linearstorepart0
!> insert the NODES and DOF types (1 to ntgl) for a given element
  SUBROUTINE linearstorepart1(ass, iel, lnods, ltyps)
    TYPE(lineartype)::ass
    INTEGER, DIMENSION(*)::lnods, ltyps
    DO jedof = 1, ass%ieldof(iel+1)-ass%ieldof(iel)
      local = ass%ieldof(iel)-1+jedof
      ass%jeldoftyp(local) = (lnods(jedof)-1)*ass%ntgl+ltyps(jedof)
    END DO
  END SUBROUTINE linearstorepart1
!> stores a given element force and stiffness
  SUBROUTINE linearstorepart2(ass, iel, symm, nedof, efor, emat)
    IMPLICIT REAL(8) (a-h, o-z)
    LOGICAL::symm
    TYPE(lineartype)::ass
    REAL(8), DIMENSION(*)::efor
    REAL(8), DIMENSION(nedof, *)::emat
    IF (.NOT. symm) ass%symm = .FALSE.
    IF (nedof .NE. ass%ieldof(iel+1)-ass%ieldof(iel)) THEN
      WRITE (*, *) "iel=", iel
      WRITE (*, *) "nedof=", nedof
      WRITE (*, *) "incompatible with previously declared dimension"
      STOP
    END IF
    DO jedof = 1, nedof
      local = ass%ieldof(iel)-1+jedof
      ass%cliquevector(local) = efor(jedof)
      DO iedof = 1, nedof
        ass%cliquematrix(manymanyindexincliquematrix(ass%iclique, ass%ieldof, iel, iedof, jedof)) = emat(iedof, jedof)
      END DO
    END DO
  END SUBROUTINE linearstorepart2
!> stores a boundary condition
  SUBROUTINE linearstoreboundarycondition(ass, mnods, mtyps, val)
    IMPLICIT REAL(8) (a-h, o-z)
    TYPE(lineartype)::ass
    INTEGER::mnods, mtyps
    ass%nmp = ass%nmp+1
    CALL vectinsert(ass%mpcvec, ass%nmp, val)
    CALL listinsert(ass%mpctyp, ass%nmp, (mnods-1)*ass%ntgl+mtyps)
  END SUBROUTINE linearstoreboundarycondition
END MODULE linear
