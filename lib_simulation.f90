!*********************************************************************************************
!*** The Input FROM THE entra FILE IS SUPPOSED TO BE ORGANIZED, IF NOT COMPLETELY, AT LEAST
!*** IN A WAY THAT NO EMPTY OR GARBAGE LINES OCCUR BETWEEN NODE AND ELEMENT INPUT LINES
!*** PRELIMINARY CHECKS MUST HAVE BEEN DONE TO AVOID DIFFICULT TO TRACE EXECUTION
!*** ERRORS
!*********************************************************************************************
!
!*************************************************************
!*** QUANTITIES DEFINED OUTSIDE RESTART WILL BE FLAGGED O.S.R.
!*** IN THIS CASE, INPUT FILES WILL STILL BE ACTIVE
!*************************************************************
!
!*********************************************************************************************
!*** DEFAULT VARIABLES AND POSITIONS
!*** SHOULD NOT BE CHANGED
!*** AND NEW CREATIONS SHOULD COMPLY WITH THE NOMENCLATURE
!*** ADDED QUANTITIES SHOULD COMPLY WITH THE LIMITS (FEW ARRAY BOUNDARIES ARE CHECKED)
!*** BE ESPECIALLY CAREFUL WITH GPHIST AND ELHIST: PLEASE DO NOT OVERLAP PRE-DEFINED INDICES
!*********************************************************************************************
!
!****************************************************************************************************************************
!*** WARNING: THE FIRST 2 GAUSS POINTS IN GPHIST ARE RESERVED FOR OUTPUTTING QUANTITIES ONLY DO NOT OVERLAP
!***          THE FIRST GAUSS POINT CORRESPONDS TO THE <<LOWER LAYER AND
!***          THE SECOND GAUSS POINT CORRESPONDS TO THE >>UPPER LAYER
!***          IF THERE IS ONLY ONE LAYER (USUALLY THE LOWER) THEN THE UPPER LAYER GIVES THE SAME VALUE
!***          BOTH ARE OCCUPIED
!***          ELEMENTS THAT WISH TO DO SO SHOULD PASS XI3=0 TO THE MATERIAL ROUTINE TO ENSURE THAT
!***          THE ELEMENT WILL ONLY PASS VALUES FOR THE LOWER LAYER
!***          (GPHIST AND GPSTRE)
!***          THESE TWO GAUSS POINTS ARE SAID TO BE COSMETIC
!***
!*** WARNING: ELEMENTS WITHOUT MATERIALS WILL STILL HAVE A ZERO MATERIAL INSERTED
!***          THIS WILL BE USEFUL WHEN PERFORMING TOPOLOGY QUESTIONS: ZERO MATERIAL ELEMENTS
!***          ARE BODY FORCE, EDGE FORCE, POINT FORCE, ETC
!***
!*** WARNING: CONTINUUM OR SHELL ELEMENTS CANNOT SHARE CONNECTIVITIES; I.E. 12 3 15 2 IDENTIFIES AT
!***          MOST 1 ELEMENT WITH NONZERO MATERIAL AND ANY NUMBER OF ZERO MATERIAL ELEMENTS
!***
!*** WARNING: NEVER USE THE UPDATED VARIABLE (2), ONLY CALCULATE IT
!***          NEVER CALCULATE THE PREVIOUS VARIABLE (1), ONLY USE IT
!***          *** FAILURE TO COMPLY WITH THIS WILL LIKELY PRODUCE WRONG RESULTS ***
!***          *** BE VERY CAUTIOUS WITH THIS REQUIREMENT ***
!***
!*** WARNING: C++ RESTART READINGS AND WRITINGS SHOULD BE SYNCRONIZED WITH THE PRESENT CONVENTION
!***          THIS INCLUDES THE CONSTANT PARAMETERS FOR DIMENSIONING
!***          IN THE FUTURE THIS CAN SIT IN A FILE
!***
!*** CONVENTIONS: A) WHEN PERFORMING CERTAIN OPERATIONS, A NEGATIVE VALUE FOR A FACE, AN ELEMENT, A NODE, ETC WILL ASK FOR
!***                 DEFORMED VALUES AND A POSITIVE ONE FOR ORIGINAL OR UNDEFORMED VALUES
!***
!***              B) LOADMODE=0 -> INTERPOLATION
!***                 LOADMODE=1 -> 1.0
!***                 LOADMODE=2 -> 0.0
!***                 LOADMODE=3 -> RLOAD
!***
!***                 INTERPOLATIONLOAD=0
!***                 ONELOAD=1
!***                 ZEROLOAD=2
!***                 PARLOAD=3
!***                 THIS IS THE MAIN FORM OF CONTROLLING THE RHS OF THE CODE
!
!*** GAUSS POINT HISTORY VARIABLES:
!*** GPHIST: 1 -         EFFECTIVE PLASTIC STRAIN OR EFFECTIVE EQUIVALENT STRAIN
!*** GPHIST: 2 -         DAMAGE OR VOLUME FRACTION (ITYPCRIT USES EITHER ONE REGARDLESS OF MEANING)
!*** GPHIST: 3 -         (HRATIO) RATIO BETWEEN THE CURRENT THICKNESS AND THE ORIGINAL THICKNESS
!*** GPHIST: 4 -         ELLIPTICITY INDICATOR OR ELASTIC WORK OR PLASTIC WORK
!*** GPHIST: 5 -         YIELD LOADING FLAG (1- IS LOADING PLASTICALLY, 0- IS NOT LOADING PLASTICALLY)
!*** GPHIST: 6 -         EQUIVALENT STRESS VALUE FOR OUTPUT
!*** GPHIST: 7:15 -      INVERSE PLASTIC DEFORMATION GRADIENT
!*** GPHIST: 16:18 -     AVAILABLE
!*** GPHIST: 19 -        UPPER BOUND ERROR ON INTEGRATION
!*** GPHIST: 20 -        PSEUDO YOUNG'S MODULUS
!*** GPHIST: 21 -        PSEUDO POISSON RATIO
!*** GPHIST: 22 -        SOURCE OF NONLOCAL MODELS
!*** GPHIST: 23:25 -     DX, DY, DZ
!*** GPHIST: 26:27 -     FREE
!*** GPHIST: 28:36 -     FRAME MAIN ANISOTROPIC DIRECTION
!*** GPHIST: 37 -        VOLUMETRIC PART OF THE ACCUMULATED PLASTIC STRAIN
!*** GPHIST: 38 -        EQUIVALENT STRAIN RATE
!*** GPHIST: 39 -        HEAT VALUE
!*** GPHIST: 40:45 -     VISCOELASTIC BACK STRESS
!
!*** STRESS COMPONENTS IN FRAME 0:
!*** GPSTRE: 1 SXX - GLOBAL COORDINATES STRESS COMPONENTS (KIRCHHOFF OR CAUCHY)
!*** GPSTRE: 2 SYY
!*** GPSTRE: 3 SZZ
!*** GPSTRE: 4 SXY
!*** GPSTRE: 5 SXZ
!*** GPSTRE: 6 SYZ - ALL COMPONENTS ARE SOMEHOW "ACTIVE" REGARDLESS OF ELEMENT
!
!*** WARNING:  THE FIRST 2 GAUSS POINTS IN GPHIST ARE RESERVED FOR OUTPUTTING QUANTITIES ONLY
!***           THE FIRST GAUSS POINT CORRESPONDS TO THE << LOWER LAYER AND
!***           THE SECOND GAUSS POINT CORRESPONDS TO THE >> UPPER LAYER
!***           IF THERE IS ONLY ONE LAYER (USUALLY THE LOWER) THEN THE UPPPER LAYER GIVES THE SAME VALUE
!***           ELEMENTS THAT WISH TO DO SO SHOULD PASS XI3=0.0 TO THE MATERIAL ROUTINE TO ENSURE THAT
!***           THE ELEMENT WILL ONLY PASS VALUES FOR THE LOWER LAYER
!***           (GPHIST AND GPSTRE)
!***           THESE TWO GAUSS POINTS ARE SAID TO BE COSMETIC
!***
!****************************************************************************************************************************
!
!***********************************************************************************************************
!*** ELHIST: 1:3 -       EXTRA NODES IN EDGE OR SURFACE ELEMENTS FOR CONTACT AND COHESIVE LAWS / ADDITIONAL
!***                     INTERNAL NODES / 2 void in element
!*** ELHIST: 4:6 -       CONTACT FORCES FOR CONTACT OR COHESIVE ELEMENTS
!*** ELHIST: 7 -         FIXED CHARACTERISTIC LENGTH (DETERMINED ONCE)
!*** ELHIST: 8 -         COHESIVE ACTIVATION FLAG (1 FOR COHESIVE AND 0 FOR CLOSED)
!*** ELHIST: 9 -         YOUNG'S MODULUS OR COHESIVE KINEMATIC HISTORY VALUE
!*** ELHIST: 10 -        STRAIN ENERGY
!*** ELHIST: 11:13 -     GAP HISTORY DATA FOR CONTACT ELEMENTS
!*** ELHIST: 14 -        USE CONTINUUM AS COHESIVE
!*** ELHIST: 15:23 -     ORTHOGONAL FRAME FOR FRACTURE EI,EII AND EIII AND BEAM STUFF
!*** ELHIST: 24:26 -     BEAM PSEUDONODES
!*** ELHIST: 28   -      TOTAL SENSITIVITY
!*** ELHIST: 29   -      DESIGN VARIABLE
!*** ELHIST: 30 -       >LOCAL OBJECTIVE FUNCTION FOR OPTIMIZATION (FIBERS AND OTHERS)
!*** ELHIST: 33 -       >LOCAL STRESS INTENSITY FOR OPTIMIZATION
!***********************************************************************************************************
!
!******************************************************************************************************************************************
!*** MAPS:   1 -         INITIAL THICKNESS, funcsmoothING PARAMETER, CROSS SECTION, NORMAL PRESSURE, PENALTY COEFFICIENT, DAMPING PARAMETER
!*** MAPS:   2:5 -       PRESSURE OR VOLUME LOAD VALUES (DEPENDING ON ELEMENT)
!                        OR FRICTION COEFFICIENT (2), INTERPOLATION OF NORMALS (3), SELF CONTACT (4),
!                        SHIFTED FACES (5) AND DECOUPLED (6 -> FOLLOWING NUMBER) (DEPENDING ON ELEMENT)
!                        (2) CROSS SECTION IN BEAMS
!*** MAPS:   6 -         CRACK CRITICAL VALUE (G1C, ELLIPTICITY, STRESS, VOID FRACTION) OR CONTACT DECOUPLED (DEPENDING ON ELEMENT)
!                        BEWARE THAT ONLY NON-CONTINUUM ELEMENTS ARE DECOUPLED
!*** MAPS:   7 -         PLANE STRESS ACTIVATION
!*** MAPS:   8 -         ORDERED PAIR NUMBER FOR FORCE/PRESSURE LOADING (MOVED FROM GUI)
!*** MAPS:   9 -         FIXED CHARACTERISTIC LENGTH (INPUT) OR
!                        STABILIZATION PARAMETER ACCORDING TO CONTEXT.
!                        IT CAN ALSO BE THE WIDTH OF THICK COHESIVE ELEMENTS
!                        OR THE MAXIMUM NUMBER OF NODES IN THE SUPPORT for EFG/MLS
!*** MAPS:   10 -        FINITE NORMAL STRAIN PLANE STRESS FLAG
!                        OR elementshapefunctionbubble ACTIVATION
!*** MAPS:   11 -        ANISOTROPIC ANGLE FOR SHELLS OR ANISOTROPIC ANGLE #1 FOR 3D
!*** MAPS:   12 -        NUMBER OF ADDITIONAL INTEGRATION POINTS ALONG THICKNESS WHEN APPLIED
!                        IF MAPS(12)=3 THEN 5 INTEGRATION POINTS ARE USED
!                        OR ANISOTROPIC ANGLE #2 FOR 3D
!*** MAPS:   13 -        MIXED FORMULATION
!*** MAPS:   14 -        GLOBAL rotationATION FLAG FOR ANISOTROPIC ANGLES
!                        1 -> ANGLES ARE RELATIVE TO GLOBAL COORDINATES
!*** MAPS:   15          MAXIMUM EFFECTIVE PLASTIC STRAIN FOR INITIATION
!*** MAPS:   16:18       NORMAL TO CRACK PRESCRIBED
!******************************************************************************************************************************************
!
!*** MAPR:   1 -         ELASTICITY MODULUS
!*** MAPR:   2 -         POISSON COEFFICIENT
!*** MAPR:   3 -         ORDERED PAIR FOR THE HARDENING LAW (TO BE CHANGED ASAP)
!*** MAPR:   4 -         MASS DENSITY
!*** MAPR:   5 -         THERMAL CONDUCTIVITY
!            6 -         SPECIFIC HEAT
!            7 -         PLASTIC DAMPING PARAMETER
!            8 -         AMBIENT TEMPERATURE
!            9 -         MELTING TEMPERATURE
!           10 -         TEMPERATURE EXPONENT IN JOHNSON-COOK LAW
!           11 -         COEFFICIENT OF THERMAL DILATATION (ALPHA)
!           12 -         TAYLOR-QUINNEY COEFFICIENT FOR HEAT GENERATION
!           13 -         YIELD FUNCTION
!           14 -         R11 OR A
!           15 -         R22 OR B
!           16 -         R33 OR C
!           17 -         R12 OR F
!           18 -         R13 OR G
!           19 -         R23 OR H
!           20 -         EXPONENT FOR BARLAT 91
!           21 -         VISCOELASTIC STIFFNESS RATIO
!           22 -         CHARACTERISTIC TIME
!           23 -         VISCOPLASTIC VISCOSITY
!           24 -         VISCOPLASTIC EXPONENT (<1)
!
!***************************************************************************************************************************************
!
!           21 -         VISCOELASTIC DAMPING
!           22 -         VISCOELASTIC CHARACTERISTIC TIME
!
!***************************************************************************************************************************************
!
!           23 -         VISCOPLASTIC DAMPING
!           24 -         VISCOPLASTIC EXPONENT
!
!***************************************************************************************************************************************
!
!*** DEGREES-OF-FREEDOM (NODOF, ALPHADOF)
!
!*** NODOF: [1,3]-       DISPLACEMENT
!*** NODOF: [4,6]-       SPECIAL DISPLACEMENT/ANGLE-VECTOR PARAMETERS
!*** NODOF: [7]  -       PRESSURE / PHIL
!*** NODOF: [8]  -       TEMPERATURE / PHIU
!*** NODOF: [9]  -       NON-LOCAL #1  // OPTIMIZATION
!*** NODOF: [10] -       NON-LOCAL #2  // OPTIMIZATION
!*** NODOF: [11] -       ANOTHER SPACE // OPTIMIZATION
!
!*** IN CERTAIN CASES, 4->XX, 5->YY, 6->ZZ, 7->XY, 8->XZ, 9->YZ
!
!*** STAGE/STATE: 2 -> CURRENT, 1 -> PREVIOUS
!---------------------------------------------------------------------------------------------
!*** ISTYLE:               TYPE OF ANALYSIS
!                          0-> LINEAR SOLUTION
!                          1-> INCREASE CONTROL IMPLICIT INTEGRATION
!                          2-> DISPLACEMENT CONTROL
!                          3-> Immersed contact
!                          4-> UNLOAD
!                          5-> CONTRACT MESH
!                          6-> EXPLICIT
!                          7-> CLASSICAL OPTIMIZATION
!
!--------------------------------!
!**********  HERE GUI  **********!
!--------------------------------!
!
!*** PRPANAL: [1] -         >> EMPTY AND KEEP EMPTY <<
!*** PRPANAL: [2] 1-        NEWTON-RAPHSON ERROR TOLERANCE (MIN=1.0E-10)
!*** PRPANAL: [3] 2-        NUMBER OF LINE SEARCH ITERATIONS (ONLY IF DAMPING==0)
!*** PRPANAL: [4] 3-        ONE STEP ANALYSIS FLAG (1 FOR TRUE)
!*** PRPANAL: [5] 4-        CUTTING STEP ALGORITHM FLAG (1 FOR TRUE)
!*** PRPANAL: [6] 5-        NUMBER OF HOMOTOPY STEPS (AUTOMATICALLY SET TO 2 IF INERTIA=1)
!*** PRPANAL: [7] 6-        UPPER LIMIT ON STEP SIZE FLAG (1 TO SET A LIMIT EQUIVALENT TO THE FIRST)
!*** PRPANAL: [8] 7-        NUMBER OF COMPLEMENTARITY funcsmoothING REDUCTIONS
!*** PRPANAL: [9] 8-        DAMPING (0 -> OFF AND /=0 -> ON)
!***                        0 -> CLASSICAL LINE SEARCH OR NAKED NEWTON UPDATE
!***                        1 -> LIMIT RADIUS EXACTLY
!***                        2 -> PROGRESSIVELY LIMIT RADIUS
!***                        3 -> SERIOUS DAMPING
!***                        4 -> HEAVY DAMPING
!*** PRPANAL: [10] X        ORDERED PAIR FOR LOAD CONTROL
!*** PRPANAL: [11] X        NODE 1 OF CONTROL
!*** PRPANAL: [12] X        NODE 2 OF CONTROL
!*** PRPANAL: [13] 16-      IDOF (DOF TYPE) TO CONTROL
!*** PRPANAL: [14] 9-       INERTIA FLAG
!*** PRPANAL: [15] 17-      DEACTIVATE CONSTITUTIVE SELF-RESTRAINT
!*** PRPANAL: [16] X -      MATERIAL CONSIDERED OUTSIDE THE TOOL
!*** PRPANAL: [17] 10-      CONTACT INDICATOR (1=ON)
!*** PRPANAL: [18] 11-      DEACTIVATE ALE (FOR SHELLS) 1=ALE OFF
!*** PRPANAL: [19] 12-      DISPLACEMENT INCREMENT FOR CONTROL
!*** PRPANAL: [20] 13-      TOLERANCE FOR CONTACT DETECTION
!*** PRPANAL: [21] 14-      CONSTITUTIVE EXTRAPOLATION
!*** PRPANAL: [22] 15-      PERFORMS NEUTRAL STEP AFTER RESTART
!*** PRPANAL: [23] 18-      0->NO funcsmoothING, 1->funcsmoothING OF EFFECTIVE STRAIN, 6->funcsmoothING OF STRAIN
!*** MGLOB / GLOB
!
!--------------------------------------------------------------------------------------------------------------------------------------
!**************************************************************************************************************************************
!************************************************
!*** INPUT FILE (.MALHA) FROM GID
!*** INPUT FILE (.ENTRA) USER-DEFINED FROM SIMPRE
!************************************************
!***********************
!*** WORKING SEGMENT
!*** MINIMAL DIAGNOSTICS
!*** USES RESTART
!***********************
MODULE OVERALL
  USE BASFUN
  USE LINEAR
  USE bucketsort
  IMPLICIT REAL(8) (A-H, O-Z)
!*****************
!*** DYNAMIC ARRAY
!*****************
  TYPE DYNARRAY
    INTEGER::N = 0
    REAL(8), DIMENSION(:), ALLOCATABLE::X
    REAL(8), DIMENSION(:), ALLOCATABLE::Y
  END TYPE DYNARRAY
  SAVE
!----------------
!*** BUCKET DATA
!----------------
  TYPE(BUCKETSS)::BS, BSO
!--------------
!*** MLS CHECK
!--------------
  REAL(8), DIMENSION(:), ALLOCATABLE::DSUPPORT
  INTEGER, DIMENSION(:, :), ALLOCATABLE::MLSVISIBLE
  INTEGER, DIMENSION(:), ALLOCATABLE::NVISIBLE
  INTEGER, PARAMETER::MLSMAX = 5500
  INTEGER::IMESHLESS = 0
!**********************
!*** NONLINEAR SOLUTION
!**********************
  INTEGER::ISTYLE = 0
!****************
!*** STRING SIZES
!****************
  INTEGER, PARAMETER::LSTRL = 350
  INTEGER, PARAMETER::LSTRM = 100
  INTEGER, PARAMETER::LSTRS = 25
!************************
!*** GET STUFF FROM INPUT
!************************
  PRIVATE::GETREA, GETINT, GETSST
!***********************
!*** INITIAL CONDITIONS
!*** FOR TIME-STEPPING
!*** ALGORITHMS
!***********************
  INTEGER, DIMENSION(:), ALLOCATABLE::ICIN
  INTEGER, DIMENSION(:), ALLOCATABLE::ICNO
  INTEGER, DIMENSION(:), ALLOCATABLE::ICDF
  REAL(8), DIMENSION(:), ALLOCATABLE::ICVL
  LOGICAL, DIMENSION(:), ALLOCATABLE::ICDT
!*******************
!*** MPC PROPERTIES
!*******************
  TYPE MPCPROPT
    INTEGER::NSLAVES                            ! NUMBER OF SLAVES
    REAL(8), DIMENSION(:), ALLOCATABLE::PROP    ! PROPERTIES
    INTEGER, DIMENSION(:), ALLOCATABLE::SLAVES  ! LIST OF SLAVE NODES
  END TYPE MPCPROPT
!****************
!*** NODE GROUPS
!****************
  INTEGER::NGROUPNO = 0
  INTEGER, DIMENSION(:), ALLOCATABLE::GRNI, GRNO
  INTEGER, DIMENSION(:), ALLOCATABLE::NIGR, NOGR
!*************************
!*** GET A GENERAL NUMBER
!*************************
  INTERFACE GETNUM
    MODULE PROCEDURE GETREA, GETINT, GETSST
  END INTERFACE GETNUM
!************
!*** MONITOR
!************
  INTEGER::NMONITOR = 0
  INTEGER, DIMENSION(:), ALLOCATABLE::MONITORED
!******************
!*** STRAIN ENERGY
!******************
  REAL(8)::STRAINENERGY = 0.0d00
  REAL(8)::TOTALENERGY = 0.0d00
  REAL(8)::KINETICENERGY = 0.0d00
!*********************
!*** REACTION REQUEST
!*********************
  INTEGER::NNODREAC = 0
  INTEGER, DIMENSION(:), ALLOCATABLE::NODREAC
!*********
!*** IF3d
!*********
  LOGICAL::IF3D = .FALSE.
!*****************
!*** STEP CUTTING
!*****************
  LOGICAL::CUTSTEP = .FALSE.
!*********
!*** MPCS
!*********
  TYPE(MPCPROPT), DIMENSION(:), ALLOCATABLE::MPCPROP
!***************************
!*** TIMES AND LOAD FACTORS
!***************************
  REAL(8)::TIME = 0.0d00                        ! TIME
  REAL(8)::TIMESHIFT = 0.0d00                   ! SHIFT FOR DISPLACEMENT CONTROL
  REAL(8)::DTIME = 0.0d00                       ! TIME VARIATION
  REAL(8)::DTIMEO = 0.0d00                      ! OLD TIME VARIATION
  REAL(8)::TTIME = 0.0d00                       ! TOTAL TIME
  REAL(8)::DTCRIT = huge(0.0d00)                ! CRITICAL TIME STEP
  REAL(8)::DTMAX = huge(0.0d00)                 ! MAXIMUM TIME STEP
  REAL(8)::TIMEO = 0.0d00                       ! OLD TIME
  INTEGER::ISTEP = 0                            ! STEP VALUE
  INTEGER::IOUTS = 0                            ! OUTPUT STEP VALUE
  INTEGER::IEQIT = 0                            ! EQUILIBRIUM ITERATION
  INTEGER::IHOMOTOPY = 0                        ! HOMOTOPY STEP
  INTEGER::NHOMOTOPY = 0                        ! HOMOTOPY STEPS
  INTEGER::ISRED = 0                            ! funcsmoothING PARAMETER REDUCTION STEP
  REAL(8)::RLOAO = 0.0d00                       ! OLD LOAD FACTOR
  REAL(8)::RLOAD = 0.0d00                       ! NEW LOAD FACTOR
  REAL(8)::DLOAD = 0.0d00                       ! LOAD VARIATION
  INTEGER::NSPECT = 0                           ! NUMBER OF SPECIAL TIMES
  REAL(8), DIMENSION(:), ALLOCATABLE::SPECT     ! SPECIAL TIMES
  INTEGER, DIMENSION(:, :), ALLOCATABLE::HSPECT ! FLAG FOR "ALREADY STOPPED"
!**************************
!*** LOAD MODE/LOAD FACTOR
!*** 0 - INTERPOLATION
!*** 1 - ONELOAD
!*** 2 - ZEROLOAD
!*** 3 - RLOAD
!**************************
  INTEGER::LOADMODE = 0
  INTEGER, PARAMETER::INTERPOLATIONLOAD = 0
  INTEGER, PARAMETER::ONELOAD = 1
  INTEGER, PARAMETER::ZEROLOAD = 2
  INTEGER, PARAMETER::PARLOAD = 3
!************************
!*** RESTARTS INDICATORS
!************************
  LOGICAL::WRES = .FALSE.
  LOGICAL::RRES = .FALSE.
!*******************************
!*** NORMAL VECTORS (DIRECTORS)
!*******************************
  REAL(8), DIMENSION(:, :), ALLOCATABLE::VNORN, VNORN0
!*******************
!*** RELEVANT FILES
!*******************
  CHARACTER(LSTRM)::NOMEF = " "      ! FILE NAME
  CHARACTER(LSTRM)::NOMEFBASE = " "  ! FILE NAME BASE
  CHARACTER(lstrs)::malha = "malha"  ! mesh input extension
  CHARACTER(lstrs)::grnos = "groups" ! node groups
  CHARACTER(lstrs)::entra = "entra"  ! detail input extension
  CHARACTER(lstrs)::aviso = "aviso"  ! warning
  CHARACTER(lstrs)::resta = "resta"  ! restart
  CHARACTER(lstrs)::volfi = "volum"  ! volume
  CHARACTER(lstrs)::enefi = "enefi"  ! fracture energy file
  CHARACTER(lstrs)::order = "order"  ! orderedpairs
  CHARACTER(lstrs)::reacf = "react"  ! reactions
  CHARACTER(lstrs)::reacp = "reacp"  ! reactions data
  CHARACTER(LSTRL)::TEXTO = " "      ! TEMPORARY STRING
!***********************
!*** OVERALL DIMENSIONS
!***********************
  REAL(8)::ATAX = 0.0d00, ATAY = 0.0d00, ATAZ = 0.0d00
  REAL(8)::EDGEMIN = 1.0d90
  REAL(8)::EDGEMAX = -1.0d90
  REAL(8)::CRACKLENGTH = 0.0d00
!****************************
!*** RESIDUAL NORM AND STUFF
!****************************
  REAL(8)::RESIDFORC = 0.0d00
!*****************
!*** UNIT NUMBERS
!*****************
  INTEGER, PARAMETER::UMAL = 10
  INTEGER, PARAMETER::UGRN = 20
  INTEGER, PARAMETER::UENT = 11
  INTEGER, PARAMETER::UAVI = 12
  INTEGER, PARAMETER::URST = 13
  INTEGER, PARAMETER::UVOL = 14
  INTEGER, PARAMETER::UORD = 16
  INTEGER, PARAMETER::UREA = 17
  INTEGER, PARAMETER::UREP = 18
  INTEGER, PARAMETER::UENE = 19
!********************
!*** SOME PARAMETERS
!*** CONSTANTS
!********************
  INTEGER, PARAMETER::MVPG = 42      ! MAXIMUM NUMBER OF VARIABLES PER GAUSS POINT
  INTEGER, PARAMETER::MVPE = 32      ! MAXIMUM NUMBER OF VARIABLES PER ELEMENT
  INTEGER, PARAMETER::NMGA = 30      ! MAXIMUM NUMBER OF GAUSS POINTS PER ELEMENT
  INTEGER, PARAMETER::NMAP = 35      ! NUMBER OF MATERIAL PROPERTIES
  INTEGER, PARAMETER::NSEP = 35      ! NUMBER OF SECTION PROPERTIES
  INTEGER, PARAMETER::NTEL = 7       ! TYPES OF ELEMENTS
  INTEGER, PARAMETER::NTGL = 11      ! TYPES OF DOFS
  INTEGER, PARAMETER::NPRPANAL = 32  ! MAXIMUM NUMBER OF ANALYSIS PROPERTIES
!*******************
!*** ANALYSIS PROPS
!*** AND MORE STUFF
!*******************
  INTEGER::ITYPOUTPUT = 0               ! TYPE OF OUTPUT (0-> Boundary, 1-> Full)
  REAL(8), DIMENSION(NPRPANAL)::PRPANAL ! ANALYSIS PROPERTIES
  INTEGER::IDOFTARGET = 0               ! DEGREE-OF-FREEDOM TARGET
  INTEGER::INOTARGET = 0                ! NODE TARGET
  REAL(8)::RTARGET = 0.0d00             ! TARGET VALUE
!*********************************
!*** HISTORICAL DATA AND STRESSES
!*********************************
  REAL(8), DIMENSION(:, :, :, :), ALLOCATABLE::GPHIST  ! VARIABLE,GAUSSPOINT,ELEMENT,NEWOROLD:MVPG,NMGA,NEL,2
  REAL(8), DIMENSION(:, :, :), ALLOCATABLE::ELHIST    ! VARIABLE,  ELEMENT, NEW (2) OR OLD (1)
  REAL(8), DIMENSION(:, :, :, :), ALLOCATABLE::GPSTRE  ! STRESS COMPONENT,GAUSSPOINT,ELEMENT,NEWOROLD:NCT,NMGA,NEL,2
!************************
!*** EXTRAPOLATED VALUES
!************************
  REAL(8), DIMENSION(:, :), ALLOCATABLE::EPSEXTRAP  ! EFFECTIVE PLASTIC STRAIN
  REAL(8), DIMENSION(:, :), ALLOCATABLE::VOIDEXTRAP ! VOID FRACTION EXTRAPOLATED
!******************
!*** ORDERED PAIRS
!******************
  TYPE(DYNARRAY), DIMENSION(:), ALLOCATABLE::ORDE ! ORDERED PAIRS
!*****************************
!*** MATERIAL PROPERTIES, ETC
!*****************************
  REAL(8)::SIZENL = 0.0d00                     ! NONLOCAL SIZE
  REAL(8)::CHARL = 0.0d00                      ! CHARACTERISTIC LENGTH
  INTEGER, DIMENSION(:), ALLOCATABLE::TYPM     ! MATERIAL NUMBER
  REAL(8), DIMENSION(:, :), ALLOCATABLE::MAPR  ! MATERIAL PROPERTIES
  REAL(8), DIMENSION(:, :), ALLOCATABLE::MAPS  ! SECTION PROPERTIES
  INTEGER, DIMENSION(:), ALLOCATABLE::MAPI     ! TOPOLOGICAL TYPE OF ELEMENT OF SECTION
  INTEGER, DIMENSION(:), ALLOCATABLE::GLOBALPI ! SPECIFIC TYPE OF ELEMENT
!******************************
!*** NODAL COORDINATES
!*** VARIABLE CHANGE, REACTION
!*** NODAL DOFS
!******************************
  REAL(8), DIMENSION(:, :), ALLOCATABLE::NOCO                  ! COORDINATES
  REAL(8), DIMENSION(:, :), ALLOCATABLE::DDES, REAC            ! UNKNOWN INCREMENT AND REACTIONS
  REAL(8), DIMENSION(:, :, :), ALLOCATABLE::NODOF, ACEL, VELO  ! UNKNOWNS AND ACCELERATIONS AND VELOCITIES
  REAL(8), DIMENSION(:, :, :), ALLOCATABLE::ALPHADOF           ! INTERNAL ELEMENT DEGREES OF FREEDOM
  REAL(8), DIMENSION(:, :), ALLOCATABLE::DYNVELO1, DYNVELO2
!**********************
!*** STIFFNESS ET AL.
!**********************
  TYPE(LINEARTYPE)::ASS
!**************************
!*** MESH DATA
!*** E.G. "CONNECTIVITIES"
!**************************
  INTEGER::NNO = 0       ! NUMBER OF EXTERNAL NODES
  INTEGER::NNOT = 0      ! NUMBER OF TOTAL NODES
  INTEGER::NEL = 0       ! NUMBER OF ELEMENTS
  INTEGER::NMP = 0       ! NUMBER OF MPCS
  INTEGER::NIC = 0       ! NUMBER OF INITIAL CONDITIONS
  INTEGER::NOR = 0       ! NUMBER OF ORDERED PAIRS
  INTEGER::NMT = 0       ! NUMBER OF MATERIALS
  INTEGER::NAR = 0       ! NUMBER OF EDGES
  INTEGER::NFA = 0       ! NUMBER OF FACES
  INTEGER::NAREX = 0     ! NUMBER OF OUTER EDGES
  INTEGER::NFAEX = 0     ! NUMBER OF OUTER FACES
  INTEGER::NNOEX = 0     ! NUMBER OF OUTER NODES
  INTEGER, DIMENSION(:), ALLOCATABLE:: &
    ELNO, NOEL, ELNI, NOIL, ELMA, MAEL, IMEL, ELAR, ELFA, FANO, &
    FAAR, ARNO, AREL, FAEL, NOFA, ARFA, NOAR, ELIR, ELFI, FANI, &
    FAIR, ARNI, ARIL, FAIL, NOFI, ARFI, NOIR, NOLOCAL, LAREX, &
    LFAEX, LNOEX, FADOF
!**********************************
!**** EXTERNAL FACE CONNECTIVITIES
!**********************************
  INTEGER, DIMENSION(:), ALLOCATABLE::FABNI, FABNO
!*****************************
!*** TEMPORARY ELNI AND ELNO
!*** DURING THE READING STAGE
!*****************************
  INTEGER, DIMENSION(:), ALLOCATABLE::TPEL, TPIL
  INTEGER, DIMENSION(:), ALLOCATABLE::ELGNUMBER
!****************
!*** MESH VOLUME
!****************
  REAL(8)::VOLMESH = 0.0d00
CONTAINS
!****************
!*** UPDATE DTIME
!*** CHK0
!****************
  SUBROUTINE UPDATEDTIME(DTIMENEW)
    IF (DTIME .GT. EPSMACH()) THEN
      DTIMEO = DTIME
    ELSE
      DTIMEO = DTMAX
    END IF
    DTIME = DTIMENEW
    IF (DTIMENEW .LE. -1.0d-20) THEN
      CALL ERRO(" INCORRECT DTIME REQUESTED ")
    END IF
    WRITE (*, "(A)") "Updated Time Increment"
    WRITE (*, "(A,E15.5,A)") "Which is now ", DTIME, " s"
  END SUBROUTINE UPDATEDTIME
!*******************
!*** ALL ENTITIES OF
!*** A GIVEN TYPE
!*** CHK0
!*******************
  SUBROUTINE ENTITIES(N, ENT)
    INTEGER, DIMENSION(:), ALLOCATABLE::ENT
    N = 0
    CALL STRINGREADINTEGERANDREMOVE(TEXTO, I)
    I = I+1
    IF (I .NE. 0) THEN
      N = GRNI(I+1)-GRNI(I)
      IF (.NOT. allocated(ENT)) CALL ALLOCSAFE(N, ENT)
      IF (size(ENT) .LT. N) CALL ALLOCSAFE(N, ENT)
      IK = 0
      DO J = GRNI(I), GRNI(I+1)-1
        IK = IK+1
        ENT(IK) = GRNO(J)
      END DO
      CALL LISTREMOVEREPEATED(N, ENT, .FALSE.)
    END IF
    CALL LISTTRIM(N, ENT)
  END SUBROUTINE ENTITIES
!********************
!*** SINGLE ENTITY OF
!*** A GIVEN TYPE
!*** CHK0
!********************
  SUBROUTINE SINGLEENTITIES(ENT)
    INTEGER::ENT
    ENT = 0
    CALL stringreadintegerandremove(TEXTO, I)
    I = I+1
    IF (I .NE. 0) THEN
      K = 0
      DO J = 1, NGROUPNO
        IF (GRNI(J+1)-GRNI(J) .EQ. 1) THEN
          K = K+1
          IF (K .EQ. I) THEN
            ENT = GRNO(GRNI(J))
            RETURN
          END IF
        END IF
      END DO
    END IF
  END SUBROUTINE SINGLEENTITIES
!*****************************
!*** ELEMENT TOPOLOGICAL NAMES
!1 - POINT
!2 - LINE (BAR2)
!3 - TRIANGLE (TRIA3)
!4 - QUAD (QUAD4)
!5 - TET (TETRA4)
!6 - HEX (HEXA8)
!7 - WEDGE (PENTA6)
!*** CHK0
!*****************************
  CHARACTER(6) FUNCTION NOMES(K)
    INTEGER::K
    SELECT CASE (K)
    CASE (1)
      nomes = "point"
    CASE (2)
      nomes = "bar2"
    CASE (3)
      nomes = "tria3"
    CASE (4)
      nomes = "quad4"
    CASE (5)
      nomes = "tetra4"
    CASE (6)
      nomes = "hexa8"
    CASE (7)
      nomes = "penta6"
    CASE default
      CALL erro("wrong call to nomes")
    END SELECT
  END FUNCTION NOMES
!************************
!*** TYPE OF GEOMETRY
!1 - POINT->0
!2 - LINE (BAR2)->1
!3 - TRIANGLE (TRIA3)->2
!4 - QUAD (QUAD4)->2
!5 - TET (TETRA4)->3
!6 - HEX (HEXA8)->3
!7 - WEDGE (PENTA6)->3
!*** CHK0
!************************
  INTEGER FUNCTION GEOTOP(K)
    SELECT CASE (K)
    CASE (1)
      GEOTOP = 0
    CASE (2)
      GEOTOP = 1
    CASE (3)
      GEOTOP = 2
    CASE (4)
      GEOTOP = 2
    CASE (5:7)
      GEOTOP = 3
    CASE DEFAULT
      WRITE (*, *) "K=", ks
      CALL ERRO("WRONG CALL TO GEOTOP")
    END SELECT
  END FUNCTION GEOTOP
!******************************
!*** NUMBER OF CORNER NODES
!*** AS A FUNCTION OF TOPOLOGY
!*** CHK0
!******************************
  INTEGER FUNCTION NNOSC(ITY)
    N = 0
    SELECT CASE (ITY)
    CASE (1)
      N = 1
    CASE (2)
      N = 2
    CASE (3)
      N = 3
    CASE (4)
      N = 4
    CASE (5)
      N = 4
    CASE (6)
      N = 8
    CASE (7)
      N = 6
    CASE DEFAULT
      CALL ERRO("WRONG CALL TO NNOSC")
    END SELECT
    NNOSC = N
  END FUNCTION NNOSC
!********************
!*** NUMBER OF EDGES
!*** CHK0
!********************
  INTEGER FUNCTION NARES(ITY)
    NE = 0
    SELECT CASE (ITY)
    CASE (1)
      NE = 0
    CASE (2)
      NE = 1
    CASE (3)
      NE = 3
    CASE (4)
      NE = 4
    CASE (5)
      NE = 6
    CASE (6)
      NE = 12
    CASE (7)
      NE = 9
    CASE DEFAULT
      WRITE (*, *) "ITY=", ITY
      CALL ERRO("WRONG CALL TO NARES")
    END SELECT
    NARES = NE
  END FUNCTION NARES
!********************
!*** NUMBER OF FACES
!*** CHK0
!********************
  INTEGER FUNCTION NFACES(ITY)
    NF = 0
    SELECT CASE (ITY)
    CASE (1)
      NF = 0
    CASE (2)
      NF = 0
    CASE (3)
      NF = 1
    CASE (4)
      NF = 1
    CASE (5)
      NF = 4
    CASE (6)
      NF = 6
    CASE (7)
      NF = 5
    CASE DEFAULT
      CALL ERRO("WRONG CALL TO NFACES")
    END SELECT
    NFACES = NF
  END FUNCTION NFACES
!*********************************
!*** NUMBER OF NODES IN EACH FACE
!*** NUMNF
!*** CHK0
!*********************************
  INTEGER FUNCTION NUMNF(ITY, IFA)
    SELECT CASE (ITY)
    CASE (1)
      NUMNF = 0
    CASE (2)
      NUMNF = 0
    CASE (3)
      NUMNF = 3
    CASE (4)
      NUMNF = 4
    CASE (5)
      NUMNF = 3
    CASE (6)
      NUMNF = 4
    CASE (7)
      SELECT CASE (IFA)
      CASE (1:3)
        NUMNF = 4
      CASE (4:5)
        NUMNF = 3
      END SELECT
    CASE DEFAULT
      CALL ERRO("WRONG CALL TO NUMNF")
    END SELECT
  END FUNCTION NUMNF
!***************************************
!*** LIST OF EDGES ACCORDING TO TOPOLOGY
!*** CHK0
!***************************************
  SUBROUTINE LISTARES(ITY, LL)
    USE BASFUN
    INTEGER, DIMENSION(2, 12)::LL
    CALL listsetconstant(24, LL)
    SELECT CASE (ITY)
    CASE (1)
      RETURN
    CASE (2)
      LL(1, 1) = 1
      LL(2, 1) = 2
    CASE (3)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(1, 2) = 2
      LL(2, 2) = 3
      LL(1, 3) = 3
      LL(2, 3) = 1
    CASE (4)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(1, 2) = 2
      LL(2, 2) = 3
      LL(1, 3) = 3
      LL(2, 3) = 4
      LL(1, 4) = 4
      LL(2, 4) = 1
    CASE (5)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(1, 2) = 2
      LL(2, 2) = 3
      LL(1, 3) = 1
      LL(2, 3) = 3
      LL(1, 4) = 3
      LL(2, 4) = 4
      LL(1, 5) = 1
      LL(2, 5) = 4
      LL(1, 6) = 2
      LL(2, 6) = 4
    CASE (6)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(1, 2) = 2
      LL(2, 2) = 3
      LL(1, 3) = 3
      LL(2, 3) = 4
      LL(1, 4) = 4
      LL(2, 4) = 1
      LL(1, 5) = 1
      LL(2, 5) = 5
      LL(1, 6) = 2
      LL(2, 6) = 6
      LL(1, 7) = 3
      LL(2, 7) = 7
      LL(1, 8) = 4
      LL(2, 8) = 8
      LL(1, 9) = 5
      LL(2, 9) = 8
      LL(1, 10) = 5
      LL(2, 10) = 6
      LL(1, 11) = 6
      LL(2, 11) = 7
      LL(1, 12) = 7
      LL(2, 12) = 8
    CASE (7)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(1, 2) = 2
      LL(2, 2) = 3
      LL(1, 3) = 1
      LL(2, 3) = 3
      LL(1, 4) = 4
      LL(2, 4) = 5
      LL(1, 5) = 5
      LL(2, 5) = 6
      LL(1, 6) = 4
      LL(2, 6) = 6
      LL(1, 7) = 1
      LL(2, 7) = 4
      LL(1, 8) = 2
      LL(2, 8) = 5
      LL(1, 9) = 3
      LL(2, 9) = 6
    CASE DEFAULT
      CALL ERRO("WRONG CALL TO LISTARES")
    END SELECT
  END SUBROUTINE LISTARES
!****************************************
!*** LIST OF FACES ACCORDING TO TOPOLOGY
!*** CHK0
!****************************************
  SUBROUTINE LISTFACES(ITY, LL)
    USE BASFUN
    INTEGER, DIMENSION(4, 6)::LL
    CALL listsetconstant(24, LL, 0)
    SELECT CASE (ITY)
    CASE (1)
      RETURN
    CASE (2)
      RETURN
    CASE (3)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(3, 1) = 3
    CASE (4)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(3, 1) = 3
      LL(4, 1) = 4
    CASE (5)
      LL(1, 1) = 1
      LL(2, 1) = 4
      LL(3, 1) = 3
      LL(1, 2) = 1
      LL(2, 2) = 2
      LL(3, 2) = 4
      LL(1, 3) = 1
      LL(2, 3) = 3
      LL(3, 3) = 2
      LL(1, 4) = 2
      LL(2, 4) = 3
      LL(3, 4) = 4
    CASE (6)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(3, 1) = 6
      LL(4, 1) = 5
      LL(1, 2) = 2
      LL(2, 2) = 3
      LL(3, 2) = 7
      LL(4, 2) = 6
      LL(1, 3) = 3
      LL(2, 3) = 4
      LL(3, 3) = 8
      LL(4, 3) = 7
      LL(1, 4) = 1
      LL(2, 4) = 5
      LL(3, 4) = 8
      LL(4, 4) = 4
      LL(1, 5) = 4
      LL(2, 5) = 3
      LL(3, 5) = 2
      LL(4, 5) = 1
      LL(1, 6) = 5
      LL(2, 6) = 6
      LL(3, 6) = 7
      LL(4, 6) = 8
    CASE (7)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(3, 1) = 5
      LL(4, 1) = 4
      LL(1, 2) = 2
      LL(2, 2) = 3
      LL(3, 2) = 6
      LL(4, 2) = 5
      LL(1, 3) = 1
      LL(2, 3) = 4
      LL(3, 3) = 6
      LL(4, 3) = 3
      LL(1, 4) = 4
      LL(2, 4) = 5
      LL(3, 4) = 6
      LL(1, 5) = 1
      LL(2, 5) = 3
      LL(3, 5) = 2
    CASE DEFAULT
      CALL ERRO("WRONG CALL TO LISTFACES")
    END SELECT
  END SUBROUTINE LISTFACES
!****************************************
!*** LIST OF FACES ACCORDING TO TOPOLOGY
!*** CHK0
!****************************************
  SUBROUTINE LISTFACES2(ITY, LL)
    USE BASFUN
    INTEGER, DIMENSION(4, 6)::LL
    CALL listsetconstant(24, LL, 0)
    SELECT CASE (ITY)
    CASE (1)
      RETURN
    CASE (2)
      RETURN
    CASE (3)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(3, 1) = 3
    CASE (4)
      LL(1, 1) = 1
      LL(2, 1) = 2
      LL(3, 1) = 3
      LL(4, 1) = 4
    CASE (5)
      LL(1, 1) = 5
      LL(2, 1) = 4
      LL(3, 1) = 3
      LL(1, 2) = 1
      LL(2, 2) = 6
      LL(3, 2) = 5
      LL(1, 3) = 3
      LL(2, 3) = 2
      LL(3, 3) = 1
      LL(1, 4) = 2
      LL(2, 4) = 4
      LL(3, 4) = 6
    CASE (6)
      LL(1, 1) = 1
      LL(2, 1) = 6
      LL(3, 1) = 10
      LL(4, 1) = 5
      LL(1, 2) = 2
      LL(2, 2) = 7
      LL(3, 2) = 11
      LL(4, 2) = 6
      LL(1, 3) = 3
      LL(2, 3) = 7
      LL(3, 3) = 12
      LL(4, 3) = 8
      LL(1, 4) = 5
      LL(2, 4) = 9
      LL(3, 4) = 8
      LL(4, 4) = 4
      LL(1, 5) = 1
      LL(2, 5) = 2
      LL(3, 5) = 3
      LL(4, 5) = 4
      LL(1, 6) = 10
      LL(2, 6) = 11
      LL(3, 6) = 12
      LL(4, 6) = 9
    CASE (7)
      LL(1, 1) = 1
      LL(2, 1) = 8
      LL(3, 1) = 4
      LL(4, 1) = 7
      LL(1, 2) = 2
      LL(2, 2) = 9
      LL(3, 2) = 5
      LL(4, 2) = 8
      LL(1, 3) = 7
      LL(2, 3) = 6
      LL(3, 3) = 9
      LL(4, 3) = 3
      LL(1, 4) = 4
      LL(2, 4) = 5
      LL(3, 4) = 6
      LL(1, 5) = 3
      LL(2, 5) = 2
      LL(3, 5) = 1
    CASE DEFAULT
      CALL ERRO("WRONG CALL TO LISTFACES2")
    END SELECT
  END SUBROUTINE LISTFACES2
!*************************
!*** HEURISTICS SCALETIME
!*** AFTER SOME ITERATION
!*** STAGE HAS BEEN
!*** PERFORMED
!*** CHK0
!*************************
  SUBROUTINE HEURISTICSCALETIME(IMAXS, IT, LIMIT)
    INTEGER, SAVE::ITOLD = 0
    INTEGER::ITSUCCESS
    INTEGER::ITFAILURE
!-----------------------------------------------
!*** FEWER THAN ITSUCCESS => INCREASE STEP SIZE
!-----------------------------------------------
    ITSUCCESS = nint(0.3d00*LIMIT)
!----------------------------------------------
!*** MORE THAN ITFAILURE => DECREASE STEP SIZE
!----------------------------------------------
    ITFAILURE = nint(0.8d00*LIMIT)
    IF (ITOLD .EQ. 0) ITOLD = IT
    IF (IT .GE. LIMIT) THEN
      RLOAD = RLOAO
      TIME = TIMEO
!---------------
!*** CUTS DTIME
!---------------
      SCALETIME = 0.5d00*DTIME
      CALL UPDATEDTIME(SCALETIME)
      TIME = TIME+DTIME
!-----------------
!*** RESETS NODOF
!-----------------
      DO CONCURRENT(J=1:NNO, I=1:NTGL)
        NODOF(I, J, 2) = NODOF(I, J, 1)
      END DO
      DO CONCURRENT(J=1:NEL, I=1:MVPE)
        ELHIST(I, J, 2) = ELHIST(I, J, 1)
      END DO
      DO CONCURRENT(J=NNO+1:NNOT, I=1:NTGL)
        ALPHADOF(I, J-NNO, 2) = ALPHADOF(I, J-NNO, 1)
      END DO
      WRITE (*, *) "***************************************"
      WRITE (*, *) "CUTTED STEP DUE TO EXCESS OF ITERATIONS"
      WRITE (*, *) "***************************************"
    ELSE
!--------------------------------
!*** INCREASE/DECREASE SCALETIME
!--------------------------------
      IF (ITOLD .LE. ITSUCCESS .AND. IT .LE. ITSUCCESS) THEN
        SCALETIME = 1.25d00
      ELSE IF (ITOLD .GT. ITFAILURE .AND. IT .GT. ITFAILURE) THEN
        SCALETIME = 0.75d00
      ELSE
        SCALETIME = max(min(sqrt(1.0d00*(ITFAILURE)/max(ITOLD, IT)), 1.25d00), 0.75d00)
      END IF
!--------------------------------------------------
!*** EFFECTIVE PLASTIC STRAIN INCREMENT CORRECTION
!--------------------------------------------------
      EPSMAX = 0.0d00
      DO IEL = 1, NEL
        DO JGA = 1, 2
          EPSMAX = max(EPSMAX, GPHIST(1, JGA, IEL, 2)-GPHIST(1, JGA, IEL, 1))
        END DO
      END DO
      IF (EPSMAX .GT. EPSMACH()) THEN
        SCALETIME = max(0.70d00, min(SCALETIME, 0.80d00/abs(EPSMAX)))
      END IF
      IF (abs(EPSMAX) .GT. 1.0d00) THEN
        WRITE (*, *) "VERY LARGE EPSMAX=", EPSMAX
      END IF
!---------------------------------------
!*** VOID FRACTION INCREMENT CORRECTION
!---------------------------------------
      VOIDMAX = 0.0d00
      DO IEL = 1, NEL
        DO JGA = 1, 2
          VOIDMAX = max(VOIDMAX, GPHIST(2, JGA, IEL, 2)-GPHIST(2, JGA, IEL, 1))
        END DO
      END DO
      IF (VOIDMAX .GT. EPSMACH()) THEN
        SCALETIME = max(0.70d00, min(SCALETIME, 0.80d00/abs(VOIDMAX)))
      END IF
      IF (abs(VOIDMAX) .GT. 1.0d00) THEN
        WRITE (*, *) "VERY LARGE VOIDMAX=", VOIDMAX
      END IF
!-------------------------------------------------------------
!*** NOW ALTERS TIME STEP SIZE ACCORDING TO PREVIOUS POLICIES
!-------------------------------------------------------------
      IF (IMAXS .NE. 0) THEN
        CALL UPDATEDTIME(min(SCALETIME*DTIME, DTMAX))
      ELSE
        CALL UPDATEDTIME(SCALETIME*DTIME)
      END IF
      WRITE (*, *) "****************************"
      WRITE (*, "(A,E11.3)") "ESTIMATED SCALETIME=", DTIME/DTIMEO
      WRITE (*, *) "****************************"
!--------------------------------------
!*** OUTPUTS AND UPDATES THE VARIABLES
!--------------------------------------
      CALL SAIDAS()
      CALL TIMEX(1)
    END IF
!----------------
!*** SAVES ITOLD
!----------------
    ITOLD = IT
  END SUBROUTINE HEURISTICSCALETIME
!**********************************************
!*** GENERAL ELEMENT TYPE (TOPOLOGICAL FAMILY)
!*** CHK0
!**********************************************
  FUNCTION NELPRE(IEL)
    NELPRE = 0
    IF (IEL .GT. 0) THEN
      IF (ELMA(IEL) .GT. 0) THEN
        NELPRE = MAPI(ELMA(IEL))
      END IF
    ELSE
      STOP "WRONG REQUEST TO NELPRE"
    END IF
  END FUNCTION NELPRE
!**********************************************
!*** GENERAL ELEMENT SPECIES (SPECIFIC ELEMENT)
!*** CHK0
!**********************************************
  FUNCTION NELOPT(IEL)
    NELOPT = 0
    IF (ELMA(IEL) .GT. 0) THEN
      NELOPT = GLOBALPI(ELMA(IEL))
    ELSE
      STOP "WRONG REQUEST TO NELOPT"
    END IF
  END FUNCTION NELOPT
!****************************************************
!*** CONTINUUM ELEMENT REGARDLESS OF FRACTURED OR NOT
!****************************************************
  LOGICAL FUNCTION ISCONTINUUMELEMENT(IEL)
    ISCONTINUUMELEMENT = .FALSE.
    IF (MATNUM(IEL) .NE. 0) ISCONTINUUMELEMENT = .TRUE.
  END FUNCTION ISCONTINUUMELEMENT
!************************************************
!*** RETURNS TRUE IF THE ELEMENT IS A CONTACT ONE
!*** IS CONTACT
!*** CHK0
!************************************************
  LOGICAL FUNCTION ISCONTACT(IEL)
    LOGICAL::RES
    IF (IEL .LE. 0) THEN
      ISCONTACT = .FALSE.
      RETURN
    END IF
    SELECT CASE (NELOPT(IEL))
    CASE (12)
      RES = .TRUE.
    CASE DEFAULT
      RES = .FALSE.
    END SELECT
    ISCONTACT = RES
  END FUNCTION ISCONTACT
!************************************
!*** RETURNS TRUE IF ELEMENT IS A MLS
!*** MESHLESS
!************************************
  LOGICAL FUNCTION ISMESHLESS(IEL)
    LOGICAL::RES
    SELECT CASE (NELOPT(IEL))
    CASE (37, 52)
      RES = .TRUE.
    CASE DEFAULT
      RES = .FALSE.
    END SELECT
    ISMESHLESS = RES
  END FUNCTION ISMESHLESS
!*************************************************************
!*** MATERIAL NUMBER, 0 MEANS THAT THE ELEMENT HAS NO MATERIAL
!*** chk0
!*************************************************************
  INTEGER FUNCTION MATNUM(IEL)
    MATNUM = TYPM(ELMA(IEL))
  END FUNCTION MATNUM
!**********************************
!*** GETS real, integer and strings
!*** from an .entra file
!**********************************
  SUBROUTINE GETREA(RRRR)
    REAL(8)::RRRR
    CALL stringreadrealandremove(TEXTO, RRRR)
  END SUBROUTINE GETREA
  SUBROUTINE GETINT(IIII)
    CALL stringreadintegerandremove(TEXTO, IIII)
  END SUBROUTINE GETINT
  SUBROUTINE GETSST(IIII)
    CHARACTER(*)::IIII
    CALL stringreadstringandremove(TEXTO, IIII)
  END SUBROUTINE GETSST
  SUBROUTINE GETSTR
    INTEGER::IRS
    CALL filereadsaline(TEXTO, IRS, UENT)
  END SUBROUTINE GETSTR
!*****************************************
!*** POSITIONS AT A GIVEN KEY IN THE FILE
!*** chk0
!*****************************************
  SUBROUTINE FILEPOSITIONSF(AA, IEX)
    CHARACTER(*)::AA
    CHARACTER(len(AA))::BB
    CHARACTER(len(AA)+3)::TEX
    TEX = " "
    BB = AA
    IEX = 0
    CALL STRINGLOWERCASE(BB)
    REWIND (UENT)
    DO
      READ (UENT, "(A)", IOSTAT=IO) TEX
      TEX = trim(adjustl(TEX))
      IF (IO .NE. 0) EXIT
      CALL STRINGLOWERCASE(TEX)
      IF (trim(adjustl(TEX)) .EQ. "@"//trim(adjustl(BB))) THEN
        IEX = 1
        EXIT
      END IF
    END DO
  END SUBROUTINE FILEPOSITIONSF
!*********************
!*** OUTPUTS A WARNING
!*** chk0
!*********************
  SUBROUTINE warn(text, ivar, chav)
    CHARACTER(*)::text
    INTEGER, OPTIONAL::ivar
    CHARACTER(*), OPTIONAL::chav
    LOGICAL::l1, l2
    CHARACTER(len=300)::dumt
    l1 = present(ivar)
    l2 = present(chav)
    IF (l1 .AND. l2) THEN
      WRITE (dumt, *, err=100) ", ", ivar, ", ", chav
    ELSE IF (.NOT. l1 .AND. l2) THEN
      WRITE (dumt, *, err=100) ", ", chav
    ELSE IF (l1 .AND. .NOT. l2) THEN
      WRITE (dumt, *, err=100) ", ", ivar
    ELSE IF (.NOT. l1 .AND. .NOT. l2) THEN
      WRITE (dumt, *, err=100) " "
    END IF
    CALL stringremoveheadspaces(dumt)
    CALL stringremovedoublespaces(dumt)
100 CONTINUE
    WRITE (uavi, "(3a)", iostat=ier) "aviso:", trim(text), trim(dumt)
  END SUBROUTINE warn
!*******************
!*** OUTPUTS A ERROR
!*** chk0
!*******************
  SUBROUTINE erro(text, ivar, chav)
    CHARACTER(*)::text
    INTEGER, OPTIONAL::ivar
    CHARACTER(*), OPTIONAL::chav
    LOGICAL::l1, l2
    CHARACTER(len=300)::dumt
    l1 = present(ivar)
    l2 = present(chav)
    IF (l1 .AND. l2) THEN
      WRITE (dumt, *, err=100) ", ", ivar, ", ", chav
    ELSE IF (.NOT. l1 .AND. l2) THEN
      WRITE (dumt, *, err=100) ", ", chav
    ELSE IF (l1 .AND. .NOT. l2) THEN
      WRITE (dumt, *, err=100) ", ", ivar
    ELSE IF (.NOT. l1 .AND. .NOT. l2) THEN
      WRITE (dumt, *, err=100) " "
    END IF
    CALL stringremoveheadspaces(dumt)
    CALL stringremovedoublespaces(dumt)
100 CONTINUE
    WRITE (uavi, "(3a)", iostat=ier) "error:", trim(text), trim(dumt)
    WRITE (*, '(3a)') "! a fatal error has occurred:", trim(text), trim(dumt)
    STOP
  END SUBROUTINE erro
!*****************************************************
!*** MANAGES TIME
!*** IJOB=-1-> STARTING QUANTITIES
!*** IJOB=0->  UPDATES HISTORY AND DOFS
!*** IJOB=1->  UPDATES HISTORY, DOFS, STEPS
!              TIME AND PROPORTIONAL LOADS
!*** IMPORTANT: ALWAYS DETERMINES RELEVANT QUANTITIES
!***            BEFORE UPDATING HISTORY VARIABLES
!*****************************************************
!*** CHK0
  SUBROUTINE TIMEX(IJOB)
    IMPLICIT REAL(8) (A-H, O-Z)
    INTEGER, DIMENSION(:), ALLOCATABLE::ISOLAT
    REAL(8), DIMENSION(:, :), ALLOCATABLE::UPDCOOR
    IF (IJOB .EQ. -1) THEN
!-----------------------
!*** INITIAL CONDITIONS
!-----------------------
      IF (abs(TIME) .LE. EPSMACH()) THEN
        DO I = 1, NIC
          IDF = ICDF(I)
          RVL = ICVL(I)
          DO J = ICIN(I), ICIN(I+1)-1
            INO = ICNO(J)
            IF (ICDT(I)) THEN
              VELO(IDF, INO, 1) = RVL
              VELO(IDF, INO, 2) = RVL
              NODOF(IDF, INO, 2) = DTIME*0.5d00*RVL
            ELSE
              NODOF(IDF, INO, 1) = RVL
              NODOF(IDF, INO, 2) = RVL
            END IF
          END DO
        END DO
!----------------------------------------------
!*** ITERATE FOR ACCELERATION ONLY IF REQUIRED
!----------------------------------------------
        IF (abs(PRPANAL(14)) .GT. EPSMACH()) THEN
          CALL ALLOCSAFE(NNO, ISOLAT)
          WRITE (*, *) "BEGIN ITERATING FOR ACCELERATION"
          CALL DRVFIR(0)
          CALL DRVFIR(1)
          MEQIT = 12
          LOADMODE_SAVE = LOADMODE
          LOADMODE = ZEROLOAD
          ACEL = 0.0d00
          DO IEQIT = 1, MEQIT
            CALL DRVFIR(2)
            CALL DRVDES
            CALL DETERRORS(ISOLAT, RESIDISP, RESIDNL, RESIDT, PARTDISP, PARTNL, PARTT)
            DO INO = 1, NNO
              DO JGL = 1, NTGL
                ACEL(JGL, INO, 1) = ACEL(JGL, INO, 1)+DDES(JGL, INO)
                ACEL(JGL, INO, 2) = ACEL(JGL, INO, 1)
              END DO
            END DO
          END DO
          CALL ALLOCSAFE(0, ISOLAT)
          TIME = 5.0d-14
          TIMEO = TIME
          LOADMODE = LOADMODE_SAVE
          WRITE (*, *) "END ITERATING FOR ACCELERATION"
        END IF
      END IF
    ELSE
!----------------------------------
!*** NOW PERFORMS THE REQUIRED JOB
!----------------------------------
      IF (IJOB .EQ. 1) THEN
!------------------
!*** STEP INCREASE
!------------------
        ISTEP = ISTEP+1
!----------------------
!*** START TIME UPDATE
!----------------------
        RLOAO = RLOAD
        TIMEO = TIME
        TIMET = TIMEO+DTIME
        IF (TIMET .GT. TTIME .AND. PRPANAL(14) .LE. EPSMACH()) THEN
          CALL UPDATEDTIME(TTIME-TIMEO)
        END IF
        TIME = TIMEO+DTIME
        TIMET = TIMEO+DTIME
!----------------------------------
!*** GO THROUGH SPECIAL TIME STEPS
!*** AND SETS TIME
!----------------------------------
        IF (PRPANAL(14) .LE. EPSMACH()) THEN
          DO IS = 1, NSPECT
            IF (HSPECT(IS, 2) .EQ. 0 .AND. SPECT(IS) .LE. TIMET) THEN
              CALL UPDATEDTIME(SPECT(IS)-TIMEO)
              HSPECT(IS, 2) = 1
              EXIT
            END IF
          END DO
        END IF
        TIME = TIMEO+DTIME
      END IF
!----------------------------------
!*** EXTRAPOLATION BEFORE UPDATING
!----------------------------------
      CALL EXTRAP
!----------------------------
!*** STRAIN RATE CALCULATION
!----------------------------
      IF (IHOMOTOPY .EQ. 1 .AND. DTIME .GT. 0.0d00) THEN
        DO CONCURRENT(J=1:NEL, I=1:NMGA)
          GPHIST(38, I, J, 2) = (GPHIST(1, I, J, 2)-GPHIST(1, I, J, 1))/DTIME
        END DO
      END IF
!------------------------------------
!*** NOW THE REMAINING COPIES
!*** CONVERGED BECOMES TRUE (1<---2)
!------------------------------------
      IF (NSPECT .GT. 0) THEN
        HSPECT(1:NSPECT, 1) = HSPECT(1:NSPECT, 2)
      END IF
      DO CONCURRENT(K=1:NEL, J=1:NMGA, I=1:6)
        GPSTRE(I, J, K, 1) = GPSTRE(I, J, K, 2)
      END DO
!--------------------------
!*** SETS AVERAGES TO ZERO
!*** FOR THE OUTPUT GAUSS
!*** POINTS
!--------------------------
      DO CONCURRENT(IEL=1:NEL, IV=1:2)
        GPSTRE(1:6, IV, IEL, 2) = 0.0d00
        GPHIST(1:MVPG, IV, IEL, 2) = 0.0d00
      END DO
!--------------------------
!*** HISTORY UPDATE PART 1
!--------------------------
      DO CONCURRENT(K=1:NEL, J=1:NMGA, I=1:MVPG)
        GPHIST(I, J, K, 1) = GPHIST(I, J, K, 2)
      END DO
!-----------------
!*** STATE UPDATE
!-----------------
      IF (.NOT. allocated(DYNVELO1)) CALL ALLOCSAFE(NTGL, NNO, DYNVELO1)
      DO CONCURRENT(J=1:NNO, I=1:NTGL)
        DYNVELO1(I, J) = NODOF(I, J, 2)
      END DO
      DO CONCURRENT(J=1:NNO, I=1:NTGL)
        NODOF(I, J, 1) = NODOF(I, J, 2)
        VELO(I, J, 1) = VELO(I, J, 2)
        ACEL(I, J, 1) = ACEL(I, J, 2)
      END DO
      DO CONCURRENT(J=NNO+1:NNOT, I=1:NTGL)
        ALPHADOF(I, J-NNO, 1) = ALPHADOF(I, J-NNO, 2)
      END DO
!------------------------------
!*** STRAIN ENERGY CALCULATION
!------------------------------
      STRAINENERGY = 0.0d00
      DO IEL = 1, NEL
        STRAINENERGY = STRAINENERGY+ELHIST(10, IEL, 2)
      END DO
      WRITE (*, *) "CALCULATED STRAIN ENERGY"
!-----------------
!*** TOTAL ENERGY
!-----------------
      TOTALENERGY = STRAINENERGY-PRODDISP(REAC, NODOF(1:NTGL, 1:NNO, 2))
!---------------------
!*** CONTACT DETECTION
!*** 2D AND 3D
!---------------------
      CALL ALLOCSAFE(3, NNO, UPDCOOR)
      DO CONCURRENT(ino=1:nno, id=1:3)
        UPDCOOR(ID, INO) = NOCO(ID, INO)+NODOF(ID, INO, 1)
      END DO
      CALL BUCKETCREATE(BS, NNO, NEL, ELNI, ELNO, UPDCOOR)
      DEALLOCATE (UPDCOOR)
      IF (PRPANAL(17) .GT. 0.1d00) CALL CONTACTDETECTION
!--------------------------
!*** ELEMENT HISTORY UPDATE
!*** AFTER CONTACT
!*** OF COURSE
!--------------------------
      DO CONCURRENT(J=1:NEL, I=1:MVPE)
        ELHIST(I, J, 1) = ELHIST(I, J, 2)
      END DO
!---------------------------------------------
!*** ATTENTION: DOES NOT SET ROTATIONS TO ZERO
!*** THIS LINE IS HERE FOR REFERENCE ONLY
!*** DO NOT UNCOMMENT
!*** THE SAME FOR ALPHADOF!
!---------------------------------------------
!      NODOF(4:6,1:NNO,1:2)=0.0D00
!      ALPHADOF(1:NTGL,1:,1:2)=0.0D00
    END IF
    CALL BUCKETCREATE(BSO, NNO, NEL, ELNI, ELNO, NOCO(1:3, 1:NNO))
!--------------------------------
!*** NORMAL VECTORS FROM GEOMETRY
!*** ALWAYS RECALCULATED
!--------------------------------
    CALL VNORNFROMGEOMETRY
!--------------------------------------
!*** IF NECESSARY, UPDATES TARGET VALUE
!--------------------------------------
    IF (INOTARGET .NE. 0 .AND. IDOFTARGET .NE. 0) THEN
      RTARGET = max(abs(NODOF(IDOFTARGET, INOTARGET, 2)), RTARGET)
    END IF
  END SUBROUTINE TIMEX
!*******************
!*** SIZE OF AN EDGE
!*** CHK0
!*******************
  REAL(8) FUNCTION EDGESIZE(JAR)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(2, 3)::XN
    IAR = abs(JAR)
    IF (JAR .LT. 0) THEN
      R = 1.0d00
    ELSE
      R = 0.0d00
    END IF
    IN1 = ARNO(ARNI(IAR))
    IN2 = ARNO(ARNI(IAR+1)-1)
    XN(1, 1:3) = NOCO(1:3, IN1)+R*NODOF(1:3, IN1, 1)
    XN(2, 1:3) = NOCO(1:3, IN2)+R*NODOF(1:3, IN2, 1)
    EDGESIZE = VECTNORM2(3, XN(1, 1:3)-XN(2, 1:3))
  END FUNCTION EDGESIZE
!******************
!*** AREA OF A FACE
!*** CHK0
!******************
  REAL(8) FUNCTION AREAFACE(JFA)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3, 4)::XN
    IFA = abs(JFA)
    IF (JFA .LT. 0) THEN
      R = 1.0d00
    ELSE
      R = 0.0d00
    END IF
    NNF = FANI(IFA+1)-FANI(IFA)
    DO IN = 1, NNF
      DO ID = 1, 3
        XN(ID, IN) = NOCO(ID, FANO(FANI(IFA)+IN-1))+R*NODOF(ID, FANO(FANI(IFA)+IN-1), 1)
      END DO
    END DO
    IF (NNF .EQ. 3) THEN
      AREAFACE = GEO3DAREATRIANGLE(XN(1:3, 1), XN(1:3, 2), XN(1:3, 3))
    ELSE IF (NNF .EQ. 4) THEN
      AREAFACE = GEO3DAREAQUAD(XN(1:3, 1), XN(1:3, 2), XN(1:3, 3), XN(1:3, 4))
    ELSE
      STOP "areaface"
    END IF
  END FUNCTION AREAFACE
!*************************************
!*** PROVIDE A LOCAL ORDERING OF NODES
!*** IN A LAYER OF HEX
!*************************************
  SUBROUTINE ORDERLOCALNODESHEX(IELEM, LN, ILN)
    IMPLICIT REAL(8) (A-H, O-Z)
    INTEGER, DIMENSION(8)::LN, ILN, PER
    INTEGER, DIMENSION(4, 6)::LFA
    CALL LISTFACES(6, LFA)
    DO I = 1, 8
      PER(I) = 9-I
    END DO
!------------------------------------------
!*** FIND IF TWO OPPOSING FACES ARE 'OUTER'
!------------------------------------------
    ISUCC = 0
    IFA1 = 1
    JFA1 = ELFA(ELFI(IELEM)-1+IFA1)
    IFA2 = 3
    JFA2 = ELFA(ELFI(IELEM)-1+IFA2)
    IF (IFOUTERFACE(JFA1) .AND. IFOUTERFACE(JFA2)) THEN
      ISUCC = 1
      LN(1) = 1
      LN(2) = 5
      LN(3) = 6
      LN(4) = 2
      LN(5) = 4
      LN(6) = 8
      LN(7) = 7
      LN(8) = 3
      IF (IPRESSOTHER3D(IELEM, JFA1) .GT. 0 .AND. IPRESSOTHER3D(IELEM, JFA2) .EQ. 0) THEN
        CALL listpermute(8, PER, LN, 1)
      END IF
    END IF
    IFA1 = 2
    JFA1 = ELFA(ELFI(IELEM)-1+IFA1)
    IFA2 = 4
    JFA2 = ELFA(ELFI(IELEM)-1+IFA2)
    IF (IFOUTERFACE(JFA1) .AND. IFOUTERFACE(JFA2)) THEN
      ISUCC = 1
      LN(1) = 4
      LN(2) = 8
      LN(3) = 5
      LN(4) = 1
      LN(5) = 3
      LN(6) = 7
      LN(7) = 6
      LN(8) = 2
      IF (IPRESSOTHER3D(IELEM, JFA1) .EQ. 0 .AND. IPRESSOTHER3D(IELEM, JFA2) .GT. 0) THEN
        CALL listpermute(8, PER, LN, 1)
      END IF
    END IF
    IFA1 = 5
    JFA1 = ELFA(ELFI(IELEM)-1+IFA1)
    IFA2 = 6
    JFA2 = ELFA(ELFI(IELEM)-1+IFA2)
    IF (IFOUTERFACE(JFA1) .AND. IFOUTERFACE(JFA2)) THEN
      ISUCC = 1
      LN(1) = 4
      LN(2) = 1
      LN(3) = 2
      LN(4) = 3
      LN(5) = 8
      LN(6) = 5
      LN(7) = 6
      LN(8) = 7
      IF (IPRESSOTHER3D(IELEM, JFA1) .GT. 0 .AND. IPRESSOTHER3D(IELEM, JFA2) .EQ. 0) THEN
        CALL listpermute(8, PER, LN, 1)
      END IF
    END IF
    IF (ISUCC .EQ. 0) THEN
!----------------------------------
!*** CHECK WHAT FACE IS THE LARGEST
!----------------------------------
      AMAX = -huge(1.0d00)
      IFM = 0
      IFL = 0
      DO I = ELFI(IELEM), ELFI(IELEM+1)-1
        IFL = IFL+1
        IFA = ELFA(I)
        AR = AREAFACE(IFA)
        IF (AR .GT. AMAX) THEN
          AMAX = AR
          IFM = IFL
        END IF
      END DO
!--------------------------------------
!*** SELECTS THE CORRECT NODE NUMBERING
!--------------------------------------
      SELECT CASE (IFM)
      CASE (1, 3)
        LN(1) = 1
        LN(2) = 5
        LN(3) = 6
        LN(4) = 2
        LN(5) = 4
        LN(6) = 8
        LN(7) = 7
        LN(8) = 3
      CASE (2, 4)
        LN(1) = 4
        LN(2) = 8
        LN(3) = 5
        LN(4) = 1
        LN(5) = 3
        LN(6) = 7
        LN(7) = 6
        LN(8) = 2
      CASE (5, 6)
        LN(1) = 4
        LN(2) = 1
        LN(3) = 2
        LN(4) = 3
        LN(5) = 8
        LN(6) = 5
        LN(7) = 6
        LN(8) = 7
      END SELECT
    END IF
    DO I = 1, 8
      ILN(LN(I)) = I
    END DO
  END SUBROUTINE ORDERLOCALNODESHEX
!**************************
!*** VOLUME OF A 'MATERIAL'
!**************************
  REAL(8) FUNCTION VOLUMEMAT(JMA)
    IMPLICIT REAL(8) (A-H, O-Z)
    IMA = abs(JMA)
    IS = sign(1, JMA)
    RV = 0.0d00
    IF (IF3D) THEN
      DO IEL = 1, NEL
        IF (ISCONTINUUMELEMENT(IEL)) THEN
          IF (ELMA(IEL) .EQ. IMA) THEN
            JEL = IEL*IS
            RV = RV+VOLUMEEL(JEL)
          END IF
        END IF
      END DO
    END IF
    VOLUMEMAT = RV
  END FUNCTION VOLUMEMAT
!**************************
!*** VOLUME OF A TET OR HEX
!*** CHK0
!**************************
  REAL(8) FUNCTION VOLUMEEL(JEL)
    IMPLICIT REAL(8) (A-H, O-Z)
    INTEGER, PARAMETER::MN = 8
    REAL(8), DIMENSION(3, MN)::XN
    IEL = abs(JEL)
    IF (JEL .LT. 0) THEN
      R = 1.0d00
    ELSE
      R = 0.0d00
    END IF
    IEP = NELPRE(IEL)
    IF (GEOTOP(IEP) .EQ. 3) THEN
      NNF = NNOSC(IEP)
      IF (NNF .GT. MN) CALL ERRO("TOO MANY NODES FOR A VOLUMETRIC ELEMENT")
      DO IN = 1, NNF
        DO ID = 1, 3
          XN(ID, IN) = NOCO(ID, ELNO(ELNI(IEL)+IN-1))+R*NODOF(ID, ELNO(ELNI(IEL)+IN-1), 1)
        END DO
      END DO
      IF (NNF .EQ. 4) THEN
        VOLUMEEL = geo3dtetvolume(XN(1:3, 1), XN(1:3, 2), XN(1:3, 3), XN(1:3, 4))
      ELSE IF (NNF .EQ. 8) THEN
        VOLUMEEL = geo3dhexvolume(XN(1:3, 1), XN(1:3, 2), XN(1:3, 3), XN(1:3, 4), XN(1:3, 5), XN(1:3, 6), XN(1:3, 7), XN(1:3, 8))
      ELSE
        STOP "volumeel"
      END IF
    ELSE
      VOLUMEEL = 0.0d00
    END IF
  END FUNCTION VOLUMEEL
!**********************
!*** "VOLUME" OF A NODE
!**********************
  REAL(8) FUNCTION VOLUMENODE(INO)
    IMPLICIT REAL(8) (A-H, O-Z)
    RE = 0.0d00
    KE = 0
    DO IK = NOIL(INO), NOIL(INO+1)-1
      IEL = NOEL(IK)
      IF (GEOTOP(NELPRE(IEL)) .EQ. 3) THEN
        KE = KE+1
        RE = RE+VOLUMEEL(IEL)
      END IF
    END DO
    VOLUMENODE = RE
  END FUNCTION VOLUMENODE
!*******************************************
!*** DISTANCE BETWEEN TWO NODES
!*** OR BETWEEN THE UNDEFORMED
!*** AND DEFORMED POSITIONS OF THE SAME NODE
!*** NODE DISTANCE
!*** CHK0
!*******************************************
  REAL(8) FUNCTION RNODEDIST(JN1, JN2)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::XN1, XN2
    IN1 = abs(JN1)
    IN2 = abs(JN2)
    IF (JN1 .LT. 0) THEN
      R1 = 1.0d00
    ELSE
      R1 = 0.0d00
    END IF
    IF (JN2 .LT. 0) THEN
      R2 = 1.0d00
    ELSE
      R2 = 0.0d00
    END IF
    DO ID = 1, 3
      XN1(ID) = NOCO(ID, IN1)+R1*NODOF(ID, IN1, 1)
      XN2(ID) = NOCO(ID, IN2)+R2*NODOF(ID, IN2, 1)
    END DO
    RNODEDIST = VECTNORM2(3, XN2-XN1)
  END FUNCTION RNODEDIST
!***************************************
!*** CHECKS IF AN EDGE IS OUTER IFF
!*** THERE IS AT MOST ONE FACE WITH MORE
!*** NODES THAN THE EDGE SHARING IT
!*** JUST FOR NON-VOLUME
!*** CHK0
!***************************************
  LOGICAL FUNCTION IFOUTEREDGE(IAR)
    IF (.NOT. IF3D) THEN
      IFOUTEREDGE = .FALSE.
      NED = ARIL(IAR+1)-ARIL(IAR)
      IK = 0
      DO I = 1, NED
        IEL = AREL(ARIL(IAR)-1+I)
        IF (ISCONTINUUMELEMENT(IEL)) IK = IK+1
      END DO
      IF (IK .EQ. 1) IFOUTEREDGE = .TRUE.
    ELSE
      DO I = ARFI(IAR), ARFI(IAR+1)-1
        IF (IFOUTERFACE(ARFA(I))) THEN
          IFOUTEREDGE = .TRUE.
          RETURN
        END IF
      END DO
      IFOUTEREDGE = .FALSE.
    END IF
  END FUNCTION IFOUTEREDGE
!******************************************
!*** CHECKS IF A FACE IS OUTER IFF
!*** THERE IS AT MOST ONE ELEMENT WITH MORE
!*** NODES THAN THE FACE SHARING IT
!*** CHK0
!******************************************
  LOGICAL FUNCTION IFOUTERFACE(IFA)
    IF (IF3D) THEN
      IFOUTERFACE = .FALSE.
      NED = FAIL(IFA+1)-FAIL(IFA)
      IK = 0
      DO I = 1, NED
        IEL = FAEL(FAIL(IFA)-1+I)
        IF (ISCONTINUUMELEMENT(IEL)) IK = IK+1
      END DO
      IF (IK .LE. 1) IFOUTERFACE = .TRUE.
    ELSE
      IFOUTERFACE = .TRUE.
    END IF
  END FUNCTION IFOUTERFACE
!*****************************
!*** CHECKS IF A NODE IS OUTER
!*** EITHER 2D OR 3D
!*** CHK0
!*****************************
  LOGICAL FUNCTION IFOUTERNODE(INO)
    IFOUTERNODE = .FALSE.
    IF (IF3D) THEN
      DO I = NOFI(INO), NOFI(INO+1)-1
        IFA = NOFA(I)
        IF (IFOUTERFACE(IFA)) THEN
          IFOUTERNODE = .TRUE.
          RETURN
        END IF
      END DO
    ELSE
      DO I = NOIR(INO), NOIR(INO+1)-1
        IAR = NOAR(I)
        IF (IFOUTEREDGE(IAR)) THEN
          IFOUTERNODE = .TRUE.
          RETURN
        END IF
      END DO
    END IF
  END FUNCTION IFOUTERNODE
!*******************************************************
!*** provide TWO EDGES EXTERNAL OF A GIVEN EXTERNAL NODE
!*** CHK0
!*******************************************************
  SUBROUTINE EXTERNALEDGESOFNODE2D(INO, IL1, IL2)
    IL1 = 0
    IL2 = 0
    DO IA = NOIR(INO), NOIR(INO+1)-1
      IAR = NOAR(IA)
      IF (IFOUTEREDGE(IAR)) THEN
        IF (IL1 .EQ. 0) THEN
          IL1 = IAR
        ELSE
          IL2 = IAR
        END IF
      END IF
    END DO
  END SUBROUTINE EXTERNALEDGESOFNODE2D
!***************************************************
!*** LIST OF FACES EXTERNAL OF A GIVEN EXTERNAL NODE
!*** CHK0
!***************************************************
  SUBROUTINE EXTERNALFACESOFNODE3D(INO, NEA, LEA)
    INTEGER, DIMENSION(:), ALLOCATABLE::LEA
    NEA = 0
    DO IA = NOFI(INO), NOFI(INO+1)-1
      IFA = NOFA(IA)
      IF (IFOUTERFACE(IFA)) THEN
        NEA = NEA+1
        CALL LISTINSERT(LEA, NEA, IFA)
      END IF
    END DO
  END SUBROUTINE EXTERNALFACESOFNODE3D
!*****************************************
!*** AVERAGE NODAL NORMAL DEFORMED VERSION
!*** 2D
!*** CHK0
!*****************************************
  SUBROUTINE EXTERNALNODENORMAL2D(INO, N)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(2)::N
    REAL(8), DIMENSION(3)::NODEN, N1, N2
    CALL EXTERNALEDGESOFNODE2D(INO, IL1, IL2)
    IF ((IL1 .NE. 0) .AND. (IL2 .EQ. 0)) THEN
      STOP "ERROR IN EXTERNALNODENORMAL2D"
    END IF
    IF (IL1*IL2 .NE. 0) THEN
      N1 = EDGENORMAL2D(-IL1)
      N2 = EDGENORMAL2D(-IL2)
      NODEN = N1+N2
      CALL VECTNORMALIZE(3, NODEN)
      N(1:2) = NODEN(1:2)
    ELSE
      STOP "MISTAKE IN EN2D"
      N = 0.0d00
    END IF
  END SUBROUTINE EXTERNALNODENORMAL2D
!************************
!*** AVERAGE NODAL NORMAL
!*** DEFORMED VERSION
!*** 3D
!*** CHK0
!************************
  SUBROUTINE EXTERNALNODENORMAL3D(INO, N)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::N
    INTEGER, DIMENSION(:), ALLOCATABLE::LEA
    CALL EXTERNALFACESOFNODE3D(INO, NEA, LEA)
!*** ARITHMECTIC AVERAGE
    N = 0.0d00
    DO IK = 1, NEA
      N = N+FACENORMAL(-LEA(IK))
    END DO
    CALL VECTNORMALIZE(3, N)
  END SUBROUTINE EXTERNALNODENORMAL3D
!**************************
!*** linear edge normal
!*** DEFORMED version
!*** normally interpolated
!*** inward
!*** chk0
!**************************
  SUBROUTINE LINEAREDGENORMAL2D(INO, INO1, INO2, N)
    IMPLICIT REAL(8) (A-H, O-Z)
    LOGICAL::FOUND
    REAL(8), DIMENSION(3)::X, X1, X2
    REAL(8), DIMENSION(2)::N, N1, N2
    X = NOCO(1:3, INO)+NODOF(1:3, INO, 1)
    X1 = NOCO(1:3, INO1)+NODOF(1:3, INO1, 1)
    X2 = NOCO(1:3, INO2)+NODOF(1:3, INO2, 1)
    CALL EXTERNALNODENORMAL2D(INO1, N1)
    CALL EXTERNALNODENORMAL2D(INO2, N2)
    CALL GEO2DDETERMINESCOORDINATE(X1(1:2), X2(1:2), X(1:2), XI, FOUND)
    XI = min(1.0d00, max(-1.0d00, XI))
    N = ((1.0d00-XI)/2.0d00)*N1+((1.0d00+XI)/2.0d00)*N2
    CALL VECTNORMALIZE(2, N)
    N = -N
  END SUBROUTINE LINEAREDGENORMAL2D
!**************************
!*** LINEAR FACE NORMAL
!*** NORMALLY INTERPOLATED
!*** DEFORMED VERSION
!*** inward
!*** CHK0
!**************************
  SUBROUTINE LINEARFACENORMAL(INO, INO1, INO2, INO3, N)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::N, XNO, FF
    REAL(8), DIMENSION(3, 3)::XFN, XCF
    N = 0.0d00
!-------------------
!*** COORDINATES INO
!-------------------
    DO ID = 1, 3
      XNO(ID) = NOCO(ID, INO)+NODOF(ID, INO, 1)
    END DO
!--------------------------
!*** COORDINATES AND NORMAL
!--------------------------
    XCF(1, 1:3) = NOCO(1:3, INO1)+NODOF(1:3, INO1, 1)
    XCF(2, 1:3) = NOCO(1:3, INO2)+NODOF(1:3, INO2, 1)
    XCF(3, 1:3) = NOCO(1:3, INO3)+NODOF(1:3, INO3, 1)
    XFN(1, 1:3) = VNORN(1:3, INO1)
    XFN(2, 1:3) = VNORN(1:3, INO2)
    XFN(3, 1:3) = VNORN(1:3, INO3)
!--------------------
!*** NOW DETERMINE FF
!--------------------
    CALL GEO3DPROJTRIANGLE(XNO, XCF(1, 1:3), XCF(2, 1:3), XCF(3, 1:3), FF)
!----------
!*** NORMAL
!----------
    N = 0.0d00
    DO IN = 1, 3
      N = N+FF(IN)*XFN(IN, 1:3)
    END DO
    CALL vectnormalize(3, N)
    N = -N
  END SUBROUTINE LINEARFACENORMAL
!****************************
!*** OTHER NODE GIVEN AN EDGE
!*** AND ONE OF THE NODES
!*** CHK0
!****************************
  INTEGER FUNCTION NODEOTHER(INO, IAR)
    NODEOTHER = 0
    IF (ARNO(ARNI(IAR)) .EQ. INO) THEN
      NODEOTHER = ARNO(ARNI(IAR)+1)
    ELSE IF (ARNO(ARNI(IAR)+1) .EQ. INO) THEN
      NODEOTHER = ARNO(ARNI(IAR))
    END IF
  END FUNCTION NODEOTHER
!***************************
!*** AN EDGE GIVEN TWO NODES
!*** CHK0
!***************************
  INTEGER FUNCTION IEDGEFROMNODES(INO1, INO2)
    JAR = 0
    DO IK = NOIR(INO1), NOIR(INO1+1)-1
      IAR = NOAR(IK)
      IF (NODEOTHER(INO1, IAR) .EQ. INO2) THEN
        JAR = IAR
        EXIT
      END IF
    END DO
    IEDGEFROMNODES = JAR
  END FUNCTION IEDGEFROMNODES
!**********************************************************
!*** DETERMINES THE OTHER CONTINUUM ELEMENT SHARING AN EDGE
!*** WHEN CALLED WITH IEL=0 FINDS THE FIRST
!*** CHK0
!**********************************************************
  INTEGER FUNCTION IELOTHER2D(IEL, IAR)
    IEL2 = 0
    NED = ARIL(IAR+1)-ARIL(IAR)
    IF (NED .GE. 2 .OR. IEL .EQ. 0) THEN
      DO I = 1, NED
        IEL3 = AREL(ARIL(IAR)-1+I)
        IF (IEL3 .NE. IEL .AND. ISCONTINUUMELEMENT(IEL3)) THEN
          IEL2 = IEL3
          EXIT
        END IF
      END DO
    END IF
    IELOTHER2D = IEL2
  END FUNCTION IELOTHER2D
!*********************************************************
!*** DETERMINES THE OTHER CONTINUUM ELEMENT SHARING A FACE
!*** CALLED WITH IEL=0 FINDS THE FIRST
!*** CHK0
!*********************************************************
  INTEGER FUNCTION IELOTHER3D(IEL, IFA)
    IEL2 = 0
    NED = FAIL(IFA+1)-FAIL(IFA)
    IF (NED .GE. 1 .OR. IEL .EQ. 0) THEN
      DO I = 1, NED
        IEL3 = FAEL(FAIL(IFA)-1+I)
        IF (IEL3 .NE. IEL .AND. ISCONTINUUMELEMENT(IEL3)) THEN
          IEL2 = IEL3
          EXIT
        END IF
      END DO
    END IF
    IELOTHER3D = IEL2
  END FUNCTION IELOTHER3D
!********************************************************
!*** DETERMINES THE OTHER PRESSURE ELEMENT SHARING A EDGE
!*** CALLED WITH IEL=0 FINDS THE FIRST
!*** CHK0
!********************************************************
  INTEGER FUNCTION IPRESSOTHER2D(IEL, IAR)
    IEL2 = 0
    NED = ARIL(IAR+1)-ARIL(IAR)
    IF (NED .GE. 1 .OR. IEL .EQ. 0) THEN
      DO I = 1, NED
        IEL3 = AREL(ARIL(IAR)-1+I)
        IF (IEL3 .NE. IEL .AND. (.NOT. ISCONTINUUMELEMENT(IEL3))) THEN
          IEL2 = IEL3
          EXIT
        END IF
      END DO
    END IF
    IPRESSOTHER2D = IEL2
  END FUNCTION IPRESSOTHER2D
!********************************************************
!*** DETERMINES THE OTHER PRESSURE ELEMENT SHARING A FACE
!*** CALLED WITH IEL=0 FINDS THE FIRST
!*** CHK0
!********************************************************
  INTEGER FUNCTION IPRESSOTHER3D(IEL, IFA)
    IEL2 = 0
    NED = FAIL(IFA+1)-FAIL(IFA)
    IF (NED .GE. 1 .OR. IEL .EQ. 0) THEN
      DO I = 1, NED
        IEL3 = FAEL(FAIL(IFA)-1+I)
        IF (IEL3 .NE. IEL .AND. (.NOT. ISCONTINUUMELEMENT(IEL3))) THEN
          IEL2 = IEL3
          EXIT
        END IF
      END DO
    END IF
    IPRESSOTHER3D = IEL2
  END FUNCTION IPRESSOTHER3D
!*********************************************************
!*** DETERMINES THE TWO CONTINUUM ELEMENTS SHARING AN EDGE
!*** CHK0
!*********************************************************
  SUBROUTINE TWOELEMENTSEDGE(IAR, IE1, IE2)
    IE1 = IELOTHER2D(0, IAR)
    IF (IE1 .NE. 0) THEN
      IE2 = IELOTHER2D(IE1, IAR)
    ELSE
      IE2 = 0
    END IF
  END SUBROUTINE TWOELEMENTSEDGE
!**********************************************
!*** DETERMINES THE TWO ELEMENTS SHARING A FACE
!*** CHK0
!**********************************************
  SUBROUTINE TWOELEMENTSFACE(IFA, IE1, IE2)
    IE1 = IELOTHER3D(0, IFA)
    IE2 = IELOTHER3D(IE1, IFA)
  END SUBROUTINE TWOELEMENTSFACE
!************************
!*** NORMAL TO FACE
!*** NEGATIVE JFA IS
!*** THE DEFORMED VERSION
!*** POINTS OUTWARD
!*** CHK0
!************************
  FUNCTION FACENORMAL(JFA)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::FACENORMAL
    REAL(8), DIMENSION(3, 4)::XCF
    IFA = abs(JFA)
    IF (JFA .LT. 0) THEN
      R = 1.0d00
    ELSE
      R = 0.0d00
    END IF
    NNF = FANI(IFA+1)-FANI(IFA)
    INL = 0
    DO I = FANI(IFA), FANI(IFA+1)-1
      INO = FANO(I)
      INL = INL+1
      DO ID = 1, 3
        XCF(ID, INL) = NOCO(ID, INO)+R*NODOF(ID, INO, 1)
      END DO
    END DO
    CALL GEO3DGENERALNORMAL(XCF, FACENORMAL, NNF)
    CALL VECTNORMALIZE(3, FACENORMAL)
  END FUNCTION FACENORMAL
!***********************
!*** NORMAL TO EDGE 2D
!*** IF IT IS OUTER EDGE
!*** POINTS OUTWARD
!*** CHK0
!***********************
  FUNCTION EDGENORMAL2D(JAR)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::EDGENORMAL2D
    REAL(8), DIMENSION(3, 2)::XN
    IAR = abs(JAR)
    XN(1:3, 1) = NOCO(1:3, ARNO(ARNI(IAR)))
    XN(1:3, 2) = NOCO(1:3, ARNO(ARNI(IAR)+1))
    IF (JAR .LT. 0) THEN
      XN(1:3, 1) = XN(1:3, 1)+NODOF(1:3, ARNO(ARNI(IAR)), 1)
      XN(1:3, 2) = XN(1:3, 2)+NODOF(1:3, ARNO(ARNI(IAR)+1), 1)
    END IF
    EDGENORMAL2D(1) = XN(2, 2)-XN(2, 1)! 2>1
    EDGENORMAL2D(2) = XN(1, 1)-XN(1, 2)! 1>2=> OUTER
    EDGENORMAL2D(3) = 0.0d00
    CALL VECTNORMALIZE(3, EDGENORMAL2D)
  END FUNCTION EDGENORMAL2D
!*********************
!*** NORMAL TO EDGE 3D
!*** CHK0
!*********************
  FUNCTION EDGENORMAL3D(JAR)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::EDGENORMAL3D, VN1, VN2, VN, DIF
    REAL(8), DIMENSION(3, 2)::XN
    IAR = abs(JAR)
    IN1 = ARNO(ARNI(IAR))
    IN2 = ARNO(ARNI(IAR)+1)
    IF (JAR .LT. 0) THEN
      VN1 = VNORN(1:3, IN1)
      VN2 = VNORN(1:3, IN2)
    ELSE
      VN1 = VNORN0(1:3, IN1)
      VN2 = VNORN0(1:3, IN2)
    END IF
    XN(1:3, 1) = NOCO(1:3, IN1)
    XN(1:3, 2) = NOCO(1:3, IN2)
    IF (JAR .LT. 0) THEN
      XN(1:3, 1) = XN(1:3, 1)+NODOF(1:3, IN1, 1)
      XN(1:3, 2) = XN(1:3, 2)+NODOF(1:3, IN2, 1)
    END IF
    DIF = XN(1:3, 2)-XN(1:3, 1)
    CALL VECTNORMALIZE(3, DIF)
    VN = VN1+VN2
    CALL VECTNORMALIZE(3, VN)
    CALL GEO3DCROSSPROD(DIF, VN, EDGENORMAL3D)
    CALL VECTNORMALIZE(3, EDGENORMAL3D)
  END FUNCTION EDGENORMAL3D
!*****************************************************************************************
!*** CHECKS IF TWO GLOBAL NODES INSIDE AN ELEMENT ARE EITHER CONTERCLOCKWISE OR CLOCKWISE
!*** RESULT 0: DO NOT BELONG TO THE ELEMENT
!***        1: COUNTERCLOCKWISE
!***       -1: CLOCKWISE
!*****************************************************************************************
  INTEGER FUNCTION ISIGN2NODES(ING1, ING2, IEL)
    NNL = ELNI(IEL+1)-ELNI(IEL)
    IBEG = ELNI(IEL)-1
    IRES = 0
    DO I = 1, NNL
      I1 = I
      I2 = SCALARMODULARPOSITION(NNL, I+1)
      IF (ELNO(IBEG+I1) .EQ. ING1 .AND. ELNO(IBEG+I2) .EQ. ING2) THEN
        IRES = 1
        EXIT
      ELSE IF (ELNO(IBEG+I1) .EQ. ING2 .AND. ELNO(IBEG+I2) .EQ. ING1) THEN
        IRES = -1
        EXIT
      END IF
    END DO
    ISIGN2NODES = IRES
  END FUNCTION ISIGN2NODES
!*************************************************************************************
!*** CHECKS IF TWO GLOBAL NODES INSIDE A FACE ARE EITHER CONTERCLOCKWISE OR CLOCKWISE
!*** RESULT 0: DO NOT BELONG TO THE ELEMENT
!***        1: COUNTERCLOCKWISE
!***       -1: CLOCKWISE
!*************************************************************************************
  INTEGER FUNCTION ISIGN2NODESFACES(ING1, ING2, IFA)
    NNL = FANI(IFA+1)-FANI(IFA)
    IBEG = FANI(IFA)-1
    IRES = 0
    DO I = 1, NNL
      I1 = I
      I2 = SCALARMODULARPOSITION(NNL, I+1)
      IF (FANO(IBEG+I1) .EQ. ING1 .AND. FANO(IBEG+I2) .EQ. ING2) THEN
        IRES = 1
        EXIT
      ELSE IF (FANO(IBEG+I1) .EQ. ING2 .AND. FANO(IBEG+I2) .EQ. ING1) THEN
        IRES = -1
        EXIT
      END IF
    END DO
    ISIGN2NODESFACES = IRES
  END FUNCTION ISIGN2NODESFACES
!************************************
!*** DETERMINES WHICH EDGE IS SHARED
!*** BY TWO GIVEN ELEMENTS
!************************************
  INTEGER FUNCTION SHAREEDGE(IEL1, IEL2)
    INTEGER::SHARING
    SHARING = 0
    IF (IEL1 .GT. 0 .AND. IEL2 .GT. 0) THEN
      IF (ISCONTINUUMELEMENT(IEL1) .AND. ISCONTINUUMELEMENT(IEL2)) THEN
        DO IK = ELIR(IEL1), ELIR(IEL1+1)-1
          IAR = ELAR(IK)
          DO JK = ARIL(IAR), ARIL(IAR+1)-1
            KEL = AREL(JK)
            IF (KEL .EQ. IEL2) THEN
              SHARING = IAR
            END IF
          END DO
        END DO
      END IF
    END IF
    SHAREEDGE = SHARING
  END FUNCTION SHAREEDGE
!************************************
!*** DETERMINES WHICH FACE IS SHARED
!*** BY TWO GIVEN ELEMENTS
!************************************
  INTEGER FUNCTION SHAREFACE(IEL1, IEL2)
    INTEGER::SHARING
    SHARING = 0
    IF (IEL1 .GT. 0 .AND. IEL2 .GT. 0) THEN
      IF (ISCONTINUUMELEMENT(IEL1) .AND. ISCONTINUUMELEMENT(IEL2)) THEN
        DO IK = ELFI(IEL1), ELFI(IEL1+1)-1
          IFA = ELFA(IK)
          DO JK = FAIL(IFA), FAIL(IFA+1)-1
            KEL = FAEL(JK)
            IF (KEL .EQ. IEL2) THEN
              SHARING = IFA
            END IF
          END DO
        END DO
      END IF
    END IF
    SHAREFACE = SHARING
  END FUNCTION SHAREFACE
!**************************
!*** FINDS ALL OUTER EDGES
!**************************
  SUBROUTINE OUTEREDGES(IAREX, LAEX)
    INTEGER, DIMENSION(:), ALLOCATABLE::LAEX
    IAREX = 0
    DO IAR = 1, NAR
      IF (IFOUTEREDGE(IAR)) THEN
        IAREX = IAREX+1
      END IF
    END DO
    IF (IAREX .GT. 0) THEN
      ALLOCATE (LAEX(IAREX))
      IAREX = 0
      DO IAR = 1, NAR
        IF (IFOUTEREDGE(IAR)) THEN
          IAREX = IAREX+1
          LAEX(IAREX) = IAR
        END IF
      END DO
    END IF
  END SUBROUTINE OUTEREDGES
!**************************
!*** FINDS ALL OUTER FACES
!**************************
  SUBROUTINE OUTERFACES(IAREX, LAEX)
    INTEGER, DIMENSION(:), ALLOCATABLE::LAEX
    IAREX = 0
    DO IFA = 1, NFA
      IF (IFOUTERFACE(IFA)) THEN
        IAREX = IAREX+1
      END IF
    END DO
    IF (IAREX .GT. 0) THEN
      ALLOCATE (LAEX(IAREX))
      IAREX = 0
      DO IFA = 1, NFA
        IF (IFOUTERFACE(IFA)) THEN
          IAREX = IAREX+1
          LAEX(IAREX) = IFA
        END IF
      END DO
    END IF
  END SUBROUTINE OUTERFACES
!**************************
!*** FINDS ALL OUTER NODES
!**************************
  SUBROUTINE OUTERNODES(INOEX, NOEX)
    INTEGER, DIMENSION(:), ALLOCATABLE::NOEX, LIST
    INOEX = 0
    CALL allocsafe(NNO, LIST)
    LIST = 0
    SELECT CASE (IF3D)
    CASE (.FALSE.)
      DO IK = 1, NAREX
        IAR = LAREX(IK)
        DO JK = ARNI(IAR), ARNI(IAR+1)-1
          INO = ARNO(JK)
          IF (LIST(INO) .NE. 1) THEN
            LIST(INO) = 1
            INOEX = INOEX+1
          END IF
        END DO
      END DO
!*** LONELY NODES
      DO INO = 1, NNO
        IF (NOIR(INO+1) .EQ. NOIR(INO)) THEN
          INOEX = INOEX+1
        END IF
      END DO
      IF (INOEX .GT. 0) ALLOCATE (NOEX(INOEX))
      INOEX = 0
      list = 0
      DO IK = 1, NAREX
        IAR = LAREX(IK)
        DO JK = ARNI(IAR), ARNI(IAR+1)-1
          INO = ARNO(JK)
          IF (LIST(INO) .NE. 1) THEN
            LIST(INO) = 1
            INOEX = INOEX+1
            NOEX(INOEX) = INO
          END IF
        END DO
      END DO
!*** LONELY NODES
      DO INO = 1, NNO
        IF (NOIR(INO+1) .EQ. NOIR(INO)) THEN
          INOEX = INOEX+1
          NOEX(INOEX) = INO
        END IF
      END DO
    CASE (.TRUE.)
      DO IK = 1, NFAEX
        IFA = LFAEX(IK)
        DO JK = FANI(IFA), FANI(IFA+1)-1
          INO = FANO(JK)
          IF (LIST(INO) .NE. 1) THEN
            LIST(INO) = 1
            INOEX = INOEX+1
          END IF
        END DO
      END DO
!*** LONELY NODES
      DO INO = 1, NNO
        IF (NOFI(INO+1) .EQ. NOFI(INO)) THEN
          INOEX = INOEX+1
        END IF
      END DO
      IF (INOEX .GT. 0) ALLOCATE (NOEX(INOEX))
      INOEX = 0
      list = 0
      DO IK = 1, NFAEX
        IFA = LFAEX(IK)
        DO JK = FANI(IFA), FANI(IFA+1)-1
          INO = FANO(JK)
          IF (LIST(INO) .NE. 1) THEN
            LIST(INO) = 1
            INOEX = INOEX+1
            NOEX(INOEX) = INO
          END IF
        END DO
      END DO
!*** LONELY NODES
      DO INO = 1, NNO
        IF (NOFI(INO+1) .EQ. NOFI(INO)) THEN
          INOEX = INOEX+1
          NOEX(INOEX) = INO
        END IF
      END DO
    END SELECT
    IF (allocated(LIST)) DEALLOCATE (LIST)
  END SUBROUTINE OUTERNODES
!*********************************
!*** DETERMINES THE LOCAL NODE
!*** FROM ELEMENT AND GLOBAL NODE
!*** CHK0
!*********************************
  INTEGER FUNCTION LOCALNODE(IEL, ING)
    ISUCC = 0
    INL = 0
    DO IK = ELNI(IEL), ELNI(IEL+1)-1
      INL = INL+1
      IF (ELNO(IK) .EQ. ING) THEN
        ISUCC = 1
        EXIT
      END IF
    END DO
    IF (ISUCC .EQ. 1) THEN
      LOCALNODE = INL
    ELSE
      LOCALNODE = 0
    END IF
  END FUNCTION LOCALNODE
!*********************************
!*** DETERMINES THE LOCAL EDGE
!*** FROM ELEMENT AND GLOBAL EDGE
!*** CHK0
!*********************************
  INTEGER FUNCTION LOCALEDGE(IEL, ING)
    ISUCC = 0
    INL = 0
    DO IK = ELIR(IEL), ELIR(IEL+1)-1
      INL = INL+1
      IF (ELAR(IK) .EQ. ING) THEN
        ISUCC = 1
        EXIT
      END IF
    END DO
    IF (ISUCC .EQ. 1) THEN
      LOCALEDGE = INL
    ELSE
      LOCALEDGE = 0
    END IF
  END FUNCTION LOCALEDGE
!*********************************
!*** DETERMINES THE LOCAL FACE
!*** FROM ELEMENT AND GLOBAL FACE
!*********************************
  INTEGER FUNCTION LOCALFACE(IEL, ING)
    INL = 0
    ISUCC = 0
    DO IK = ELFI(IEL), ELFI(IEL+1)-1
      INL = INL+1
      IF (ELFA(IK) .EQ. ING) THEN
        ISUCC = 1
        EXIT
      END IF
    END DO
    IF (ISUCC .EQ. 1) THEN
      LOCALFACE = INL
    ELSE
      LOCALFACE = 0
    END IF
  END FUNCTION LOCALFACE
!********************************************
!*** GIVES THE VALUE OF A GIVEN ORDERED PAIR
!*** TAKING INTO ACCOUNT:
!*** TIMESHIFT
!*** ORDERED PAIRS
!********************************************
  REAL(8) FUNCTION FUNCVAL(IO)
    IMPLICIT REAL(8) (A-H, O-Z)
    FUNCVAL = 0.0d00
    IF (IO .GT. 0 .AND. IO .LE. NOR) THEN
      CALL FUNCLINEARINTERPOL(ORDE(IO)%N, ORDE(IO)%X, ORDE(IO)%Y, TIME-TIMESHIFT, FUNCVAL, DY)
    ELSE
      CALL WARN("WRONG REQUEST TO FUNCVAL")
    END IF
  END FUNCTION FUNCVAL
!************************************************************
!*** GIVES THE EXTERNAL LOAD FACTOR FOR A GIVEN ORDERED PAIR
!*** DEPENDING ON LOADMODE
!************************************************************
  REAL(8) FUNCTION EXTERNLOAD(IO)
    IMPLICIT REAL(8) (A-H, O-Z)
    IF (IO .NE. 0) THEN
      SELECT CASE (LOADMODE)
      CASE (0)
        EXTERNLOAD = FUNCVAL(IO)
      CASE (1)
        EXTERNLOAD = 1.0d00
      CASE (2)
        EXTERNLOAD = 0.0d00
      CASE (3)
        EXTERNLOAD = RLOAD
      CASE DEFAULT
        STOP "ERROR IN EXTERNLOAD"
      END SELECT
    ELSE
      EXTERNLOAD = 1.0d00
    END IF
  END FUNCTION EXTERNLOAD
!*******************************************
!*** AVERAGE FORCE
!*** ESTIMATE
!*** TO USE WITH "PENALTY-BASED" APPROACHES
!*******************************************
  REAL(8) FUNCTION AVERAGEFORCE()
    IMPLICIT REAL(8) (A-H, O-Z)
!---------------------------
!*** LARGEST YOUNG'S MODULUS
!---------------------------
    YOUNGMAX = 0.0d00
    DO IEL = 1, NEL
      IF (ISCONTINUUMELEMENT(IEL)) THEN
        YOUNGMAX = max(YOUNGMAX, MAPR(1, ELMA(IEL)))
      END IF
    END DO
    AVERAGEFORCE = YOUNGMAX*AVERAGEMESHLENGTH()**2
  END FUNCTION AVERAGEFORCE
!************************
!*** AVERAGE MESH LENGTH
!************************
  REAL(8) FUNCTION AVERAGEMESHLENGTH()
    IMPLICIT REAL(8) (A-H, O-Z)
    AVERAGEMESHLENGTH = 0.5d00*(EDGEMIN+EDGEMAX)
  END FUNCTION AVERAGEMESHLENGTH
!**************************************
!*** UPDATES NODOF WITH A CERTAIN STEP
!*** AND CALCULATES THE ERROR
!*** TAKES CARE WITH THE ROTATION
!**************************************
  SUBROUTINE UPDOF(RESID, STEPTEMP)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), PARAMETER::pi = 4.0d00*atan(1.0d00)
    INTEGER, DIMENSION(:), ALLOCATABLE::isolat
!--------------------------------------
!*** FIND ALL NODES WHICH ARE ISOLATED
!--------------------------------------
    CALL ALLOCSAFE(NNO, ISOLAT)
    DO INO = 1, NNO
      KK = 0
      DO IK = NOIL(INO), NOIL(INO+1)-1
        IEL = NOEL(IK)
        IF (ISCONTINUUMELEMENT(IEL)) THEN
          KK = KK+1
        END IF
      END DO
      IF (KK .EQ. 0) THEN
        ISOLAT(INO) = 1
      END IF
    END DO
!-------------------
!*** SAVE STEP SIZE
!-------------------
    STEP = STEPTEMP
!---------------------------------
!*** SETS GLOBAL RESIDUAL TO ZERO
!---------------------------------
    RESID = 0.0d00
!---------------------
!*** SETS DENOMINATOR
!*** FOR DISPLACEMENTS
!---------------------
    DENOM = CHARL
!-------------------------------------------------
!*** OBTAINS ERRORS FOR DISPLACEMENT AND ROTATION
!-------------------------------------------------
    CALL DETERRORS(ISOLAT, RESIDISP, RESIDNL, RESIDT, PARTDISP, PARTNL, PARTT)
!----------------------------------
!*** SCALES ERRORS
!*** FOR DISPLACEMENT AND ROTATION
!----------------------------------
    RESIDISP = RESIDISP/DENOM
    RESIDT = RESIDT/1.0d-5
!-------------------------------
!*** ACCOUNTS FOR DISPLACEMENTS
!*** AND ROTATIONS
!-------------------------------
    RESID = max(RESIDISP, RESIDT)
!------------------------
!*** PERFORMS STEP UPDATE
!------------------------
    IF (abs(DTIME) .LE. EPSMACH()) THEN
      WRITE (*, "(A)") "DTIME CLOSE TO ZERO"
      RETURN
    END IF
!----------------------------------
!*** DOES THE REST
!*** NEWMARK, BETA=1/4, GAMMA=1/2
!*** CONSTANT AVERAGE ACCELERATION
!----------------------------------
    IF (STEP .GT. EPSMACH()) THEN
      WRITE (*, *) "STEP=", STEP
      DO I = 1, NNO
        DO J = 1, NTGL
          IF (ISOLAT(I) .EQ. 0) THEN
            NODOF(J, I, 2) = NODOF(J, I, 2)+STEP*DDES(J, I)
            IF (PRPANAL(14) .GT. 0.0d00) THEN
              IF (nint(PRPANAL(6)) .EQ. 2) THEN
                IF (IHOMOTOPY .EQ. 1) THEN
                  VELO(J, I, 2) = -VELO(J, I, 1)+4.0d00*(NODOF(J, I, 2)-NODOF(J, I, 1))/DTIME
                  IF (abs(PRPANAL(14)) .GT. 1.0d-12) THEN
                    ACEL(J, I, 2) = -ACEL(J, I, 1)+16.0d00*(NODOF(J, I, 2)-NODOF(J, I, 1))/(DTIME**2)-8.0d00*VELO(J, I, 1)/DTIME
                  END IF
                ELSE
                  VELO(J, I, 2) = DYNVELO1(J, I)/DTIME-4.0d00*NODOF(J, I, 1)/DTIME+3.0d00*NODOF(J, I, 2)/DTIME
                  IF (abs(PRPANAL(14)) .GT. 1.0d-12) THEN
                    ACEL(J, I, 2) = DYNVELO2(J, I)/DTIME-4.0d00*VELO(J, I, 1)/DTIME+3.0d00*VELO(J, I, 2)/DTIME
                  END IF
                END IF
              ELSE
                VELO(J, I, 2) = -VELO(J, I, 1)+2.0d00*(NODOF(J, I, 2)-NODOF(J, I, 1))/DTIME
                IF (abs(PRPANAL(14)) .GT. 1.0d-12) THEN
                  ACEL(J, I, 2) = -ACEL(J, I, 1)+4.0d00*(NODOF(J, I, 2)-NODOF(J, I, 1))/(DTIME**2)-4.0d00*VELO(J, I, 1)/DTIME
                END IF
              END IF
            ELSE
              VELO(J, I, 2) = -VELO(J, I, 1)+2.0d00*(NODOF(J, I, 2)-NODOF(J, I, 1))/DTIME
            END IF
          END IF
        END DO
      END DO
      DO I = NNO+1, NNOT
        DO J = 1, NTGL
          ALPHADOF(J, I-NNO, 2) = ALPHADOF(J, I-NNO, 2)+STEP*DDES(J, I)
        END DO
      END DO
      RLOAD = RLOAD+STEP*DLOAD
    END IF
    CALL ALLOCSAFE(0, ISOLAT)
  END SUBROUTINE UPDOF
!*********************
!*** DETERMINES ERRORS
!*** AND PART VALUES
!*** CHK0
!*********************
  SUBROUTINE DETERRORS(ISOLAT, RESIDISP, RESIDNL, RESIDT, PARTDISP, PARTNL, PARTT)
    IMPLICIT REAL(8) (A-H, O-Z)
    INTEGER, DIMENSION(*)::ISOLAT
    RESIDISP = 0.0d00
    RESIDNL = 0.0d00
    RESIDT = 0.0d00
    PARTDISP = 0.0d00
    PARTNL = 0.0d00
    PARTT = 0.0d00
    DO I = 1, NNO
      IF (ISOLAT(I) .EQ. 0) THEN
        RESIDISP = max(RESIDISP, vectnormuniform(3, DDES(1, I)))
        RESIDNL = max(RESIDNL, abs(DDES(9, I)), abs(DDES(10, I)))
        RESIDT = max(RESIDT, abs(DDES(8, I)))
        IF (IF3D) THEN
          PARTDISP = max(PARTDISP, VECTNORMUNIFORM(3, NODOF(1, I, 2)))
        ELSE
          PARTDISP = max(PARTDISP, VECTNORMUNIFORM(2, NODOF(1, I, 2)))
        END IF
        PARTDISP = max(PARTDISP, VECTNORMUNIFORM(3, NODOF(1, I, 2)))
        PARTNL = max(PARTNL, abs(NODOF(9, I, 2)), abs(NODOF(10, I, 2)), abs(NODOF(7, I, 2)))
        partt = max(partt, abs(nodof(8, i, 2)))
      END IF
    END DO
  END SUBROUTINE DETERRORS
!**************************************
!*** checks if it is acceleration mode
!**************************************
  LOGICAL FUNCTION ACCELERATIONMODE()
    IF ((abs(TIME) .LE. EPSMACH()) .AND. (abs(PRPANAL(14)) .GT. EPSMACH()) .AND. (LOADMODE .EQ. ZEROLOAD)) THEN
      ACCELERATIONMODE = .TRUE.
    ELSE
      ACCELERATIONMODE = .FALSE.
    END IF
  END FUNCTION ACCELERATIONMODE
!*******************************
!*** the derivative of unknowns
!*** must be used at element
!*** level (!) always
!*******************************
  REAL(8) FUNCTION DERIVATIVEDESL()
    IF (ACCELERATIONMODE()) THEN
      DERIVATIVEDESL = 0.0d00
    ELSE
      DERIVATIVEDESL = 1.0d00
    END IF
  END FUNCTION DERIVATIVEDESL
!*******************************
!*** the derivative of velocity
!*** with respect to unknowns
!*** chk
!*******************************
  REAL(8) FUNCTION DERIVATIVEVELO()
    IF (ACCELERATIONMODE()) THEN
      DERIVATIVEVELO = 0.0d00
    ELSE
      IF (PRPANAL(14) .GT. 0.0d00) THEN
        IF (nint(PRPANAL(6)) .EQ. 2) THEN
          IF (IHOMOTOPY .EQ. 1) THEN
            DERIVATIVEVELO = 4.0d00/DTIME
          ELSE
            DERIVATIVEVELO = 3.0d00/DTIME
          END IF
        ELSE
          DERIVATIVEVELO = 2.0d00/DTIME
        END IF
      ELSE
        DERIVATIVEVELO = 2.0d00/DTIME
      END IF
    END IF
  END FUNCTION DERIVATIVEVELO
!***********************************
!*** the derivative of acceleration
!*** according to the step
!***********************************
  REAL(8) FUNCTION DERIVATIVEACEL()
    IF (ACCELERATIONMODE()) THEN
      DERIVATIVEACEL = 1.0d00
    ELSE IF (abs(PRPANAL(14)) .GT. EPSMACH()) THEN
      IF (nint(PRPANAL(6)) .EQ. 2) THEN
        IF (IHOMOTOPY .EQ. 1) THEN
          DERIVATIVEACEL = 16.0d00/(DTIME**2)
        ELSE
          DERIVATIVEACEL = 9.0d00/(DTIME**2)
        END IF
      ELSE
        DERIVATIVEACEL = 4.0d00/(DTIME**2)
      END IF
    ELSE
      DERIVATIVEACEL = 0.0d00
    END IF
  END FUNCTION DERIVATIVEACEL
!************************
!*** QUALITY MEASURES
!*** QUALITY OF TRIANGLE
!*** DEFORMED MESH
!************************
  REAL(8) FUNCTION QUALITYOFTRIANGLE(INO1, INO2, INO3)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::X1, X2, X3
    IORIG = 0
    IN1 = abs(INO1)
    IN2 = abs(INO2)
    IN3 = abs(INO3)
    X1 = NOCO(1:3, IN1)+NODOF(1:3, IN1, 1)
    X2 = NOCO(1:3, IN2)+NODOF(1:3, IN2, 1)
    X3 = NOCO(1:3, IN3)+NODOF(1:3, IN3, 1)
    QUALITYOFTRIANGLE = geo3dtrianglequality(X1, X2, X3)
  END FUNCTION QUALITYOFTRIANGLE
!******************
!*** QUALITY OF TET
!*** DEFORMED MESH
!******************
  REAL(8) FUNCTION QUALITYOFTET(INO1, INO2, INO3, INO4)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::X1, X2, X3, X4
    IN1 = abs(INO1)
    IN2 = abs(INO2)
    IN3 = abs(INO3)
    IN4 = abs(INO4)
    X1 = NOCO(1:3, IN1)+NODOF(1:3, IN1, 1)
    X2 = NOCO(1:3, IN2)+NODOF(1:3, IN2, 1)
    X3 = NOCO(1:3, IN3)+NODOF(1:3, IN3, 1)
    X4 = NOCO(1:3, IN4)+NODOF(1:3, IN4, 1)
    QUALITYOFTET = geo3dtetquality(X1, X2, X3, X4)
  END FUNCTION QUALITYOFTET
!****************
!*** MESH QUALITY
!****************
  REAL(8) FUNCTION AVERAGEMESHQUALITY()
    IMPLICIT REAL(8) (A-H, O-Z)
    RES = 0.0d00
    IKOUNT = 0
    DO IEL = 1, NEL
      IF (NELPRE(IEL) .EQ. 3) THEN
        IKOUNT = IKOUNT+1
        N1 = ELNO(ELNI(IEL))
        N2 = ELNO(ELNI(IEL)+1)
        N3 = ELNO(ELNI(IEL)+2)
        RES = RES+QUALITYOFTRIANGLE(N1, N2, N3)
      ELSE IF (NELPRE(IEL) .EQ. 5) THEN
        IKOUNT = IKOUNT+1
        N1 = ELNO(ELNI(IEL))
        N2 = ELNO(ELNI(IEL)+1)
        N3 = ELNO(ELNI(IEL)+2)
        N4 = ELNO(ELNI(IEL)+3)
        RES = RES+QUALITYOFTET(N1, N2, N3, N4)
      END IF
    END DO
    IF (IKOUNT .NE. 0) THEN
      AVERAGEMESHQUALITY = RES/IKOUNT
    ELSE
      AVERAGEMESHQUALITY = 0.0d00
    END IF
  END FUNCTION AVERAGEMESHQUALITY
!***********************
!*** reads the mesh data
!*** and group data
!*** directly
!*** chk0
!***********************
  SUBROUTINE LEMESH
    INTEGER, PARAMETER::nl = 13
    INTEGER, DIMENSION(:), ALLOCATABLE::ITEMP
    INTEGER, DIMENSION(:), ALLOCATABLE::ELTYP
    DO I = 1, NL
      READ (UMAL, *)
    END DO
!--------------------------------
!*** reading of node coordinates
!--------------------------------
    READ (UMAL, *) NNO
    IF (nno .LE. 0) STOP "wrong nno"
    nnot = nno
    CALL allocsafe(3, nno, noco)
    DO in = 1, nno
      READ (umal, *) ino, (noco(ikk, ino), ikk=1, 3)
      IF (ino .NE. in) STOP "wrong node numbering in the mesh file"
    END DO
!--------------------------------------
!*** reading of element connectivities
!--------------------------------------
    READ (umal, *)
    READ (umal, *) nel
    CALL allocsafe(nel+1, elni)
    CALL allocsafe(nel, eltyp)
    DO iel = 1, nel
      READ (umal, *) jel, ity
      IF (jel .NE. iel) STOP "wrong element ordering in the mesh file"
      elni(jel) = nnosc(ity)
    END DO
    CALL manymanycreaterowpointersandallocate(nel, elni, elno)
    DO i = nel, 1, -1
      BACKSPACE (umal)
    END DO
    DO iel = 1, nel
      READ (umal, *) jel, ity, (elno(elni(jel)-1+k), k=1, nnosc(ity))
      eltyp(jel) = ity !*** eltyp is the topological species of jel
    END DO
!-------------------------------
!*** check for classical errors
!-------------------------------
    DO ik = 1, elni(nel+1)-1
      ino = elno(ik)
      IF (ino .LE. 0 .OR. ino .GT. nno) THEN
        STOP "error in connectivities"
      END IF
    END DO
!------------------------------------
!*** now provides elements from type
!------------------------------------
    CALL manymanymatrixtransposeoseindicesfromlist(nel, ntel, tpil, eltyp, tpel)
!----------------------------------
!*** starts reading of node groups
!----------------------------------
    ngroupno = 0
    READ (ugrn, *) ngash
    DO i = 1, ngash
      READ (ugrn, *) inode, lg
      ngroupno = max(ngroupno, lg)
    END DO
    CALL allocsafe(ngroupno+1, grni)
    DO ik = ngash, 1, -1
      BACKSPACE (ugrn)
    END DO
    DO i = 1, ngash
      READ (ugrn, *) inode, lg
      grni(lg) = grni(lg)+1
    END DO
    CALL manymanycreaterowpointers(ngroupno, grni)
    CALL allocsafe(grni(ngroupno+1)-1, grno)
    CALL allocsafe(ngroupno, itemp)
    DO ik = ngash, 1, -1
      BACKSPACE (ugrn)
    END DO
    DO i = 1, ngash
      READ (ugrn, *) inode, lg
      grno(grni(lg)+itemp(lg)) = inode
      itemp(lg) = itemp(lg)+1
    END DO
    CALL manymanycleansupsymbol(ngroupno, grni, grno)
!-----------------------
!*** now element groups
!-----------------------
!*** allocates elgnumber
    CALL allocsafe(nel, elgnumber)
!----------------------------------------
!*** provides numbers for element groups
!*** each
!----------------------------------------
    READ (ugrn, *) nel2
    IF (nel2 .NE. nel) CALL erro("number of element groups")
    DO iel2 = 1, nel2
      READ (ugrn, *) jel2, elgnumber(iel2)
    END DO
!------------------------------------
!*** finishes reading of node groups
!*** allocates elma
!------------------------------------
    CALL allocsafe(nel, elma)
  END SUBROUTINE LEMESH
!*******************
!*** read reactions
!*** chk0
!*******************
  SUBROUTINE losnos
    nnodreac = 0
    CALL filepositionsf("readreactions", iex) !chk0
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getstr
      CALL entities(nnodreac, nodreac)
    END IF
  END SUBROUTINE losnos
!************************
!*** READS ORDERED PAIRS
!*** chk0
!************************
  SUBROUTINE lpares
    INTEGER, DIMENSION(:), ALLOCATABLE::per
    nor = 0
    CALL filepositionsf("op", iex) ! ELEMENTARY OK
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getnum(nor)
      IF (allocated(orde)) DEALLOCATE (orde)
      ALLOCATE (orde(nor))
      DO i = 1, nor
        CALL getstr
        CALL getnum(orde(i)%n)
        CALL allocsafe(orde(i)%n, orde(i)%x)
        CALL allocsafe(orde(i)%n, orde(i)%y)
        DO j = 1, orde(i)%n
          CALL getstr
          CALL getnum(orde(i)%x(j))
          CALL getnum(orde(i)%y(j))
        END DO
        CALL getstr
      END DO
      imx = 0
      DO i = 1, nor
        imx = max(imx, orde(i)%n)
      END DO
      CALL allocsafe(imx, per)
      DO i = 1, nor
        n = orde(i)%n
        CALL vectsortpermutation(n, orde(i)%x, per)
        CALL vectpermute(n, orde(i)%x, per)
        CALL vectpermute(n, orde(i)%y, per)
      END DO
      CALL allocsafe(0, per)
!----------------------------
!*** write to disk
!*** for further processing
!----------------------------
      DO i = 1, nor
        iu = fileprovidechannel()
        WRITE (texto, "(I8)") I
        OPEN (iu, file=trim(adjustl(nomef))//".funcs"//trim(adjustl(texto)), status="unknown")
        xmin = orde(i)%x(1)
        xmax = orde(i)%x(orde(i)%n)
        dx = xmax-xmin
        xmin = xmin-0.01*dx
        xmax = xmax+0.01*dx
        dx = xmax-xmin
        ns = 1000
        DO j = 0, ns
          xv = (1.0d00*j/(1.0d00*ns))*dx+xmin
          CALL funclinearinterpol(orde(i)%n, orde(i)%x, orde(i)%y, xv, yw, dyv)
          WRITE (iu, "(12e11.3)") xv, yw
        END DO
        CLOSE (iu)
      END DO
    END IF
  END SUBROUTINE lpares
!**************
!*** reads mpc
!*** chk0
!**************
  SUBROUTINE readmpcoftype
    CALL getstr
    CALL getnum(ngash)
    imp = 0
    DO ik = 1, ngash
      imp = imp+1
      CALL getstr
      CALL getnum(iop)
      iop = iop+1
      CALL getstr
!-----------
!*** slaves
!-----------
      CALL entities(mpcprop(imp)%nslaves, mpcprop(imp)%slaves)
!---------------
!*** properties
!---------------
      nmprop = 1+2*ntgl
      CALL allocsafe(nmprop, mpcprop(imp)%prop)
      CALL getstr
      DO j = 1, nmprop-1
        CALL getnum(mpcprop(imp)%prop(j+1))
      END DO
      mpcprop(imp)%prop(1) = iop
    END DO
  END SUBROUTINE readmpcoftype
!***************
!*** READS MPCS
!*** chk0
!***************
  SUBROUTINE lmpcs
    nmp = 0
!*** first counts the existing
!*** mpcs
    CALL FILEPOSITIONSF("IMPOSEDDOF", IEX)
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getnum(nmp)
      IF (allocated(mpcprop)) DEALLOCATE (mpcprop)
      ALLOCATE (mpcprop(nmp))
      DO imp = 1, nmp
        CALL getstr
        CALL getnum(iop)
        CALL getstr
        CALL entities(mpcprop(imp)%nslaves, mpcprop(imp)%slaves)
        nmprop = 1+2*ntgl
        CALL allocsafe(nmprop, mpcprop(imp)%prop)
        CALL getstr
        DO j = 1, nmprop-1
          CALL getnum(mpcprop(imp)%prop(j+1))
        END DO
        mpcprop(imp)%prop(1) = iop+1
        WRITE (*, *) "imp=", imp, "mpcprop=", mpcprop(imp)%prop
      END DO
    END IF
  END SUBROUTINE lmpcs
!*****************************
!*** READS INITIAL CONDITIONS
!*** chk0
!*****************************
  SUBROUTINE lminc
    INTEGER, DIMENSION(:), ALLOCATABLE::ng
    nic = 0
    CALL filepositionsf("initialconditions", iex)
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getnum(nic)
      CALL allocsafe(nic+1, icin)
      CALL allocsafe(nic, icdf)
      CALL allocsafe(nic, icvl)
      CALL allocsafe(nic, icdt)
      CALL allocsafe(nic, ng)
      DO i = 1, nic
        CALL getstr
        CALL getnum(nog)
        kg = nog+1
        IF (kg .EQ. 0) CALL erro("wrong node group request")
        CALL getstr
        CALL getnum(iboo)
        IF (iboo .EQ. 0) THEN
          icdt(i) = .FALSE.
        ELSE
          icdt(i) = .TRUE.
        END IF
        CALL getnum(icdf(i))
        icdf(i) = icdf(i)+1
        CALL getnum(icvl(i))
        ng(i) = kg
      END DO
!--------------------
!*** group treatment
!--------------------
      DO i = 1, nic
        icin(i) = icin(i)+grni(ng(i)+1)-grni(ng(i))
      END DO
      CALL manymanycreaterowpointersandallocate(nic, icin, icno)
      DO i = 1, nic
        igr = ng(i)
        ik = 0
        DO j = grni(igr), grni(igr+1)-1
          ino = grno(j)
          ik = ik+1
          icno(icin(i)-1+ik) = ino
        END DO
      END DO
      CALL allocsafe(0, ng)
    END IF
  END SUBROUTINE lminc
!***********************
!*** READS RESTART DATA
!*** and opens required
!*** files
!*** chk0
!***********************
  SUBROUTINE lerestart
    LOGICAL::icheck
    CALL filepositionsf("restart", iex)
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getstr
      CALL getnum(iwres)
      CALL getnum(irres)
      IF (iwres .EQ. 1) THEN
        wres = .TRUE.
      ELSE
        wres = .FALSE.
      END IF
      IF (irres .EQ. 1) THEN
        rres = .TRUE.
      ELSE
        rres = .FALSE.
      END IF
      IF (rres) THEN
        INQUIRE (file=trim(adjustl(nomef))//"."//trim(resta), exist=icheck)
        IF (.NOT. icheck) rres = .FALSE.
      END IF
    ELSE
      wres = .FALSE.
      rres = .FALSE.
    END IF
    WRITE (*, "(A,2L2)") "Write Restart, Read Restart", wres, rres
    SELECT CASE (rres)
    CASE (.FALSE.)
      CALL fileopen(nomef, aviso, uavi, "substitui", .FALSE., ierr)
      CALL fileopen(nomef, volfi, uvol, "substitui", .FALSE., ierr)
      CALL fileopen(nomef, enefi, uene, "substitui", .FALSE., ierr)
      CALL fileopen(nomef, order, uord, "substitui", .FALSE., ierr)
      CALL fileopen(nomef, reacf, urea, "substitui", .FALSE., ierr)
      CALL fileopen(nomef, reacp, urep, "substitui", .FALSE., ierr)
    CASE (.TRUE.)
      CALL fileopen(nomef, aviso, uavi, "continua", .FALSE., ierr)
      CALL fileopen(nomef, volfi, uvol, "continua", .FALSE., ierr)
      CALL fileopen(nomef, enefi, uene, "continua", .FALSE., ierr)
      CALL fileopen(nomef, order, uord, "continua", .FALSE., ierr)
      CALL fileopen(nomef, reacf, urea, "continua", .FALSE., ierr)
      CALL fileopen(nomef, reacp, urep, "substitui", .FALSE., ierr)
    END SELECT
  END SUBROUTINE lerestart
!********************
!*** READS TIME DATA
!*** chk0
!********************
  SUBROUTINE letime
    CALL filepositionsf("timedata", iex)
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getstr
      CALL getnum(ttime)
      CALL getnum(dtmax)
      CALL getnum(nspect)
      CALL allocsafe(nspect, spect)
      CALL allocsafe(nspect, 2, hspect)
      DO is = 1, nspect
        READ (uent, *) spect(is)
      END DO
      WRITE (*, "(A,I8,2E15.4)") "nspect,ttime,dtmax", nspect, ttime, dtmax
    END IF
  END SUBROUTINE letime
!*******************
!*** READS analysis
!*** chk0
!*******************
  SUBROUTINE leanal
!*** OUTPUT
    CALL filepositionsf("typoutput", iex)
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getstr
      CALL getnum(itypoutput)
    END IF
!*** TARGET
    CALL filepositionsf("target", iex)
    IF (iex .NE. 0) THEN
      CALL getstr ! 1
      CALL getstr ! single node string
      CALL singleentities(inotarget) ! single node
      CALL getstr
      CALL getnum(idoftarget)
      idoftarget = idoftarget+1
    END IF
!*** now analysis
    CALL filepositionsf("linear", iex)
    IF (iex .NE. 0) THEN
      istyle = 0
    ELSE
      CALL filepositionsf("increase", iex)
      IF (iex .NE. 0) THEN
        ISTYLE = 1
        CALL getstr ! 1
        CALL getstr ! an
        CALL getstr ! PROPS
        DO i = 1, nprpanal
          CALL getnum(prpanal(i))
        END DO
        iop = 0
        in1 = 0
        in2 = 0
      END IF
      CALL filepositionsf("rveanalysis", iex)
      IF (IEX .NE. 0) THEN
        ISTYLE = 8
        CALL GETSTR ! 1
        CALL GETSTR ! AN
        CALL GETSTR ! MATERIAL
        CALL GETSTR ! PROPS
        DO I = 1, NPRPANAL
          CALL GETNUM(PRPANAL(I))
        END DO
      END IF
!----------------------------------------------------
!*** converts to new format and outputs the solution
!----------------------------------------------------
      prpanal(23) = prpanal(18)
      IF (prpanal(23) .EQ. 2) THEN
        prpanal(23) = 6
      END IF
      prpanal(22) = prpanal(15)
      prpanal(21) = prpanal(14)
      prpanal(20) = prpanal(13)
      prpanal(19) = prpanal(12)
      prpanal(18) = prpanal(11)
      prpanal(15) = prpanal(17)
      prpanal(17) = prpanal(10)
      prpanal(14) = prpanal(9)
      prpanal(13) = prpanal(16)+1 ! only for dispcontrol
      prpanal(12) = in2
      prpanal(11) = in1
      prpanal(10) = iop
      PRPANAL(16) = 0.0d00
      prpanal(9) = prpanal(8)
      prpanal(8) = prpanal(7)
      prpanal(7) = prpanal(6)
      prpanal(6) = prpanal(5)
      prpanal(5) = prpanal(4)
      prpanal(4) = prpanal(3)
      prpanal(3) = prpanal(2)
      prpanal(2) = prpanal(1)
      WRITE (*, "(A)") "------------------------------------------------------------------------------------------------------"
      WRITE (*, "(A)") " Analysis options -- some may be modified by simplas during the analysis (check for further messages) "
      WRITE (*, "(A)") " Analysis options -- some may be inactive in certain combinations"
      WRITE (*, "(A)") "------------------------------------------------------------------------------------------------------"
      WRITE (*, "(A,E14.6)") "Newton Raphson Error Tolerance:", prpanal(2)
      WRITE (*, "(A,I7)") "Number of line search iterations (if line search is on):", nint(prpanal(3))
      WRITE (*, "(A,L2)") "One step analysis flag:", nint(prpanal(4))
      WRITE (*, "(A,L2)") "Cutting step algorithm:", nint(prpanal(5))
      WRITE (*, "(A,I7)") "Number of homotopy steps:", nint(prpanal(6))
      WRITE (*, "(A,L2)") "Upper limit on step size flag:", nint(prpanal(7))
      WRITE (*, "(A,I7)") "Number of complementarity funcsmoothing reductions:", nint(prpanal(8))
      WRITE (*, "(A,I7)") "Damping:", nint(prpanal(9))
      WRITE (*, "(A,I7)") "Ordered pair in control:", nint(prpanal(10))
      WRITE (*, "(A,I7)") "Node 1 in control:", nint(prpanal(11))
      WRITE (*, "(A,I7)") "Node 2 in control:", nint(prpanal(12))
      WRITE (*, "(A,I7)") "Degree of freedom type in control:", nint(prpanal(13))
      WRITE (*, "(A,L2)") "Deactivate constitutive self-restraint:", nint(prpanal(15))
      WRITE (*, "(A,L2)") "Inertia:", nint(prpanal(14))
      WRITE (*, "(A,L2)") "Contact indicator:", nint(prpanal(17))
      WRITE (*, "(A,L2)") "Deactivate ALE:", nint(prpanal(18))
      WRITE (*, "(A,E14.6)") "Relative DOF increment:", prpanal(19)
      WRITE (*, "(A,E14.6)") "Tolerance for contact detection:", prpanal(20)
      WRITE (*, "(A,L2)") "Constitutive extrapolation:", nint(prpanal(21))
      WRITE (*, "(A,L2)") "Performs neutral step after restart:", nint(prpanal(22))
      WRITE (*, "(A,I7)") "Screened-Poisson funcsmoothing:", nint(prpanal(23))
      WRITE (*, "(A)") "------------------------------------------------------------------------------------------------------"
    END IF
  END SUBROUTINE leanal
!*****************
!*** READS MONITOR
!*** chk0
!*****************
  SUBROUTINE lemonitor
    CALL filepositionsf("monitor", iex)
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getnum(nmonitor)
      IF (nmonitor .GT. 0) THEN
        IF (.NOT. allocated(monitored)) ALLOCATE (monitored(nmonitor))
        DO i = 1, nmonitor
          CALL getstr
          CALL singleentities(monitored(i))
        END DO
      END IF
    END IF
  END SUBROUTINE lemonitor
!**************************
!*** READ A GIVEN MATERIAL
!*** chk0
!**************************
  SUBROUTINE readagivenmaterial(ity, matnumber)
    INTEGER, DIMENSION(*)::matnumber
    CALL getstr
    CALL getnum(kn)
    DO l = 1, kn
!*** insert ordered pair
!*** if required
      CALL getstr
      CALL getnum(iop)
      iop = iop+1
!*** material
      CALL getstr
      CALL getnum(imn)
      imn = matnumber(imn+1)
      typm(imn) = ity
!*** props
      CALL getstr
      DO i = 1, nmap
        CALL getnum(mapr(i, imn))
      END DO
!*** shifts everytime
      DO i = nmap, 4, -1
        mapr(i, imn) = mapr(i-1, imn)
      END DO
      mapr(3, imn) = iop
    END DO
  END SUBROUTINE readagivenmaterial
!*********************
!*** MATERIAL READING
!*** chk0
!*********************
  SUBROUTINE materialreading(matnumber)
    INTEGER, DIMENSION(*)::matnumber
    nmttemp = 0
    CALL filepositionsf("material", iex)
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getnum(nmttemp)
      IF (nmttemp .GT. nmt) CALL erro("Number of materials must be equal or less than number of sections")
      CALL filepositionsf("elast", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(1, matnumber)
      CALL filepositionsf("j2plast", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(2, matnumber)
      CALL filepositionsf("kirchhoffsaintvenant", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(3, matnumber)
      CALL filepositionsf("neohooke", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(4, matnumber)
      CALL filepositionsf("generalplasticity", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(5, matnumber)
      CALL filepositionsf("orthotropic", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(7, matnumber)
      CALL filepositionsf("extendedgtn", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(8, matnumber)
      CALL filepositionsf("rousselier", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(9, matnumber)
      CALL filepositionsf("cohesive", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(10, matnumber)
      CALL filepositionsf("multiple", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(11, matnumber)
      CALL filepositionsf("ogden", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(12, matnumber)
      CALL filepositionsf("ogdenhill", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(13, matnumber)
      CALL filepositionsf("magma", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(14, matnumber)
      CALL filepositionsf("capdruckerprager", jex)
      IF (jex .NE. 0) CALL readagivenmaterial(15, matnumber)
    END IF
  END SUBROUTINE materialreading
!*************************
!*** GIVEN SECTION READING
!*** chk0
!*************************
  SUBROUTINE readagivensection(ik, inm, hasmat, nse, matnumber)
    CHARACTER::cgash
    INTEGER, DIMENSION(*)::matnumber
    LOGICAL::hasmat
    CALL getstr
    CALL getnum(nset)
    nse = nse+nset
    IF (ik .EQ. 2) THEN
      DO jse = 1, nset
        ise = jse+nse-nset
        globalpi(ise) = inm
        WRITE (texto, *) globalpi(ise)
        texto = trim(adjustl(texto))
        cgash = texto(1:1)
        READ (cgash, *) mapi(ise)
        WRITE (*, "(A,3I8)") "Section, Specific Element, Topological Species", ise, globalpi(ise), mapi(ise)
        CALL getstr
        CALL getnum(iop)
        iop = iop+1
        IF (hasmat) THEN
          CALL getstr
          CALL getnum(ima)
          ima = ima+1
          IF (matnumber(ima) .NE. 0) THEN
            WRITE (*, *) "ima,matnumber(ima),ise", ima, matnumber(ima), ise
            CALL erro("Different sections require different materials")
          END IF
          matnumber(ima) = ise
          WRITE (*, *) "ima,matnumber(ima),ise", ima, matnumber(ima), ise
        END IF
        CALL getstr
!-----------------
!*** sets material
!-----------------
        CALL getnum(nm)
        nm = nm+1
        DO im = tpil(mapi(ise)), tpil(mapi(ise)+1)-1
          iel = tpel(im)
          IF (elgnumber(iel) .EQ. nm .AND. elma(iel) .EQ. 0) THEN
            elma(iel) = ise
          END IF
        END DO
!----------------------
!*** section properties
!----------------------
        CALL getstr
        DO i = 1, nsep
          CALL getnum(maps(i, ise))
        END DO
!---------------------------------
!*** shifts to insert ordered pair
!---------------------------------
        DO I = NSEP, 9, -1
          MAPS(I, ISE) = MAPS(I-1, ISE)
        END DO
        MAPS(8, ISE) = IOP
      END DO
    END IF
  END SUBROUTINE readagivensection
!*******************
!*** SECTION READING
!*** chk0
!*******************
  SUBROUTINE sectionreading(matnumber)
    INTEGER, DIMENSION(:), ALLOCATABLE::matnumber
!-------------------------------------------
!*** determines the number of sections
!*** and then reads the relevant sections
!-------------------------------------------
    DO ik = 1, 2
      WRITE (*, *) ik, "of 2 section reading"
      nse = 0
      CALL filepositionsf("pointload", jex)
      IF (jex .NE. 0) THEN
        inm = 11
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("y2dcontact", jex)
      IF (jex .NE. 0) THEN
        inm = 110
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("z3dcontact", jex)
      IF (jex .NE. 0) THEN
        inm = 18
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("barload", jex)
      IF (jex .NE. 0) THEN
        inm = 21
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("bernoulli2d", jex)
      IF (jex .NE. 0) THEN
        inm = 24
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("timoshenko3d", jex)
      IF (jex .NE. 0) THEN
        inm = 25
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("inclined", jex)
      IF (jex .NE. 0) THEN
        inm = 26
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("tripress", jex)
      IF (jex .NE. 0) THEN
        inm = 31
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("quadpress", jex)
      IF (jex .NE. 0) THEN
        inm = 41
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("pointcontact", jex)
      IF (jex .NE. 0) THEN
        inm = 12
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("damping2d", jex)
      IF (jex .NE. 0) THEN
        inm = 14
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("damping3d", jex)
      IF (jex .NE. 0) THEN
        inm = 15
        CALL readagivensection(ik, inm, .FALSE., nse, matnumber)
      END IF
      CALL filepositionsf("triplane", jex)
      IF (jex .NE. 0) THEN
        inm = 32
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("areaconstraint", jex)
      IF (jex .NE. 0) THEN
        inm = 33
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("triaxisymmetric", jex)
      IF (jex .NE. 0) THEN
        inm = 34
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("trishell", jex)
      IF (jex .NE. 0) THEN
        inm = 35
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("trivoid", jex)
      IF (jex .NE. 0) THEN
        inm = 36
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("trimls", jex)
      IF (jex .NE. 0) THEN
        inm = 37
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("triarea", jex)
      IF (jex .NE. 0) THEN
        inm = 38
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("trisound", jex)
      IF (jex .NE. 0) THEN
        inm = 39
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("quadplane", jex)
      IF (jex .NE. 0) THEN
        inm = 42
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("quadaxisymmetric", jex)
      IF (jex .NE. 0) THEN
        inm = 44
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("quadshell", jex)
      IF (jex .NE. 0) THEN
        inm = 45
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("quadvoid", jex)
      IF (jex .NE. 0) THEN
        inm = 46
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("quadplasma", jex)
      IF (jex .NE. 0) THEN
        inm = 47
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("quadshellpiezo", jex)
      IF (jex .NE. 0) THEN
        inm = 48
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("tet3d", jex)
      IF (jex .NE. 0) THEN
        inm = 51
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("tetmls", jex)
      IF (jex .NE. 0) THEN
        inm = 52
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("raviart3d", jex)
      IF (jex .NE. 0) THEN
        inm = 53
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("tetopt", jex)
      IF (jex .NE. 0) THEN
        inm = 54
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("penta3d", jex)
      IF (jex .NE. 0) THEN
        inm = 71
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("hex3d", jex)
      IF (jex .NE. 0) THEN
        inm = 61
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("hex3dplasma", jex)
      IF (jex .NE. 0) THEN
        inm = 62
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("hexiga", jex)
      IF (jex .NE. 0) THEN
        inm = 63
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      CALL filepositionsf("solidshellfiber", jex)
      IF (jex .NE. 0) THEN
        inm = 64
        CALL readagivensection(ik, inm, .TRUE., nse, matnumber)
      END IF
      IF (ik .EQ. 1) THEN
        nsections = nse
        CALL allocsafe(nsep, nsections, maps)
        CALL allocsafe(nsections, mapi)
        CALL allocsafe(nsections, globalpi)
        CALL allocsafe(nsections, matnumber)
        NMT = NSECTIONS
        CALL allocsafe(nmap, nmt, mapr)
        CALL allocsafe(nmt, typm)
      END IF
    END DO
  END SUBROUTINE SECTIONREADING
!************************
!*** READS MATERIAL DATA
!*** and the nonlocal
!*** data
!*** chk0
!************************
  SUBROUTINE lmater
    INTEGER, DIMENSION(:), ALLOCATABLE::matnumber
    ALLOCATE (matnumber(1))
    CALL sectionreading(matnumber) ! ELEMENTARY OK
    CALL materialreading(matnumber) ! ELEMENTARY OK
!-------------------------
!*** and the nonlocal part
!-------------------------
    CALL filepositionsf("nonlocal", iex)
    IF (iex .NE. 0) THEN
      CALL getstr
      CALL getstr
      CALL getnum(sizenl)
    END IF
    IF (.NOT. RRES) THEN
!-----------------------------
!*** allocates relevant arrays
!-----------------------------
      WRITE (*, *) "mvpg,nmga,nel", mvpg, nmga, nel
      ALLOCATE (gphist(mvpg, nmga, nel, 2), stat=ierr)
      WRITE (*, *) "ierr=", ierr
      CALL allocsafe(mvpg, nmga, nel, 2, gphist)
      CALL allocsafe(mvpe, nel, 2, elhist)
      CALL allocsafe(6, nmga, nel, 2, gpstre)
!-------------------------
!*** deallocates matnumber
!-------------------------
      IF (allocated(matnumber)) DEALLOCATE (matnumber)
    END IF
    IF (allocated(MATNUMBER)) DEALLOCATE (MATNUMBER)
  END SUBROUTINE LMATER
!********************
!*** calculates vnorn
!*** from
!*** geometry
!*** chk0
!********************
  SUBROUTINE vnornfromgeometry
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(3, 4)::xcf
    REAL(8), DIMENSION(3)::vn
    CALL allocsafe(3, nno, vnorn0)
    DO ifa = 1, nfa
      IF (ifouterface(ifa)) THEN
        nnc = fani(ifa+1)-fani(ifa)
        IF (nnc .EQ. 3 .OR. nnc .EQ. 4) THEN
          k = 0
          DO j = fani(ifa), fani(ifa+1)-1
            ino = fano(j)
            k = k+1
            DO id = 1, 3
              xcf(id, k) = noco(id, ino)
            END DO
          END DO
          vn = 0.0d00
          CALL geo3dgeneralnormal(xcf(1:3, 1:nnc), vn, nnc)
          DO j = fani(ifa), fani(ifa+1)-1
            ino = fano(j)
            DO id = 1, 3
              vnorn0(id, ino) = vnorn0(id, ino)+vn(id)
            END DO
          END DO
        END IF
      END IF
    END DO
    DO ino = 1, nno
      CALL vectnormalize(3, vnorn0(1:3, ino))
    END DO
    CALL allocsafe(3, NNO, VNORN)
    DO ifa = 1, nfa
      IF (IFOUTERFACE(IFA)) THEN
        NNC = FANI(IFA+1)-FANI(IFA)
        IF (NNC .EQ. 3 .OR. NNC .EQ. 4) THEN
          K = 0
          DO J = FANI(IFA), FANI(IFA+1)-1
            INO = FANO(J)
            K = K+1
            DO ID = 1, 3
              XCF(ID, K) = NOCO(ID, INO)+NODOF(ID, INO, 1)
            END DO
          END DO
          CALL geo3dgeneralnormal(XCF(1:3, 1:NNC), VN, NNC)
          DO J = FANI(IFA), FANI(IFA+1)-1
            INO = FANO(J)
            DO ID = 1, 3
              VNORN(ID, INO) = VNORN(ID, INO)+VN(ID)
            END DO
          END DO
        END IF
      END IF
    END DO
    DO ino = 1, nno
      CALL vectnormalize(3, vnorn(1:3, ino))
    END DO
  END SUBROUTINE vnornfromgeometry
!*************************
!*** outer faces from node
!*** part 1
!*************************
  SUBROUTINE OUTERFACESFROMNODEPART1(INO, NOF)
    INTEGER::NOF
    NOF = 0
    DO K = NOFI(INO), NOFI(INO+1)-1
      IFA = NOFA(K)
      IF (IFOUTERFACE(IFA)) THEN
        NOF = NOF+1
      END IF
    END DO
  END SUBROUTINE OUTERFACESFROMNODEPART1
!*************************
!*** outer faces from node
!*** part 2
!*************************
  SUBROUTINE OUTERFACESFROMNODEPART2(INO, LFA)
    INTEGER::NOF
    INTEGER, DIMENSION(*)::LFA
    NOF = 0
    DO K = NOFI(INO), NOFI(INO+1)-1
      IFA = NOFA(K)
      IF (IFOUTERFACE(IFA)) THEN
        NOF = NOF+1
        LFA(NOF) = IFA
      END IF
    END DO
  END SUBROUTINE OUTERFACESFROMNODEPART2
!*****************************
!*** EVALUATES REMAINING links
!*** chk0
!*****************************
  SUBROUTINE REMAININGCONNECTIVITIES
    REAL(4)::RNNU
    REAL(8), DIMENSION(3)::XC
    REAL(8), DIMENSION(MLSMAX)::RANKING
    INTEGER, DIMENSION(MLSMAX)::PER
!*******************************************
!*** DETECTS IF THE PROBLEM IS 3D OR NOT BY
!*** USING THE NODES INFO
!*******************************************
    IF3D = .FALSE.
    DO IEL = 1, NEL
      IEP = NELPRE(IEL)
      IF (GEOTOP(IEP) .EQ. 3) THEN
        IF3D = .TRUE.
        EXIT
      END IF
    END DO
!****************************************
!*** STANDARD ALLOCATION
!*** NODOF AND VELO STORE 3 HISTORY STEPS
!****************************************
    IF (.NOT. allocated(NODOF)) CALL ALLOCSAFE(NTGL, NNO, 2, NODOF)
    IF (.NOT. allocated(VELO)) CALL ALLOCSAFE(NTGL, NNO, 2, VELO)
    IF (.NOT. allocated(ACEL)) CALL ALLOCSAFE(NTGL, NNO, 2, ACEL)
!**********
!*** GROUPS
!**********
    CALL MANYMANYMATRIXTRANSPOSEOSEINDICES(NGROUPNO, NNO, GRNI, NIGR, GRNO, NOGR)
    CALL MANYMANYMATRIXTRANSPOSEOSEINDICESFROMLIST(NEL, NMT, IMEL, ELMA, MAEL)
    CALL MANYMANYMATRIXTRANSPOSEOSEINDEX(NEL, NNO, ELNI, ELNO, NOLOCAL, NOIL, NOEL)
    MFA = 6*NEL
    CALL ALLOCSAFE(MFA+1, FAIL)
    CALL ALLOCSAFE(MFA+1, FANI)
    CALL ALLOCSAFE(MFA+1, FAIR)
    CALL SIDESTO
    CALL MANYMANYMATRIXTRANSPOSEOSEINDICES(NAR, NNO, ARNI, NOIR, ARNO, NOAR)
    CALL FACESTO
    CALL MANYMANYMATRIXTRANSPOSEOSEINDICES(NEL, NFA, ELFI, FAIL, ELFA, FAEL)
    CALL MANYMANYMATRIXTRANSPOSEOSEINDICES(NFA, NNO, FANI, NOFI, FANO, NOFA)
    CALL FAAREST
    CALL MANYMANYMATRIXTRANSPOSEOSEINDICES(NEL, NAR, ELIR, ARIL, ELAR, AREL)
    CALL MANYMANYMATRIXTRANSPOSEOSEINDICES(NFA, NAR, FAIR, ARFI, FAAR, ARFA)
    IF (IF3D) THEN
      CALL OUTERFACES(NFAEX, LFAEX)
    ELSE
      CALL OUTEREDGES(NAREX, LAREX)
    END IF
    CALL OUTERNODES(NNOEX, LNOEX)
!**************
!*** MORE STUFF
!**************
    CALL ASSIGNSALPHADOFS
    CALL allocsafe(NTGL, max(1, NNOT-NNO), 2, ALPHADOF)
    CALL allocsafe(NTGL, NNOT, DDES)
    CALL allocsafe(NTGL, NNOT, REAC)
!****************
!*** MESHLESS MLS
!****************
    IMESHLESS = 0
    DO IEL = 1, NEL
      IF (ISMESHLESS(IEL)) THEN
        IMESHLESS = 1
        EXIT
      END IF
    END DO
    IF (IMESHLESS .NE. 0) THEN
!-----------------
!*** PERTURB NODES
!-----------------
      DO INO = 1, NNO
        IF (.NOT. IFOUTERNODE(INO)) THEN
          RL = 1.0d30
          DO IK = NOIR(INO), NOIR(INO+1)-1
            IAR = NOAR(IK)
            RL = min(RL, EDGESIZE(IAR))
          END DO
          RL = 2.0d-2*RL
          CALL random_seed()
          CALL random_number(RNNU)
          NOCO(1, INO) = NOCO(1, INO)+RL*(2.0d00*RNNU-1.0d00)
          CALL random_seed()
          CALL random_number(RNNU)
          NOCO(2, INO) = NOCO(2, INO)+RL*(2.0d00*RNNU-1.0d00)
          CALL random_seed()
          CALL random_number(RNNU)
          NOCO(3, INO) = NOCO(3, INO)+RL*(2.0d00*RNNU-1.0d00)
        END IF
      END DO
      TOL = 1.0d-9
      ALLOCATE (DSUPPORT(NEL), NVISIBLE(NEL), MLSVISIBLE(NEL, MLSMAX))
!---------------------
!*** LOOP ALL ELEMENTS
!---------------------
      DO IEL = 1, NEL
        NNE = ELNI(IEL+1)-ELNI(IEL)
        IF (ISCONTINUUMELEMENT(IEL) .AND. (NNE .EQ. 3 .OR. NNE .EQ. 4)) THEN
          XC = 0.0d00
          DO IK = ELNI(IEL), ELNI(IEL+1)-1
            INO = ELNO(IK)
            XC = XC+NOCO(1:3, INO)
          END DO
          XC = 1.0d00*XC/(1.0d00*NNE)
!-----------------------
!*** SUPPORT OF ELEMENT
!-----------------------
          RL = 0.0d00
          DO IK = ELNI(IEL), ELNI(IEL+1)-1
            INO = ELNO(IK)
            RL = max(RL, VECTNORM2(3, NOCO(1:3, INO)-XC))
          END DO
          DSUPPORT(IEL) = 8.0d00*RL
!-----------------------------------
!*** NUMBER OF NODES IN THE SUPPORT
!-----------------------------------
          N = 0
          DO JNO = 1, NNO
            IF (vectnorm2(3, XC-NOCO(1:3, JNO)) .LE. DSUPPORT(IEL)) THEN
              N = N+1
              MLSVISIBLE(IEL, N) = JNO
            END IF
          END DO
          IF (N .GT. MLSMAX) THEN
            WRITE (*, "(A)") "TOO LARGE (1) SUPPORT FOR MLS INTERPOLATION"
            WRITE (*, "(A,I8,A)") "A MAXIMUM OF ", MLSMAX, " NODES IS ALLOWED"
            STOP
          END IF
!----------------------------------------
!*** REMOVE REPEATED NODES IN THE SUPPORT
!----------------------------------------
          CALL listremoverepeated(N, MLSVISIBLE(IEL, 1:N), .FALSE.)
          NVISIBLE(IEL) = N
!---------- up TO HERE SEEMS OK
!---------------------------
!*** RANKING
!*** AND SETS TO ZERO INSIDE
!---------------------------
          DO IN = 1, N
            INO = MLSVISIBLE(IEL, IN)
            RANKING(IN) = vectnorm2(3, XC-NOCO(1:3, INO))
          END DO
!--------------------
!*** SORT BY RANKING
!--------------------
          CALL vectSORTpermutation(N, RANKING, PER)
!------------------------------------
!*** PERFORM PERMUTATION BASED ON PER
!------------------------------------
          CALL listpermute(N, PER, MLSVISIBLE(IEL, 1:N), 1)
          IF (N .GT. MLSMAX) THEN
            WRITE (*, "(A)") "TOO LARGE (2) SUPPORT FOR MLS INTERPOLATION"
            WRITE (*, "(A,I8,A)") "A MAXIMUM OF ", MLSMAX, " NODES IS ALLOWED"
            STOP
          END IF
          DO IN = 1, N-1
            INO1 = MLSVISIBLE(IEL, IN)
            INO2 = MLSVISIBLE(IEL, IN+1)
            R1 = vectnorm2(3, XC-NOCO(1:3, INO1))
            R2 = vectnorm2(3, XC-NOCO(1:3, INO2))
            IF (R1 .GT. R2) STOP "WRONG ORDER"
          END DO
          NVISIBLE(IEL) = N
!-----------
!*** CUTOFF
!-----------
          IF (MAPS(9, ELMA(IEL)) .GT. 3.0d00) THEN
            NMX = nint(MAPS(9, ELMA(IEL)))
          ELSE
            NMX = 40 !10!40!20!40
          END IF
          IF (N .GT. NMX) THEN
            N = NMX
            NVISIBLE(IEL) = NMX
            DSUPPORT(IEL) = 0.0d00
            DO IN = 1, N
              DSUPPORT(IEL) = max(DSUPPORT(IEL), VECTNORM2(3, XC-NOCO(1:3, MLSVISIBLE(IEL, IN))))
            END DO
            DSUPPORT(IEL) = 1.15d00*DSUPPORT(IEL)
          ELSE
            WRITE (*, *) "N,NMX", N, NMX
            STOP "ERROR - SUPPORT TOO SMALL"
          END IF
!------------------------------
!*** CHECKS IF ORDERING WAS OK
!------------------------------
          NV = NVISIBLE(IEL)
          IF (N .NE. NV) THEN
            WRITE (*, *) "N,NV", N, NV
            STOP "ERROR NV NOT N"
          END IF
        END IF
      END DO
    END IF
    NGASH = 0
    DO IFI = 1, NFAEX
      IFA = LFAEX(IFI)
      NGASH = NGASH+FANI(IFA+1)-FANI(IFA)
    END DO
    ALLOCATE (FABNI(NFAEX+1), FABNO(NGASH))
    FABNI(1) = 1
    DO IEL = 1, NFAEX
      IFA = LFAEX(IEL)
      FABNI(IEL+1) = FABNI(IEL)+FANI(IFA+1)-FANI(IFA)
      DO IK = 1, FANI(IFA+1)-FANI(IFA)
        FABNO(FABNI(IEL)-1+IK) = FANO(FANI(IFA)-1+IK)
      END DO
    END DO
  END SUBROUTINE REMAININGCONNECTIVITIES
!*******************
!*** EDGES RELATIONS
!*******************
  SUBROUTINE sidesto
    INTEGER, DIMENSION(2)::listp1, listp2
    INTEGER, DIMENSION(2, 12)::ll, ll2
    CALL allocsafe(nel+1, elir)
    DO iel = 1, nel
      jtyp = nelpre(iel)
      IF (jtyp .LE. 0) THEN
        STOP "WRONG NELPRE"
      END IF
      elir(iel) = nares(jtyp)
    END DO
    CALL manymanycreaterowpointersandallocate(nel, elir, elar)
    DO icyclic = 1, 2
      itemp = elir(nel+1)-1
      CALL listsetconstant(itemp, elar)
      nar = 0
      DO iel = 1, nel
        jtyp = nelpre(iel)
        CALL listares(jtyp, ll)
        ik = 0
        DO i = elir(iel), elir(iel+1)-1
          ik = ik+1
          n1 = ll(1, ik)
          n2 = ll(2, ik)
          listp1(1) = elno(elni(iel)-1+n1)
          listp1(2) = elno(elni(iel)-1+n2)
          IF (listp1(1) .EQ. listp1(2)) CYCLE
          in = 0
          do1: DO i1 = elni(iel), elni(iel+1)-1
            ino = elno(i1)
            DO j = noil(ino), noil(ino+1)-1
              jel = noel(j)
              ktyp = nelpre(jel)
              CALL listares(ktyp, ll2)
              jk = 0
              DO k = elir(jel), elir(jel+1)-1
                jk = jk+1
                n1 = ll2(1, jk)
                n2 = ll2(2, jk)
                l = elar(k)
                IF (l .LE. 0) CYCLE
                listp2(1) = elno(elni(jel)-1+n1)
                listp2(2) = elno(elni(jel)-1+n2)
                IF (listp2(1) .EQ. listp2(2)) CYCLE
                IF (listifcoincide(2, listp1, .FALSE., listp2, .FALSE.)) THEN
                  in = l
                  EXIT do1
                END IF
              END DO
            END DO
          END DO do1
          IF (in .NE. 0) THEN
            elar(i) = in
          ELSE
            nar = nar+1
            elar(i) = nar
            IF (icyclic .EQ. 2) THEN
              arno(manymanyiaddress(arni, nar, 1)) = listp1(1)
              arno(manymanyiaddress(arni, nar, 2)) = listp1(2)
            END IF
          END IF
        END DO
      END DO
      IF (icyclic .EQ. 1) THEN
        CALL allocsafe(nar+1, arni)
        DO ikak = 1, nar
          arni(ikak) = 2
        END DO
        arni(nar+1) = 0
        CALL manymanycreaterowpointersandallocate(nar, arni, arno)
      END IF
    END DO
  END SUBROUTINE sidesto
!--------------------
!*** faces relations
!--------------------
  SUBROUTINE facesto
    INTEGER, DIMENSION(4, 6)::ll1, ll2
    INTEGER, DIMENSION(4)::listp1, listp2
    CALL allocsafe(nel+1, elfi)
    DO iel = 1, nel
      elfi(iel) = nfaces(nelpre(iel))
    END DO
    CALL manymanycreaterowpointersandallocate(nel, elfi, elfa)
    DO icyclic = 1, 2
      CALL listsetconstant(elfi(nel+1)-1, elfa, 0)
      nfa = 0
      DO iel = 1, nel
        jtyp = nelpre(iel)
        nf = nfaces(jtyp)
        CALL listfaces(jtyp, ll1)
        ik = 0
        DO i = elfi(iel), elfi(iel+1)-1
          ik = ik+1
          nnf = numnf(jtyp, ik)
          DO j = 1, nnf
            itemp = ll1(j, ik)
            listp1(j) = elno(elni(iel)-1+itemp)
          END DO
          IF (nnf .LT. 4) listp1(4) = 0
          in = 0
          imatch = 0
          do1: DO i1 = elni(iel), elni(iel+1)-1
            ino = elno(i1)
            IF (ino .LE. 0) CYCLE
            DO j = noil(ino), noil(ino+1)-1
              jel = noel(j)
              IF (jel .EQ. iel) CYCLE
              ktyp = nelpre(jel)
              nf2 = nfaces(ktyp)
              CALL listfaces(ktyp, ll2)
              jk = 0
              DO k = elfi(jel), elfi(jel+1)-1
                jk = jk+1
                nnf2 = numnf(ktyp, jk)
                DO m = 1, nnf2
                  jtemp = ll2(m, jk)
                  IF (jtemp .LE. 0) CYCLE
                  listp2(m) = elno(elni(jel)-1+jtemp)
                END DO
                IF (nnf .LT. 4) listp2(4) = 0
                l = elfa(k)
                IF (listifcoincide(nnf, listp1, .FALSE., listp2, .FALSE.)) THEN
                  imatch = 1
                  in = l
                  EXIT do1
                END IF
              END DO
            END DO
          END DO do1
          SELECT CASE (icyclic)
          CASE (1)
            IF (in .EQ. 0) THEN
              nfa = nfa+1
              elfa(i) = nfa
              fani(nfa) = nnf
            ELSE
              elfa(i) = in
            END IF
          CASE (2)
            IF (in .EQ. 0) THEN
              nfa = nfa+1
              elfa(i) = nfa
              DO j = 1, nnf
                fano(manymanyiaddress(fani, nfa, j)) = listp1(j)
              END DO
            ELSE
              elfa(i) = in
            END IF
          END SELECT
        END DO
      END DO
      IF (icyclic .EQ. 1) THEN
        CALL manymanycreaterowpointersandallocate(nfa, fani, fano)
      END IF
    END DO
  END SUBROUTINE facesto
!****************
!*** FACES EDGES
!****************
  SUBROUTINE faarest
    INTEGER, DIMENSION(4, 6)::ll
    CALL allocsafe(nfa+1, fair)
    DO icyclic = 1, 2
      DO iel = 1, nel
        jtyp = nelpre(iel)
        nf = nfaces(jtyp)
        CALL listfaces2(jtyp, ll)
        DO i = 1, nf
          nnf = numnf(jtyp, i)
          ifg = elfa(elfi(iel)-1+i)
          IF (icyclic .EQ. 1) THEN
            fair(ifg) = nnf
          ELSE
            DO j = 1, nnf
              itemp = ll(j, i)
              faar(manymanyiaddress(fair, ifg, j)) = elar(elir(iel)-1+itemp)
            END DO
          END IF
        END DO
      END DO
      IF (icyclic .EQ. 1) CALL manymanycreaterowpointersandallocate(nfa, fair, faar)
    END DO
  END SUBROUTINE faarest
!*******************
!*** ELEMENT LIBRARY
!*******************
  SUBROUTINE LIELEM(IEL, IJOB)
!----------------------------------------
!*** IJOB==0 -> ESTABLISHING DIMENSIONS
!*** IJOB==1 -> ESTABLISHING DESTINATIONS
!*** IJOB==2 -> CLASSICAL EQUILIBRIUM
!----------------------------------------
    ITEMP = NELPRE(IEL)
    JTEMP = NELOPT(IEL)
    SELECT CASE (ITEMP)
    CASE (1)
      SELECT CASE (JTEMP)
      CASE (11)
!          CALL FORCNOD(IEL)
!*** ALL PENALTY CONTACT ELEMENTS FINISHED 23/06/2014
!*** CONTACT-ALL
      CASE (12)
        IF (.NOT. if3d) THEN
!          call contactall(iel,2,3,ijob)
        ELSE
!          call contactall(iel,3,4,ijob)
        END IF
      CASE (14)
!          CALL ELM_DAMPING(IEL,2)
      CASE (15)
!          CALL ELM_DAMPING(IEL,3)
      CASE (18)
!          CALL ELM_ZCONTACT(IEL)
      CASE (19)
!          CALL ELM_SPRING(IEL)
      CASE (110)
!          CALL ELM_YCONTACT(IEL)
      END SELECT
    CASE (2)
      SELECT CASE (JTEMP)
      CASE (21)
!          CALL ELM_EDGEPRESS(IEL)
      CASE (26)
!          CALL ELM_INCLINED(IEL)
      END SELECT
    CASE (3)
!***
      SELECT CASE (JTEMP)
      CASE (31)
!          CALL ELM_TRIPRESS(IEL)
      CASE (32)
!          CALL ELM_TRIPLANE(IEL,1)
      CASE (33)
!          CALL ELM_FIBERCONSTRAINT(IEL)
      CASE (34)
!          CALL ELM_TRIPLANE(IEL,3)
      CASE (36)
!          CALL ELM_TRIVOID(IEL)
      CASE (37)
!          CALL ELM_TRIMLS(IEL)
      CASE (38)
!          CALL ELM_TRIAREA(IEL)
      CASE (39)
!          CALL ELM_TRIDIFF(IEL)
      END SELECT
    CASE (4)
!*** ALL QUADS FINISHED 28/11/2013
      SELECT CASE (JTEMP)
      CASE (41)
!          CALL ELM_QUADPRESS(IEL)
      CASE (42)
!          CALL ELM_QUADPLANE(IEL,1)
      CASE (44)
!          CALL ELM_QUADPLANE(IEL,3)
      CASE (46)
!          CALL ELM_QUADVOID(IEL)
      END SELECT
    CASE (5)
!*** TETS
      SELECT CASE (JTEMP)
      CASE (51)
        CALL ELM_TET3D(IEL, IJOB)
      CASE (52)
!          CALL ELM_TETMLS(IEL)
      CASE (54)
!          CALL ELM_VOLCONSTRAINT(IEL)
      END SELECT
    CASE (6)
!*** HEXS
      SELECT CASE (JTEMP)
      CASE (61)
!          CALL ELM_HEX(IEL,IJOB)
      CASE (62)
!          CALL ELM_FIBERCONSTRAINTHEX(IEL)
      CASE (63)
!          CALL IGA_HEX(IEL)
      CASE (64)
!          CALL ELM_SOLIDSHELLFIBER(IEL,IJOB)
      END SELECT
    CASE (7)
      SELECT CASE (JTEMP)
      END SELECT
    END SELECT
  END SUBROUTINE LIELEM
!***************************************
!*** INTERNAL NODES FOR A GIVEN ELEMENT
!***************************************
  INTEGER FUNCTION INTERNALNODES(IEL)
    JTEMP = NELOPT(IEL)
    INTERNALNODES = 0
    SELECT CASE (JTEMP)
    CASE (51)
      IMIXED = nint(MAPS(13, ELMA(IEL)))
      IF (IMIXED .EQ. 1) THEN
        WRITE (*, *) "internalnodes!"
        INTERNALNODES = 1
      END IF
    CASE (39)
      INTERNALNODES = 1
    CASE (61)
      IMIXED = nint(MAPS(13, ELMA(IEL)))
      IF (IMIXED .EQ. 1) INTERNALNODES = 3
    END SELECT
  END FUNCTION INTERNALNODES
!****************************************
!*** EDGE/FACE NODES FOR A GIVEN ELEMENT
!****************************************
  LOGICAL FUNCTION FACEDOFS(IEL)
    JTEMP = NELOPT(IEL)
    FACEDOFS = .FALSE.
    SELECT CASE (JTEMP)
    CASE (53)
      FACEDOFS = .TRUE.
    END SELECT
  END FUNCTION FACEDOFS
!****************************************
!*** DETERMINES NUMBER OF ADDITIONAL
!*** INTERNAL NODES AND ASSIGNS TO ELHIST
!*** ALSO MAKES THE SAME FOR EDGES/FACES
!****************************************
  SUBROUTINE ASSIGNSALPHADOFS
    KNO = 0
    DO IEL = 1, NEL
      IF (ISCONTINUUMELEMENT(IEL)) THEN
        KI = INTERNALNODES(IEL)
        IF (KI .GT. 3) STOP "MAXIMUM NUMBER OF INTERNAL NODES IS 3"
        DO L = 1, KI
          ELHIST(L, IEL, 1) = KNO+L
          ELHIST(L, IEL, 2) = KNO+L
        END DO
        KNO = KNO+KI
      END IF
    END DO
    CALL allocsafe(NFA, FADOF)
    DO IEL = 1, NEL
      IF (ISCONTINUUMELEMENT(IEL)) THEN
        IF (FACEDOFS(IEL)) THEN
          DO IK = ELFI(IEL), ELFI(IEL+1)-1
            IFA = ELFA(IK)
            IF (IFA .LE. 0) STOP "NON-POSITIVE FACE"
            IF (FADOF(IFA) .EQ. 0) THEN
              KNO = KNO+1
              FADOF(IFA) = KNO+NNO
            END IF
          END DO
        END IF
      END IF
    END DO
    NNOT = NNO+KNO
  END SUBROUTINE ASSIGNSALPHADOFS
!***************************
!*** global array driver
!*** do not alter the OPENMP
!*** directives
!*** chk0
!***************************
  SUBROUTINE DRVFIR(IJOB)
!----------------------------------------
!*** IJOB==0 -> ESTABLISHING DIMENSIONS
!*** IJOB==1 -> ESTABLISHING DESTINATIONS
!*** IJOB==2 -> CLASSICAL EQUILIBRIUM
!----------------------------------------
    CALL INICIA(CUTF)
    SELECT CASE (IJOB)
    CASE (0)
      WRITE (*, *) "BEGIN ESTABLISHING DIMENSIONS "
      CALL LINEARSTART0(ASS, NEL, NTGL)
      DO IEL = 1, NEL
        CALL LIELEM(IEL, 0)
      END DO
      CALL LINEAREND0(ASS)
      WRITE (*, *) "END ESTABLISHING DIMENSIONS"
    CASE (1)
      WRITE (*, *) "BEGIN ESTABLISHING DESTINATIONS "
!$OMP PARALLEL
!$OMP DO SCHEDULE(DYNAMIC)
      DO IEL = 1, NEL
        CALL LIELEM(IEL, 1)
      END DO
!$OMP END DO
!$OMP END PARALLEL
      CALL linearend1(ass)
      WRITE (*, *) "END ESTABLISHING DESTINATIONS "
    CASE (2:4)
      WRITE (*, *) "BEGIN CLIQUE FILLING "
      CALL linearstart1(ass)
!$OMP PARALLEL
!$OMP DO SCHEDULE(DYNAMIC)
      DO IEL = 1, NEL
        CALL PREPAREEL(IEL)
        CALL LIELEM(IEL, IJOB)
      END DO
!$OMP END DO
!$OMP END PARALLEL
      DO IMP = 1, NMP
        CALL LIMPC(IMP)
      END DO
      WRITE (*, *) "END CLIQUE FILLING "
      WRITE (*, *) "BEGIN ASSEMBLING "
      CALL LINEARASSEMBLINGNUMERIC(ASS)
      WRITE (*, *) "END ASSEMBLING "
    END SELECT
  END SUBROUTINE DRVFIR
!**********************************
!*** PREPARATION OF A GIVEN ELEMENT
!*** CHK0,1
!**********************************
  SUBROUTINE PREPAREEL(IEL)
!*** ALWAYS SET THE OUTPUT AVERAGES OF HISTORY TO ZERO
    DO IGA = 1, 2
      DO IST = 1, 6
        GPSTRE(IST, IGA, IEL, 2) = 0.0d00
      END DO
      DO IVG = 1, MVPG
        GPHIST(IVG, IGA, IEL, 2) = 0.0d00
      END DO
    END DO
!*** EXTRAPOLATION
    VOID = 0.0d00
    NN = ELNI(IEL+1)-ELNI(IEL)
    DO IK = 1, NN
      INO = ELNO(ELNI(IEL)-1+IK)
      VOID = VOID+NODOF(9, INO, 2)
    END DO
    VOID = VOID/(1.0d00*NN)
    DO IGA = 1, NMGA
      GPHIST(27, IGA, IEL, 1) = VOID
    END DO
!*** NEVER USE GPHIST AND ELHIST AT (2)
!*** IN THE ELEMENTS AND MATERIALS
!*** SO KEEP THIS STUFF COMMENTED OUT
!*** TO ENSURE NO PROBLEMS EXIST IN THE FUTURE
!   DO IGA=3,NMGA
!      DO IVG=1,MVPG
!          GPHIST(IVG,IGA,IEL,2)=GPHIST(IVG,IGA,IEL,1)
!       ENDDO
!    ENDDO
  END SUBROUTINE PREPAREEL
!**********************************
!*** initialization of all elements
!*** and mpcs
!*** chk0
!**********************************
  SUBROUTINE inicia(cutf)
    VOLMESH = 0.0d00
    DTCRIT = huge(0.0d00)
    CUTF = 1.0d00
    CUTFAC = 1.0d00
    KINETICENERGY = 0.0d00
    CHARL = AVERAGEMESHLENGTH()
  END SUBROUTINE inicia
!********************************************
!*** calculation of the variation of unknowns
!*** drvdes
!*** chk0
!********************************************
  SUBROUTINE drvdes()
    REAL(8), DIMENSION(:, :), ALLOCATABLE::REACS
    ALLOCATE (REACS(3, NNOT))
    IF ((IHOMOTOPY .EQ. NHOMOTOPY) .AND. (IHOMOTOPY .GT. 1)) THEN
      REACS(1:3, 1:NNOT) = REAC(1:3, 1:NNOT)
    END IF
    WRITE (*, *) "LINEAR SLV"
    CALL LINEARSOLVER(ASS, DDES, REAC, RESIDFORC)
    IF ((IHOMOTOPY .EQ. NHOMOTOPY) .AND. (IHOMOTOPY .GT. 1)) THEN
      REAC(1:3, 1:NNOT) = REACS(1:3, 1:NNOT)
    END IF
    WRITE (*, "(A,I7,A,E15.5,A,E15.5)") "NUMBER OF SECONDS TOOK BY SOLVER=", NS, " FORCE RESIDUAL=", RESIDFORC
    IF (ISNAN(RESIDFORC)) RESIDFORC = 0.0d00
    DEALLOCATE (REACS)
  END SUBROUTINE drvdes
!*********************
!*** TRIAL POSITION
!*** GIVEN A NODE INO
!*********************
  SUBROUTINE DEFORMEDPOSITION(INO, XNO)
    IMPLICIT REAL(8) (A-H, O-Z)
    REAL(8), DIMENSION(3)::XNO
    DO ID = 1, 3
      XNO(ID) = NOCO(ID, INO)+NODOF(ID, INO, 2)
    END DO
  END SUBROUTINE DEFORMEDPOSITION
!*************************
!*** 2D/3D CONTACT
!*** DETECTION
!*** WITH SEVERAL OPTIONS
!*** CHK0
!*************************
  SUBROUTINE CONTACTDETECTION
    INTEGER, SAVE::IFIRST = 1
    REAL(8), DIMENSION(:, :), ALLOCATABLE::XTG
    REAL(8), DIMENSION(3, 4)::XNODES
    REAL(8), DIMENSION(4)::FF
    REAL(8), DIMENSION(3)::xno
    INTEGER, DIMENSION(:), ALLOCATABLE::LFABUCK
!-----------------------------------------
!*** ELHIST(1) IS NOW THE TARGET ELEMENT!
!-----------------------------------------
!----------------------
!*** OPENS REPORT FILE
!----------------------
    ICD = fileprovidechannel()
    IF (IFIRST .EQ. 0) THEN
      CALL fileopen("LIST_OF_CONTACT_ELEMENTS", "TXT", ICD, "CONTINUA", .FALSE., IERR)
    ELSE
      IFIRST = 0
      CALL fileopen("LIST_OF_CONTACT_ELEMENTS", "TXT", ICD, "SUBSTITUI", .FALSE., IERR)
    END IF
    WRITE (*, *) "INSIDE CONTACT, IF3D", IF3D
!--------------------------
!*** SIZE OF MESH
!*** CLEANS CONNECTIVITIES
!--------------------------
    RC = AVERAGEMESHLENGTH()
    DO IEL = 1, NEL
      IF (.NOT. ISCONTINUUMELEMENT(IEL)) ELHIST(1, IEL, 2) = 0.0d00
    END DO
!--------------------
!*** MEAN COORDINATES
!*** OF ELEMENTS
!--------------------
    ALLOCATE (XTG(3, NEL))
    DO IEL = 1, NEL
      XTG(1:3, IEL) = 0.0d00
      RS = 0.0d00
      DO I = ELNI(IEL), ELNI(IEL+1)-1
        CALL DEFORMEDPOSITION(ELNO(I), XNO)
        XTG(1:3, IEL) = XTG(1:3, IEL)+XNO
        RS = RS+1.0d00
      END DO
      IF (RS .GT. 1.0d-20) XTG(1:3, IEL) = XTG(1:3, IEL)/(1.0d00*RS)
    END DO
!-------------
!*** DETECTION
!-------------
    DO IKK = 1, NNOEX
      INO = LNOEX(IKK)
      IEL = ISINGLEELEMENT(INO)
      IF (.NOT. ISCONTACT(IEL)) CYCLE
!------------------------
!*** INCIDENT COORDINATES
!------------------------
      CALL DEFORMEDPOSITION(INO, XNO)
!---------------------
!*** CANDIDATE ELEMENT
!---------------------
      CALL BUCKETNODEPOLYGON(BS, INO, NFABUCK, LFABUCK)
      JEL = 0
      IGASHDO: DO LFAB = 1, NFABUCK
        KEL = LFABUCK(LFAB)
        IF (ISCONTACT(KEL)) CYCLE
!---------------------------
!*** CYCLES IF IT IS TOO FAR
!---------------------------
        IF (VECTNORM2(3, XNO(1:3)-XTG(1:3, KEL)) .GT. 2.0d00*EDGEMAX) CYCLE IGASHDO
!----------------------------------------------
!*** CHECKS FOR THE ONLY TWO ALLOWABLE ELEMENTS
!----------------------------------------------
        IF (IF3D) THEN
          IF (ELNI(KEL+1)-ELNI(KEL) .NE. 4) CYCLE IGASHDO
        ELSE
          IF (ELNI(KEL+1)-ELNI(KEL) .NE. 3) CYCLE IGASHDO
        END IF
!--------------------------------------------
!*** CYCLES IF NODE BELONGS TO TARGET ELEMENT
!--------------------------------------------
        DO JK = ELNI(KEL), ELNI(KEL+1)-1
          JNO = ELNO(JK)
          IF (JNO .EQ. INO) CYCLE IGASHDO
        END DO
!---------------------------------
!*** IF NOT SELF CONTACT, CYCLES
!*** WHEN THE MATERIAL IS THE SAME
!*** 3D IF NOT SEL
!---------------------------------
        IF (MAPS(4, ELMA(IEL)) .LE. 0.5d00) THEN
          DO JK = NOIL(INO), NOIL(INO+1)-1
            KK = NOEL(JK)
            IF (ELMA(NOEL(JK)) .EQ. ELMA(KEL)) CYCLE IGASHDO
          END DO
        END IF
!---------------------------
!*** OBTAINS ALL COORDINATES
!*** OF ELEMENT KEL
!---------------------------
        IT = 0
        DO JK = ELNI(KEL), ELNI(KEL+1)-1
          IT = IT+1
          JNO = ELNO(JK)
          CALL deformedposition(JNO, XNODES(1:3, IT))
        END DO
!----------------------------
!*** NOW ADDRESSES THE ACTUAL
!*** PROJECTION
!----------------------------
        IF (IF3D) THEN
          CALL geo3dprojtet(XNO, XNODES(1:3, 1), XNODES(1:3, 2), XNODES(1:3, 3), XNODES(1:3, 4), FF)
        ELSE
          CALL geo3dprojtriangle(XNO, XNODES(1:3, 1), XNODES(1:3, 2), XNODES(1:3, 3), FF)
        END IF
!----------------------------------
!*** CHECKS FOR ADMISSIBILITY OF FF
!*** SHAPE FUNCTIONS
!*** IOK
!----------------------------------
        IOK = 1
        IF (IOK .EQ. 1) THEN
          DO JK = 1, ELNI(KEL+1)-ELNI(KEL)
            IF (FF(JK) .LT. 0.0d00 .OR. FF(JK) .GT. 1.0d00) IOK = 0
          END DO
          SUM = 0.0d00
          DO JK = 1, ELNI(KEL+1)-ELNI(KEL)
            SUM = SUM+FF(JK)
          END DO
          IF (abs(SUM-1.0d00) .GT. 1.0d-3) THEN
            IOK = 0
            STOP "NOT SUMMING ONE"
          END IF
        END IF
        IF (IOK .EQ. 1) THEN
          JEL = KEL
          EXIT IGASHDO
        ELSE
          JEL = 0
        END IF
      END DO IGASHDO
!-------------------
!*** DEFINES ELEMENT
!-------------------
      IF (JEL .NE. 0) THEN
        ELHIST(1, IEL, 2) = JEL
        WRITE (ICD, "(8I8)") ISTEP, INO, nint(ELHIST(1, IEL, 2))
      ELSE
        ELHIST(1, IEL, 2) = 0.0d00
      END IF
    END DO
    CLOSE (ICD)
  END SUBROUTINE CONTACTDETECTION
!*************************************
!*** NEWTON ITERATION FOR EQUILIBRIUM
!*************************************
  SUBROUTINE NEWTONITERATION(LOADOPTION, ITLS, IDAMPING, ICUTSTEP, LIMIT, ERRORHISTORY, TOLER, ISUCC)
    IMPLICIT REAL(8) (a-h, o-z)
    REAL(8), DIMENSION(:), ALLOCATABLE::ERRORHISTORY
    LOGICAL::ACCEPTABLEITERATION
    INTEGER::LOADOPTION
    CUTSTEP = .FALSE.
    LTEMP = LOADMODE
    LOADMODE = LOADOPTION
    IEQIT = 0
    ISUCC = 0
    ITERATIONS: DO
      IEQIT = IEQIT+1
      CALL DRVFIR(2)
      CALL DRVDES
      CALL DAMPINGCHOICE(ITLS, RESID, IDAMPING)
      IF ((CUTSTEP .OR. .NOT. ACCEPTABLEITERATION(RESID)) .AND. ICUTSTEP .EQ. 1) THEN
        WRITE (*, *) "CUTTING STEP NOW"
        WRITE (*, *) "CUTSTEP=", CUTSTEP
        IEQIT = LIMIT
        LOADMODE = LTEMP
      END IF
      WRITE (*, "(A,3I8,E12.4)") "HOMOTOPY COUNTER, ISRED, EQUILIBRIUM ITERATION COUNTER, ERROR", IHOMOTOPY, ISRED, IEQIT, RESID
      RESIDP = PRODDISP(DDES, REAC)
      IF (ISNAN(RESIDP)) THEN
        IEQIT = LIMIT
        LOADMODE = LTEMP
        RETURN
      END IF
      WRITE (*, *) "RESIDP", RESIDP
      IF (IEQIT .EQ. 1) THEN
        RESIDP0 = RESIDP
        IF (abs(RESIDP0) .GT. 1.0d-20) RESIDP0 = 1.0d00
        CALL VECTINSERT(ERRORHISTORY, IEQIT, 1.0d00)
      ELSE
        CALL VECTINSERT(ERRORHISTORY, IEQIT, abs(RESIDP/RESIDP0))
      END IF
      IF (IEQIT .GE. LIMIT) EXIT ITERATIONS
      IF (RESID .LE. TOLER .AND. IEQIT .GE. 3) THEN
        ISUCC = 1
        EXIT ITERATIONS
      END IF
    END DO ITERATIONS
    LOADMODE = LTEMP
  END SUBROUTINE NEWTONITERATION
!*****************************
!*** LIST FACES IN INTERFACES
!*** AND/OR BOUNDARIES
!*** TO ESTABLISH OUTPUT
!*****************************
  SUBROUTINE LISTFACESINTERFACES(NFI, LFI)
    INTEGER, DIMENSION(:), ALLOCATABLE::LFI
    LOGICAL::CONDITION = .FALSE.
    NFI = 0
    DO IFA = 1, NFA
      IEL1 = 0
      IEL2 = 0
      IEL1 = IELOTHER3D(0, IFA)
      IF (IEL1 .NE. 0) IEL2 = IELOTHER3D(IEL1, IFA)
      CONDITION = IEL1*IEL2 .EQ. 0
      IF (CONDITION) THEN
        NFI = NFI+1
      ELSEIF ((ELMA(IEL1) .NE. ELMA(IEL2)) .AND. (ELMA(IEL1)*ELMA(IEL2) .NE. 0)) THEN
        NFI = NFI+1
      END IF
    END DO
    IF (NFI .NE. 0) ALLOCATE (LFI(NFI))
    NFI = 0
    DO IFA = 1, NFA
      IEL1 = 0
      IEL2 = 0
      IEL1 = IELOTHER3D(0, IFA)
      IF (IEL1 .NE. 0) IEL2 = IELOTHER3D(IEL1, IFA)
      CONDITION = IEL1*IEL2 .EQ. 0
      IF (CONDITION) THEN
        NFI = NFI+1
        LFI(NFI) = IFA
      ELSEIF ((ELMA(IEL1) .NE. ELMA(IEL2)) .AND. (ELMA(IEL1)*ELMA(IEL2) .NE. 0)) THEN
        NFI = NFI+1
        LFI(NFI) = IFA
      END IF
    END DO
  END SUBROUTINE LISTFACESINTERFACES
!*****************************
!*** LIST EDGES IN INTERFACES
!*** AND/OR BOUNDARIES
!*** TO ESTABLISH OUTPUT
!*****************************
  SUBROUTINE LISTEDGESINTERFACES(NAI, LAI)
    INTEGER, DIMENSION(:), ALLOCATABLE::LAI
    LOGICAL::CONDITION = .FALSE.
    NAI = 0
    DO IFA = 1, NFA
      IEL1 = 0
      IEL2 = 0
      IEL1 = IELOTHER2D(0, IFA)
      IF (IEL1 .NE. 0) IEL2 = IELOTHER2D(IEL1, IFA)
      CONDITION = IEL1*IEL2 .EQ. 0
      IF (CONDITION) THEN
        NAI = NAI+1
      ELSEIF (ELMA(IEL1) .NE. ELMA(IEL2)) THEN
        NAI = NAI+1
      END IF
    END DO
    ALLOCATE (LAI(NAI))
    NAI = 0
    DO IFA = 1, NFA
      IEL1 = 0
      IEL2 = 0
      IEL1 = IELOTHER2D(0, IFA)
      IF (IEL1 .NE. 0) IEL2 = IELOTHER2D(IEL1, IFA)
      CONDITION = IEL1*IEL2 .EQ. 0
      IF (CONDITION) THEN
        NAI = NAI+1
      ELSEIF (ELMA(IEL1) .NE. ELMA(IEL2)) THEN
        NAI = NAI+1
      END IF
      LAI(NAI) = IFA
    END DO
  END SUBROUTINE LISTEDGESINTERFACES
END MODULE overall
!*************************************
!*** DETERMINES AN EXTERNAL NODE
!*** MATCHING THE CONTACT ELEMENT IEL
!*** WITH ONE POINT
!*************************************
INTEGER FUNCTION JNOONE3D(IEL)
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  JNOONE3D = 0
!*** INCIDENT NODE MUST EXIST
  INI = ELNO(ELNI(IEL))
  IF (INI .EQ. 0 .OR. .NOT. ISCONTACT(IEL)) RETURN
!*** INCIDENT NODE MUST BE AN EXTERNAL NODE
  IF (.NOT. IFOUTERNODE(INI)) RETURN
!*** ANALYSIS FLAG MUST (17) BE ON
  IF (nint(PRPANAL(17)) .NE. 1) RETURN
!*** CANDIDATES GET FROM CLOSEST NODES IN 3D
  RMIND = 1.0d20
  JNO = 0
  DO I = 1, NNOEX
    INO = LNOEX(I)
!*** CYCLE IF IT IS THE SAME
    IF (INO .EQ. INI) CYCLE
!*** CHECK FOR COMMON ELEMENTS
    IFOUND = 0
    DO J = NOIL(INO), NOIL(INO+1)-1
      KEL = NOEL(J)
      IF (KEL .EQ. IEL) THEN
        IFOUND = 1
        EXIT
      END IF
    END DO
    IF (IFOUND .EQ. 1) CYCLE
    RDIST = RNODEDIST(INO, INI)
    IF (RDIST .LE. RMIND) THEN
      RMIND = RDIST
      JNO = INO
    END IF
  END DO
!*** NOW CHECKS IF IT IS CLOSE ENOUGH IN THE REFERENCE CONFIGURATION
  RMED = 0.0d00
  DO I = NOIR(INI), NOIR(INI+1)-1
    IAR = NOAR(I)
    RMED = RMED+EDGESIZE(IAR)
  END DO
  RMED = RMED/max(1.0d00, 1.0d00*(NOIR(INI+1)-NOIR(INI)))
  TOL = PRPANAL(20)
  WRITE (*, *) "TOLDETECTION", TOL
  IF (abs(TOL) .LE. EPSMACH()) TOL = 2.0d-4
  IF (abs(RMED) .GT. EPSMACH() .AND. RMIND .LE. TOL*RMED) THEN
    JNOONE3D = JNO
  ELSE
    JNOONE3D = 0
  END IF
END FUNCTION JNOONE3D
!*********************************************
!*** gives the value of a given ordered pair
!*** by linear interpolation
!*** ensuring that unloading is accounted for
!*** chk0
!*********************************************
SUBROUTINE FUNCPROP(RIO, ARG, F, DF)
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  IO = nint(RIO)
  IF (IO .GT. 0 .AND. IO .LE. NOR) THEN
    XV = ARG
    CALL funclinearinterpol(ORDE(IO)%N, ORDE(IO)%X, ORDE(IO)%Y, XV, F, DF)
  ELSE
    F = RIO
    DF = 0.0d00
  END IF
END SUBROUTINE FUNCPROP
!*************************************************
!*** CODE ITSELF
!*** exec=true: Performs Newton-Raphson iteration
!*** exec=false: Determines the relevant data
!*** chk0
!*************************************************
SUBROUTINE SSIMPLAS(EXEC)
  USE omp_lib
  USE OVERALL
  USE BASFUN
  IMPLICIT REAL(8) (A-H, O-Z)
  LOGICAL::EXEC
  NT = OMP_GET_MAX_THREADS()
  CALL OMP_SET_NUM_THREADS(NT)
  CALL ARRANQ
  !*** startup
  CALL DEFMOD
  IF (EXEC) THEN
    CALL ANAIMP
  ELSE
    IEQIT = 1
    CALL DRVFIR(0)
    CALL DRVFIR(1)
    CALL DRVFIR(2)
  END IF
  WRITE (*, "(A,I8)") "NUMBER OF SECONDS TAKEN", NSEG
END SUBROUTINE SSIMPLAS
!********************
!*** start up segment
!*** chk0
!********************
SUBROUTINE arranq
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  CHARACTER(lstrl)::lnm
  LOGICAL::fich1
  INTEGER::RESULT2
!*** LICENSE
  CALL getarg(1, texto(1:len(texto)))
  IF (texto .EQ. "") THEN
    RESULT2 = system('ls *.entra > listfiles')
    iu = fileprovidechannel()
    OPEN (iu, file="listfiles", status="unknown")
    nf = 0
    DO
      READ (iu, "(a)", iostat=io) lnm
      IF (io .NE. 0) EXIT
      nf = nf+1
    END DO
    IF (nf .EQ. 1) THEN
      texto = lnm
    ELSE
      WRITE (*, "(a)", advance="no") "Please insert the name of input file:"
      DO
        READ (*, "(a)") texto
        IF (texto .NE. "") EXIT
      END DO
    END IF
  END IF
  nomef(1:) = texto(1:)
  nomef = adjustl(nomef)
  texto = " "
  CALL stringlowercase(nomef)
  ii = index(nomef, "."//trim(entra))
  IF (ii .NE. 0) nomef(ii:) = " "
  jj = len_trim(nomef)
  IF (nomef(jj:jj) .EQ. ".") THEN
    nomef(jj:) = " "
  END IF
  nomefbase = nomef
  INQUIRE (file=trim(trim(nomefbase)//"."//entra), exist=fich1, err=1000)
  IF (.NOT. fich1) THEN
    CALL stringremovedigits(nomefbase)
    CALL stringremoveallspaces(nomefbase)
  END IF
  WRITE (*, *) "Estimated entra filename=", trim(adjustl(nomefbase))
  WRITE (*, "(a)") "We are now trying to OPEN", trim(adjustl(nomef))//"."//trim(adjustl(malha))
  INQUIRE (file=trim(trim(nomefbase)//"."//entra), exist=fich1, err=1000)
  IF (.NOT. fich1) THEN
    WRITE (*, "(A)") "The file ""entra"" appears to be inexistent"
    WRITE (*, "(A)") " this will soon cause a fatal error"
  END IF
  INQUIRE (file=trim(trim(nomef)//"."//malha), exist=fich1, err=1000)
  IF (.NOT. fich1) THEN
    WRITE (*, "(A)") "The file ""malha"" appears to be inexistent"
    STOP
  END IF
  CALL fileopen(nomefbase, entra, uent, "desconhecido", .FALSE., ierr); IF (ierr .NE. 0) GOTO 1000
  CALL fileopen(nomefbase, malha, umal, "desconhecido", .FALSE., ierr); IF (ierr .NE. 0) GOTO 1000
  CALL fileopen(nomefbase, grnos, ugrn, "desconhecido", .FALSE., ierr); IF (ierr .NE. 0) GOTO 1000
  RETURN
1000 CONTINUE
  WRITE (*, "(a)", iostat=io) "! error in the count down..................."
  WRITE (*, "(a)", iostat=io) "! hidden files.............................."
  WRITE (*, "(a)", iostat=io) "! file permissions.........................."
  STOP
END SUBROUTINE arranq
!********************
!*** model definition
!*** chk0
!********************
SUBROUTINE defmod
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  REAL(8), DIMENSION(3)::xc
  CALL lerestart
  WRITE (*, *) "nel before everything", nel
  CALL letime
  WRITE (*, "(a)") "time data input - checked"
  CALL lpares
  WRITE (*, "(a)") "definition of ordered pairs - checked"
  CALL lemesh
  WRITE (*, "(a)") "mesh and set input and definition - checked"
  CALL restartioc(.FALSE.)
  CALL lmater
  WRITE (*, "(a)") "many-to-many relations - checked"
  CALL losnos
  WRITE (*, "(a)") "nodes part 1 - checked"
  CALL remainingconnectivities
  WRITE (*, "(a)") "nodes part 2 - checked"
  CALL leanal
  WRITE (*, "(a)") "analysis data input - checked"
  CALL lemonitor
  WRITE (*, "(a)") "input of monitor sets - checked"
  CALL lmpcs
  WRITE (*, "(a)") "input of multiple-point constraints - checked"
  CALL lminc
  WRITE (*, "(a)") "input of initial conditions - checked"
!----------------------------
!*** relevant mesh dimensions
!----------------------------
  atax = 0.0d00
  atay = 0.0d00
  ataz = 0.0d00
  cc = huge(1.0d00)
  xl = cc
  xu = -cc
  yl = cc
  yu = -cc
  zl = cc
  zu = -cc
  DO ino = 1, nno
    CALL vectsetconst(3, xc, 0.0d00)
    DO i = 1, 3
      xc(i) = noco(i, ino)
    END DO
    xl = min(xl, xc(1))
    xu = max(xu, xc(1))
    yl = min(yl, xc(2))
    yu = max(yu, xc(2))
    zl = min(zl, xc(3))
    zu = max(zu, xc(3))
  END DO
  atax = xu-xl
  atay = yu-yl
  ataz = zu-zl
  edgemin = +1.0d99
  edgemax = -1.0d99
  DO iar = 1, nar
    rle = edgesize(iar)
    IF (rle .GT. epsmach()) THEN
      edgemin = min(edgemin, rle)
      edgemax = max(edgemax, rle)
    END IF
  END DO
  CALL VNORNFROMGEOMETRY
!*** OUTPUTS PARAMETERS FOR READING LATER
!*** CHANGE FORMAT FOR MORE DATA
  WRITE (UREP, "(6I8)") NTGL, NOR, MVPG, MVPE, NMONITOR
  CLOSE (UREP)
END SUBROUTINE defmod
!*******************
!*** RESTART ROUTINE
!*** CHK0,1
!*******************
SUBROUTINE RESTARTIOC(WRITING)
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  LOGICAL::WRITING
  INTEGER::IRST = 0
  CHARACTER(LSTRM)::FICH
  IF (WRES .AND. WRITING) THEN
    FICH = trim(adjustl(NOMEF))//"."//trim(adjustl(RESTA))
    IRST = FILEPROVIDECHANNEL()
    OPEN (IRST, FILE=trim(FICH), STATUS="UNKNOWN")
    WRITE (*, "(A)") "STARTED WRITING RESTART"
    WRITE (IRST, "(8I9)") NNO, NNOT, NEL, NTGL, NMGA, MVPG, MVPE
    WRITE (IRST, "(2I9,2E12.5)") ISTEP, IOUTS, TIME, TIMESHIFT
    DO IST = 1, 2
      DO INO = 1, NNO
        DO IGL = 1, NTGL
          WRITE (IRST, "(E12.5)") NODOF(IGL, INO, IST)
          WRITE (IRST, "(E12.5)") VELO(IGL, INO, IST)
          WRITE (IRST, "(E12.5)") ACEL(IGL, INO, IST)
        END DO
      END DO
    END DO
    DO IST = 1, 2
      DO INO = 1, NNOT-NNO
        DO IGL = 1, NTGL
          WRITE (IRST, "(E12.5)") ALPHADOF(IGL, INO, IST)
        END DO
      END DO
    END DO
    DO IST = 1, 2
      DO IEL = 1, NEL
        DO IGA = 1, NMGA
          DO IPG = 1, MVPG
            WRITE (IRST, "(E12.5)") GPHIST(IPG, IGA, IEL, IST)
          END DO
          DO IPG = 1, 6
            WRITE (IRST, "(E12.5)") GPSTRE(IPG, IGA, IEL, IST)
          END DO
        END DO
        DO IPG = 1, MVPE
          WRITE (IRST, "(E12.5)") ELHIST(IPG, IEL, IST)
        END DO
      END DO
    END DO
    flush (IRST)
    CLOSE (IRST)
    WRITE (*, "(A)") "FINISHED WRITING RESTART"
  END IF
  IF (RRES .AND. .NOT. WRITING) THEN
    WRITE (*, "(A)") "STARTED READING RESTART"
    FICH = trim(adjustl(NOMEF))//"."//trim(adjustl(RESTA))
    IRST = fileprovidechannel()
    OPEN (IRST, FILE=trim(FICH), STATUS="UNKNOWN")
    READ (IRST, *) NNO2, NNOT2, NEL2, NTGL2, NMGA2, MVPG2, MVPE2
    IF (NNO2 .NE. NNO) STOP "WRONG NNO"
    IF (NNOT2 .NE. NNOT) STOP "WRONG NNOT"
    IF (NEL2 .NE. NEL) STOP "WRONG NEL"
    IF (NTGL2 .NE. NTGL) STOP "WRONG NTGL"
    IF (NMGA2 .NE. NMGA) STOP "WRONG NMGA"
    IF (MVPG2 .NE. MVPG) STOP "WRONG MVPG"
    IF (MVPE2 .NE. MVPE) STOP "WRONG MVPE"
    READ (IRST, *) ISTEP, IOUTS, TIME, TIMESHIFT
    ALLOCATE (NODOF(NTGL, NNO, 2), VELO(NTGL, NNO, 2), ACEL(NTGL, NNO, 2))
    DO IST = 1, 2
      DO INO = 1, NNO
        DO IGL = 1, NTGL
          READ (IRST, *) NODOF(IGL, INO, IST)
          READ (IRST, *) VELO(IGL, INO, IST)
          READ (IRST, *) ACEL(IGL, INO, IST)
        END DO
      END DO
    END DO
    IF (NNOT .GT. NNO) THEN
      ALLOCATE (ALPHADOF(NTGL, NNOT-NNO, 2))
      DO IST = 1, 2
        DO INO = 1, NNOT-NNO
          DO IGL = 1, NTGL
            READ (IRST, *) ALPHADOF(IGL, INO, IST)
          END DO
        END DO
      END DO
    END IF
    ALLOCATE (GPHIST(MVPG, NMGA, NEL, 2), GPSTRE(6, NMGA, NEL, 2), ELHIST(MVPE, NEL, 2))
    DO IST = 1, 2
      DO IEL = 1, NEL
        DO IGA = 1, NMGA
          DO IPG = 1, MVPG
            READ (IRST, *) GPHIST(IPG, IGA, IEL, IST)
          END DO
          DO IPG = 1, 6
            READ (IRST, *) GPSTRE(IPG, IGA, IEL, IST)
          END DO
        END DO
        DO IPG = 1, MVPE
          READ (IRST, *) ELHIST(IPG, IEL, IST)
        END DO
      END DO
    END DO
    CLOSE (IRST)
    WRITE (*, "(A)") "FINISHED READING RESTART"
  END IF
END SUBROUTINE RESTARTIOC
!*****************
!*** ANALYSIS PART
!*** chk0,1
!*****************
SUBROUTINE anaimp
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  EXTERNAL::PROCOPT
  CALL startanalysis
  WRITE (*, *) "Dtime:", dtime
  SELECT CASE (istyle)
  CASE (0)
    CALL timex(-1)
    WRITE (*, "(a)") "Linear Solution"
    WRITE (*, "(A,E15.5)") "dtime:", dtime
    ieqit = 1
!*** sets time to ttime
    time = ttime
    CALL drvfir(0)
    CALL drvfir(1)
    CALL drvfir(2)
    WRITE (*, "(A,E15.5)") "dtime:", dtime
    CALL drvdes
    ieqit = 1
    dtime = 1.0d-9
    CALL updof(resid, 1.0d00)
    WRITE (*, *) "STEP 1"
!*** new timex
    CALL timex(1)
!*** end of new timex
    ieqit = 1
!*** stress only
    WRITE (*, *) "STEP 2"
    CALL drvfir(2)
!*** third drvfir
    WRITE (*, *) "STEP 3"
    CALL drvdes
    CALL timex(1)
    WRITE (*, *) "STEP 4"
!*** SAIDAS
    WRITE (*, *) "STEP 5"
    CALL SAIDAS
  CASE (1, 3)
    CALL TIMEX(-1)
    WRITE (*, "(a)") "Increasing Control Algorithm"
    WRITE (*, "(A,E15.5)") "dtime:", dtime
    CALL INCREASECONTROL
  CASE (2)
    CALL timex(-1)
    WRITE (*, "(a)") "Displacement Control Algorithm"
    WRITE (*, "(A,E15.5)") "dtime:", dtime, "time=", time, "timeshift=", timeshift
    CALL DISPCONTROL
  CASE (8)
    CALL rveanalysis()
  END SELECT
END SUBROUTINE ANAIMP
!************************
!*** STARTS THE ANALYSIS
!*** CHK0
!************************
SUBROUTINE STARTANALYSIS
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  IF (abs(DTIME) .LE. 1.0d-25) THEN
    IF (abs(DTIMEO) .GT. 1.0d-25) THEN
      WRITE (*, *) "FIRST CHOICE"
      CALL UPDATEDTIME(DTIMEO)
    ELSE
      WRITE (*, *) "SECOND CHOICE"
      CALL UPDATEDTIME(DTMAX)
    END IF
  END IF
!---------------------------------------
!*** CHECKS IF AT LEAST ONE MATERIAL
!*** HAS RHO>0 IN THE CASE PRPANAL(14)>0
!---------------------------------------
  ICHECK = 0
  DO IMT = 1, NMT
    IF (MAPR(4, IMT) .GT. EPSMACH()) THEN
      ICHECK = 1
      EXIT
    END IF
  END DO
  IF (PRPANAL(14) .GT. EPSMACH() .AND. ICHECK .EQ. 0) PRPANAL(14) = 0
!----------------------------------
!*** SETS NUMBER OF HOMOTOPY STEPS
!*** TO TWO IF ANALYSIS IS DYNAMIC
!*** AND REMOVES CUTTING STEP
!----------------------------------
  IF (PRPANAL(14) .GT. EPSMACH()) THEN
    PRPANAL(5) = 0
  END IF
!------------------------------------------------------
!*** AND SETS THE VALUE OF HOMOTOPY STEPS TO 2 OR MORE
!*** IF CONTACT IS ACTIVE
!------------------------------------------------------
  IF (PRPANAL(17) .GT. 0.1d00 .AND. PRPANAL(6) .LE. 1) THEN
    PRPANAL(6) = 2
  END IF
!-------------------------------------------------------------------
!*** SETS THE VALUE OF HOMOTOPY STEPS TO 2 IF SMOOTHING IS REQUIRED
!-------------------------------------------------------------------
  IF (PRPANAL(23) .EQ. 1 .AND. PRPANAL(6) .LE. 1) THEN
    PRPANAL(6) = 2
  END IF
  IF (PRPANAL(23) .EQ. 6) THEN
    PRPANAL(6) = 7
  END IF
END SUBROUTINE STARTANALYSIS
!********************************************************************
!*** CHECKS IF A STEP IS ACCEPTABLE BY USING THE NORM OF DISPLACEMENT
!*** CHK0
!********************************************************************
LOGICAL FUNCTION ACCEPTABLEITERATION(RESID)
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  REAL(8), DIMENSION(:), ALLOCATABLE, SAVE::LISTRESIDUALS
  ACCEPTABLEITERATION = .TRUE.
  IF (ISNAN(RESID)) THEN
    ACCEPTABLEITERATION = .FALSE.
    RETURN
  END IF
  IF (IEQIT .NE. 0) THEN
    CALL VECTINSERT(LISTRESIDUALS, IEQIT, RESID)
  END IF
  IF (IEQIT .GE. 9) THEN
    IF (LISTRESIDUALS(IEQIT) .GT. 1.25d00*LISTRESIDUALS(IEQIT-1) .AND. LISTRESIDUALS(IEQIT-1) .GT. &
        1.25d00*LISTRESIDUALS(IEQIT-2) .AND. LISTRESIDUALS(IEQIT-2) .GT. 1.25d00*LISTRESIDUALS(IEQIT-3) .AND. &
        LISTRESIDUALS(IEQIT-3) .GT. 1.25d00*LISTRESIDUALS(IEQIT-4) .AND. LISTRESIDUALS(IEQIT-4) .GT. 1.25d00*LISTRESIDUALS(IEQIT-5)) THEN
      ACCEPTABLEITERATION = .FALSE.
    END IF
  END IF
  IF (IEQIT .GE. 6) THEN
    IF (LISTRESIDUALS(IEQIT) .GT. 1.75d00*LISTRESIDUALS(IEQIT-1) .AND. LISTRESIDUALS(IEQIT-1) .GT. &
        1.75d00*LISTRESIDUALS(IEQIT-2) .AND. LISTRESIDUALS(IEQIT-2) .GT. 1.75d00*LISTRESIDUALS(IEQIT-3)) THEN
      ACCEPTABLEITERATION = .FALSE.
    END IF
  END IF
  IF (IEQIT .GE. 5) THEN
    IF (LISTRESIDUALS(IEQIT) .GT. 5.0d00*LISTRESIDUALS(IEQIT-1) .AND. LISTRESIDUALS(IEQIT-1) .GT. &
        5.0d00*LISTRESIDUALS(IEQIT-2)) THEN
      ACCEPTABLEITERATION = .FALSE.
    END IF
  END IF
END FUNCTION ACCEPTABLEITERATION
!************************
!*** ANALYSIS PROPERTIES
!*** CHK0
!************************
SUBROUTINE ANALYSISPROPERTIES(LIMIT, TOLER, ITLS, NSTEP, ICUTSTEP, IMAXS, NSRED, IDAMPING, ISTAB, IOP, INO1, INO2, IDOF)
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  LIMIT = 250
  IF (abs(PRPANAL(2)) .GT. 0.0d00) THEN
    TOLER = max(1.0d-15, min(PRPANAL(2), 1.0d1))
  ELSE
    TOLER = 1.0d-5
  END IF
  ITLS = scalarclamp(nint(PRPANAL(3)), 0, 5)
  NSTEP = scalarclamp(nint(PRPANAL(4)), 0, 9999)
  ICUTSTEP = scalarclamp(nint(PRPANAL(5)), 0, 1)
  NHOMOTOPY = scalarclamp(nint(PRPANAL(6)), 1, 1000)
  IMAXS = scalarclamp(nint(PRPANAL(7)), 0, 1)
  NSRED = scalarclamp(nint(PRPANAL(8)), 1, 10)
  IDAMPING = scalarclamp(nint(PRPANAL(9)), 0, 4)
  ISTAB = scalarclamp(nint(PRPANAL(22)), 0, 1)
  IOP = nint(PRPANAL(10))
  INO1 = nint(PRPANAL(11))
  INO2 = nint(PRPANAL(12))
  IDOF = nint(PRPANAL(13))
  WRITE (*, *) "ANALYSIS PROPERTIES IOP,INO1,INO2,IDOF", IOP, INO1, INO2, IDOF
END SUBROUTINE ANALYSISPROPERTIES
!*********************
!*** INCREASE CONTROL
!*** NEWTON-RAPHSON
!*** CHK0
!*********************
SUBROUTINE INCREASECONTROL
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  REAL(8), DIMENSION(:), ALLOCATABLE::ERRORHISTORY
  REAL(8), DIMENSION(:), ALLOCATABLE::rq, q, rqold, qtil
  INTEGER, PARAMETER::mdr = 15!12
  ALLOCATE (rq(nel), q(nel), rqold(nel), qtil(nel))
  IURES = fileprovidechannel()
  OPEN (IURES, FILE=trim(adjustl(NOMEF))//"."//"RESIDUALS", STATUS="UNKNOWN")
  CALL ANALYSISPROPERTIES(LIMIT, TOLER, ITLS, NSTEP, ICUTSTEP, IMAXS, NSRED, IDAMPING, ISTAB, IOP, INO1, INO2, IDOF)
  ITO = 0
  KSTEP = 0
  INCREMENTS: DO
    KSTEP = KSTEP+1
    WRITE (*, "(A,E15.5)") "TIME-TIMESHIFT=", TIME-TIMESHIFT
    CALL drvfir(0)
    CALL drvfir(1)
    IF (nhomotopy .EQ. 2 .AND. nint(prpanal(23)) .NE. 0) THEN
      w = 0.5d00
      DO iel = 1, nel
        q(iel) = gphist(22, 1, iel, 1)
        rq(iel) = 0.0d00
      END DO
      DO idr = 1, mdr
        ihomotopy = 1
        CALL NEWTONITERATION(INTERPOLATIONLOAD, ITLS, IDAMPING, ICUTSTEP, LIMIT, ERRORHISTORY, TOLER, ISUCC)
        DO iel = 1, nel
          qtil(iel) = gphist(22, 1, iel, 2)
          rqold(iel) = rq(iel)
          rq(iel) = qtil(iel)-q(iel)
          gphist(22, 1, iel, 2) = (1.0d00-w)*q(iel)+w*qtil(iel)
          q(iel) = gphist(22, 1, iel, 2)
        END DO
        rn2 = vectnorm2(nel, rq)
        WRITE (*, *) "rn2=", rn2
        IF (rn2 .LE. 1.0d-6) EXIT
        CALL timex(0)
        ihomotopy = 2
        CALL NEWTONITERATION(INTERPOLATIONLOAD, ITLS, IDAMPING, ICUTSTEP, LIMIT, ERRORHISTORY, TOLER, ISUCC)
        IF (idr .NE. 1) THEN
          gash = vectdot(nel, rqold-rq, rqold-rq)
          IF (abs(gash) .GT. 1.0d-20) THEN
            w = w*(1.0d00+vectdot(nel, rqold-rq, rq)/gash)
          END IF
          IF (w .LT. 0.0d00) w = 0.0001d00
          IF (w .GT. 1.0d00) w = 1.0d00
        END IF
        CALL timex(0)
      END DO
    ELSE
      HOMOTOPY: DO IHOMOTOPY = 1, NHOMOTOPY
        CALL NEWTONITERATION(INTERPOLATIONLOAD, ITLS, IDAMPING, ICUTSTEP, LIMIT, ERRORHISTORY, TOLER, ISUCC)
        IF (NHOMOTOPY .NE. 1) CALL TIMEX(0)
      END DO HOMOTOPY
    END IF
    WRITE (*, "(A,I4)") "NUMBER OF SECONDS FOR THE NEWTON-RAPHSON ANALYSIS", NSEG
!*** EXITS IF TIME HAS PASSED
    CALL POSTITERATIONS(ICUTSTEP, ITO, IURES, IMAXS, KSTEP, LIMIT, NSTEP, IEXIT, ERRORHISTORY)
    IF (IEXIT .EQ. 1) EXIT
  END DO INCREMENTS
  CLOSE (IURES)
END SUBROUTINE INCREASECONTROL
!*****************
!*** optimization
!*****************
SUBROUTINE CLASSICALOPTIMIZATION
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  REAL(8), DIMENSION(:), ALLOCATABLE::ERRORHISTORY
  IURES = fileprovidechannel()
  IHOMOTOPY = 1
  OPEN (IURES, FILE=trim(adjustl(NOMEF))//"."//"RESIDUALS", STATUS="UNKNOWN")
  CALL ANALYSISPROPERTIES(LIMIT, TOLER, ITLS, NSTEP, ICUTSTEP, IMAXS, NSRED, IDAMPING, ISTAB, IOP, INO1, INO2, IDOF)
  ITO = 0
  KSTEP = 0
  LOADMODE = INTERPOLATIONLOAD
  ITY = ITYPOUTPUT
  ITYPOUTPUT = -33
  TIME = 0.0d00
  NODOF(1:6, 1:NNO, 1:2) = 0.0d00
  CALL drvfir(0)
  CALL drvfir(1)
  INCREMENTS: DO
    WRITE (*, *) "ISTEP=", istep
!--------------------------------
!*** FIRST STAGE: SOLUTION FOR A
!--------------------------------
    KSTEP = KSTEP+1
    IF (ISTEP .LE. NSTEP) ITYPOUTPUT = ITY
    WRITE (*, "(A,E15.5)") "TIME-TIMESHIFT=", TIME-TIMESHIFT
    CALL NEWTONITERATION(INTERPOLATIONLOAD, ITLS, IDAMPING, ICUTSTEP, LIMIT, ERRORHISTORY, TOLER, ISUCC)
    WRITE (*, "(A,I4)") "NUMBER OF SECONDS FOR THE NEWTON-RAPHSON ANALYSIS", NSEG
    CALL POSTITERATIONS(ICUTSTEP, ITO, IURES, &
                        IMAXS, KSTEP, LIMIT, NSTEP, IEXIT, ERRORHISTORY)
    IF (IEXIT .EQ. 1) EXIT
  END DO INCREMENTS
  ITYPOUTPUT = ITY
  TIME = 0
  DTIME = DTMAX
  WRITE (*, *) "END INCREMENTATION"
  LOADMODE = ZEROLOAD
!-------------------------------------
!*** SECOND STAGE: CALCULATES \LAMBDA
!-------------------------------------
  CALL DRVFIR(3)
  CALL DRVDES
!-----------------------------------------------
!*** STORES LAMBDA IN 7:9
!*** THIS MEANS THAT IT'S +K^-1.INTERNAL_CHANGED
!-----------------------------------------------
  DO ITG = 1, 3
    DO INO = 1, NNO
      NODOF(ITG+6, INO, 1) = -DDES(ITG, INO)
      NODOF(ITG+6, INO, 2) = -DDES(ITG, INO)
    END DO
  END DO
  IF (vectifnan(NTGL*NNO, DDES)) STOP "WRONG DDES!"
!-------------
!*** KILL DDES
!-------------
  DDES = 0.0d00
!--------------------------
!*** CALCULATES ELHIST(28)
!*** AS DG/DQ-LAMBDA.DI/DQ
!--------------------------
  CALL DRVFIR(4)
!---------------------------
!*** outputs stuff
!---------------------------
  CALL SAIDAPP
  CLOSE (iures)
END SUBROUTINE classicaloptimization
!---------------------
! PHI AND GPHI
! chk0,1
!---------------------
SUBROUTINE PROCOPT(N, Q, PHI, GPHI)
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  REAL(8), PARAMETER::POWERDOSFRACOS = 1.0d12!09
  REAL(8), DIMENSION(N)::Q, GPHI
  REAL(8), PARAMETER::TWOP = 2.0d00
  DO IEL = 1, NEL
    ELHIST(29, IEL, 1:2) = Q(IEL)
  END DO
  CALL CLASSICALOPTIMIZATION
  OB = 0.0d00
  DO IEL = 1, NEL
    OB = OB+ELHIST(30, IEL, 2)
  END DO
  OB = OB/VOLMESH
  PHI = OB**(1.0d00/TWOP)
  DO IEL = 1, NEL
    GPHI(IEL) = ELHIST(28, IEL, 2)/VOLMESH
  END DO
  DO IEL = 1, NEL
!!!     GPHI(IEL)=((1.0D00/TWOP)*OB**(1.0D00/TWOP-1.0D00))*GPHI(IEL)
  END DO
  CALL VECTNORMALIZE(NEL, GPHI)
  DO IEL = 1, NEL
    GPHI(IEL) = tanh(POWERDOSFRACOS*GPHI(IEL))
  END DO
END SUBROUTINE PROCOPT
!-------------------
!*** strain simplex
!-------------------
SUBROUTINE strainsimplex(iel, vol, grad, strain)
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  REAL(8), DIMENSION(3, 3)::grad
  REAL(8), DIMENSION(6)::strain
  REAL(8), DIMENSION(4, 3)::rno, rna
!--------------------------------------
  strain = 0.0d00
  DO id = 1, 3
    DO jd = 1, 3
      grad(id, jd) = matrixkronecker(id, jd)
    END DO
  END DO
!--------------------------------------
  IF (iscontinuumelement(iel)) THEN
    SELECT CASE (if3d)
    CASE (.FALSE.)
      ndi = 2
      nn = 3
      IF (elni(iel+1)-elni(iel) .EQ. nn) THEN
        DO kn = 1, nn
          jn = elno(elni(iel)-1+kn)
          DO id = 1, ndi
            rno(kn, id) = noco(id, jn)
            rna(kn, id) = noco(id, jn)+nodof(id, jn, 2)!+ddes(id,jn)
          END DO
        END DO
        CALL elementtritetstrain(ndi, nn, rno(1:nn, 1:ndi), rna(1:nn, 1:ndi), vol, grad, strain)
      END IF
    CASE (.TRUE.)
      ndi = 3
      nn = 4
      IF (elni(iel+1)-elni(iel) .EQ. nn) THEN
        DO kn = 1, nn
          jn = elno(elni(iel)-1+kn)
          DO id = 1, ndi
            rno(kn, id) = noco(id, jn)
            rna(kn, id) = noco(id, jn)+nodof(id, jn, 2)!+ddes(id,jn)
          END DO
        END DO
        CALL elementtritetstrain(ndi, nn, rno(1:nn, 1:ndi), rna(1:nn, 1:ndi), vol, grad, strain)
      END IF
    END SELECT
  END IF
END SUBROUTINE STRAINSIMPLEX
!-----------------
!*** RVE ANALYSIS
!-----------------
SUBROUTINE rveanalysis
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  REAL(8), DIMENSION(:, :, :), ALLOCATABLE::firstdernodes
  REAL(8), DIMENSION(:, :, :), ALLOCATABLE::ainfluence, binfluence
  REAL(8), DIMENSION(3, 3)::grad
  REAL(8), DIMENSION(6)::strain
  REAL(8), DIMENSION(6, 6)::mtx1, mtx2
  iures = fileprovidechannel()
  ALLOCATE (firstdernodes(3, 3, nno))
  OPEN (iures, file=trim(adjustl(nomef))//"."//"residuals", status="unknown")
  CALL analysisproperties(limit, toler, itls, nstep, icutstep, imaxs, nsred, idamping, istab, iop, ino1, ino2, idof)
  iuenergy = fileprovidechannel()
  nhomotopy = 6
  CALL saidapp
  OPEN (iuenergy, file="energiesfromrve.txt", status="unknown")
  iuenergy2 = fileprovidechannel()
  OPEN (iuenergy2, file="energiesfromrve2.txt", status="unknown")
  ALLOCATE (ainfluence(6, 6, nel), binfluence(6, 6, nel))
  DO ihomotopy = 1, nhomotopy
    nodof = 0.0d00
    gphist = 0.0d00
    elhist(4:9, :, :) = 0.0d00
    elhist(11:, :, :) = 0.0d00
    time = 0.0d00
    kstep = 0
    CALL drvfir(0)
    CALL drvfir(1)
    CALL applypressureproperties(ihomotopy)
!--------------------------------
!*** first stage: solution for a
!--------------------------------
    kstep = kstep+1
    WRITE (*, "(a,e15.5)") "time-timeshift=", time-timeshift
    istyletemp = istyle
    istyle = 0
    CALL anaimp
    istyle = istyletemp
    iexit = 1
    kel = 0
    DO iel = 1, nel
      IF (iscontinuumelement(iel)) THEN
        kel = kel+1
        CALL strainsimplex(iel, vol, grad, strain)
        DO ij = 1, 6
          ainfluence(ij, ihomotopy, kel) = strain(ij)
          binfluence(ij, ihomotopy, kel) = max(gpstre(ij, 1, iel, 2), gpstre(ij, 1, iel, 1))
        END DO
      END IF
    END DO
    nelout = kel
  END DO
  imatmin = 0
  DO iel = 1, nel
    IF (.NOT. iscontinuumelement(iel)) THEN
      imatmin = max(imatmin, elma(iel))
    END IF
  END DO
  WRITE (iuenergy, "(i8)") nelout
  WRITE (*, *) "part b"
  kel = 0
  vtt = 0.0d00
  DO iel = 1, nel
    IF (iscontinuumelement(iel)) THEN
      kel = kel+1
      mm = elma(iel)-imatmin
      IF (mm .LT. 0) mm = elma(iel)
!----------------------------------------------------------------------------------------------------
      WRITE (iuenergy, "(i8,e15.5,i8,36e15.5)") kel, volumeel(iel), mm, 1000.0d00*ainfluence(1:6, 1:6, kel)
!----------------------------------------------------------------------------------------------------
      mtx1 = ainfluence(1:6, 1:6, kel)
      CALL matrixinverse(6, det, mtx1, mtx2)
      CALL matrixmatrixproduct(6, 6, 6, binfluence(1:6, 1:6, kel), mtx2, mtx1, 0)
      WRITE (iuenergy2, "(i8,e15.5,i8,72e15.5)") kel, volumeel(iel), mm, 1000.0d00*binfluence(1:6, 1:6, kel), mtx1(1:6, 1:6)
      vtt = vtt+volumeel(iel)
    END IF
  END DO
  WRITE (*, *) "vtt=", vtt
  DEALLOCATE (ainfluence)
  DEALLOCATE (binfluence)
  CLOSE (iuenergy)
  CLOSE (iuenergy2)
END SUBROUTINE rveanalysis
!******************************
!*** apply pressure properties
!******************************
SUBROUTINE applypressureproperties(icase)
  USE overall
  IMPLICIT REAL(8) (A-H, O-Z)
  REAL(8), DIMENSION(3, 3)::mtx
! ID=1,2,3
! 1,1,1::1,2,3
! 2,2,2::1,2,3
! 3,3,3::1,2,3
  SELECT CASE (icase)
  CASE (1)
    ID = 1
    JD = 1
  CASE (2)
    ID = 2
    JD = 2
  CASE (3)
    ID = 3
    JD = 3
  CASE (4)
    ID = 1
    JD = 2
  CASE (5)
    ID = 1
    JD = 3
  CASE (6)
    ID = 2
    JD = 3
  CASE DEFAULT
    STOP "ERROR IN RVE BOUNDARY"
  END SELECT
  MTX = 0.0d00
  MTX(ID, JD) = 1.0d00
  MTX(JD, ID) = 1.0d00
  DO IEL = 1, NEL
    NNE = ELNI(IEL+1)-ELNI(IEL)
    IF ((NNE .EQ. 3) .AND. (.NOT. ISCONTINUUMELEMENT(IEL))) THEN
      IMA = ELMA(IEL)
      MAPS(1, IMA) = MTX(1, 1)
      MAPS(2, IMA) = MTX(1, 2)
      MAPS(3, IMA) = MTX(1, 3)
      MAPS(4, IMA) = 0.0d00
      MAPS(5, IMA) = MTX(2, 1)
      MAPS(6, IMA) = MTX(2, 2)
      MAPS(7, IMA) = MTX(2, 3)
      MAPS(9, IMA) = 0.0d00
      MAPS(10, IMA) = MTX(3, 1)
      MAPS(11, IMA) = MTX(3, 2)
      MAPS(12, IMA) = MTX(3, 3)
      MAPS(13, IMA) = 0.d00
    END IF
  END DO
END SUBROUTINE applypressureproperties
!************************************
!*** postiteration tasks and function
!************************************
SUBROUTINE POSTITERATIONS(ICUTSTEP, ITO, IURES, IMAXS, KSTEP, LIMIT, NSTEP, IEXIT, ERRORHISTORY)
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  REAL(8), DIMENSION(*)::ERRORHISTORY
!----------------------------
!*** exits if time has passed
!----------------------------
  iexit = 0
  IF (dtime .LE. 0.0d00 .OR. time .GE. 0.99999d00*ttime) THEN
    WRITE (*, *) "Stopping due to end-of-time condition (time,ttime)", time, ttime
    iexit = 1
  END IF
  IF (ICUTSTEP .EQ. 1) THEN
    CALL HEURISTICSCALETIME(IMAXS, IEQIT, LIMIT)
  ELSE
    CALL SAIDAS
    CALL TIMEX(1)
  END IF
  IF ((ieqit .NE. limit .OR. icutstep .EQ. 0) .AND. iexit .EQ. 1) THEN
    iexit = 1
  ELSE
    iexit = 0
  END IF
  IF (ihomotopy .EQ. 1) THEN
    WRITE (iures, *)
    DO iq = 1, ieqit
      ito = ito+1
      WRITE (iures, "(I8,E13.5)") ito, errorhistory(iq)
    END DO
  END IF
  IF (kstep .NE. 1 .AND. nstep .EQ. 1 .AND. (ieqit .NE. limit .OR. icutstep .EQ. 0)) iexit = 1
END SUBROUTINE POSTITERATIONS
!*************************
!*** displacement control
!*** chk0,1
!*************************
SUBROUTINE DISPCONTROL
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  LOGICAL::ACCEPTABLEITERATION
  REAL(8), DIMENSION(:), ALLOCATABLE::ERRORHISTORY
  REAL(8), DIMENSION(:, :), ALLOCATABLE::UE, UI, L
  IURES = FILEPROVIDECHANNEL()
  OPEN (iures, file=trim(adjustl(nomef))//"."//"residuals", status="unknown")
  CALL ANALYSISPROPERTIES(LIMIT, TOLER, ITLS, NSTEP, ICUTSTEP, IMAXS, NSRED, IDAMPING, ISTAB, IOP, INO1, INO2, IDOF)
  CALL UPDATEDTIME(DTMAX)
  IHOMOTOPY = 1
  ALLOCATE (UE(NTGL, NNOT), UI(NTGL, NNOT), L(NTGL, NNOT))
  UE = 0.0d00
  UI = 0.0d00
  L = 0.0d00
  ITO = 0
  KSTEP = 0
  ISUCC = 0
  CUTSTEP = .FALSE.
  IEQIT = 0
  SINAL = 1.0d00
  IFAIL = 0
  increments: DO
    kstep = kstep+1
    CALL drvfir(0)
    CALL drvfir(1)
    WRITE (*, "(A,E15.5)") "Time-Timeshift=", time-timeshift
    homotopy: DO ihomotopy = 1, nhomotopy
      cutstep = .FALSE.
      ieqit = 0
      isucc = 0
      iterations: DO
        ieqit = ieqit+1
        ito = ito+1
        WRITE (*, *) "************* RLOAD=", RLOAD
        LOADMODE = ONELOAD
        CALL DRVFIR(2)
        CALL DRVDES
        UE = DDES
        LOADMODE = ZEROLOAD
        CALL DRVFIR(2)
        CALL DRVDES
        UI = -DDES
        UE = UE+UI
        CURRENTDISPLACEMENT1 = NODOF(IDOF, INO1, 2)-NODOF(IDOF, INO1, 1)
        IF (INO2 .NE. 0 .AND. INO2 .NE. INO1) THEN
          CURRENTDISPLACEMENT2 = NODOF(IDOF, INO2, 2)-NODOF(IDOF, INO2, 1)
        ELSE
          CURRENTDISPLACEMENT2 = 0.0d00
        END IF
        DU = CURRENTDISPLACEMENT1-CURRENTDISPLACEMENT2
        S = DU-SINAL*PRPANAL(19)*DTIME/DTMAX
        IF (INO2 .NE. 0 .AND. INO2 .NE. INO1) THEN
          L(IDOF, INO2) = -1.0d00
        END IF
        L(IDOF, INO1) = 1.0d00
        SI = PRODDISP(L, UI)
        SE = PRODDISP(L, UE)
        RLOADP = RLOAD
        IF (abs(SE) .GE. EPSMACH()) THEN
          RLOAD = (SI-S)/SE
        ELSE
          RLOAD = RLOADP
        END IF
        DLOAD = RLOAD-RLOADP
        DDES = RLOAD*UE-UI
        LOADMODE = PARLOAD
!*** DLOAD IS CORRECT NOW USES THE OLD VALUE
        RLOAD = RLOADP ! OLD VALUE
!*** FREEZES FOR 3 FIRST STEPS
        CALL DAMPINGCHOICE(ITLS, RESID, IDAMPING)
        IF ((CUTSTEP .OR. .NOT. ACCEPTABLEITERATION(RESID)) .AND. ICUTSTEP .EQ. 1) THEN
          IEQIT = LIMIT
        END IF
        WRITE (*, "(A,4I8,7E15.6)") "INO1, INO2, IDOF, EQUILIBRIUM ITERATION COUNTER, ERROR, RLOAD, S,C1,C2", INO1, INO2, IDOF, IEQIT, RESID, RLOAD, S, CURRENTDISPLACEMENT1, CURRENTDISPLACEMENT2
        RESIDP = PRODDISP(DDES, REAC)
        IF (IEQIT .EQ. 1) THEN
          RESIDP0 = RESIDP
          IF (abs(RESIDP0) .GT. 1.0d-20) RESIDP0 = 1.0d00
          CALL VECTINSERT(ERRORHISTORY, IEQIT, 1.0d00)
        ELSE
          CALL VECTINSERT(ERRORHISTORY, IEQIT, abs(RESIDP/RESIDP0))
        END IF
        IF (IEQIT .GE. LIMIT) THEN
          IFAIL = IFAIL+1
          IF (IFAIL .EQ. 3) THEN
            SINAL = -SINAL
            IFAIL = 0
          END IF
          EXIT HOMOTOPY
        END IF
        IF (RESID .LE. TOLER .AND. IEQIT .GE. 6) THEN
          IFAIL = 0
          ISUCC = 1
          EXIT ITERATIONS
        END IF
      END DO ITERATIONS
      IF (NHOMOTOPY .NE. 1) THEN
        CALL TIMEX(0)
      END IF
    END DO homotopy
    WRITE (*, "(A,I4)") "NUMBER OF SECONDS FOR THE NEWTON-RAPHSON ANALYSIS", NSEG
!*** EXITS IF TIME HAS PASSED
    CALL POSTITERATIONS(ICUTSTEP, ITO, IURES, IMAXS, KSTEP, LIMIT, NSTEP, IEXIT, ERRORHISTORY)
    IF (IEXIT .EQ. 1) EXIT
  END DO INCREMENTS
!*** RESIDUAL STORAGE
  CLOSE (IURES)
END SUBROUTINE DISPCONTROL
!*********************************************************
!*** internal product with displacement degrees-of-freedom
!*** chk0
!*********************************************************
REAL(8) FUNCTION PRODDISP(V1, V2)
  USE OVERALL
  IMPLICIT REAL(8) (A-H, O-Z)
  REAL(8), DIMENSION(NTGL, NNO)::V1, V2
  RES = 0.0d00
  DO I = 1, NNO
    DO J = 1, 3
      RES = RES+V1(J, I)*V2(J, I)
    END DO
  END DO
  PRODDISP = RES
END FUNCTION PRODDISP
!***************************
!*** average quantities for
!*** chk0
!**************************X*
SUBROUTINE nodeaveraging(coo, cov, coe, ino)
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
!*** sum
  REAL(8), DIMENSION(6)::coo
  REAL(8), DIMENSION(mvpg)::cov
  REAL(8), DIMENSION(mvpe)::coe
  CALL vectsetconst(mvpg, cov)
  CALL vectsetconst(mvpe, coe)
  CALL vectsetconst(6, coo)
!*** flags the existence of non-continuum elements
  ifl = 0
  DO j = noil(ino), noil(ino+1)-1
    iel = noel(j)
    IF (.NOT. iscontinuumelement(iel)) THEN
      ifl = 1
      EXIT
    END IF
  END DO
!*** all elements around
  nm = 0
  DO i = noil(ino), noil(ino+1)-1
    iel = noel(i)
    IF (.NOT. iscontinuumelement(iel)) CYCLE
    coo(1:6) = coo(1:6)+gpstre(1:6, 1, iel, 1)
    cov(1:mvpg) = cov(1:mvpg)+gphist(1:mvpg, 1, iel, 1)
    nm = nm+1
  END DO
  IF (nm .GT. 0) THEN
    coo = 2.0d00*coo/nm
    cov = 2.0d00*cov/nm
    DO j = 1, 6
      IF (abs(coo(j)) .LE. 1.0d-16) coo(j) = 0.0d00
    END DO
    DO j = 1, mvpg
      IF (abs(cov(j)) .LE. 1.0d-16) cov(j) = 0.0d00
    END DO
  END IF
!*** watch out for the special elements (yes, it is correct)
  nm = 0
  DO i = noil(ino), noil(ino+1)-1
    iel = noel(i)
    IF (iscontinuumelement(iel) .AND. ifl .EQ. 1) CYCLE
    coe(1:mvpe) = coe(1:mvpe)+elhist(1:mvpe, iel, 1)
    nm = nm+1
  END DO
  IF (nm .GT. 0) coe = coe/nm
END SUBROUTINE nodeaveraging
!********************************
!*** extrapolation stage
!*** for effective plastic strain
!*** and void fraction
!*** chk0
!********************************
SUBROUTINE extrap
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  CALL allocsafe(nmga, nel, epsextrap)
  CALL allocsafe(nmga, nel, voidextrap)
  IF (nint(prpanal(21)) .EQ. 1 .AND. abs(dtimeo) .GT. 1.0d-20 .AND. istep .GE. 3) THEN
    rconst = 1.0d00+dtime/dtimeo
    sconst = -dtime/dtimeo
    DO iel = 1, nel
      DO iga = 1, nmga
        epsextrap(iga, iel) = rconst*gphist(1, iga, iel, 2)+sconst*gphist(1, iga, iel, 1)
        voidextrap(iga, iel) = rconst*gphist(2, iga, iel, 2)+sconst*gphist(2, iga, iel, 1)
      END DO
    END DO
  ELSE
    DO iel = 1, nel
      DO iga = 1, nmga
        epsextrap(iga, iel) = gphist(1, iga, iel, 2)
        voidextrap(iga, iel) = gphist(2, iga, iel, 2)
      END DO
    END DO
  END IF
END SUBROUTINE extrap
SUBROUTINE SAIDAPP
  USE OVERALL
  IMPLICIT REAL(8) (a-h, o-z)
  REAL(8), DIMENSION(:, :), ALLOCATABLE::SCALAR, ESCALAR
  REAL(8), DIMENSION(:, :, :), ALLOCATABLE::VECTOR, EVECTOR, TENS, ETENSOR
  CHARACTER(30), DIMENSION(:), ALLOCATABLE::CSCALAR, CESCALAR, CVECTOR, CEVECTOR, CTENSOR, CETENSOR
  INTEGER, DIMENSION(:), ALLOCATABLE::NELPR, NCNOS
  REAL(8), DIMENSION(6)::COO
  REAL(8), DIMENSION(MVPE)::COE
  REAL(8), DIMENSION(MVPG)::COV
  CHARACTER(6), DIMENSION(:), ALLOCATABLE::NAMES
!*** sizes
  NSCALAR = 9
  NVECTOR = 5
  NTENSOR = 1
  NESCALAR = 5
  NEVECTOR = 3
  NETENSOR = 1
!*** ALLOCATES THE QUANTITIES
  CALL allocsafe(max(1, NSCALAR), NNO, SCALAR)
  CALL allocsafe(max(1, NESCALAR), NEL, ESCALAR)
  CALL allocsafe(3, max(1, NVECTOR), NNO, VECTOR)
  CALL allocsafe(3, max(1, NEVECTOR), NEL, EVECTOR)
  CALL allocsafe(6, max(1, NTENSOR), NNO, TENS)
  CALL allocsafe(6, max(1, NETENSOR), NEL, ETENSOR)
  IF (.NOT. allocated(CSCALAR)) ALLOCATE (CSCALAR(max(1, NSCALAR)))
  IF (.NOT. allocated(CESCALAR)) ALLOCATE (CESCALAR(max(1, NESCALAR)))
  IF (.NOT. allocated(CVECTOR)) ALLOCATE (CVECTOR(max(1, NVECTOR)))
  IF (.NOT. allocated(CEVECTOR)) ALLOCATE (CEVECTOR(max(1, NEVECTOR)))
  IF (.NOT. allocated(CTENSOR)) ALLOCATE (CTENSOR(max(1, NTENSOR)))
  IF (.NOT. allocated(CETENSOR)) ALLOCATE (CETENSOR(max(1, NETENSOR)))
!*** nodal scalars
  CSCALAR(1) = "EFF"
  CSCALAR(2) = "FRACN"
  CSCALAR(3) = "THICKRATIO"
  CSCALAR(4) = "ELL"
  CSCALAR(5) = "YIELDING"
  CSCALAR(6) = "SY"
  CSCALAR(7) = "P"
  CSCALAR(8) = "T"
  CSCALAR(9) = "NONLOCAL"
!*** nodal vectors
  CVECTOR(1) = "U"
  CVECTOR(2) = "V"
  CVECTOR(3) = "A"
  CVECTOR(4) = "REAC"
  CVECTOR(5) = "COHESIVE"
!*** nodal tensors
  CTENSOR(1) = "STRESS2PKN"
!*** element scalars
  CESCALAR(1) = "PHI"
  CESCALAR(2) = "MATERIAL"
  CESCALAR(3) = "COHESIVE"
  CESCALAR(4) = "FRACE"
  CESCALAR(5) = "PSI"
!*** element vectors
  CEVECTOR(1) = "ANIS1"
  CEVECTOR(2) = "ANIS2"
  CEVECTOR(3) = "ANIS3"
!*** element tensors
  CETENSOR(1) = "STRESS2PKE"
!*** AT THE NODES
  DO INO = 1, NNO
    KEL = ISINGLEELEMENT(INO)
    CALL NODEAVERAGING(COO, COV, COE, INO)
    SCALAR(1:6, INO) = COV(1:6)
    IF (abs(NODOF(7, INO, 2)) .GT. EPSMACH()) THEN
      SCALAR(7, INO) = NODOF(7, INO, 2)
    ELSE
      SCALAR(7, INO) = (COO(1)+COO(2)+COO(3))/3.0d00
    END IF
    SCALAR(8, INO) = NODOF(8, INO, 2)
    SCALAR(9, INO) = NODOF(9, INO, 2)
    VECTOR(1:3, 1, INO) = NODOF(1:3, INO, 2)
    VECTOR(1:3, 2, INO) = VELO(1:3, INO, 2)
    VECTOR(1:3, 3, INO) = ACEL(1:3, INO, 2)
    VECTOR(1:3, 4, INO) = REAC(1:3, INO)
    IF (KEL .NE. 0) THEN
      VECTOR(1:3, 5, INO) = ELHIST(4:6, KEL, 2)
    ELSE
      VECTOR(1:3, 5, INO) = COE(4:6)
    END IF
    TENS(1:6, 1, INO) = COO(1:6)
  END DO
!*** AT THE ELEMENTS
  DO IEL = 1, NEL
    ESCALAR(1, IEL) = ELHIST(30, IEL, 2)
    ESCALAR(2, IEL) = ELMA(IEL)
    ESCALAR(3, IEL) = ELHIST(14, IEL, 2)
    ESCALAR(4, IEL) = GPHIST(2, 1, IEL, 2)
    EVECTOR(1:3, 1, IEL) = GPHIST(28:30, 1, IEL, 2)
    EVECTOR(1:3, 2, IEL) = GPHIST(31:33, 1, IEL, 2)
    EVECTOR(1:3, 3, IEL) = GPHIST(34:36, 1, IEL, 2)
    ETENSOR(1:6, 1, IEL) = GPSTRE(1:6, IEL, 1, 2)
  END DO
!*** PROVIDES ADDITIONAL REQUIRED QUANTITIES
  CALL ALLOCSAFE(NFAex, NELPR)
  DO IFA = 1, NFAex
    IF (FABNI(IFA+1)-FABNI(IFA) .EQ. 3) THEN
      NELPR(IFA) = 3
    ELSE
      NELPR(IFA) = 4
    END IF
  END DO
  CALL ALLOCSAFE(NTEL, NCNOS)
  DO I = 1, NTEL
    NCNOS(I) = NNOSC(I)
  END DO
  IF (.NOT. allocated(NAMES)) ALLOCATE (NAMES(NTEL))
  DO I = 1, NTEL
    NAMES(I) = NOMES(I)
  END DO
!*** CALLS THE ENSIGHT OUTPUT ROUTINE
!  itypoutput = 1
  IF (itypoutput .GE. 0) THEN
    IF (if3d .AND. itypoutput .EQ. 1) THEN
      CALL fileoutputensight( &
        IOUTS, NOMEF, &
        NNO, NOCO(1:3, :), &
        NFaex, &
        NSCALAR, NVECTOR, NTENSOR, &
        CSCALAR, CVECTOR, CTENSOR, &
        SCALAR, VECTOR, TENS, &
        0, 0, 0, &
        CESCALAR, CEVECTOR, CETENSOR, &
        ESCALAR, EVECTOR, ETENSOR, NAMES, NELPR, NCNOS, fabni, fabno)
    ELSE
      CALL fileoutputensight( &
        IOUTS, NOMEF, &
        NNO, NOCO(1:3, :), &
        NEL, &
        NSCALAR, NVECTOR, NTENSOR, &
        CSCALAR, CVECTOR, CTENSOR, &
        SCALAR, VECTOR, TENS, &
        NESCALAR, NEVECTOR, NETENSOR, &
        CESCALAR, CEVECTOR, CETENSOR, &
        ESCALAR, EVECTOR, ETENSOR, NAMES, NELPR, NCNOS, ELNI, ELNO)
    END IF
    IOUTS = IOUTS+1
  END IF
END SUBROUTINE SAIDAPP
!***************************
!*** general output routine
!*** chk0
!***************************
SUBROUTINE SAIDAS()
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  INTEGER, PARAMETER::numax = 500
  REAL(8), DIMENSION(numax)::var
  REAL(8), DIMENSION(6)::coo2
  REAL(8), DIMENSION(mvpg)::cov2
  REAL(8), DIMENSION(mvpe)::cot2
  REAL(8), DIMENSION(NTGL)::REC
  LOGICAL::checkexit = .FALSE.
!--------------
!*** spects
!*** check exit
!--------------
  CHECKEXIT = .TRUE.
  IF (NSPECT .NE. 0 .AND. ISTEP .NE. 0) THEN
    CHECKEXIT = .FALSE.
    DO IS = 1, NSPECT
      IF (abs(SPECT(IS)-TIME) .LE. max(1.0d-3*DTIMEO, 1.0d-3*DTIME)) THEN
        CHECKEXIT = .TRUE.
        EXIT
      END IF
    END DO
  END IF
!----------------
!*** target nodes
!----------------
  IF (inotarget .NE. 0 .AND. idoftarget .NE. 0) THEN
    checkexit = .FALSE.
    WRITE (*, *) "nodof(idoftarget,inotarget,2)", nodof(idoftarget, inotarget, 2), "rtarget", rtarget
    IF (abs(nodof(idoftarget, inotarget, 2)) .GE. 0.9999d00*rtarget) THEN
      checkexit = .TRUE.
    END IF
  END IF
  IF (checkexit) THEN
    WRITE (*, "(A,I8)") " Outputting for step ", istep
    WRITE (*, "(A,I8,a,I8,a)") " with ", nel, " elements and ", nno, " nodes"
    WRITE (*, "(A,E15.5)") "dtime:", dtime, "time=", time, "timeshift=", timeshift
    CALL SAIDAPP
!----------------------------------
!*** monitoring and reaction output
!----------------------------------
    rec = 0.0d00
    DO in = 1, nnodreac
      i = nodreac(in)
      DO j = 1, ntgl!SIZE(reac,1)
        rec(j) = rec(j)+reac(j, i)
      END DO
    END DO
!*** only for nor<200
    IF (nor .LE. 200) THEN
!*** reaction sum
      IF (nnodreac .GT. 0) WRITE (urea, "(40e16.8)") time, (rec(i), i=1, ntgl), (externload(io), io=1, nor), cracklength
      WRITE (uvol, "(4e18.9,2I8)") time, volmesh, strainenergy, kineticenergy, nno, nel
      WRITE (uene, "(20e18.9)") time, averagemeshquality()
!*** monitor
      IF (nmonitor .GT. 0) THEN
        var = 0.0d00
        var(1) = time
        DO io = 1, nor
          var(1+io) = funcval(io)!externload(io)
        END DO
        var(nor+2) = cracklength
        var(nor+3) = rload
        WRITE (uord, "(500e18.9)", advance="no") var(1:nor+3)
        DO i = 1, nmonitor
          ln = monitored(i)
          CALL nodeaveraging(coo2, cov2, cot2, ln)
          var = 0.0d00
          mn = 1
          mx = mn-1+ntgl
          var(mn:mx) = nodof(1:ntgl, ln, 2)
          mn = mx+1
          mx = mn-1+6
          var(mn:mx) = coo2(1:6)
          mn = mx+1
          mx = mn-1+mvpg
          var(mn:mx) = cov2(1:mvpg)
          mn = mx+1
          mx = mn-1+mvpe
          var(mn:mx) = cot2(1:mvpe)
          mn = mx+1
          mx = mn-1+ntgl
          var(mn:mx) = velo(1:ntgl, ln, 2)
          mn = mx+1
          mx = mn-1+3
          var(mn:mx) = noco(1:3, ln)
          DO k = 1, numax
            IF (isnan(var(k)) .OR. abs(var(k)) .LE. 1.0d-25) var(k) = 0.0d00
          END DO
          WRITE (uord, "(500e18.9)", advance="no") var(1:mx)
        END DO
      END IF
      WRITE (uord, *)
    ELSE
      CALL warn("Too many ordered pairs for monitoring")
    END IF
  END IF
END SUBROUTINE SAIDAS
!***************************************
!*** performs a step update with damping
!*** according to a damping strategy
!*** chk0
!***************************************
SUBROUTINE dampingchoice(itls, resid, idamping)
  USE overall
  IMPLICIT REAL(8) (a-h, o-z)
  stp = 1.0d00
  SELECT CASE (idamping)
  CASE (0)
     CALL UPDOF(RESID, 1.0d00)
  CASE (1)
    CALL UPDOF(RESID, 0.0d00)
    IF (RESID .GT. 0.25d00) THEN
      STP = 0.25d00/RESID
    ELSE
      STP = 1.0d00
    END IF
    CALL UPDOF(RESID, STP)
  CASE (2)
    CALL updof(resid, 0.0d00)
    IF (resid .GT. 0.00001d00) stp = 0.99d00
    IF (resid .GT. 0.0001d00) stp = 0.90d00
    IF (resid .GT. 0.001d00) stp = 0.75d00
    IF (resid .GT. 0.01d00) stp = 0.60d00
    IF (resid .GT. 0.1d00) stp = 0.50d00
    IF (resid .GT. 0.5d00) stp = 0.40d00
    IF (resid .GT. 1.0d00) stp = 0.30d00
    IF (resid .GT. 10.0d00) stp = 0.25d00
    CALL updof(resid, stp)
  CASE (3)
    CALL updof(resid, 0.0d00)
    stp = 1.0d00
    IF (resid .GT. 0.00001d00) stp = 0.60d00
    IF (resid .GT. 0.0001d00) stp = 0.50d00
    IF (resid .GT. 0.001d00) stp = 0.40d00
    IF (resid .GT. 0.01d00) stp = 0.10d00
    IF (resid .GT. 0.1d00) stp = 0.0500d00
    IF (resid .GT. 0.5d00) stp = 0.0250d00
    IF (resid .GT. 1.0d00) stp = 0.0125d00
    IF (resid .GT. 10.0d00) stp = 0.00675d00
    IF (resid .GT. 100.0d00) stp = 0.001d00
    CALL updof(resid, stp)
  CASE (4)
    CALL updof(resid, 0.0d00)
    IF (resid .GT. 0.00001d00) stp = 0.5d00
    IF (resid .GT. 0.0001d00) stp = 0.20d00
    IF (resid .GT. 0.001d00) stp = 0.10d00
    IF (resid .GT. 0.01d00) stp = 0.01d00
    IF (resid .GT. 0.1d00) stp = 0.005d00
    IF (resid .GT. 0.5d00) stp = 0.002d00
    IF (resid .GT. 1.0d00) stp = 0.001d00
    IF (resid .GT. 10.0d00) stp = 0.0005d00
    CALL updof(resid, stp)
  CASE default
    CALL erro("Wrong damping request")
  END SELECT
END SUBROUTINE dampingchoice
INTEGER FUNCTION isingleelement(ino)
  USE overall
  isingleelement = 0
  DO ik = noil(ino), noil(ino+1)-1
    iel = noel(ik)
    IF (elni(iel+1)-elni(iel) .EQ. 1) THEN
      isingleelement = iel
      RETURN
    END IF
  END DO
END FUNCTION isingleelement
