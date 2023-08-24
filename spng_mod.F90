!OCL  NOEVAL
MODULE SPNG_MOD

! Purpose :
! -------
!    Module for sponge applied at the top of the models.
!    This new sponge scheme can be used in 3D models.
!    - sponge is done in spectral space for GMV and spectral GFL.
!    - sponge is done in grid-point space for grid-point GFL.
!    Replaces the old sponge under NSPONGE=2

! Interface :
! ---------
!    Empty.

! External :  
! --------
!    None.

! Method :
! ------
!    See Documentation.

! Reference :
! ---------

! Author :
! ------
!    K. YESSAD (CNRM/GMAP), after old sponge
!    Original : October 2011

! Modifications :
! -------------
!    T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!    K. Yessad (July 2014): Move some variables.
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST   , ONLY : RPI

IMPLICIT NONE
SAVE

!=============================================================================

!      1.    TYPE DEFINITIONS
!            ----------------

! none for the time being.

!=============================================================================

!      2.    DECLARATIONS
!            ------------

! Namelist variables:
! * LNSPONGE : to switch on new sponge:
! * RSPONBT  : altitude of the bottom of the sponge layer.
! * RSPONTP  : altitude of the top of the sponge layer.
! * RSPONTAU : time scale of absorption.
! * RSPONGN  : minimum value for RSPONGF.
! Other variables:
! * RSPONGF  : sponge function at full levels.
! Remarks:
! * for RSPONBT,RSPONTP,RSPONTAU,RSPONGN,RSPONGF: index 1 for GMV, 2 for sp. GFL, 3 for gp. GFL.

TYPE :: TSPNG
LOGICAL :: LNSPONGE
REAL(KIND=JPRB), ALLOCATABLE :: RSPONGF(:,:)

CONTAINS
  
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
    
END TYPE TSPNG

!!TYPE(TSPNG), POINTER :: YRSPNG => NULL()


!=============================================================================

CONTAINS

!      3.    ENCAPSULATED ROUTINES (SETUP, ALLOC, DEALLOC)
!            ---------------------------------------------

SUBROUTINE SUSPNG(YDSPNG,YDRIP,YDDYNA,NFLEVG,PSTZ)





USE YOMRIP , ONLY : TRIP
USE YOMDYNA, ONLY : TDYNA
TYPE(TSPNG)       , TARGET, INTENT(INOUT) :: YDSPNG
TYPE(TRIP)        , INTENT(INOUT) :: YDRIP
TYPE(TDYNA)       , INTENT(INOUT) :: YDDYNA
INTEGER(KIND=JPIM), INTENT(IN)    :: NFLEVG
REAL(KIND=JPRB)   , INTENT(IN)    :: PSTZ(:)
CHARACTER(LEN = 80) :: CLFMT
INTEGER(KIND=JPIM) :: JLEV,JJ
REAL(KIND=JPRB) :: ZARG, ZTSTEP, ZTSTEP_REF, ZEXPO, ZRSPONGN
REAL(KIND=JPRB) :: ZALTF(NFLEVG)

REAL(KIND=JPRB) :: RSPONBT(3)
REAL(KIND=JPRB) :: RSPONTP(3)
REAL(KIND=JPRB) :: RSPONTAU(3)
REAL(KIND=JPRB) :: RSPONGN(3)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

LOGICAL, POINTER :: LNSPONGE






END SUBROUTINE SUSPNG

!--------------------------------------------------------------------------
SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)

CLASS(TSPNG), INTENT(IN) :: SELF
INTEGER     , INTENT(IN) :: KDEPTH
INTEGER     , INTENT(IN) :: KOUTNO

INTEGER :: IDEPTHLOC 






END SUBROUTINE PRINT_CONFIGURATION
!--------------------------------------------------------------------------

END MODULE SPNG_MOD
