MODULE YOMSLPHY

USE PARKIND1, ONLY : JPIM, JPRB
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

SAVE


TYPE :: TSLPHY

!     ------------------------------------------------------------------

! * Variables for split ECMWF physics.

! RSLWX   : level of implicitness of semi-Lagrangian/physics.
! NVTEND  : third dimension of SAVTEND (number of 3D fields).
! SAVTEND : buffer to store physical tendencies.
! SAVTEND5: trajectory for SAVTEND.

REAL(KIND=JPRB),ALLOCATABLE :: RSLWX(:)
INTEGER(KIND=JPIM) :: NVTEND
REAL(KIND=JPRB),ALLOCATABLE :: SAVTEND(:,:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SAVTEND5(:,:,:,:)
! Pointers for SAVTEND
INTEGER(KIND=JPIM) :: MU_SAVTEND,MV_SAVTEND,MT_SAVTEND
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TSLPHY
!============================================================================
!TYPE(TSLPHY), POINTER :: YRSLPHY => NULL()

CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  
  CLASS(TSLPHY), INTENT(IN) :: SELF
  INTEGER      , INTENT(IN) :: KDEPTH
  INTEGER      , INTENT(IN) :: KOUTNO

  INTEGER :: IDEPTHLOC
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


  
  
  
  
  
  
  
  
  
  


END SUBROUTINE PRINT_CONFIGURATION

END MODULE YOMSLPHY
