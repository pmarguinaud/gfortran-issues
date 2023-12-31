MODULE YOMCUMFS
USE PARKIND1, ONLY : JPRB
USE YOMHOOK, ONLY : LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

SAVE

!       ----------------------------------------------------------------
!**    ** *YOMCUMFS* CONTAINS VARIABLES FOR SIMPLIFIED CONVECTION SCHEME 
!       ----------------------------------------------------------------

TYPE :: TCUMFS
LOGICAL :: LECUMFS
LOGICAL :: LREGCV
LOGICAL :: LMFCFL2_SHSTEP
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TCUMFS
!============================================================================

!!TYPE(TCUMFS), POINTER :: YRCUMFS => NULL()

!*    *YOMCUMFS* CONTAINS VARIABLES FOR SIMPLIFIED CONVECTION SCHEME 

!     P. LOPEZ      E.C.M.W.F.    09/01/2002

!     NAME            TYPE     DESCRIPTION
!     ----            ----     -----------

!***  LECUMFS         LOGICAL  SWITCH ON SIMPLIFIED CONVECTIVE MASS-FLUX SCHEME IN TRAJECTORY
!***  LREGCV          LOGICAL  SWITCH ON REGULARIZATIONS OF SIMPLIFIED CONVECTION SCHEME 
!***                           IN TANGENT-LINEAR AND ADJOINT CALCULATIONS
!***  LMFCFL2_SHSTEP  LOGICAL  SWITCH ON REDUCTION OF MASS FLUX CFL CRITERION FOR SHORT TIME STEPS 

!     ------------------------------------------------------------------
CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)

  
  CLASS(TCUMFS), INTENT(IN) :: SELF
  INTEGER      , INTENT(IN) :: KDEPTH
  INTEGER      , INTENT(IN) :: KOUTNO
  
  INTEGER :: IDEPTHLOC
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  
  

  
  
  
  
  
END SUBROUTINE PRINT_CONFIGURATION
END MODULE YOMCUMFS
