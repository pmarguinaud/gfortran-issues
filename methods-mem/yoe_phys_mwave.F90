MODULE YOE_PHYS_MWAVE

USE PARKIND1  ,ONLY : JPRB, JPIM

IMPLICIT NONE


PRIVATE 

!       ----------------------------------------------------------------
!PHYSICS VARIABLES FOR MWAVE OPERATOR
!     D.Salmond      E.C.M.W.F.    23 Jan 2018
!     Moved out these arrays from GFL     
!       ----------------------------------------------------------------

TYPE,PUBLIC :: TEPHYSMWAVE
  LOGICAL :: LPHYS_MWAVE_FILLED_IN=.FALSE.
  REAL(KIND=JPRB),ALLOCATABLE :: PHYS_MWAVE(:,:,:,:)

  CONTAINS
  PROCEDURE :: CREATE
  PROCEDURE :: DESTROY
  PROCEDURE :: ZERO
  PROCEDURE :: COPY
END TYPE TEPHYSMWAVE

INTEGER(KIND=JPIM),PUBLIC :: N_PHYS_MWAVE

CONTAINS
!============================================================================
SUBROUTINE CREATE(SELF,YDGEOMETRY)
USE GEOMETRY_MOD , ONLY : GEOMETRY

TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
CLASS(TEPHYSMWAVE) :: SELF



END SUBROUTINE CREATE
!============================================================================
SUBROUTINE DESTROY(SELF)

CLASS(TEPHYSMWAVE) :: SELF



END SUBROUTINE DESTROY
!============================================================================

SUBROUTINE ZERO(SELF)
CLASS(TEPHYSMWAVE) :: SELF

END SUBROUTINE ZERO
!============================================================================
SUBROUTINE COPY(SELF,RHS)
CLASS(TEPHYSMWAVE) :: SELF
CLASS(TEPHYSMWAVE) :: RHS



END SUBROUTINE COPY

END MODULE YOE_PHYS_MWAVE
