MODULE YOMATLAS

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

PRIVATE

PUBLIC :: TATLAS
PUBLIC :: DEALLO_TATLAS

!     ------------------------------------------------------------------

!* ATLAS

TYPE :: TATLAS
END TYPE TATLAS

! ------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------

SUBROUTINE DEALLO_TATLAS(YDATLAS)

USE YOMHOOK, ONLY : LHOOK, DR_HOOK, JPHOOK

TYPE(TATLAS), POINTER, INTENT(INOUT) :: YDATLAS

REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE








END SUBROUTINE DEALLO_TATLAS

! ------------------------------------------------------------------

END MODULE YOMATLAS
