MODULE YOMCSGEOM

USE PARKIND1, ONLY : JPIM, JPRB, JPRD

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    * Computational sphere horizontal geometry

!     RCOLON(NGPTOT) cosine of longitude on transformed sphere
!     RSILON(NGPTOT)   sine        "             "         "
!     RINDX (NGPTOT) Longitude index
!     RINDY (NGPTOT) Latitude index
!     RATATH(NGPTOT) RA*TAN(THETA) on real sphere
!     RATATX(NGPTOT) Curvature term for LAM (for u eq.)

TYPE TCSGEOM
  REAL(KIND=JPRD), POINTER :: RCOLON(:) => NULL()
  REAL(KIND=JPRD), POINTER :: RSILON(:) => NULL()
  REAL(KIND=JPRB), POINTER :: RINDX (:) => NULL()
  REAL(KIND=JPRB), POINTER :: RINDY (:) => NULL()
  REAL(KIND=JPRB), POINTER :: RATATH(:) => NULL()
  REAL(KIND=JPRB), POINTER :: RATATX(:) => NULL()
END TYPE TCSGEOM

TYPE TCSGEOM_BLOCKED
  REAL(KIND=JPRD), POINTER :: RCOLON(:,:) => NULL()
  REAL(KIND=JPRD), POINTER :: RSILON(:,:) => NULL()
  REAL(KIND=JPRB), POINTER :: RINDX (:,:) => NULL()
  REAL(KIND=JPRB), POINTER :: RINDY (:,:) => NULL()
  REAL(KIND=JPRB), POINTER :: RATATH(:,:) => NULL()
  REAL(KIND=JPRB), POINTER :: RATATX(:,:) => NULL()
END TYPE TCSGEOM_BLOCKED

! define blocked and non-blocked (_NB) structures
! note that the blocked structure YRCSGEOM will be initialised to point into 
! the non-blocked structure YRCSGEOM_NB
!!TYPE(TCSGEOM), POINTER :: YRCSGEOM(:) => NULL()
!!TYPE(TCSGEOM), POINTER :: YRCSGEOM_NB => NULL()

! ------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------

SUBROUTINE DEALLO_TCSGEOM(YDCSGEOM)

USE YOMHOOK, ONLY : LHOOK, DR_HOOK, JPHOOK

TYPE(TCSGEOM), POINTER, INTENT(INOUT) :: YDCSGEOM(:)
INTEGER(KIND=JPIM) :: J
REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE







END SUBROUTINE DEALLO_TCSGEOM

!     ------------------------------------------------------------------

SUBROUTINE DEALLO_TCSGEOM_NB(YDCSGEOM)

USE YOMHOOK, ONLY : LHOOK, DR_HOOK, JPHOOK

TYPE(TCSGEOM), INTENT(INOUT) :: YDCSGEOM
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE












END SUBROUTINE DEALLO_TCSGEOM_NB

SUBROUTINE DEALLO_TCSGEOM_B(YDCSGEOM)

USE YOMHOOK, ONLY : LHOOK, DR_HOOK, JPHOOK

TYPE(TCSGEOM_BLOCKED), INTENT(INOUT) :: YDCSGEOM
REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE












END SUBROUTINE DEALLO_TCSGEOM_B

!     ------------------------------------------------------------------

END MODULE YOMCSGEOM
