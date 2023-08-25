!
! Copyright 2011 ECMWF
!
! This software was developed at ECMWF for evaluation
! and may be used for academic and research purposes only.
! The software is provided as is without any warranty.
!
! This software can be used, copied and modified but not
! redistributed or sold. This notice must be reproduced
! on each copy made.
!

!> Handle geometry for the IFS model

MODULE GEOMETRY_MOD

USE PARKIND1 , ONLY : JPIM, JPRB
USE TYPE_GEOMETRY, ONLY : GEOMETRY

IMPLICIT NONE
PRIVATE

PUBLIC :: GEOMETRY_CLONE, GEOMETRY_DELETE, GEOMETRY_SAME, GEOMETRY

SAVE
INTEGER(KIND=JPIM), PRIVATE :: NUSEGEOM=0

! ------------------------------------------------------------------------------

CONTAINS


! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

SUBROUTINE GEOMETRY_CLONE(SELF,OTHER)
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK


TYPE(GEOMETRY), INTENT(INOUT) :: SELF
TYPE(GEOMETRY), INTENT(INOUT)    :: OTHER

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END SUBROUTINE GEOMETRY_CLONE

! ------------------------------------------------------------------------------

SUBROUTINE GEOMETRY_DELETE(SELF)
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCSGEOM, ONLY : DEALLO_TCSGEOM, DEALLO_TCSGEOM_NB, DEALLO_TCSGEOM_B
USE YOMGSGEOM, ONLY : DEALLO_TGSGEOM, DEALLO_TGSGEOM_NB, DEALLO_TGSGEOM_B
USE YOMVERT  , ONLY : DEALLOC_VERTICAL_GEOM


TYPE(GEOMETRY), INTENT(INOUT) :: SELF

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


















END SUBROUTINE GEOMETRY_DELETE
! ------------------------------------------------------------------------------
FUNCTION GEOMETRY_SAME(SELF,OTHER)
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
LOGICAL :: GEOMETRY_SAME
TYPE(GEOMETRY), INTENT(IN) :: SELF
TYPE(GEOMETRY), INTENT(IN)    :: OTHER

LOGICAL :: LLSAME
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END FUNCTION GEOMETRY_SAME
! ------------------------------------------------------------------------------

END MODULE GEOMETRY_MOD
