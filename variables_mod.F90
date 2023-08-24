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

!> Handle variables for the IFS model

MODULE VARIABLES_MOD
use parkind1, only : jpim

IMPLICIT NONE
PRIVATE

PUBLIC :: VARIABLES, VARIABLES_CREATE, VARIABLES_DELETE, VARIABLES_CLONE, &
        & HAS_MODEL_FIELDS, HAS_OLD_FIELDS, IS_LINEAR

! ------------------------------------------------------------------------------

TYPE :: VARIABLES
  LOGICAL :: linit = .false.
  LOGICAL :: lctrl = .false.
  LOGICAL :: linear = .false.
  integer(kind=jpim), pointer :: fieldids(:)=>null()
END TYPE VARIABLES

! ------------------------------------------------------------------------------

interface variables_create
  module procedure variables_create_old, variables_create_new
end interface

! ------------------------------------------------------------------------------


! ------------------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------------------

SUBROUTINE VARIABLES_CREATE_OLD(SELF, LDCTL, LDLIN)

TYPE(VARIABLES), INTENT(INOUT) :: SELF
LOGICAL, INTENT(IN) :: LDCTL
LOGICAL, OPTIONAL, INTENT(IN) :: LDLIN



END SUBROUTINE VARIABLES_CREATE_OLD

! ------------------------------------------------------------------------------

SUBROUTINE VARIABLES_CREATE_NEW(SELF, KIDS)

TYPE(VARIABLES), INTENT(INOUT) :: SELF
integer(kind=jpim), intent(in) :: kids(:)






END SUBROUTINE VARIABLES_CREATE_NEW

! ------------------------------------------------------------------------------

SUBROUTINE VARIABLES_CLONE(SELF, OTHER)

TYPE(VARIABLES), INTENT(INOUT) :: SELF
TYPE(VARIABLES), INTENT(IN) :: OTHER





END SUBROUTINE VARIABLES_CLONE

! ------------------------------------------------------------------------------

SUBROUTINE VARIABLES_DELETE(SELF)

TYPE(VARIABLES), INTENT(INOUT) :: SELF


END SUBROUTINE VARIABLES_DELETE

! ------------------------------------------------------------------------------

LOGICAL FUNCTION HAS_MODEL_FIELDS(SELF)

TYPE(VARIABLES), INTENT(IN) :: SELF


END FUNCTION HAS_MODEL_FIELDS

! ------------------------------------------------------------------------------

LOGICAL FUNCTION HAS_OLD_FIELDS(SELF)

TYPE(VARIABLES), INTENT(IN) :: SELF


END FUNCTION HAS_OLD_FIELDS

! ------------------------------------------------------------------------------

LOGICAL FUNCTION IS_LINEAR(SELF)

TYPE(VARIABLES), INTENT(IN) :: SELF


END FUNCTION IS_LINEAR

! ------------------------------------------------------------------------------

END MODULE VARIABLES_MOD
