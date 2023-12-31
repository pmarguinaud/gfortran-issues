MODULE YOMFPOP

USE YOMFPIOS  , ONLY : TNAMFPIOS
USE TYPE_FAOPH, ONLY : TFAOPH
USE TYPE_FPOFN, ONLY : TFPOFN

IMPLICIT NONE

SAVE

! I/O handling for Fullpos

TYPE TFPIOH

! General I/O control parameters
TYPE(TNAMFPIOS) :: YNAMFPIOS

! FA I/O parameters
TYPE (TFAOPH), ALLOCATABLE :: YFPOPH(:)

! Filenames
TYPE (TFPOFN), ALLOCATABLE :: YFPOFN(:)

END TYPE TFPIOH

END MODULE YOMFPOP
