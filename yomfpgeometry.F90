MODULE YOMFPGEOMETRY

USE PARKIND1  ,ONLY : JPIM, JPRB
USE TYPE_FPUSERGEO, ONLY : TFPUSERGEO
USE YOMFPGEO      , ONLY : TFPGEO
USE YOMFPGIND     , ONLY : TFPGIND

IMPLICIT NONE

SAVE

PRIVATE
PUBLIC :: TFPGEOMETRY, LFPOSBUF, LFPDISTRIB
!     ------------------------------------------------------------------

! FULLPOS GEOMETRY
! ================

! YFPUSERGEO(:) : User geometries

! YFPGEO      : target mixed grids parameters
! YFPGEO_DEP  : interpolation mixed grids parameters
! YFPGIND     : control arrays for transposition from interpolation geometry to target geometry and vice-versa

! NMDLRESOL   : tag of the input resolution (from the spectral transforms package)
! LFPOSBUF    : .TRUE. if fullpos-shaped buffer should be used (meaning : any change of horizontal geometry)
! LFPOSHOR    : .TRUE. for actual horizontal interpolations and not sampling
! LFPDISTRIB  : .TRUE. if additional transposition between departure geometry and arrival geometry is active

TYPE TFPGEOMETRY

TYPE(TFPUSERGEO), ALLOCATABLE :: YFPUSERGEO(:)

TYPE(TFPGEO)  :: YFPGEO
TYPE(TFPGEO)  :: YFPGEO_DEP
TYPE(TFPGIND) :: YFPGIND

INTEGER(KIND=JPIM) :: NMDLRESOL = 0
LOGICAL :: LFPOSHOR = .TRUE.

END TYPE TFPGEOMETRY

CONTAINS

LOGICAL FUNCTION LFPOSBUF(YDFPGEOMETRY)

TYPE(TFPGEOMETRY), INTENT(IN) :: YDFPGEOMETRY




END FUNCTION LFPOSBUF

LOGICAL FUNCTION LFPDISTRIB(YDFPGEOMETRY)

TYPE(TFPGEOMETRY), INTENT(IN) :: YDFPGEOMETRY






END FUNCTION LFPDISTRIB

!     ------------------------------------------------------------------
END MODULE YOMFPGEOMETRY
