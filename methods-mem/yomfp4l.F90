MODULE YOMFP4L

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!     Descriptors of a post-processing request

! === ALL FIELDS ===

!     NFIELDG : number of fields
!     ICOD    : internal field codes
!     ZLEV    : level values
!     LLSURF  : Surface / upper-air 
!     CLPREF  : Kind of level / FA prefix name of each field
!     CLNAME  : FA suffix name of each field
!     IGRIB   : GRIB code of each field

! === GRID POINT FIELDS ONLY ===

!     IDOM : number of subdomains for each field
!     IDMP : indexes of subdomains for each field

! === SPECTRAL FIELDS ONLY ===

!     NFIELDL : local number of fields in the "Vertical" distribution 
!     IVSETG  : V-set for each field (global)
!     IVLOCG  : Local field adress for each field


TYPE TRQFP

! All fields
INTEGER(KIND=JPIM) :: NFIELDG = 0
INTEGER(KIND=JPIM), ALLOCATABLE :: ICOD(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: IGRIB(:)
LOGICAL,            ALLOCATABLE :: LLSURF(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: IVEC(:)
REAL(KIND=JPRB),    ALLOCATABLE :: ZLEV(:)
CHARACTER(LEN=12),  ALLOCATABLE :: CLNAME(:)
CHARACTER(LEN=8),   ALLOCATABLE :: CLPREF(:)

! Gridpoint fields only (however ...)
INTEGER(KIND=JPIM), ALLOCATABLE :: IDOM(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: IDMP(:,:)

! Spectral fields only

INTEGER(KIND=JPIM) :: NFIELDL = 0
INTEGER(KIND=JPIM), ALLOCATABLE :: IVSETG(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: IVLOCG(:)

END TYPE TRQFP


CONTAINS

INTEGER(KIND=JPIM) FUNCTION IFPSEARCH(YDTYPE,KCOD)



TYPE(TRQFP), INTENT(IN) :: YDTYPE
INTEGER(KIND=JPIM), INTENT(IN) :: KCOD

INTEGER(KIND=JPIM) :: J
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END FUNCTION IFPSEARCH

!     ------------------------------------------------------------------
END MODULE YOMFP4L
