MODULE YOMGFL

USE PARKIND1, ONLY : JPRB, JPIM
USE YOM_YGFL, ONLY : TYPE_GFLD !! TYPE_GFL_DESC

IMPLICIT NONE
SAVE

!     -------------------------------------------------------------------------

TYPE :: TGFL

!*    GFL gridpoint arrays 
!     For the part of GFL that maps into spectral space see yomsp.F90 (SPGFL etc.)
!     All arrays have the layout(NPROMA,NFLEVG,"number of variables",NGPBLKS)

! GFL        - main GFL array holding t0 and t1 GFL variables
! GFLT1      - GFL array for t+dt quantities
! GFLSLP     - GFL array for use in semi-lagrangian physics
! GFLPT      - physics tendency GFL array
! GFLPC      - predictor/corrector auxiliary arrays (3TL)

! GFL5       - trajectory GFL array
! GFLSLP5    - trajectory for GFLSLP
! GFL_DEPART - used in 3-D FGAT (LIDMODEL)

! YGFL       - pointer to the YGFL structure stored in the model object : new in CY45
!     -------------------------------------------------------------------------

REAL(KIND=JPRB), ALLOCATABLE :: GFL   (:,:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: GFLT1 (:,:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: GFLSLP(:,:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: GFLPT (:,:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: GFLPC (:,:,:,:)

REAL(KIND=JPRB), ALLOCATABLE :: GFL5       (:,:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: GFLSLP5    (:,:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: GFL_DEPART (:,:,:,:)

TYPE(TYPE_GFLD), POINTER :: YGFL => NULL()

END TYPE TGFL

REAL(KIND=JPRB),ALLOCATABLE :: GFL_LHN  (:)
REAL(KIND=JPRB),ALLOCATABLE :: GFL_WKA  (:,:)
REAL(KIND=JPRB),ALLOCATABLE :: GFL_WKA2  (:,:,:)

!     -------------------------------------------------------------------------

CONTAINS

SUBROUTINE ZERO_YOMGFL(SELF)

USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK


TYPE(TGFL), INTENT(INOUT) :: SELF

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE











END SUBROUTINE ZERO_YOMGFL

!-------------------------------------------------------------------------

SUBROUTINE COPY_YOMGFL(SELF,RHS)

USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK


TYPE(TGFL), INTENT(INOUT) :: SELF
TYPE(TGFL), INTENT(IN)    :: RHS

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE




















END SUBROUTINE COPY_YOMGFL

!-------------------------------------------------------------------------

SUBROUTINE MUL_YOMGFL(SELF,PZ)
 
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK


TYPE(TGFL),      INTENT(INOUT) :: SELF
REAL(KIND=JPRB), INTENT(IN)    :: PZ

INTEGER(KIND=JPIM) :: IBL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE






END SUBROUTINE MUL_YOMGFL

!-------------------------------------------------------------------------

SUBROUTINE AXPBY_YOMGFL(SELF,PA,RHS,PB)
 
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK


TYPE(TGFL),      INTENT(INOUT) :: SELF
REAL(KIND=JPRB), INTENT(IN)    :: PA
TYPE(TGFL),      INTENT(IN)    :: RHS
REAL(KIND=JPRB), INTENT(IN)    :: PB

INTEGER(KIND=JPIM) :: IBL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE








END SUBROUTINE AXPBY_YOMGFL


!-------------------------------------------------------------------------

SUBROUTINE DIFF_YOMGFL(SELF,RHS)

USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK


TYPE(TGFL),      INTENT(INOUT) :: SELF
TYPE(TGFL),      INTENT(IN)    :: RHS

INTEGER(KIND=JPIM) :: IBL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE








END SUBROUTINE DIFF_YOMGFL


!-------------------------------------------------------------------------

SUBROUTINE DOT_PROD_YOMGFL(YDGEOMETRY,FLD1,FLD2,PPROD)

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK


TYPE(GEOMETRY),  INTENT(IN)  :: YDGEOMETRY
TYPE(TGFL),      INTENT(IN)  :: FLD1
TYPE(TGFL),      INTENT(IN)  :: FLD2
REAL(KIND=JPRB), INTENT(OUT) :: PPROD

INTEGER(KIND=JPIM) :: JF
REAL(KIND=JPRB)    :: ZTMP
REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE










END SUBROUTINE DOT_PROD_YOMGFL

!-------------------------------------------------------------------------

SUBROUTINE RANDOM_YOMGFL(SELF)
 
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK


TYPE(TGFL), INTENT(INOUT) :: SELF

INTEGER(KIND=JPIM) :: IBL, JF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE








END SUBROUTINE RANDOM_YOMGFL

END MODULE YOMGFL
