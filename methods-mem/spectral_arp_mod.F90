MODULE SPECTRAL_ARP_MOD

!   Author.
!   -------
!     Martin Leutbecher
!
!   Modifications.
!   -------------- 
!     Original      02-Jan-2009
!     M.Leutbecher  09-Jan-2009 parallelisation of rs2d/rs3d by zonal wavenumber
!     F.Bouttier    31-Jul-2012 added LAM support
!     F.Vana        17-Dec-2015 support for single precision
! ------------------------------------------------------------------

USE GEOMETRY_MOD        , ONLY : GEOMETRY
USE PARKIND1            , ONLY : JPIM, JPRB, JPRD
USE YOMHOOK             , ONLY : LHOOK, DR_HOOK, JPHOOK
USE SPECTRAL_FIELDS_MOD
USE SPECTRAL_FIELDS_DATA, ONLY : LELAM
USE RANDOM_NUMBERS_MIX

IMPLICIT NONE
PRIVATE
PUBLIC SPECTRAL_ARP, ALLOCATE_ARP, DEALLOCATE_ARP, SET_ARP, SET_ARP2D, EVOLVE_ARP, SUM_ARPS, SET_SEED_ARP

TYPE SPECTRAL_ARP
  !
  !   spectral field sf containing the current state of the auto-regressive process
  !
  TYPE(SPECTRAL_FIELD) :: SF
  !
  !   spectral field sf containing the current state of sum of the 
  !   auto-regressive processes and switch if this should be activated
  !
  LOGICAL              :: LSUM
  TYPE(SPECTRAL_FIELD) :: SFSUM
  !
  !   master seed that generates all other seeds
  !
  INTEGER(KIND=JPIM)   :: ISEED0
  !
  !   initial master seed that generates all other seeds (set by allocate_arp only)
  !
  INTEGER(KIND=JPIM)   :: ISEED0T0
  !
  !   seeds (global variable) for local randomNumberStreams
  !     (depending on [level] spherical harmonics index and field
  !
  INTEGER(KIND=JPIM),       DIMENSION(:,:),   POINTER :: ISEED2D=>NULL()
  INTEGER(KIND=JPIM),       DIMENSION(:,:,:), POINTER :: ISEED3D=>NULL() 
  !
  !   (local) randomNumberStreams for the noise used in the AR(1) processes
  !
  TYPE(RANDOMNUMBERSTREAM), DIMENSION(:,:),   POINTER :: RS2D=>NULL() 
  TYPE(RANDOMNUMBERSTREAM), DIMENSION(:,:,:), POINTER :: RS3D=>NULL() 
  !
  !   standard deviation as function of [level,] total wavenumber and field
  !
  REAL(KIND=JPRB),          DIMENSION(:,:),   POINTER :: SDEV2D=>NULL() 
  REAL(KIND=JPRB),          DIMENSION(:,:,:), POINTER :: SDEV3D=>NULL() 
  !   
  !   switch for clipping and
  !   amplitude for clipping the noise (aclip) and for clipping the AR1 (aclipar) as function of [level,] total wavenumber and field
  !
  LOGICAL :: LCLIP
  REAL(KIND=JPRB),          DIMENSION(:,:),   POINTER :: ACLIP2D=>NULL() 
  REAL(KIND=JPRB),          DIMENSION(:,:,:), POINTER :: ACLIP3D=>NULL() 
  REAL(KIND=JPRB),          DIMENSION(:,:),   POINTER :: ACLIPAR2D=>NULL() 
  REAL(KIND=JPRB),          DIMENSION(:,:,:), POINTER :: ACLIPAR3D=>NULL() 
  !
  !   correlation between successive steps as function of [level,] total wavenumber and field
  !
  REAL(KIND=JPRB),          DIMENSION(:,:),   POINTER :: PHI2D=>NULL() 
  REAL(KIND=JPRB),          DIMENSION(:,:,:), POINTER :: PHI3D=>NULL() 
END TYPE SPECTRAL_ARP


! ------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------

SUBROUTINE ALLOCATE_ARP(YDGEOMETRY,YDARP,KS3D,KS2D,KS1D,KGRIB,KSEED,LDCLIP,LDSUM)

TYPE (GEOMETRY), INTENT(IN) :: YDGEOMETRY
TYPE (SPECTRAL_ARP), INTENT(OUT) :: YDARP
INTEGER(KIND=JPIM), INTENT(IN) :: KS3D, KS2D, KS1D, KGRIB(KS3D+KS2D), KSEED
LOGICAL, OPTIONAL, INTENT(IN) :: LDCLIP, LDSUM

INTEGER(KIND=JPIM) :: INFLEVG,INFLEVL,ISPEC2,ISPEC2G
INTEGER(KIND=JPIM) :: JS, JFLD, JL, IL
INTEGER(KIND=JPIM) :: JM, IM, IJSG, IJS, JNX, ILENX
INTEGER(KIND=JPIM) :: JJJ
LOGICAL            :: LLCLIP, LLSUM

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE



END SUBROUTINE ALLOCATE_ARP
!
!
!
SUBROUTINE SET_SEED_ARP(YDARP,KSEED)

TYPE (SPECTRAL_ARP), INTENT(INOUT) :: YDARP
INTEGER(KIND=JPIM), INTENT(IN) ::  KSEED

INTEGER(KIND=JPIM) :: INFLEVG,INFLEVL,ISPEC2,ISPEC2G
INTEGER(KIND=JPIM) :: JS, JFLD, JL, IL, ISMAX,IS2D,IS3D
INTEGER(KIND=JPIM) :: JM, IM, IJSG, IJS, JNX, ILENX
INTEGER(KIND=JPIM) :: JJJ
LOGICAL            :: LLCLIP

TYPE(RANDOMNUMBERSTREAM) :: YL_SEEDS
REAL(KIND=JPRB), DIMENSION(:), ALLOCATABLE :: ZSEED
REAL(KIND=JPRD), PARAMETER :: ZHUGE = REAL(HUGE(1_JPIM)-1,JPRD)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

































END SUBROUTINE SET_SEED_ARP
  
!
!
!
SUBROUTINE DEALLOCATE_ARP(YDARP)



TYPE (SPECTRAL_ARP), INTENT(INOUT) :: YDARP

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE























END SUBROUTINE DEALLOCATE_ARP
!
!
!
SUBROUTINE SET_ARP2D(YDARP,SDEV2D,PHI2D,ACLIP2D)




TYPE (SPECTRAL_ARP), INTENT(INOUT) :: YDARP
REAL(KIND=JPRB), DIMENSION(:,:),   INTENT(IN) :: SDEV2D, PHI2D
REAL(KIND=JPRB), DIMENSION(:,:),   OPTIONAL, INTENT(IN) :: ACLIP2D

INTEGER(KIND=JPIM) :: JN , JS
REAL(KIND=JPRB)    :: ZPHI_FAC0, ZPHI, ZEPSILON

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE












END SUBROUTINE SET_ARP2D
!
!
!
SUBROUTINE SET_ARP(YDARP,SDEV2D,SDEV3D,PHI2D,PHI3D,ACLIP2D,ACLIP3D)




TYPE (SPECTRAL_ARP), INTENT(INOUT) :: YDARP
REAL(KIND=JPRB), DIMENSION(:,:),   INTENT(IN) :: SDEV2D, PHI2D
REAL(KIND=JPRB), DIMENSION(:,:,:), INTENT(IN) :: SDEV3D, PHI3D
REAL(KIND=JPRB), DIMENSION(:,:),    OPTIONAL,INTENT(IN) :: ACLIP2D
REAL(KIND=JPRB), DIMENSION(:,:,:),  OPTIONAL,INTENT(IN) :: ACLIP3D

INTEGER(KIND=JPIM) :: JN , JS, JLEV
REAL(KIND=JPRB)    :: ZPHI_FAC0, ZPHI

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


 
  










END SUBROUTINE SET_ARP
!
!
!
SUBROUTINE EVOLVE_ARP(YDARP,KARP,LDINIT)



TYPE (SPECTRAL_ARP),          INTENT(INOUT) :: YDARP
INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: KARP
LOGICAL, OPTIONAL,            INTENT(IN)    :: LDINIT



LOGICAL :: LLINIT
INTEGER(KIND=JPIM) :: ISTEPS,JSTEP
INTEGER(KIND=JPIM) :: JMLOC, IM,JIR,JN, INM, ILEN1, ILEN2, IDXNAR, IRMAX, INMAX, INN
INTEGER(KIND=JPIM) :: JLEV,JFLD,ILEV
REAL(KIND=JPRB)    :: ZNOISE, ZPHI, ZPHI_FAC0, ZLIMIT, ZEPSILON
REAL(KIND=JPRB), DIMENSION(:,:),   ALLOCATABLE :: ZNAR2D
REAL(KIND=JPRB), DIMENSION(:,:,:), ALLOCATABLE :: ZNAR3D




REAL(KIND=JPHOOK) :: ZHOOK_HANDLE








 












END SUBROUTINE EVOLVE_ARP
!
!
!
SUBROUTINE SUM_ARPS(YDARP)



TYPE (SPECTRAL_ARP),          INTENT(INOUT) :: YDARP



INTEGER(KIND=JPIM) :: JMLOC, IM,JIR,JN, INM
INTEGER(KIND=JPIM) :: JS,JL,JF,ISPEC2,IFLEVL

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE




















END SUBROUTINE SUM_ARPS

! ------------------------------------------------------------------

END MODULE SPECTRAL_ARP_MOD
