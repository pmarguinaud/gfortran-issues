MODULE TYPE_FPDSPHYS

! Purpose :
! -------
!    To define the user type "FPDSPHY" for the post-processing control :
!     - %CLNAME : the ARPEGE field name,
!     - %LLSRF  : the kind of field : surface (.T.) or upper air (.F.)
!     - %IGRIB  : the GRIB code,
!     - %IBITS  : the number of bits for packing before writing out to file,
!     - %INTER  : the kind of horizontal interpolation
!                 (quadratic, bilinear, nearest point)
!     - %IORDR  : the horizontal derivative order of the field 
!                  0=scalar,
!                  1=vector/U component,
!                 -1=vector/V component,
!                  2=horizontal derivative on vector quantity
!     - %CLPAIR : keyworkd for a pair of vector components or for a pair of
!                 any fields which cannot be computed independantly
!     - %IMASK   : mask used for horizontal :
!                  0 = no mask
!                  1 = land-sea mask
!                  2 = sea mask
!     - %LLMON   : whether its horizontal interpolation should be monotonic (T)
!                  or not (F)
!     - %IANO    : Indicator for the kind of field to interpolate
!                 0 = rough interpolation of the field
!                 1 = interpolation of an anomaly with respect to a reference, 
!                     whenever possible
!     - %ICOD   : an INTERNAL code number to localize a specific field among
!                 all fields
!     - %IPREP : rank of PREP field 
         
! Interface :
! ---------
!    Empty.

! External :
! --------
!    None.

! Method :
! ------
!    See Documentation.

! Reference :
! ---------
!    Fullpos technical & users guide.

! Author :
! ------
!    Ryad El Khatib *METEO-FRANCE* thanks to Mike Fisher *ECMWF*

! Modifications :
! -------------
! Original : 2003-02-25
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!       R. El Khatib   09-May-2003 LLLSM replaced by IMASK
!-----------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE
SAVE

PRIVATE
PUBLIC FPDSPHY, GFP_SU, CTRL_GFP

TYPE FPDSPHY 
INTEGER(KIND=JPIM) :: IGRIB = -9999
INTEGER(KIND=JPIM) :: IBITS =99 
INTEGER(KIND=JPIM) :: INTER =-9 
INTEGER(KIND=JPIM) :: IORDR = 0
INTEGER(KIND=JPIM) :: IANO = 0
INTEGER(KIND=JPIM) :: ICOD = 0
INTEGER(KIND=JPIM) :: IMASK = 1
INTEGER(KIND=JPIM) :: IPREP = 0
LOGICAL   :: LLMON = .FALSE.
LOGICAL   :: LLSRF = .TRUE.
CHARACTER(LEN=16) :: CLNAME = ' '
CHARACTER(LEN=8)  :: CLPAIR = ' '
END TYPE FPDSPHY

CONTAINS

!-----------------------------------------------------------------------------

TYPE(FPDSPHY) &
 & FUNCTION GFP_SU(KGRIB,KBITS,KNTER,KORDR,KANO,KMASK,LDMON,LDSRF,CDNAME, &
 & CDPAIR,KPREP)  





INTEGER(KIND=JPIM), INTENT(IN) :: KGRIB
INTEGER(KIND=JPIM), INTENT(IN) :: KBITS
INTEGER(KIND=JPIM), INTENT(IN) :: KNTER
INTEGER(KIND=JPIM), INTENT(IN) :: KORDR
INTEGER(KIND=JPIM), INTENT(IN) :: KANO
INTEGER(KIND=JPIM), INTENT(IN) :: KMASK
LOGICAL,   INTENT(IN) :: LDMON
LOGICAL,   INTENT(IN) :: LDSRF
CHARACTER(LEN=*), INTENT(IN) :: CDNAME
CHARACTER(LEN=*), INTENT(IN) :: CDPAIR
INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: KPREP
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
















END FUNCTION GFP_SU

!-----------------------------------------------------------------------------

SUBROUTINE CTRL_GFP(KCOD,KMAX,YD_YGFP_X,YD_YGFP_PHYDS)

INTEGER(KIND=JPIM),     INTENT(IN)    :: KMAX
INTEGER(KIND=JPIM),     INTENT(INOUT) :: KCOD
TYPE(FPDSPHY), INTENT(INOUT) :: YD_YGFP_X
TYPE(FPDSPHY), INTENT(INOUT) :: YD_YGFP_PHYDS(KMAX)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE







END SUBROUTINE CTRL_GFP

END MODULE TYPE_FPDSPHYS
