MODULE YOERRTFTR

USE PARKIND1  ,ONLY : JPIM     ,JPRB

!USE PARRRTM, ONLY : JPBAND, JPG, JPGMAX
!USE YOERRTM, ONLY : JPGPT

IMPLICIT NONE

SAVE

!    -------------------------------------------------------------------

!    -------------------------------------------------------------------

INTEGER(KIND=JPIM) :: NGC(16)
INTEGER(KIND=JPIM) :: NGS(16)

INTEGER(KIND=JPIM) :: NGN(256)
INTEGER(KIND=JPIM) :: NGB(256)

INTEGER(KIND=JPIM) :: NGM(256)
REAL(KIND=JPRB) ::    WT(16)

!INTEGER(KIND=JPIM) :: NGC(JPBAND)
!INTEGER(KIND=JPIM) :: NGS(JPBAND)
!
!INTEGER(KIND=JPIM) :: NGN(JPGMAX)
!INTEGER(KIND=JPIM) :: NGB(JPGMAX)
!
!INTEGER(KIND=JPIM) :: NGM(JPG*JPBAND)
!REAL(KIND=JPRB) ::    WT(JPG)

!     -----------------------------------------------------------------
!        * E.C.M.W.F. PHYSICS PACKAGE ** RRTM LW RADIATION **

!     J.-J. MORCRETTE       E.C.M.W.F.      98/07/14

!  NAME     TYPE     PURPOSE
!  ----  :  ----   : ---------------------------------------------------
!  NGC   : INTEGER :
!  NGS   : INTEGER :
!  NGN   : INTEGER :
!  NGB   : INTEGER :
!  NGM   : INTEGER :
!  WT    : REAL    :
!    -------------------------------------------------------------------
END MODULE YOERRTFTR

