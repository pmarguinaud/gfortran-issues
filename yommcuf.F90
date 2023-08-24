MODULE YOMMCUF

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!     MAXIMA FOR THE FILTERS FOR THE MONITORING OF THE COUPLING UPDATES

!     JPMFNR : MAXIMUM NUMBER OF RECURSIVE FILTERS
!     JPMFOR : MAXIMUM ORDER OF RECURSIVE FILTERS

!     ------------------------------------------------------------------
INTEGER(KIND=JPIM), PARAMETER :: JPMFNR=5
INTEGER(KIND=JPIM), PARAMETER :: JPMFOR=8

!     ------------------------------------------------------------------

!     Specifications of the digital filter for monitoring 
!     coupling-update frequency

!     NCUFNR  : number of filters applied         (NCUFNR<=JPMFNR)
!     NCUFOR  : the order of the recursive filter (NCUFOR<=JPMFOR)
!     LMCUF   : LOGICAL to switch on the monitoring
!     LREACUF : LOGICAL to read Coupling Update Frequency fields. 
!     RMCUFI  : the coupling-update time interval(s) under interest
!     RMCUFSP : to store the previous surface pressures
!     RMCUFFP : to store the filtered surface pressures in Spectral Space.
!              - first index for the different coefficients
!              - second index: different filters
!     RMCUFA  : A coefficients of the recursive filter
!     RMCUFB  : B coefficients of the recursive filter
!     SPFSP   : the resulting filtered field

!     ------------------------------------------------------------------

TYPE :: TMCUF

REAL(KIND=JPRB), ALLOCATABLE :: RMCUFSP(:,:)
REAL(KIND=JPRB), ALLOCATABLE :: RMCUFFP(:,:,:)
REAL(KIND=JPRB)              :: RMCUFA(0:JPMFOR,1:JPMFNR)  , &
                              & RMCUFB(1:JPMFOR,1:JPMFNR)  , &
                              & RMCUFI(1:JPMFNR)
INTEGER(KIND=JPIM)           :: NCUFOR   ,NCUFNR
LOGICAL                      :: LMCUF    ,LREACUF


END TYPE TMCUF

CONTAINS

SUBROUTINE ZERO_MCUF(YDMCUF)

USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK



TYPE(TMCUF), INTENT(INOUT) :: YDMCUF

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE







END SUBROUTINE ZERO_MCUF

SUBROUTINE COPY_MCUF(YDMCUFOUT,YDMCUFIN)

USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK



TYPE(TMCUF), INTENT(INOUT) :: YDMCUFOUT
TYPE(TMCUF), INTENT(IN)    :: YDMCUFIN

INTEGER(KIND=JPIM) :: JBL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END SUBROUTINE COPY_MCUF
!     ------------------------------------------------------------------

END MODULE YOMMCUF
