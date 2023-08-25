MODULE INTDYNSL_MOD

! Purpose :
! -------
!    To define and compute pointers and logical conditions used when
!    computing local quantities in the dynamics: SL scheme.
!    Allows to use some global structures under CALL_SL
!    (and also their TL and AD).

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

! Author :
! ------
!    K. YESSAD (CNRM/GMAP)
!    Original : January 2011

! Modifications :
! -------------
!  K. YESSAD (Feb 2014): split into INTDYNSL_MOD
!  S. Malardel Nov 2013: pointers for COMAD weights
!  F. Vana  21-Nov-2017: Option LHOISLT
!  F. Vana  20-Feb-2019: Quintic vertical interpolation
!  F. Vana  18-Jul-2019: SLVF
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMDYN   , ONLY : JPSLDIMK

IMPLICIT NONE
SAVE

!=============================================================================

!      1.    TYPE DEFINITIONS
!            ----------------

!      1.11  Types TLSCAW and TRSCAW: pointers for interpolation weights computed in the SL scheme.

! Linear weights:
TYPE TLSCAW
INTEGER(KIND=JPIM) :: M_WDLO        ! distances for horizontal linear interpolations in longitude
INTEGER(KIND=JPIM) :: M_WDLAT       ! distance for horizontal linear interpolations in latitude
INTEGER(KIND=JPIM) :: M_WDVER       ! distance for vertical linear interpolation
INTEGER(KIND=JPIM) :: M_WDLOMAD     ! WDLO for LCOMADH
INTEGER(KIND=JPIM) :: M_WDLAMAD     ! WDLAT for LCOMADH
INTEGER(KIND=JPIM) :: M_WDVERMAD    ! WDVER for LCOMADV
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated

CONTAINS
  
PROCEDURE, PASS :: PRINT => PRINT_TLSCAW_CONFIGURATION 
    
END TYPE TLSCAW

! Other weights:
TYPE TRSCAW
INTEGER(KIND=JPIM) :: M_WCLO(JPSLDIMK)     ! weights for horizontal cubic interpolations in longitude
INTEGER(KIND=JPIM) :: M_WCLA(JPSLDIMK)     ! weights for horizontal cubic interpolations in latitude
INTEGER(KIND=JPIM) :: M_WVINTW             ! vertical cubic interpolation weights
INTEGER(KIND=JPIM) :: M_WCLOSLD(JPSLDIMK)  ! cf. WCLO but for SLHD case
INTEGER(KIND=JPIM) :: M_WCLASLD(JPSLDIMK)  ! cf. WCLA but for SLHD case
INTEGER(KIND=JPIM) :: M_WCLOSLT            ! cf. WCLO 
INTEGER(KIND=JPIM) :: M_WCLASLT            ! cf. WCLA 
INTEGER(KIND=JPIM) :: M_WVINTWSLD          ! cf. WVINTW but for SLHD case
INTEGER(KIND=JPIM) :: M_WVINTWSLT          ! cf. WVINTW 
INTEGER(KIND=JPIM) :: M_WVINTWSLVF         ! cf. WVINTW but for SLVF case 
INTEGER(KIND=JPIM) :: M_WCLOMAD(JPSLDIMK)  ! cf. WCLO but for COMAD case
INTEGER(KIND=JPIM) :: M_WCLAMAD(JPSLDIMK)  ! cf. WCLA but for COMAD case
INTEGER(KIND=JPIM) :: M_WVINTWMAD          ! cf. WVINTW but for COMAD case
INTEGER(KIND=JPIM) :: M_WVINTWS            ! vertical spline interpolation weights
INTEGER(KIND=JPIM) :: M_WVDERW             ! weights to compute vertical derivatives (Hermite cubic vertical interpolation)
INTEGER(KIND=JPIM) :: M_WHVW               ! Hermite vertical cubic interpolation weights
INTEGER(KIND=JPIM) :: M_CW                 ! C_k weights for the vertical WENO interpolation
INTEGER(KIND=JPIM) :: NDIM                 ! total number of fields allocated

CONTAINS
  
PROCEDURE, PASS :: PRINT => PRINT_TRSCAW_CONFIGURATION 

END TYPE TRSCAW

!      1.12  Types TSCO and TCCO: pointers for coordinates computed in the SL scheme.

TYPE TSCO
! spherical geometry:
!   cos(Longitude-Longitude(grid-point))*cos(Latitude) of the interpolation
!   point (geographical longitude and latitude).
! plane projection: x - coordinate (fractional system).
INTEGER(KIND=JPIM) :: M_COSCO
! spherical geometry:
!   sin(Longitude-Longitude(grid-point))*cos(Latitude) of the interpolation
!   point (geographical longitude and latitude).
! plane projection: y - coordinate (fractional system).
INTEGER(KIND=JPIM) :: M_SINCO
! sine of the interpolation point geographical latitude.
INTEGER(KIND=JPIM) :: M_SINLA
! cosine of the geographical angle between the interpolation point
! and the grid-point.
INTEGER(KIND=JPIM) :: M_COPHI
! total number of fields allocated.
INTEGER(KIND=JPIM) :: NDIM

CONTAINS
  
PROCEDURE, PASS :: PRINT => PRINT_TSCO_CONFIGURATION 

END TYPE TSCO

TYPE TCCO
INTEGER(KIND=JPIM) :: M_RLON ! computational sphere longitude of interpolation point
INTEGER(KIND=JPIM) :: M_RLAT ! computational sphere latitude of interpolation point
INTEGER(KIND=JPIM) :: M_RQX  ! first element of the wind displacement matrix (p,q)
INTEGER(KIND=JPIM) :: M_RQY  ! second element of the wind displacement matrix (p,q)
INTEGER(KIND=JPIM) :: NDIM   ! total number of fields allocated

CONTAINS
  
PROCEDURE, PASS :: PRINT => PRINT_TCCO_CONFIGURATION 

END TYPE TCCO

!=============================================================================

!      2.    DECLARATIONS
!            ------------

!      2.11  Types TLSCAW and TRSCAW.

!TYPE(TLSCAW), POINTER :: YYTLSCAW  => NULL()    ! at full levels
!TYPE(TLSCAW), POINTER :: YYTLSCAWH => NULL()    ! at half levels
!TYPE(TRSCAW), POINTER :: YYTRSCAW  => NULL()    ! at full levels
!TYPE(TRSCAW), POINTER :: YYTRSCAWH => NULL()    ! at half levels

!      2.12  Types TSCO and TCCO.

!TYPE(TSCO), POINTER :: YYTSCO => NULL()
!TYPE(TCCO), POINTER :: YYTCCO => NULL()

!=============================================================================

CONTAINS

!      3.    SET-UP

!      3.00  General set-up.

SUBROUTINE SUINTDYNSL(YDDYN,YDDYNA,YGFL,YDTLSCAW,YDTLSCAWH,YDTRSCAW,YDTRSCAWH,YDTSCO,YDTCCO)








USE YOMDYN   , ONLY : TDYN
USE YOMDYNA  , ONLY : TDYNA
USE YOM_YGFL , ONLY : TYPE_GFLD

TYPE(TDYN)      , INTENT(INOUT) :: YDDYN
TYPE(TDYNA)     , INTENT(INOUT) :: YDDYNA
TYPE(TYPE_GFLD) , INTENT(INOUT) :: YGFL
TYPE(TLSCAW)    , INTENT(INOUT) :: YDTLSCAW   
TYPE(TLSCAW)    , INTENT(INOUT) :: YDTLSCAWH  
TYPE(TRSCAW)    , INTENT(INOUT) :: YDTRSCAW   
TYPE(TRSCAW)    , INTENT(INOUT) :: YDTRSCAWH  
TYPE(TSCO)      , INTENT(INOUT) :: YDTSCO 
TYPE(TCCO)      , INTENT(INOUT) :: YDTCCO 

LOGICAL :: LLHVI,LLVINTWS
INTEGER(KIND=JPIM) :: JGFL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE





END SUBROUTINE SUINTDYNSL


!      3.11  Set-up for types TLSCAW and TRSCAW.

SUBROUTINE SUPTR_TLSCAW(YD)







TYPE(TLSCAW),    INTENT(OUT)   :: YD



INTEGER(KIND=JPIM) :: INCR
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE























END SUBROUTINE SUPTR_TLSCAW

SUBROUTINE SUPTR_TRSCAW(YDDYN,YDDYNA,KSLDIMK,LDVINTWS,LDHVI,YD)










USE YOMDYN , ONLY : TDYN
USE YOMDYNA, ONLY : TDYNA
TYPE(TDYN)        , INTENT(INOUT) :: YDDYN
TYPE(TDYNA)       , INTENT(INOUT) :: YDDYNA
INTEGER(KIND=JPIM), INTENT(IN)    :: KSLDIMK
LOGICAL,            INTENT(IN)    :: LDVINTWS
LOGICAL,            INTENT(IN)    :: LDHVI
TYPE(TRSCAW),       INTENT(OUT)   :: YD



INTEGER(KIND=JPIM) :: INCR,JS
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE































END SUBROUTINE SUPTR_TRSCAW

!      3.12  Set-up for types TSCO and TCCO.

SUBROUTINE SUPTR_TSCO(YD)







TYPE(TSCO),  INTENT(OUT)   :: YD



REAL(KIND=JPHOOK) :: ZHOOK_HANDLE













END SUBROUTINE SUPTR_TSCO

SUBROUTINE SUPTR_TCCO(YD)







TYPE(TCCO), INTENT(OUT)   :: YD



REAL(KIND=JPHOOK) :: ZHOOK_HANDLE













END SUBROUTINE SUPTR_TCCO

!=============================================================================
SUBROUTINE PRINT_TLSCAW_CONFIGURATION(SELF, KDEPTH, KOUTNO, CNAME)
  
  CLASS(TLSCAW)   , INTENT(IN) :: SELF
  INTEGER(KIND=JPIM)         , INTENT(IN) :: KDEPTH
  INTEGER(KIND=JPIM)         , INTENT(IN) :: KOUTNO
  CHARACTER(LEN=*), INTENT(IN) :: CNAME

  INTEGER(KIND=JPIM) :: IDEPTHLOC
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  

  

  
      
     
     
   
   
  
        

  

END SUBROUTINE PRINT_TLSCAW_CONFIGURATION
!=============================================================================
SUBROUTINE PRINT_TRSCAW_CONFIGURATION(SELF, KDEPTH, KOUTNO, CNAME)
  
  CLASS(TRSCAW)   , INTENT(IN) :: SELF
  INTEGER(KIND=JPIM)         , INTENT(IN) :: KDEPTH
  INTEGER(KIND=JPIM)         , INTENT(IN) :: KOUTNO
  CHARACTER(LEN=*), INTENT(IN) :: CNAME

  INTEGER(KIND=JPIM) :: IDEPTHLOC
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


  
  

  
  
  
    
    
              
             
  
     
     
  
  
  
     
       
  
  
  


END SUBROUTINE PRINT_TRSCAW_CONFIGURATION
!=============================================================================
SUBROUTINE PRINT_TSCO_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  
  CLASS(TSCO) , INTENT(IN) :: SELF
  INTEGER(KIND=JPIM)     , INTENT(IN) :: KDEPTH
  INTEGER(KIND=JPIM)     , INTENT(IN) :: KOUTNO

  INTEGER(KIND=JPIM) :: IDEPTHLOC
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


  
  
  
  
     
     
  
        


END SUBROUTINE PRINT_TSCO_CONFIGURATION
!=============================================================================
SUBROUTINE PRINT_TCCO_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  
  CLASS(TCCO) , INTENT(IN) :: SELF
  INTEGER(KIND=JPIM)     , INTENT(IN) :: KDEPTH
  INTEGER(KIND=JPIM)     , INTENT(IN) :: KOUTNO

  INTEGER(KIND=JPIM) :: IDEPTHLOC
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


  
  
  
  
  
  
  
  


END SUBROUTINE PRINT_TCCO_CONFIGURATION
!=============================================================================

END MODULE INTDYNSL_MOD
