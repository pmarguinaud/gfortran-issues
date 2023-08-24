MODULE INTDYN_MOD

! Purpose :
! -------
!    To define and compute pointers and logical conditions used when
!    computing local quantities in the dynamics.
!    Allows to use some global structures, for example under CPG
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
!  K. YESSAD (Feb 2014): some structures have been moved in INTDYNSL_MOD.
!  F. Vana 13-Feb-2014  SLHD weights for heat variables
!  K. Yessad (June 2017): Introduce NHQE model.
!  K. Yessad (Feb 2018): remove deep-layer formulations.
!  F. Vana 21-Sep-2020: LPGREUSE - re-use of pressure gradient term quantities
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK

USE GEOMETRY_MOD      , ONLY : GEOMETRY
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE


IMPLICIT NONE
SAVE

!=============================================================================

!      1.    TYPE DEFINITIONS
!            ----------------

!      1.01  Type TYXB: pointers for output of GPXYB.

! ky: M_RPREF for "1/(full level pressure)" could be later added to this structure
TYPE TXYB
INTEGER(KIND=JPIM) :: M_DELP        ! pressure depths at full levels
INTEGER(KIND=JPIM) :: M_RDELP       ! 1/(pressure depths) at full levels
INTEGER(KIND=JPIM) :: M_LNPR        ! "delta = depth of log(pressure)" at full levels
INTEGER(KIND=JPIM) :: M_ALPH        ! "alpha" at full levels
INTEGER(KIND=JPIM) :: M_RTGR        ! ratio "rtgr": grad(pressure)/pressure = "rtgr" grad(pressure_surf)
INTEGER(KIND=JPIM) :: M_RPRE        ! 1/(half level pressure)
INTEGER(KIND=JPIM) :: M_RPP         ! 1/(pressure(lbar)*pressure(lbar-1)), where lbar=half level
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TXYB

!      1.02  Type TXYBDER: pointers for output of GPGRXYB.

TYPE TXYBDER
INTEGER(KIND=JPIM) :: M_LNPRL       ! zonal derivative of grad(delta) at full levels
INTEGER(KIND=JPIM) :: M_LNPRM       ! meridian derivative of grad(delta) at full levels
INTEGER(KIND=JPIM) :: M_ALPHL       ! zonal derivative of grad(alpha) at full levels
INTEGER(KIND=JPIM) :: M_ALPHM       ! meridian derivative of grad(alpha) at full levels
INTEGER(KIND=JPIM) :: M_ALPHPLL     ! zonal derivative of grad(alpha + log prehyd) at full levels
INTEGER(KIND=JPIM) :: M_ALPHPLM     ! meridian derivative of grad(alpha + log prehyd) at full levels
INTEGER(KIND=JPIM) :: M_COEFD       ! coefficient to compute grad(delta)
INTEGER(KIND=JPIM) :: M_COEFA       ! coefficient to compute grad(alpha)
INTEGER(KIND=JPIM) :: M_COEFAPL     ! coefficient to compute grad(alpha + log prehyd)
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TXYBDER

!      1.03  Type TRCP: pointers for output of GPRCP.

TYPE TRCP
INTEGER(KIND=JPIM) :: M_CP          ! "Cp" at full levels
INTEGER(KIND=JPIM) :: M_R           ! "R" at full levels
INTEGER(KIND=JPIM) :: M_KAP         ! "Kap = R/Cp" at full levels
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TRCP

!      1.05  Type TNHPRE: pointers for output of GNHPRE, GNHPREH, GNHGRPRE.

! ??? expected to be coded later
!     attributes: at least NHPREF, NHPREH, RNHPPI, QCHAL, QCHAM

!      1.06  Type TWSDLR: pointers for output of GNHDLR and GNHGRDLR.

! ??? expected to be coded later, to replace current par_dlr

!      1.07  Type TGEO: pointers for output of GPGEO and GPGRGEO.

! ??? expected to be coded later
!     attributes: at least PHIH, PHIF, PHIHL, PHIHM, PHIFL, PHIFM

!      1.08  Type TCTY: pointers for output of GPCTY.

TYPE TCTY
INTEGER(KIND=JPIM) :: M_EVEL        ! etadot (d prehyd / d eta)
INTEGER(KIND=JPIM) :: M_VVEL        ! omega / prehyd
INTEGER(KIND=JPIM) :: M_PSDIV       ! vertical integral of divergence without the "lrubc" contrib
INTEGER(KIND=JPIM) :: M_PSDVBC      ! vertical integral of divergence with the "lrubc" contrib
INTEGER(KIND=JPIM) :: M_DIVDP       ! grad(vec(V) * (Delta prehyd))
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TCTY

!      1.09  Type THWIND: pointers for half-level horizontal wind.

TYPE THWIND
INTEGER(KIND=JPIM) :: M_UH          ! U-wind at half levels
INTEGER(KIND=JPIM) :: M_VH          ! V-wind at half levels
INTEGER(KIND=JPIM) :: M_WWI         ! weights to compute half levels winds
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE THWIND

!      1.10  Types TTND: pointer for Lagrangian adiabatic tendencies.

TYPE TTND
INTEGER(KIND=JPIM) :: M_TNDU        ! tendency for U-wind equation
INTEGER(KIND=JPIM) :: M_TNDV        ! tendency for V-wind equation
INTEGER(KIND=JPIM) :: M_TNDU_NOC    ! tendency for U-wind equation without Coriolis term
INTEGER(KIND=JPIM) :: M_TNDV_NOC    ! tendency for V-wind equation without Coriolis term
INTEGER(KIND=JPIM) :: M_TNDT        ! tendency for temperature
INTEGER(KIND=JPIM) :: M_TNDPD       ! tendency for pressure departure variable
INTEGER(KIND=JPIM) :: M_TNDVD       ! tendency for vertical divergence variable
INTEGER(KIND=JPIM) :: M_TNDGW       ! tendency for Gw
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TTND

!      1.13  Type TGMVT: pointers for GMV trajectory under CPG5_GP.

TYPE TGMVT
INTEGER(KIND=JPIM) :: M_U      ! U-component of horizontal wind
INTEGER(KIND=JPIM) :: M_V      ! V-component of horizontal wind
INTEGER(KIND=JPIM) :: M_T      ! temperature
INTEGER(KIND=JPIM) :: M_DIV    ! horizontal divergence
INTEGER(KIND=JPIM) :: M_SPD    ! pressure departure variable
INTEGER(KIND=JPIM) :: M_SVD    ! vertical divergence variable
INTEGER(KIND=JPIM) :: NDIM     ! total number of fields allocated
END TYPE TGMVT

!      1.14  Type TGFLT: pointers for GFL trajectory under CPG5_GP.

TYPE TGFLT
INTEGER(KIND=JPIM) :: M_Q      ! specific humidity
INTEGER(KIND=JPIM) :: M_L      ! liquid water
INTEGER(KIND=JPIM) :: M_I      ! ice
INTEGER(KIND=JPIM) :: NDIM     ! total number of fields allocated
END TYPE TGFLT

!      1.15  other possible structures to be introduced later:
!            * TRT (RT,RTL,RTM)

!      1.16  TPGTERM used by pressure gradient term
TYPE TPG_TYPE
! Full level optional quantities
REAL(KIND=JPRB), POINTER, CONTIGUOUS :: PHI0F(:,:)=>NULL(), PHI0FL(:,:)=>NULL(),PHI0FM(:,:)=>NULL(),&
 & RT0(:,:)=>NULL(),RT0L(:,:)=>NULL(),RT0M(:,:)=>NULL()
CLASS (FIELD_3RB), POINTER :: F_PHI0F=>NULL(), F_PHI0FL=>NULL(), F_PHI0FM=>NULL(), F_RT0=>NULL(), &
 & F_RT0L=>NULL(), F_RT0M=>NULL()
! Half level optional quantities
REAL(KIND=JPRB), POINTER :: PHI0H(:,:)=>NULL(), PHI0HL(:,:)=>NULL(), PHI0HM(:,:)=>NULL()
CLASS (FIELD_3RB), POINTER :: F_PHI0H=>NULL(), F_PHI0HL=>NULL(), F_PHI0HM=>NULL()
! Full level output quantities
REAL(KIND=JPRB), POINTER :: PSGRTL(:,:)=>NULL(), PSGRTM(:,:)=>NULL()
CLASS (FIELD_3RB), POINTER :: F_PSGRTL=>NULL(), F_PSGRTM=>NULL()

CONTAINS
  PROCEDURE :: INIT => TPG_TYPE_INIT
  PROCEDURE :: UPDATE_VIEW => TPG_TYPE_UPDATE_VIEW
  PROCEDURE :: FINAL => TPG_TYPE_FINAL
END TYPE TPG_TYPE

!=============================================================================

!      2.    DECLARATIONS
!            ------------

!      2.01  Type TYXB.

TYPE(TXYB) :: YYTXYB0         ! at t
TYPE(TXYB) :: YYTXYB5         ! at t (trajectory)
TYPE(TXYB) :: YYTXYB9         ! at t-dt
TYPE(TXYB) :: YYTXYB95        ! at t-dt (trajectory)
TYPE(TXYB) :: YYTXYB0_PHY     ! output of MF_PHYS_PREP at t
TYPE(TXYB) :: YYTXYB9_PHY     ! output of MF_PHYS_PREP at t-dt
TYPE(TXYB) :: YYTXYBPP        ! for POS
! ky: YYTXYB: may be introduced later in GPXYB, GPGRXYB, GPCTY, GPGRP, GNHDLRB too
TYPE(TXYB) :: YYTXYB          ! for GP.. routines
TYPE(TXYB) :: YYTXYBT         ! for GP.. routines (trajectory)

!      2.02  Type TXYBDER.

TYPE(TXYBDER) :: YYTXYBDER0   ! at t
TYPE(TXYBDER) :: YYTXYBDER5   ! at t (trajectory)
TYPE(TXYBDER) :: YYTXYBDERPP  ! for POS
TYPE(TXYBDER) :: YYTXYBDER    ! for GP.. routines
TYPE(TXYBDER) :: YYTXYBDERT   ! for GP.. routines (trajectory)

!      2.03  Type TRCP:

TYPE(TRCP) :: YYTRCP0   ! at t
TYPE(TRCP) :: YYTRCP5   ! at t (trajectory)
TYPE(TRCP) :: YYTRCP9   ! at t-dt
TYPE(TRCP) :: YYTRCP95  ! at t-dt (trajectory)

!      2.05  Type TNHPRE.

! ??? expected to be coded later

!      2.06  Type TWSDLR.

! ??? expected to be coded later

!      2.07  Type TGEO.

! ??? expected to be coded later

!      2.08  Type TCTY.

TYPE(TCTY) :: YYTCTY0        ! at t
TYPE(TCTY) :: YYTCTY5        ! at t (trajectory)
TYPE(TCTY) :: YYTCTYPP       ! for POS
TYPE(TCTY) :: YYTCTY         ! for GP.. routines

!      2.09  Type THWIND.

TYPE(THWIND) :: YYTHW0       ! at t
TYPE(THWIND) :: YYTHW9       ! at t-dt
TYPE(THWIND) :: YYTHW5       ! at t (trajectory)
TYPE(THWIND) :: YYTHW95      ! at t-dt (trajectory)
TYPE(THWIND) :: YYTHWPP      ! for POS
TYPE(THWIND) :: YYTHW        ! for GP.. routines

!      2.10  Type TTND.

!TYPE(TTND) :: YYTTND Moved to YOMDYNA

!      2.13  Type TGMVT.

!TYPE(TGMVT) :: YYTGMVT95 ! Moved to YOMDYNA

!      2.14  Type TGFLT.

!TYPE(TGFLT) :: YYTGFLT95

!=============================================================================

CONTAINS

!      3.    SET-UP

SUBROUTINE SUINTDYN








REAL(KIND=JPHOOK) :: ZHOOK_HANDLE































































END SUBROUTINE SUINTDYN

!      3.01  Set-up for type TYXB.

SUBROUTINE SUPTR_TXYB(LD,YD)








LOGICAL,                 INTENT(IN)    :: LD
TYPE(TXYB),              INTENT(OUT)   :: YD



REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END SUBROUTINE SUPTR_TXYB

!      3.02  Set-up for type TXYBDER.

SUBROUTINE SUPTR_TXYBDER(LD,YD)








LOGICAL,                 INTENT(IN)    :: LD
TYPE(TXYBDER),           INTENT(OUT)   :: YD 



REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END SUBROUTINE SUPTR_TXYBDER

!      3.03  Set-up for type TRCP.

SUBROUTINE SUPTR_TRCP(YD)







TYPE(TRCP),              INTENT(OUT)   :: YD 



REAL(KIND=JPHOOK) :: ZHOOK_HANDLE












END SUBROUTINE SUPTR_TRCP

!      3.05  Set-up for type TNHPRE.
! ??? expected to be coded later

!      3.06  Set-up for type TWSDLR.

! ??? expected to be coded later

!      3.07  Set-up for type TGEO.
! ??? expected to be coded later

!      3.08  Set-up for type TCTY.

SUBROUTINE SUPTR_TCTY(YD)







TYPE(TCTY),              INTENT(OUT)   :: YD 



REAL(KIND=JPHOOK) :: ZHOOK_HANDLE














END SUBROUTINE SUPTR_TCTY

!      3.09  Set-up for type THWIND.

SUBROUTINE SUPTR_THWIND(YD)







TYPE(THWIND),              INTENT(OUT)   :: YD



REAL(KIND=JPHOOK) :: ZHOOK_HANDLE












END SUBROUTINE SUPTR_THWIND

!      3.10  Set-up for type TTND.

SUBROUTINE SUPTR_TTND(YD,LDNHEE,LDNHQE)







TYPE(TTND),              INTENT(OUT)   :: YD
LOGICAL,                 INTENT(IN)    :: LDNHEE
LOGICAL,                 INTENT(IN)    :: LDNHQE


REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END SUBROUTINE SUPTR_TTND

!      3.13  Set-up for type TGMVT.

SUBROUTINE SUPTR_TGMVT(YD,LDNHDYN)







TYPE(TGMVT),              INTENT(OUT)   :: YD
LOGICAL,                  INTENT(IN)    :: LDNHDYN


REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END SUBROUTINE SUPTR_TGMVT

!      3.14  Set-up for type TGFLT.

SUBROUTINE SUPTR_TGFLT(YD)







TYPE(TGFLT),              INTENT(OUT)   :: YD



REAL(KIND=JPHOOK) :: ZHOOK_HANDLE












END SUBROUTINE SUPTR_TGFLT

!=============================================================================

SUBROUTINE TPG_TYPE_INIT(SELF, YDGEOMETRY, PGEO0, LPGFSF, LPGREUSE)
  CLASS(TPG_TYPE)      , INTENT(INOUT) :: SELF
  TYPE(GEOMETRY)       , INTENT(IN)    :: YDGEOMETRY
  REAL(KIND=JPRB)      , INTENT(IN)    :: PGEO0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG, &
   &                                            YDGEOMETRY%YRDIM%NGPBLKS)
  LOGICAL              , INTENT(IN)    :: LPGFSF    
  LOGICAL              , INTENT(IN)    :: LPGREUSE  

  

END SUBROUTINE TPG_TYPE_INIT

SUBROUTINE TPG_TYPE_UPDATE_VIEW(SELF, BLOCK_INDEX)
  CLASS(TPG_TYPE)      , INTENT(INOUT) :: SELF
  INTEGER(KIND=JPIM)   , INTENT(IN)    :: BLOCK_INDEX

  
  
  
  
  
  
  
  
  
  
  
END SUBROUTINE TPG_TYPE_UPDATE_VIEW

SUBROUTINE TPG_TYPE_FINAL(SELF)
  CLASS(TPG_TYPE)      , INTENT(INOUT) :: SELF

  
  
  
  
  
  
  
  
  
  
  
END SUBROUTINE TPG_TYPE_FINAL

END MODULE INTDYN_MOD
