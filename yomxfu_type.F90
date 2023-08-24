MODULE YOMXFU_TYPE

USE PARKIND1  ,ONLY : JPIM, JPRB
USE YOMHOOK   ,ONLY : LHOOK, JPHOOK, DR_HOOK

USE TYPE_FLUXES, ONLY : FLUXES_DESCRIPTOR
USE PTRXFU, ONLY : TXFUPTR
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE

IMPLICIT NONE

SAVE


!     ------------------------------------------------------------

!*    Contains variables to control activation of instantaneous fluxes.

INTEGER(KIND=JPIM), PARAMETER :: JPFUXT=250  ! maximum number of timesteps where XFU can be activated

TYPE :: TXFU_KEYS

LOGICAL :: LXFU=.FALSE.                      ! controls switch on/off all XFU
LOGICAL :: LXTRD=.FALSE.                     ! activates gravity wave drag momentum XFU if .T.
LOGICAL :: LXTRC=.FALSE.                     ! activates contribution of convection to U, V, q and (cp T) XFU if .T.
LOGICAL :: LXTRT=.FALSE.                     ! activates contribution of turbulence to U, V, q and (cp T) XFU if .T.
LOGICAL :: LXPLC=.FALSE.                     ! activates convective precipitation XFU if .T.
LOGICAL :: LXPLCG=.FALSE.                    ! activates convective graupels CFU if .T.
LOGICAL :: LXPLCH=.FALSE.                    ! activates convective hail CFU if .T.
LOGICAL :: LXPLS=.FALSE.                     ! activates stratiform precipitation XFU if .T.
LOGICAL :: LXPLSG=.FALSE.                    ! activates stratiform graupels CFU if .T.
LOGICAL :: LXPLSH=.FALSE.                    ! activates stratiform hail CFU if .T.
LOGICAL :: LXR=.FALSE.                       ! activates radiation XFU if .T.
LOGICAL :: LXNEBTT=.FALSE.                   ! activates total cloudiness XFU if .T.
LOGICAL :: LXNEBPA=.FALSE.                   ! activates partial cloudiness XFU if .T.
LOGICAL :: LXCLS=.FALSE.                     ! activates U, V, T, q and relative humidity at 2 or 10 m (time t-dt) if .T.
LOGICAL :: LXMWINDCLS=.FALSE.                ! activates mean of U and V at 10 m if .T., also NU/NV if LXNUVCLS
LOGICAL :: LXNUVCLS=.FALSE.                  ! activates neutral U and V at 10 m (time t-dt) if .T.
LOGICAL :: LXTTCLS=.FALSE.                   ! activates extreme temperatures at 2 m if .T.
LOGICAL :: LXHHCLS=.FALSE.                   ! activates extreme relative moistures at 2 m if .T
LOGICAL :: LXTPWCLS=.FALSE.                  ! activates T'w at 2 m if .T
LOGICAL :: LXSOIL=.FALSE.                    ! activates soil XFU if .T.
LOGICAL :: LTXTRD=.FALSE.                    ! activates gravity wave drag momentum XFU at all levels if .T.
LOGICAL :: LTXTRC=.FALSE.                    ! activates contribution of convection to U, V, q and (cp T) XFU if .T.
LOGICAL :: LTXTRT=.FALSE.                    ! activates contribution of turbulence to U, V, q and (cp T) XFU if .T.
LOGICAL :: LTXR=.FALSE.                      ! activates radiation XFU at all levels if .T.
LOGICAL :: LTXNEB=.FALSE.                    ! activates cloudiness XFU at all levels if .T.
LOGICAL :: LTXQICE=.FALSE.                   ! total ice water content at all levels
LOGICAL :: LTXQLI=.FALSE.                    ! total liquid water content at all levels
LOGICAL :: LXICV=.FALSE.                     ! activates indices of convection
                                             ! (CAPE and moisture convergence) XFU at all levels if .T.
LOGICAL :: LXCTOP=.FALSE.                    ! activates pressure of top deep convection
LOGICAL :: LXCLP=.FALSE.                     ! activates height (in meters) of PBL XFU at all levels if .T.
LOGICAL :: LXVEIN=.FALSE.                    ! activates ventilation index
LOGICAL :: LXTGST=.FALSE.                    ! activates gusts as U and V components XFU at all levels if .T.
LOGICAL :: LXXGST=.FALSE.                    ! activates extreme gusts as U and V components XFU at all levels if .T.
LOGICAL :: LXXGST2=.FALSE.                   ! activates extreme gusts2 as U and V components XFU at all levels if .T.
LOGICAL :: LXQCLS=.FALSE.                    ! activates specific moisture at 2 meters
LOGICAL :: LXTHW=.FALSE.                     ! activates "theta'_w" surface flux
LOGICAL :: LXXDIAGH=.FALSE.                  ! activates extreme value of hail diagnostic
LOGICAL :: LXMRT=.FALSE.                     ! activates mean radiant temperature
LOGICAL :: LXVISI=.FALSE.                    ! activates visibilities diagnostic
LOGICAL :: LXVISI2=.FALSE.                   ! activates visibilities diagnostic
LOGICAL :: LXPRECIPS1=.FALSE.                ! activates precipitations types nr 1 diagnostic
LOGICAL :: LXPRECIPS2=.FALSE.                ! activates precipitations types nr 2 diagnostic

END TYPE TXFU_KEYS

TYPE, EXTENDS(TXFU_KEYS) :: TXFU

TYPE(FLUXES_DESCRIPTOR), ALLOCATABLE :: TYPE_XFU(:) ! contains the fluxes descriptor for the XFU

REAL(KIND=JPRB), POINTER :: RMWINDCALC_B(:,:)  => NULL ()  ! needed for mean wind calculation
REAL(KIND=JPRB), POINTER :: RMWINDCALC(:)  => NULL () 
CLASS (FIELD_2RB), POINTER :: F_RMWINDCALC => NULL ()

REAL(KIND=JPRB), POINTER :: RMNWINDCALC_B(:,:)  => NULL ()  ! needed for mean neutral wind calculation
REAL(KIND=JPRB), POINTER :: RMNWINDCALC(:)  => NULL () 
CLASS (FIELD_2RB), POINTER :: F_RMNWINDCALC => NULL ()


INTEGER(KIND=JPIM) :: MEANPERIOD             ! period (in seconds) for the mean calculation
INTEGER(KIND=JPIM) :: NMEANSTEPS             ! number of timesteps involved in mean calculation
INTEGER(KIND=JPIM) :: NXGSTPERIOD            ! period for maximum gusts
INTEGER(KIND=JPIM) :: NXGSTPERIOD2           ! period for second maximum gusts
INTEGER(KIND=JPIM) :: NVISIPERIOD            ! period for visibilities
INTEGER(KIND=JPIM) :: NVISIPERIOD2           ! period for second visibilities
INTEGER(KIND=JPIM) :: NXGSTTS                ! number of timesteps involved in max gust calculation
INTEGER(KIND=JPIM) :: NTYPXFU                ! number of fluxes types in buffer
INTEGER(KIND=JPIM) :: NXFUTS(0:JPFUXT)       ! array containing flux accumulation write-up steps
INTEGER(KIND=JPIM) :: NFRXFU                 ! frequency of write up of flux diagnostics
INTEGER(KIND=JPIM) :: NRAZTS(0:JPFUXT)       ! array containing instantaneous flux reset steps
INTEGER(KIND=JPIM) :: NFRRAZ                 ! frequency of reset of flux diagnostics
INTEGER(KIND=JPIM) :: N1RAZ                  ! over-riding switch for instantaneous flux reset (0 = false)
INTEGER(KIND=JPIM) :: NFDXFU                 ! total number of fields in buffer

LOGICAL :: LRESET                            ! reset extreme temperatures to zero
LOGICAL :: LRESET_GST                        ! reset Gust calculation
LOGICAL :: LRESET_GST2                       ! reset Gust2 calculation
LOGICAL :: LRESET_PRECIP                     ! reset Precips type calcultation
LOGICAL :: LRESET_PRECIP2                    ! reset Precips type calcultation
LOGICAL :: LRESET_VISI                       ! reset visibilities calculations
LOGICAL :: LRESET_VISI2                      ! reset visibilities calculations

LOGICAL :: LREAXFU                           ! read first input on historic file if .T.

TYPE(TXFUPTR) :: YXFUPT

REAL (KIND=JPRB), POINTER :: XFUBUF_B(:,:,:)  => NULL () ! Buffer for instantaneous diagnostics

! CAPE (convective available potential energy)
REAL (KIND=JPRB), POINTER :: CAPE (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_CAPE => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YCAPE

! height of the PBL out of the model
REAL (KIND=JPRB), POINTER :: CLPH (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_CLPH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YCLPH

! maximum of CLWC
REAL (KIND=JPRB), POINTER :: CLWC2 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_CLWC2 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YCLWC2

! maximum of CLWC
REAL (KIND=JPRB), POINTER :: CLWC (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_CLWC => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YCLWC

! pressure of top of deep convection
REAL (KIND=JPRB), POINTER :: CTOP (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_CTOP => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YCTOP

! contribution of convection to q
REAL (KIND=JPRB), POINTER :: DICQ (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_DICQ => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YDICQ

! contribution of convection to (cp T)
REAL (KIND=JPRB), POINTER :: DICS (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_DICS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YDICS

! contribution of turbulence to T
REAL (KIND=JPRB), POINTER :: DITQ (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_DITQ => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YDITQ

! contribution of turbulence to (cp T)
REAL (KIND=JPRB), POINTER :: DITS (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_DITS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YDITS

! heat flux in soil
REAL (KIND=JPRB), POINTER :: FCHSP (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FCHSP => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCHSP

! liquid evaporation
REAL (KIND=JPRB), POINTER :: FEVL (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FEVL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFEVL

! evapotranspiration
REAL (KIND=JPRB), POINTER :: FEVV (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FEVV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFEVV

! water flux in soil
REAL (KIND=JPRB), POINTER :: FLWSP (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FLWSP => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFLWSP

! melt snow
REAL (KIND=JPRB), POINTER :: FONTE (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FONTE => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFONTE

! transpiration
REAL (KIND=JPRB), POINTER :: FTR (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FTR => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFTR

! minimum relative humidity at 2 meters (pbl)
REAL (KIND=JPRB), POINTER :: HUN (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_HUN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YHUN

! maximum relative humidity at 2 meters (pbl)
REAL (KIND=JPRB), POINTER :: HUX (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_HUX => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YHUX

! moisture convergence
REAL (KIND=JPRB), POINTER :: MOCON (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_MOCON => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YMOCON

! mean radiant temperature at 2 meters
REAL (KIND=JPRB), POINTER :: MRT (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_MRT => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YMRT

! low cloud cover
REAL (KIND=JPRB), POINTER :: NBBAS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NBBAS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNBBAS

! convective cloud cover
REAL (KIND=JPRB), POINTER :: NBCON (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NBCON => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNBCON

! high cloud cover
REAL (KIND=JPRB), POINTER :: NBHAU (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NBHAU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNBHAU

! medium cloud cover
REAL (KIND=JPRB), POINTER :: NBMOY (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NBMOY => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNBMOY

! cloud cover
REAL (KIND=JPRB), POINTER :: NEB (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_NEB => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNEB

! total cloudiness
REAL (KIND=JPRB), POINTER :: NEBT (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NEBT => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNEBT

! U-component of neutral wind at 10 meters (pbl)
REAL (KIND=JPRB), POINTER :: NUCLS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NUCLS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNUCLS

! V-component of neutral wind at 10 meters (pbl)
REAL (KIND=JPRB), POINTER :: NVCLS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NVCLS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNVCLS

! convective graupel fall
REAL (KIND=JPRB), POINTER :: PLCG (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_PLCG => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPLCG

! convective hail fall
REAL (KIND=JPRB), POINTER :: PLCH (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_PLCH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPLCH

! convective precipitation
REAL (KIND=JPRB), POINTER :: PLCL (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_PLCL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPLCL

! convective snow fall
REAL (KIND=JPRB), POINTER :: PLCN (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_PLCN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPLCN

! large scale graupel fall (stratiform)
REAL (KIND=JPRB), POINTER :: PLSG (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_PLSG => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPLSG

! large scale hail fall (stratiform)
REAL (KIND=JPRB), POINTER :: PLSH (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_PLSH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPLSH

! large scale precipitation (stratiform)
REAL (KIND=JPRB), POINTER :: PLSL (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_PLSL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPLSL

! large scale snow fall (stratiform)
REAL (KIND=JPRB), POINTER :: PLSN (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_PLSN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPLSN

! frequent precipitations type
REAL (KIND=JPRB), POINTER :: PTYPE2 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_PTYPE2 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPTYPE2

! frequent precipitations type
REAL (KIND=JPRB), POINTER :: PTYPE (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_PTYPE => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPTYPE

! severe precipitations type
REAL (KIND=JPRB), POINTER :: PTYPESEV2 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_PTYPESEV2 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPTYPESEV2

! severe precipitations type
REAL (KIND=JPRB), POINTER :: PTYPESEV (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_PTYPESEV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YPTYPESEV

! specific humidity at 2 meters (pbl)
REAL (KIND=JPRB), POINTER :: QCLS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_QCLS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YQCLS

! ice water
REAL (KIND=JPRB), POINTER :: QICE (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_QICE => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YQICE

! liquid water
REAL (KIND=JPRB), POINTER :: QLI (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_QLI => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YQLI

! relative humidity at 2 meters (pbl)
REAL (KIND=JPRB), POINTER :: RHCLS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_RHCLS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YRHCLS

! solar radiation
REAL (KIND=JPRB), POINTER :: RSO (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_RSO => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YRSO

! surface radiation
REAL (KIND=JPRB), POINTER :: RTH (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_RTH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YRTH

! interception soil layer runoff
REAL (KIND=JPRB), POINTER :: RUISL (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_RUISL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YRUISL

! deep soil runoff
REAL (KIND=JPRB), POINTER :: RUISP (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_RUISP => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YRUISP

! surface soil runoff
REAL (KIND=JPRB), POINTER :: RUISS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_RUISS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YRUISS

! 
REAL (KIND=JPRB), POINTER :: SIC (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_SIC => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSIC

! temperature at 2 meters (pbl)
REAL (KIND=JPRB), POINTER :: TCLS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_TCLS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTCLS

! theta prime_w surface
REAL (KIND=JPRB), POINTER :: THW (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_THW => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTHW

! minimum temperature at 2 meters
REAL (KIND=JPRB), POINTER :: TN (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_TN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTN

! wet bulb temperature at 2 meters (pbl)
REAL (KIND=JPRB), POINTER :: TPWCLS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_TPWCLS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTPWCLS

! contribution of convection to U
REAL (KIND=JPRB), POINTER :: TRCU (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_TRCU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTRCU

! contribution of convection to V
REAL (KIND=JPRB), POINTER :: TRCV (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_TRCV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTRCV

! U-wind gravity wave stress
REAL (KIND=JPRB), POINTER :: TRDU (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_TRDU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTRDU

! V-wind gravity wave stress
REAL (KIND=JPRB), POINTER :: TRDV (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_TRDV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTRDV

! contribution of turbulence to U
REAL (KIND=JPRB), POINTER :: TRTU (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_TRTU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTRTU

! contribution of turbulence to V
REAL (KIND=JPRB), POINTER :: TRTV (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_TRTV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTRTV

! maximum temperature at 2 meters
REAL (KIND=JPRB), POINTER :: TX (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_TX => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YTX

! U-component of wind at 10 meters (pbl)
REAL (KIND=JPRB), POINTER :: UCLS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_UCLS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YUCLS

! U-momentum of gusts2
REAL (KIND=JPRB), POINTER :: UGST2 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_UGST2 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YUGST2

! U-momentum of gusts out of the model
REAL (KIND=JPRB), POINTER :: UGST (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_UGST => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YUGST

! V-component of wind at 10 meters (pbl)
REAL (KIND=JPRB), POINTER :: VCLS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_VCLS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YVCLS

! ventilation index in PBL
REAL (KIND=JPRB), POINTER :: VEIN (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_VEIN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YVEIN

! V-momentum of gusts2
REAL (KIND=JPRB), POINTER :: VGST2 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_VGST2 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YVGST2

! V-momentum of gusts out of the model
REAL (KIND=JPRB), POINTER :: VGST (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_VGST => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YVGST

! visibility due to water and/or ice cloud
REAL (KIND=JPRB), POINTER :: VISICLD2 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_VISICLD2 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YVISICLD2

! visibility due to water and/or ice cloud
REAL (KIND=JPRB), POINTER :: VISICLD (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_VISICLD => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YVISICLD

! visibility due to precipitations
REAL (KIND=JPRB), POINTER :: VISIHYD2 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_VISIHYD2 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YVISIHYD2

! visibility due to precipitations
REAL (KIND=JPRB), POINTER :: VISIHYD (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_VISIHYD => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YVISIHYD

! hail diagnostic
REAL (KIND=JPRB), POINTER :: XDIAGH (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_XDIAGH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YXDIAGH


CONTAINS
  PROCEDURE :: INIT => TXFU_TYPE_INIT
  PROCEDURE :: UPDATE_VIEW => TXFU_TYPE_UPDATE_VIEW
  PROCEDURE :: FINAL => TXFU_TYPE_FINAL
  PROCEDURE :: RESET => TXFU_TYPE_RESET
END TYPE TXFU

!     ------------------------------------------------------------

CONTAINS

SUBROUTINE TXFU_TYPE_INIT (SELF)

CLASS (TXFU), INTENT (INOUT) :: SELF

INTEGER (KIND=JPIM) :: ILEV, IOFF

REAL (KIND=JPHOOK) :: ZHOOK_HANDLE


































































































































































































































END SUBROUTINE TXFU_TYPE_INIT

SUBROUTINE TXFU_TYPE_UPDATE_VIEW (SELF, BLOCK_INDEX)

CLASS (TXFU), INTENT (INOUT) :: SELF
INTEGER (KIND=JPIM), INTENT (IN) :: BLOCK_INDEX

REAL (KIND=JPHOOK) :: ZHOOK_HANDLE


























































































































































END SUBROUTINE TXFU_TYPE_UPDATE_VIEW

SUBROUTINE TXFU_TYPE_FINAL (SELF)

CLASS (TXFU), INTENT (INOUT) :: SELF

REAL (KIND=JPHOOK) :: ZHOOK_HANDLE


























































































































































END SUBROUTINE TXFU_TYPE_FINAL

SUBROUTINE TXFU_TYPE_RESET (SELF, LDCONFX, LDFSTEP, YDRIP, YDPHY, YDMCC)

USE PARKIND1           , ONLY : JPIM, JPRB
USE YOMHOOK            , ONLY : LHOOK, JPHOOK, DR_HOOK
USE YOMCT1             , ONLY : N1HIS
USE YOMCT0             , ONLY : NFRHIS  ,NHISTS  ,NHISTSMIN
USE YOMCT3             , ONLY : NSTEP
USE YOMRIP             , ONLY : TRIP
USE YOMPHY             , ONLY : TPHY
USE YOMMCC             , ONLY : TMCC

CLASS(TXFU)         ,INTENT(INOUT) :: SELF
LOGICAL             ,INTENT(IN)    :: LDCONFX
LOGICAL             ,INTENT(IN)    :: LDFSTEP
TYPE(TRIP)          ,INTENT(IN)    :: YDRIP
TYPE(TPHY)          ,INTENT(IN)    :: YDPHY
TYPE(TMCC)          ,INTENT(IN)    :: YDMCC


INTEGER(KIND=JPIM) :: IRAZTS(NSTEP/SELF%NFRRAZ:NSTEP/SELF%NFRRAZ)

INTEGER(KIND=JPIM), ALLOCATABLE :: IHISTS(:)
INTEGER(KIND=JPIM) :: JJ, IFRGST, IFRGST2, IPERIOD,IFRVISI,IFRVISI2,IFRPRECIP,IFRPRECIP2
LOGICAL :: LLMEAN, LLMLPP

REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE



















END SUBROUTINE TXFU_TYPE_RESET

END MODULE YOMXFU_TYPE
