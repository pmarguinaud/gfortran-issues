MODULE YOMCFU_TYPE

USE PARKIND1  ,ONLY : JPIM, JPRB
USE YOMHOOK   ,ONLY : LHOOK, JPHOOK, DR_HOOK

USE TYPE_FLUXES, ONLY : FLUXES_DESCRIPTOR
USE PTRGFU, ONLY : TCFUPTR
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE

IMPLICIT NONE

SAVE


!     ------------------------------------------------------------

!*    Contains variables to control activation of cumulated fluxes.

INTEGER(KIND=JPIM), PARAMETER :: JPFUST=40    ! maximum number of timesteps where CFU can be activated

TYPE :: TCFU_KEYS

LOGICAL :: LCUMFU  = .FALSE.                  ! controls switch on/off all CFU
LOGICAL :: LSTRD   = .FALSE.                  ! activates gravity wave drag momentum CFU if .T.
LOGICAL :: LSTRC   = .FALSE.                  ! activates contribution of convection to U, V, q and (cp T) CFU if .T.
LOGICAL :: LSTRT   = .FALSE.                  ! activates contribution of turbulence to U, V, q and (cp T) CFU if .T.
LOGICAL :: LFPLC   = .FALSE.                  ! activates convective precipitation CFU if .T.
LOGICAL :: LFPLCG  = .FALSE.                  ! activates convective graupels CFU if .T.
LOGICAL :: LFPLCH  = .FALSE.                  ! activates convective hail CFU if .T.
LOGICAL :: LFPLS   = .FALSE.                  ! activates stratiform precipitation CFU if .T.
LOGICAL :: LFPLSG  = .FALSE.                  ! activates stratiform graupels CFU if .T.
LOGICAL :: LFPLSH  = .FALSE.                  ! activates stratiform hail CFU if .T.
LOGICAL :: LFR     = .FALSE.                  ! activates radiation CFU if .T.
LOGICAL :: LAMIP   = .FALSE.                  ! activates AMIP output if .T.
LOGICAL :: LRAYS   = .FALSE.                  ! activates more radiative CFU if .T.
LOGICAL :: LRAYD   = .FALSE.                  ! activates downwards surface radiative CFU if .T.
LOGICAL :: LNEBTT  = .FALSE.                  ! activates total cloudiness CFU if .T.
LOGICAL :: LFSF    = .FALSE.                  ! activates surface CFU if .T.
LOGICAL :: LFSOIL  = .FALSE.                  ! activates soil CFU if .T.
LOGICAL :: LNEBPAR = .FALSE.                  ! activates partial cloudiness CFU if .T.
LOGICAL :: LTSTRD  = .FALSE.                  ! activates gravity wave drag momentum CFU at all levels if .T.
LOGICAL :: LTSTRC  = .FALSE.                  ! activates contribution of convection to U, V, q and (cp T) CFU at all levels if .T.
LOGICAL :: LTSTRT  = .FALSE.                  ! activates contribution of turbulence to U, V, q and (cp T) CFU at all levels if .T.
LOGICAL :: LTFPLC  = .FALSE.                  ! activates convective precipitation CFU at all levels if .T.
LOGICAL :: LTFPLS  = .FALSE.                  ! activates stratiform precipitation CFU at all levels if .T.
LOGICAL :: LTFR    = .FALSE.                  ! activates radiation CFU at all levels if .T.
LOGICAL :: LTNEB   = .FALSE.                  ! activates cloudiness CFU at all levels if .T.
LOGICAL :: LFDUTP  = .FALSE.                  ! activates filtered duration of total precipitations CFU if .T.
LOGICAL :: LMOON   = .FALSE.                  ! activates moon radiation CFU if .T.
LOGICAL :: LFRRC   = .FALSE.                  ! activates clear sky radiation calculation if .T.
LOGICAL :: LFLASH  = .FALSE.                  ! activates diagnostics of lightning

END TYPE TCFU_KEYS

TYPE, EXTENDS(TCFU_KEYS) :: TCFU

INTEGER(KIND=JPIM) :: NCFUTS(0:JPFUST)        ! array containing flux accumulation write-up steps
INTEGER(KIND=JPIM) :: NFRRC                   ! frequency for clear sky radiation calculation
INTEGER(KIND=JPIM) :: NFRCFU                  ! frequency of write up of flux diagnostics
INTEGER(KIND=JPIM) :: NFDCFU                  ! total number of fields in buffer
INTEGER(KIND=JPIM) :: NTYPCFU                 ! number of fluxes types in buffer
INTEGER(KIND=JPIM) :: NMTFLASH                ! method used to compute lightening density
REAL(KIND=JPRB) :: CALFLASH1,CALFLASH2        ! calibration factor for lightening density


LOGICAL :: LREACFU = .FALSE.                  ! read first input on historic file if .T.

TYPE(FLUXES_DESCRIPTOR), ALLOCATABLE :: TYPE_CFU(:)  ! contains the fluxes descriptor for the CFU

TYPE(TCFUPTR) :: YCFUPT

REAL (KIND=JPRB), POINTER :: CFUBUF_B (:,:,:)   => NULL () ! Buffer for cumulative diagnostics

! liquid condensation due to convection
REAL (KIND=JPRB), POINTER :: FCCQL (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FCCQL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCCQL

! solid condensation due to convection
REAL (KIND=JPRB), POINTER :: FCCQN (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FCCQN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCCQN

! heat in soil
REAL (KIND=JPRB), POINTER :: FCHSP (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FCHSP => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCHSP

! latent heat
REAL (KIND=JPRB), POINTER :: FCL (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FCL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCL

! liquid latent heat
REAL (KIND=JPRB), POINTER :: FCLL (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FCLL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCLL

! solid latent heat
REAL (KIND=JPRB), POINTER :: FCLN (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FCLN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCLN

! large scale liquid condensation
REAL (KIND=JPRB), POINTER :: FCSQL (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FCSQL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCSQL

! large scale solid condensation
REAL (KIND=JPRB), POINTER :: FCSQN (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FCSQN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCSQN

! sensible heat
REAL (KIND=JPRB), POINTER :: FCS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FCS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFCS

! contribution of convection to q
REAL (KIND=JPRB), POINTER :: FDICQ (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FDICQ => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFDICQ

! contribution of convection to (cp T)
REAL (KIND=JPRB), POINTER :: FDICS (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FDICS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFDICS

! surface enthalpy (due to the dissipation of kinetic energy)
REAL (KIND=JPRB), POINTER :: FDISH (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FDISH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFDISH

! contribution of turbulence to T
REAL (KIND=JPRB), POINTER :: FDITQ (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FDITQ => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFDITQ

! contribution of turbulence to (cp T)
REAL (KIND=JPRB), POINTER :: FDITS (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FDITS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFDITS

! filtered duration of total precipitations
REAL (KIND=JPRB), POINTER :: FDUTP (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FDUTP => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFDUTP

! liquid evaporation
REAL (KIND=JPRB), POINTER :: FEVL (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FEVL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFEVL

! snow evaporation
REAL (KIND=JPRB), POINTER :: FEVN (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FEVN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFEVN

! evapotranspiration
REAL (KIND=JPRB), POINTER :: FEVV (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FEVV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFEVV

! deep frost
REAL (KIND=JPRB), POINTER :: FGEL (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FGEL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFGEL

! surface frost
REAL (KIND=JPRB), POINTER :: FGELS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FGELS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFGELS

! cumulative lighntning diagnotics
REAL (KIND=JPRB), POINTER :: FLASH (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FLASH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFLASH

! water in soil
REAL (KIND=JPRB), POINTER :: FLWSP (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FLWSP => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFLWSP

! cloud cover
REAL (KIND=JPRB), POINTER :: FNEB (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FNEB => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFNEB

! total cloudiness
REAL (KIND=JPRB), POINTER :: FNEBT (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FNEBT => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFNEBT

! melt snow
REAL (KIND=JPRB), POINTER :: FONTE (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FONTE => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFONTE

! convective graupel fall
REAL (KIND=JPRB), POINTER :: FPLCG (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FPLCG => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFPLCG

! convective hail fall
REAL (KIND=JPRB), POINTER :: FPLCH (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FPLCH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFPLCH

! convective precipitation
REAL (KIND=JPRB), POINTER :: FPLCL (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FPLCL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFPLCL

! convective snow fall
REAL (KIND=JPRB), POINTER :: FPLCN (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FPLCN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFPLCN

! large scale graupel fall (stratiform)
REAL (KIND=JPRB), POINTER :: FPLSG (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FPLSG => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFPLSG

! large scale hail fall (stratiform)
REAL (KIND=JPRB), POINTER :: FPLSH (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FPLSH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFPLSH

! large scale precipitation (stratiform)
REAL (KIND=JPRB), POINTER :: FPLSL (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FPLSL => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFPLSL

! large scale snow fall (stratiform)
REAL (KIND=JPRB), POINTER :: FPLSN (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FPLSN => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFPLSN

! surface direct normal irradiance
REAL (KIND=JPRB), POINTER :: FRSDNI (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRSDNI => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSDNI

! surface global normal irradiance
REAL (KIND=JPRB), POINTER :: FRSGNI (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRSGNI => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSGNI

! top clear sky shortwave radiative
REAL (KIND=JPRB), POINTER :: FRSOC0 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRSOC0 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSOC0

! surface clear sky shortwave radiative
REAL (KIND=JPRB), POINTER :: FRSOC1 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRSOC1 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSOC1

! surface down solar
REAL (KIND=JPRB), POINTER :: FRSODS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRSODS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSODS

! surface downward moon radiation
REAL (KIND=JPRB), POINTER :: FRSOLU (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRSOLU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSOLU

! surface parallel solar
REAL (KIND=JPRB), POINTER :: FRSOPS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRSOPS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSOPS

! top parallel solar
REAL (KIND=JPRB), POINTER :: FRSOPT (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRSOPT => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSOPT

! solar radiation
REAL (KIND=JPRB), POINTER :: FRSO (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FRSO => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRSO

! top clear sky longwave radiative
REAL (KIND=JPRB), POINTER :: FRTHC0 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRTHC0 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRTHC0

! surface clear sky longwave radiative
REAL (KIND=JPRB), POINTER :: FRTHC1 (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRTHC1 => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRTHC1

! surface down thermic
REAL (KIND=JPRB), POINTER :: FRTHDS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FRTHDS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRTHDS

! surface radiation
REAL (KIND=JPRB), POINTER :: FRTH (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_FRTH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFRTH

! top mesospheric enthalpy (+ dissipation)
REAL (KIND=JPRB), POINTER :: FTOPH (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FTOPH => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFTOPH

! transpiration
REAL (KIND=JPRB), POINTER :: FTR (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_FTR => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YFTR

! low cloud cover
REAL (KIND=JPRB), POINTER :: NEBBAS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NEBBAS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNEBBAS

! convective cloud cover
REAL (KIND=JPRB), POINTER :: NEBCON (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NEBCON => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNEBCON

! high cloud cover
REAL (KIND=JPRB), POINTER :: NEBHAU (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NEBHAU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNEBHAU

! medium cloud cover
REAL (KIND=JPRB), POINTER :: NEBMOY (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_NEBMOY => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YNEBMOY

! total ozone
REAL (KIND=JPRB), POINTER :: OZONT (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_OZONT => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YOZONT

! solid specific moisture
REAL (KIND=JPRB), POINTER :: QICE (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_QICE => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YQICE

! liquid specific moisture
REAL (KIND=JPRB), POINTER :: QLI (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_QLI => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YQLI

! total precipitable water
REAL (KIND=JPRB), POINTER :: QTOT (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_QTOT => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YQTOT

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

! snow mass
REAL (KIND=JPRB), POINTER :: SNS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_SNS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSNS

! surface pressure
REAL (KIND=JPRB), POINTER :: SPRES (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_SPRES => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSPRES

! contribution of convection to U
REAL (KIND=JPRB), POINTER :: STRCU (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_STRCU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSTRCU

! contribution of convection to V
REAL (KIND=JPRB), POINTER :: STRCV (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_STRCV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSTRCV

! U-wind gravity wave stress
REAL (KIND=JPRB), POINTER :: STRDU (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_STRDU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSTRDU

! V-wind gravity wave stress
REAL (KIND=JPRB), POINTER :: STRDV (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_STRDV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSTRDV

! contribution of turbulence to U
REAL (KIND=JPRB), POINTER :: STRTU (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_STRTU => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSTRTU

! contribution of turbulence to V
REAL (KIND=JPRB), POINTER :: STRTV (:,:) => NULL ()
CLASS (FIELD_3RB), POINTER :: F_STRTV => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YSTRTV

! soil moisture
REAL (KIND=JPRB), POINTER :: WS (:) => NULL ()
CLASS (FIELD_2RB), POINTER :: F_WS => NULL ()
TYPE (FLUXES_DESCRIPTOR) :: YWS


CONTAINS
  PROCEDURE :: INIT => TCFU_TYPE_INIT
  PROCEDURE :: UPDATE_VIEW => TCFU_TYPE_UPDATE_VIEW
  PROCEDURE :: FINAL => TCFU_TYPE_FINAL
END TYPE TCFU

CONTAINS

SUBROUTINE TCFU_TYPE_INIT (SELF)

CLASS (TCFU), INTENT (INOUT) :: SELF

INTEGER (KIND=JPIM) :: ILEV, IOFF

REAL (KIND=JPHOOK) :: ZHOOK_HANDLE


















































































































































































































END SUBROUTINE TCFU_TYPE_INIT

SUBROUTINE TCFU_TYPE_UPDATE_VIEW (SELF, BLOCK_INDEX)

CLASS (TCFU), INTENT (INOUT) :: SELF
INTEGER (KIND=JPIM), INTENT (IN) :: BLOCK_INDEX

REAL (KIND=JPHOOK) :: ZHOOK_HANDLE














































































































































END SUBROUTINE TCFU_TYPE_UPDATE_VIEW

SUBROUTINE TCFU_TYPE_FINAL (SELF)

CLASS (TCFU), INTENT (INOUT) :: SELF

REAL (KIND=JPHOOK) :: ZHOOK_HANDLE














































































































































END SUBROUTINE TCFU_TYPE_FINAL

END MODULE YOMCFU_TYPE
