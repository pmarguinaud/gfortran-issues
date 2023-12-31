! (C) Copyright 1988- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE YOECLDP

USE PARKIND1, ONLY : JPIM, JPRB
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

SAVE

!     -----------------------------------------------------------------
!     ** YOECLDP - CONTROL PARAMETERS FOR PROGNOSTIC CLOUD SCHEME
!     -----------------------------------------------------------------

!     * E.C.M.W.F. PHYSICS PACKAGE *

!     C. JAKOB     E.C.M.W.F.    94/02/07
!     A. Tompkins  E.C.M.W.F.  2004/12/03 total water variance setup for
!                                                  moist advection-diffusion PBL
!     A. Tompkins  E.C.M.W.F.  2004/09/02 Aerosol in microphysics switches
!     JJMorcrette  ECMWF       20100813   Aerosol index for aerosol-cloud interactions
!     R. Forbes    ECMWF       20110301   Added ice deposition parameters
!     R. Forbes    ECMWF       20150115   Added additional ice, snow and rain parameters
!     R. Forbes    ECMWF       20190115   Added inhomog, overlapliqice and effrime parameters
!     F. Vana      ECMWF       20200914   Cloud relaxation to help adjoint accuracy
!     R. Forbes    ECMWF       20201115   Added RSSICEFACTOR and extra consts for ice
!     R. Forbes    ECMWF       20201215   Renamed RCL_FACR to RCL_LAMR and added for snow
!     R. Forbes    ECMWF       20220501   Added NPTYPE_SEV2WMO

!      NAME     TYPE      PURPOSE
!      ----     ----      -------

!     *RAMID*   REAL      BASE VALUE FOR CALCULATION OF RELATIVE 
!                         HUMIDITY THRESHOLD FOR ONSET OF STRATIFORM
!                         CONDENSATION (TIEDTKE, 1993, EQUATION 24)
!     *RCLDIFF* REAL      DIFFUSION-COEFFICIENT FOR EVAPORATION BY
!                         TURBULENT MIXING (IBID., EQU. 30)
!     *RCLDIFF_CONVI*REAL ENHANCEMENT FACTOR OF  RCLDIFF FOR CONVECTION
!     *RCLCRIT* REAL      BASE VALUE OF CRITICAL CLOUD WATER CONTENT 
!                         FOR CONVERSION TO RAIN (SUNDQUIST, 1988)
!     *RCLCRIT_SEA* REAL  BASE VALUE OF CRITICAL CLOUD WATER CONTENT FOR SEA
!     *RCLCRIT_LAND* REAL BASE VALUE OF CRITICAL CLOUD WATER CONTENT FOR LAND
!     *RKCONV*  REAL      BASE VALUE FOR CONVERSION COEFFICIENT (IBID.)
!     *RPRC1*   REAL      COALESCENCE CONSTANT (IBID.)
!     *RPRC2*   REAL      BERGERON-FINDEISEN CONSTANT (IBID.)
!     *RCLDMAX* REAL      MAXIMUM CLOUD WATER CONTENT
!     *RPECONS* REAL      EVAPORATION CONSTANT AFTER KESSLER 
!                         (TIEDTKE, 1993, EQU.35)
!     *RPRECRHMAX* REAL   MAX THRESHOLD RH FOR EVAPORATION FOR ZERO COVER
!     *RTAUMEL* REAL      RELAXATION TIME FOR MELTING OF SNOW
!     *RAMIN*   REAL      LIMIT FOR A
!     *RLMIN*   REAL      LIMIT FOR L
!     *RKOOPTAU*  REAL    TIMESCALE FOR ICE SUPERSATURATION REMOVAL
!     *RSSICEFACTOR* REAL TIMESTEP FACTOR FOR ICE SUPERSATURATION REMOVAL
!     *RVICE*     REAL    FIXED ICE FALLSPEED
!     *RVRAIN*    REAL    FIXED RAIN FALLSPEED
!     *RVSNOW*    REAL    FIXED SNOW FALLSPEED
!     *RTHOMO*    REAL    TEMPERATURE THRESHOLD FOR SPONTANEOUS FREEZING OF LIQUID DROPLETS
!     *RCOVPMIN*  REAL    MINIMUM PRECIPITATION COVERAGE REQUIRED FOR THE NEW PROGNOSTIC PRECIP
!     *RCLDTOPP*  REAL    TOP PRESSURE FOR CLOUD CALCULATION
!     *NCLDTOP*   INTEGER TOP LEVEL FOR CLOUD CALCULATION
!     *NSSOPT*  INTEGER   PARAMETRIZATION CHOICE FOR SUPERSATURATION
!     *NCLDDIAG*INTEGER   CONTROLS CLOUDSC DIAGNOSTICS IN PEXTRA
!     *NCLV*     INTEGER   NUMBER OF PROGNOSTIC EQUATIONS IN CLOUDSC 
!                         (INCLUDES WATER VAPOUR AS DUMMY VARIABLE) 
!      NAERCLD         INT  INDEX TO CONTROL SWITCHES FOR 
!                           AEROSOL-MICROPHYSICS INTERACTION, LAER*
!      NAECLxx         INT  INDEX OF GEMS AEROSOLS USED IN AEROSOL-CLOUD INTERACTIONS
!      RCCN            REAL DEFAULT CCN (CM-3)
!      RNICE           REAL DEFAULT ICE NUMBER CONCENTRATION (CM-3)
!      LAERLIQAUTOLSP  LOG  AEROSOLS AFFECT RAIN AUTOCONVERSION IN LSP
!      LAERLIQAUTOCP   LOG  AEROSOLS AFFECT RAIN AUTOCONVERSION IN CP
!      LAERLIQCOLL     LOG  AEROSOLS AFFECT RAIN COLLECTION 
!      LAERICESED      LOG  AEROSOLS AFFECT ICE SEDIMENTATION
!      LAERICEAUTO     LOG  AEROSOLS AFFECT ICE AUTOCONVERSION
!      RCCNOM          REAL CONSTANT IN MENON PARAM FOR ORGANIC MATTER -> CCN
!      RCCNSS          REAL CONSTANT IN MENON PARAM SEA SALT -> CCN
!      RCCNSU          REAL CONSTANT IN MENON PARAM FOR SULPHATE -> CCN
!      RCLDTOPCF       REAL Cloud fraction threshold that defines cloud top 
!      RDEPLIQREFRATE  REAL Fraction of deposition rate in cloud top layer
!      RDEPLIQREFDEPTH REAL Depth of supercooled liquid water layer (m)
!      RCL_OVERLAPLIQICE REAL overlap of liquid and ice cloud fractions
!      RCL_EFFRIME       REAL riming efficiency coefficient
!      RVRFACTOR       REAL KESSLER FACTOR=5.09E-3 FOR EVAPORATION OF CLEAR-SKY RAIN  (KESSLER,1969)
!      RCL_INHOMOGAUT  REAL Constant for autoconversion inhomogeneity factor
!      RCL_INHOMOGACC  REAL Constant for accretion inhomogeneity factor
!      LCLOUD_INHOMOG  LOG  switch on regime-dependent FSD use for calculation of ZEaut and ZEacc
!      RTAU_CLD_TLAD   REAL Relaxation factor of semi-prognostic cloud towards
!                           its non-linear trajecotry (TL/AD)

INTEGER(KIND=JPIM),PARAMETER :: NCLV=5      ! number of microphysics variables
INTEGER(KIND=JPIM),PARAMETER :: NCLDQL=1    ! liquid cloud water
INTEGER(KIND=JPIM),PARAMETER :: NCLDQI=2    ! ice cloud water
INTEGER(KIND=JPIM),PARAMETER :: NCLDQR=3    ! rain water
INTEGER(KIND=JPIM),PARAMETER :: NCLDQS=4    ! snow
INTEGER(KIND=JPIM),PARAMETER :: NCLDQV=5    ! vapour

! Number of precipitation types in WMO Code Table 4.201
INTEGER(KIND=JPIM),PARAMETER :: NPRECTYPES=12

TYPE :: TECLDP
REAL(KIND=JPRB) :: RAMID
REAL(KIND=JPRB) :: RCLDIFF
REAL(KIND=JPRB) :: RCLDIFF_CONVI
REAL(KIND=JPRB) :: RCLCRIT
REAL(KIND=JPRB) :: RCLCRIT_SEA
REAL(KIND=JPRB) :: RCLCRIT_LAND
REAL(KIND=JPRB) :: RKCONV
REAL(KIND=JPRB) :: RPRC1
REAL(KIND=JPRB) :: RPRC2
REAL(KIND=JPRB) :: RCLDMAX
REAL(KIND=JPRB) :: RPECONS
REAL(KIND=JPRB) :: RVRFACTOR
REAL(KIND=JPRB) :: RPRECRHMAX
REAL(KIND=JPRB) :: RTAUMEL
REAL(KIND=JPRB) :: RAMIN
REAL(KIND=JPRB) :: RLMIN
REAL(KIND=JPRB) :: RKOOPTAU
REAL(KIND=JPRB) :: RSSICEFACTOR
REAL(KIND=JPRB) :: RCLDTOPP
REAL(KIND=JPRB) :: RLCRITSNOW
REAL(KIND=JPRB) :: RSNOWLIN1
REAL(KIND=JPRB) :: RSNOWLIN2
REAL(KIND=JPRB) :: RICEHI1
REAL(KIND=JPRB) :: RICEHI2
REAL(KIND=JPRB) :: RICEINIT
REAL(KIND=JPRB) :: RVICE
REAL(KIND=JPRB) :: RVRAIN
REAL(KIND=JPRB) :: RVSNOW
REAL(KIND=JPRB) :: RTHOMO
REAL(KIND=JPRB) :: RCOVPMIN
REAL(KIND=JPRB) :: RCCN
REAL(KIND=JPRB) :: RNICE
REAL(KIND=JPRB) :: RCCNOM
REAL(KIND=JPRB) :: RCCNSS
REAL(KIND=JPRB) :: RCCNSU
REAL(KIND=JPRB) :: RCLDTOPCF
REAL(KIND=JPRB) :: RDEPLIQREFRATE
REAL(KIND=JPRB) :: RDEPLIQREFDEPTH
REAL(KIND=JPRB) :: RCL_OVERLAPLIQICE
REAL(KIND=JPRB) :: RCL_EFFRIME
REAL(KIND=JPRB) :: RTAU_CLD_TLAD
!--------------------------------------------------------
! Autoconversion/accretion (Khairoutdinov and Kogan 2000)
!--------------------------------------------------------
REAL(KIND=JPRB) :: RCL_KKAAC
REAL(KIND=JPRB) :: RCL_KKBAC
REAL(KIND=JPRB) :: RCL_KKAAU
REAL(KIND=JPRB) :: RCL_KKBAUQ
REAL(KIND=JPRB) :: RCL_KKBAUN
REAL(KIND=JPRB) :: RCL_KK_CLOUD_NUM_SEA
REAL(KIND=JPRB) :: RCL_KK_CLOUD_NUM_LAND
REAL(KIND=JPRB) :: RCL_INHOMOGAUT
REAL(KIND=JPRB) :: RCL_INHOMOGACC
LOGICAL         :: LCLOUD_INHOMOG
!--------------------------------------------------------
! Ice
!--------------------------------------------------------
REAL(KIND=JPRB) :: RCL_AI
REAL(KIND=JPRB) :: RCL_BI
REAL(KIND=JPRB) :: RCL_CI
REAL(KIND=JPRB) :: RCL_DI
REAL(KIND=JPRB) :: RCL_X1I
REAL(KIND=JPRB) :: RCL_X2I
REAL(KIND=JPRB) :: RCL_X3I
REAL(KIND=JPRB) :: RCL_X4I
REAL(KIND=JPRB) :: RCL_CONST1I
REAL(KIND=JPRB) :: RCL_CONST2I
REAL(KIND=JPRB) :: RCL_CONST3I
REAL(KIND=JPRB) :: RCL_CONST4I
REAL(KIND=JPRB) :: RCL_CONST5I
REAL(KIND=JPRB) :: RCL_CONST6I
REAL(KIND=JPRB) :: RCL_CONST7I
REAL(KIND=JPRB) :: RCL_LAMBDA1I
REAL(KIND=JPRB) :: RCL_LAMBDA2I
REAL(KIND=JPRB) :: RCL_APB1
REAL(KIND=JPRB) :: RCL_APB2
REAL(KIND=JPRB) :: RCL_APB3
!--------------------------------------------------------
! Snow
!--------------------------------------------------------
REAL(KIND=JPRB) :: RCL_AS
REAL(KIND=JPRB) :: RCL_BS
REAL(KIND=JPRB) :: RCL_CS
REAL(KIND=JPRB) :: RCL_DS
REAL(KIND=JPRB) :: RCL_X1S
REAL(KIND=JPRB) :: RCL_X2S
REAL(KIND=JPRB) :: RCL_X3S
REAL(KIND=JPRB) :: RCL_X4S
REAL(KIND=JPRB) :: RCL_CONST1S
REAL(KIND=JPRB) :: RCL_CONST2S
REAL(KIND=JPRB) :: RCL_CONST3S
REAL(KIND=JPRB) :: RCL_CONST4S
REAL(KIND=JPRB) :: RCL_CONST5S
REAL(KIND=JPRB) :: RCL_CONST6S
REAL(KIND=JPRB) :: RCL_CONST7S
REAL(KIND=JPRB) :: RCL_CONST8S
REAL(KIND=JPRB) :: RCL_LAM1S
REAL(KIND=JPRB) :: RCL_LAM2S
!--------------------------------------------------------
! Rain
!--------------------------------------------------------
REAL(KIND=JPRB) :: RDENSWAT
REAL(KIND=JPRB) :: RDENSREF
REAL(KIND=JPRB) :: RCL_AR
REAL(KIND=JPRB) :: RCL_BR
REAL(KIND=JPRB) :: RCL_CR
REAL(KIND=JPRB) :: RCL_DR
REAL(KIND=JPRB) :: RCL_X1R
REAL(KIND=JPRB) :: RCL_X2R
REAL(KIND=JPRB) :: RCL_X4R
REAL(KIND=JPRB) :: RCL_X1R_MP
REAL(KIND=JPRB) :: RCL_X2R_MP
REAL(KIND=JPRB) :: RCL_X4R_MP
REAL(KIND=JPRB) :: RCL_KA273
REAL(KIND=JPRB) :: RCL_CDENOM1
REAL(KIND=JPRB) :: RCL_CDENOM2
REAL(KIND=JPRB) :: RCL_CDENOM3
REAL(KIND=JPRB) :: RCL_SCHMIDT
REAL(KIND=JPRB) :: RCL_DYNVISC
REAL(KIND=JPRB) :: RCL_CONST1R
REAL(KIND=JPRB) :: RCL_CONST2R
REAL(KIND=JPRB) :: RCL_CONST3R
REAL(KIND=JPRB) :: RCL_CONST4R
REAL(KIND=JPRB) :: RCL_CONST7R
REAL(KIND=JPRB) :: RCL_CONST8R
REAL(KIND=JPRB) :: RCL_LAM1R
REAL(KIND=JPRB) :: RCL_LAM2R
REAL(KIND=JPRB) :: RCL_LAM1R_MP
REAL(KIND=JPRB) :: RCL_LAM2R_MP
! Rain freezing
REAL(KIND=JPRB) :: RCL_CONST5R
REAL(KIND=JPRB) :: RCL_CONST6R
REAL(KIND=JPRB) :: RCL_FZRAB
REAL(KIND=JPRB) :: RCL_FZRBB
! Rain accretion
REAL(KIND=JPRB) :: RCL_CONST9R
REAL(KIND=JPRB) :: RCL_CONST10R
REAL(KIND=JPRB) :: RCL_EFF_RACW

!----------------------------------------------------------------
! Lookup arrays relating WMO precip type to precip type severity
!---------------------------------------------------------------
INTEGER(KIND=JPIM) :: NPTYPE_SEV2WMO(NPRECTYPES)

!--------------------------------------------------------
! Cloud process budget diagnostic control
!--------------------------------------------------------
LOGICAL :: LCLDEXTRA, LCLDBUDGET
LOGICAL :: LCLDBUDC    
LOGICAL :: LCLDBUDL    
LOGICAL :: LCLDBUDI    
LOGICAL :: LCLDBUDT    
LOGICAL :: LCLDBUD_TIMEINT
LOGICAL :: LCLDBUD_VERTINT

INTEGER(KIND=JPIM) :: NSSOPT
INTEGER(KIND=JPIM) :: NCLDTOP
INTEGER(KIND=JPIM) :: NAECLBC, NAECLDU, NAECLOM, NAECLSS, NAECLSU
INTEGER(KIND=JPIM) :: NCLDDIAG

! aerosols
INTEGER(KIND=JPIM) :: NAERCLD
LOGICAL :: LAERLIQAUTOLSP
LOGICAL :: LAERLIQAUTOCP
LOGICAL :: LAERLIQAUTOCPB
LOGICAL :: LAERLIQCOLL
LOGICAL :: LAERICESED
LOGICAL :: LAERICEAUTO

! variance arrays
REAL(KIND=JPRB) :: NSHAPEP
REAL(KIND=JPRB) :: NSHAPEQ
INTEGER(KIND=JPIM) :: NBETA
REAL(KIND=JPRB) :: RBETA(0:100)
REAL(KIND=JPRB) :: RBETAP1(0:100)
!----------------------------------------------------------------------------

CONTAINS

  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 

END TYPE TECLDP
!============================================================================

!!TYPE(TECLDP), POINTER :: YRECLDP => NULL()

CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)

  
  CLASS(TECLDP), INTENT(IN) :: SELF
  INTEGER      , INTENT(IN) :: KDEPTH
  INTEGER      , INTENT(IN) :: KOUTNO

  INTEGER :: IDEPTHLOC
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


END SUBROUTINE PRINT_CONFIGURATION

END MODULE YOECLDP
