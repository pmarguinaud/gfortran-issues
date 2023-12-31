MODULE RADIATION_SETUP

! RADIATION_SETUP - Setting up modular radiation scheme
!
! PURPOSE
! -------
!   The modular radiation scheme is contained in a separate
!   library. SETUP_RADIATION_SCHEME in this module sets up a small
!   derived type that contains the configuration object for the
!   radiation scheme, plus a small number of additional variables
!   needed for its implemenation in the IFS.
!
! INTERFACE
! ---------
!   SETUP_RADIATION_SCHEME is called from SUECRAD.  The radiation
!   scheme is actually run using the RADIATION_SCHEME routine (not in
!   this module).
!
! AUTHOR
! ------
!   Robin Hogan, ECMWF
!   Original: 2015-09-16
!
! MODIFICATIONS
! -------------
!   2017-03-03  R. Hogan   Put global variables in TRADIATION derived type
!   2017-11-17  S. Remy    Add Nitrates and SOA if NAERMACC=0
!   2017-11-28  R. Hogan   Delta scaling applied to particles only
!   2018-01-11  R. Hogan   Capability to scale solar spectrum in each band
!   2018-04-20  A. Bozzo   Added capability to read in aerosol optical properties
!                          at selected wavelengths
!   2019-01-21  R. Hogan   Explicit albedo and emissivity spectral definitions
!                          leading to smarter weighting in ecRad
!

!-----------------------------------------------------------------------

USE PARKIND1,         ONLY :   JPRB,JPIM
USE radiation_config, ONLY :   config_type, &
       &                       ISolverMcICA, ISolverSpartacus, &
       &                       ISolverTripleclouds, &
       &                       ILiquidModelSlingo, ILiquidModelSOCRATES, &
       &                       IIceModelFu, IIceModelBaran
USE YOEAEROP    ,ONLY : ALF_SU, ALF_OM, ALF_DD, ALF_SS, ALF_BC, ALF_NI, ALF_AM, ALF_SOA, &
     &  ASY_SU, ASY_OM, ASY_DD, ASY_SS, ASY_BC, ASY_NI, ASY_AM, ASY_SOA, &
     &  OMG_SU, OMG_OM, OMG_DD, OMG_SS, OMG_BC, OMG_NI, OMG_AM, OMG_SOA, &
     & RALI_BC, RALI_DD, RALI_OM, RALI_SU, RALI_SS, RALI_NI, RALI_AM, RALI_SOA


IMPLICIT NONE

SAVE

! Background aerosol is specified in an ugly way: using the old Tegen
! fields that are in terms of optical depth, and converted to mass
! mixing ratio via the relevant mass-extinction coefficient. The
! following are the indices to the aerosol types used to describe
! tropospheric and stratospheric background aerosol.
INTEGER(KIND=JPIM), PARAMETER :: ITYPE_TROP_BG_AER = 8 ! hydrophobic organic
INTEGER(KIND=JPIM), PARAMETER :: ITYPE_STRAT_BG_AER=12 ! non-absorbing sulphate

! This derived type contains configuration information for the
! radiation scheme plus a few additional variables and parameters
! needed for the IFS interface to it
TYPE :: TRADIATION

  ! Configuration information for the radiation scheme
  type(config_type)  :: rad_config

  ! Ultraviolet weightings
  INTEGER(KIND=JPIM) :: NWEIGHT_UV
  INTEGER(KIND=JPIM) :: IBAND_UV(100)
  REAL(KIND=JPRB)    :: WEIGHT_UV(100)
  ! Photosynthetically active radiation weightings
  INTEGER(KIND=JPIM) :: NWEIGHT_PAR
  INTEGER(KIND=JPIM) :: IBAND_PAR(100)
  REAL(KIND=JPRB)    :: WEIGHT_PAR(100)
  ! Mass-extinction coefficient (m2 kg-1) of tropospheric and
  ! stratospheric background aerosol at 550 nm
  REAL(KIND=JPRB)    :: TROP_BG_AER_MASS_EXT
  REAL(KIND=JPRB)    :: STRAT_BG_AER_MASS_EXT
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TRADIATION


CONTAINS

  ! This routine copies information between the IFS radiation
  ! configuration (stored mostly in YDERAD) and the radiation
  ! configuration of the modular radiation scheme (stored in
  ! PRADIATION%rad_config).  The optional input logical LDOUTPUT
  ! controls whether to print lots of information during the setup
  ! stage (default is no).
  SUBROUTINE SETUP_RADIATION_SCHEME(YDERDI,YDEAERATM,YDCOMPO,YDEPHY,YDERAD,PRADIATION,LDOUTPUT)

    USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
    
    USE YOERAD,   ONLY : TERAD
    USE YOEPHY,   ONLY : TEPHY
    USE YOEAERATM,ONLY : TEAERATM
    USE YOMCOMPO, ONLY : TCOMPO
    USE YOERDI  , ONLY : TERDI

    TYPE(TERDI)       ,INTENT(INOUT):: YDERDI
    TYPE(TEAERATM)    ,INTENT(INOUT)          :: YDEAERATM
    TYPE(TCOMPO)      ,INTENT(IN)             :: YDCOMPO
    TYPE(TEPHY)       ,INTENT(IN)             :: YDEPHY
    TYPE(TERAD)       ,INTENT(INOUT)          :: YDERAD
    TYPE(TRADIATION)  ,INTENT(INOUT), TARGET  :: PRADIATION

    
    
    LOGICAL, INTENT(IN), OPTIONAL :: LDOUTPUT

    
    
    INTEGER(KIND=JPIM) :: IVERBOSESETUP
    INTEGER(KIND=JPIM) :: ISTAT

    
    CHARACTER(LEN=256) :: CL_DATA_DIR

    
    REAL(KIND=JPRB)    :: ZWAVBOUND(15)
    INTEGER(KIND=JPIM) :: IBAND(16)



    
    
    LOGICAL :: LL_DO_NEAREST_SW_ALBEDO, LL_DO_NEAREST_LW_EMISS

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE




    

    
    

    

  END SUBROUTINE SETUP_RADIATION_SCHEME


  SUBROUTINE SETUP_MONO_AER_OPTICS(PRADIATION, YDCOMPO)

    
    
    

    
    

    USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
    USE RADIATION_AEROSOL_OPTICS_DATA, ONLY : AEROSOL_OPTICS_TYPE
    USE YOMCOMPO, ONLY : TCOMPO

    TYPE(TRADIATION), INTENT(IN)  :: PRADIATION
    TYPE(TCOMPO),     INTENT(IN)  :: YDCOMPO

    
    
    
    INTEGER(KIND=JPIM) :: IRH
    INTEGER(KIND=JPIM) :: IOM,ISU

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


    

    

    
    
  END SUBROUTINE SETUP_MONO_AER_OPTICS


  SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
    USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
    
    CLASS(TRADIATION),  INTENT(IN) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: KDEPTH
    INTEGER(KIND=JPIM), INTENT(IN) :: KOUTNO

    INTEGER(KIND=JPIM) :: IDEPTHLOC
    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

    
    

    
    
    
    
    
    
    
    
    
    

    
  END SUBROUTINE PRINT_CONFIGURATION

END MODULE RADIATION_SETUP
