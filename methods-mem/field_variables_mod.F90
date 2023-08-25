 

MODULE FIELD_VARIABLES_MOD
  ! The FIELD_VARIABLES type provides namespaced access to all core
  ! VARIABLE objects used.
  !
  ! These VARIABLE objects are currently wrappers around GMV and GFL
  ! fields and provide accessor methods to the individual FIELD
  ! storage objects and NPROMA array views (sub-array blocks) in
  ! OpenMP loops. The FIELD_VARIABLES%UPDATE_VIEW(BLOCK_INDEX) utility
  ! ensures that all block pointers provided by the stored variable
  ! objects are pointing to the correct sub-arrry block.

USE PARKIND1, ONLY: JPIM, JPRB
USE VARIABLE_MODULE
USE YOMGMV, ONLY : TGMV
USE YOMGFL, ONLY : TGFL

IMPLICIT NONE

TYPE GEOMETRY_VARIABLES

  ! Variables for arrays specific to the geometry
  TYPE(VARIABLE_2RB) :: RCORI
  TYPE(VARIABLE_2RB) :: RCORIC
  TYPE(VARIABLE_2RB) :: GEMU
  TYPE(VARIABLE_2RB) :: GSQM2
  TYPE(VARIABLE_2RB) :: GELAM
  TYPE(VARIABLE_2RB) :: GELAT
  TYPE(VARIABLE_2RB) :: GECLO
  TYPE(VARIABLE_2RB) :: GESLO
  TYPE(VARIABLE_2RB) :: GM
  TYPE(VARIABLE_2RB) :: GMAPPA
  TYPE(VARIABLE_2RB) :: GOMVRL
  TYPE(VARIABLE_2RB) :: GOMVRM
  TYPE(VARIABLE_2RB) :: GNORDL
  TYPE(VARIABLE_2RB) :: GNORDM
  TYPE(VARIABLE_2RB) :: GNORDLCL
  TYPE(VARIABLE_2RB) :: GNORDMCL
  TYPE(VARIABLE_2RB) :: GNORDMCM
  TYPE(VARIABLE_2RB) :: GAW
  TYPE(VARIABLE_2RB) :: OROG
  TYPE(VARIABLE_2RB) :: OROGL
  TYPE(VARIABLE_2RB) :: OROGM
  TYPE(VARIABLE_2RB) :: RINDX
  TYPE(VARIABLE_2RB) :: RINDY
  TYPE(VARIABLE_2RB) :: RATATH
  TYPE(VARIABLE_2RB) :: RATATX
  TYPE(VARIABLE_2RB) :: OROGLL
  TYPE(VARIABLE_2RB) :: OROGMM
  TYPE(VARIABLE_2RB) :: OROGLM
  TYPE(VARIABLE_2RB) :: GEXCO
  TYPE(VARIABLE_2RB) :: GEYCO
  TYPE(VARIABLE_2RB) :: GEZCO
  TYPE(VARIABLE_2RD) :: RCOLON
  TYPE(VARIABLE_2RD) :: RSILON

CONTAINS
  PROCEDURE :: UPDATE_VIEW => GEOMETRY_VARIABLES_UPDATE_VIEW
  PROCEDURE :: FINAL => GEOMETRY_VARIABLES_FINAL
END TYPE GEOMETRY_VARIABLES

TYPE ECPHYS_VARIABLES
  ! Variables for arrays in EC_PHYS_FIELDS_MOD
  TYPE(VARIABLE_3RB) :: USTRTI  ! E-W  SURFACE STRESS
  TYPE(VARIABLE_3RB) :: VSTRTI  ! N-S  SURFACE STRESS
  TYPE(VARIABLE_3RB) :: AHFSTI  ! SURFACE SENSIBLE HEAT FLUX
  TYPE(VARIABLE_3RB) :: EVAPTI  ! EVAPORATION
  TYPE(VARIABLE_3RB) :: TSKTI   ! SKIN TEMPERATURE
END TYPE ECPHYS_VARIABLES

TYPE RADIATION_VARIABLES
  ! Variables for arrays specific to radiation
  TYPE(VARIABLE_3RB) :: EMTD      ! longwave net flux
  TYPE(VARIABLE_3RB) :: TRSW      ! shortwave net transmissivity (multiply by incoming SW to get flux)
  TYPE(VARIABLE_3RB) :: EMTC      ! clear-sky net longwave flux
  TYPE(VARIABLE_3RB) :: TRSC      ! clear-sky net shortwave transmissivity
  TYPE(VARIABLE_3RB) :: EMTU
  TYPE(VARIABLE_4RB) :: TAUAER    ! prognostic aerosol variable for radiation and clouds
  TYPE(VARIABLE_2RB) :: SRSWD     ! downward SW radiation at the surface
  TYPE(VARIABLE_3RB) :: SRLWD     ! downward LW radiation at the surface
  TYPE(VARIABLE_2RB) :: SRLWDC    ! clear-sky downward LW radiation at the surface
  TYPE(VARIABLE_2RB) :: SRSWDC    ! clear-sky downward SW radiation at the surface
  TYPE(VARIABLE_2RB) :: SRSWDCS   ! clear-sky NET SW radiation at the surface
  TYPE(VARIABLE_2RB) :: SRLWDCS   ! clear-sky NET LW radiation at the surface
  TYPE(VARIABLE_2RB) :: SRSWDV    ! downward SW visible radiation at the surface
  TYPE(VARIABLE_2RB) :: SRSWDUV   ! downward SW ultraviolet/visible radiation at the surface
  TYPE(VARIABLE_2RB) :: EDRO
  TYPE(VARIABLE_2RB) :: SRSWPAR   ! downward SW PAR radiation at the surface
  TYPE(VARIABLE_2RB) :: SRSWUVB   ! downward UV-B radiation at the surface
  TYPE(VARIABLE_2RB) :: SRSWPARC  ! downward clear-sky SW PAR radiation at the surface
  TYPE(VARIABLE_2RB) :: SRSWTINC  ! TOA incident solar radiation
  TYPE(VARIABLE_2RB) :: RMOON     ! M-F military application
  TYPE(VARIABLE_2RB) :: SRFDIR    ! total sky direct downward SW radiation
  TYPE(VARIABLE_2RB) :: SRCDIR    ! clear-sky direct downward SW radiation
  TYPE(VARIABLE_3RB) :: DERIVATIVELW  ! derivative to update LW radiation between calls to full radiation scheme
END TYPE RADIATION_VARIABLES

TYPE FIELD_VARIABLES
  TYPE(VARIABLE_3RB) :: U  ! U-wind
  TYPE(VARIABLE_3RB) :: V  ! V-wind
  TYPE(VARIABLE_3RB) :: T  ! Temperature
  TYPE(VARIABLE_3RB) :: DIV  ! Divergence
  TYPE(VARIABLE_3RB) :: VOR  ! Vorticity
  TYPE(VARIABLE_3RB) :: SPD  ! Pressure departure variable
  TYPE(VARIABLE_3RB) :: SVD  ! Vertical div or velocity variable
  TYPE(VARIABLE_3RB) :: CSPDNL  ! 
  TYPE(VARIABLE_3RB) :: CSPNL  ! 
  TYPE(VARIABLE_3RB) :: CSVDPT  ! 
  TYPE(VARIABLE_3RB) :: CTNL  ! 
  TYPE(VARIABLE_3RB) :: CTPT  ! 
  TYPE(VARIABLE_3RB) :: CUNL  ! 
  TYPE(VARIABLE_3RB) :: CUPT  ! 
  TYPE(VARIABLE_3RB) :: CVNL  ! 
  TYPE(VARIABLE_3RB) :: CVPT  ! 
  TYPE(VARIABLE_3RB) :: CVWVNL  ! 
  TYPE(VARIABLE_3RB) :: DPHI  ! 
  TYPE(VARIABLE_3RB) :: EDOT  ! 
  TYPE(VARIABLE_3RB) :: GW  ! 
  TYPE(VARIABLE_3RB) :: NHX  ! 
  TYPE(VARIABLE_3RB) :: NHY  ! 
  TYPE(VARIABLE_3RB) :: SGRTL  ! 
  TYPE(VARIABLE_3RB) :: SGRTM  ! 
  TYPE(VARIABLE_3RB) :: SPDNL  ! 
  TYPE(VARIABLE_3RB) :: SPDNL_SI  ! 
  TYPE(VARIABLE_3RB) :: SPNL  ! 
  TYPE(VARIABLE_3RB) :: SPNL_SI  ! 
  TYPE(VARIABLE_3RB) :: SVDNL_SI  ! 
  TYPE(VARIABLE_3RB) :: TNL  ! 
  TYPE(VARIABLE_3RB) :: TNL_SI  ! 
  TYPE(VARIABLE_3RB) :: UNL  ! 
  TYPE(VARIABLE_3RB) :: UNL_SI  ! 
  TYPE(VARIABLE_3RB) :: VNL  ! 
  TYPE(VARIABLE_3RB) :: VNL_SI  ! 
  TYPE(VARIABLE_3RB) :: VWVNL  ! 
  TYPE(VARIABLE_3RB) :: CURHS  ! 
  TYPE(VARIABLE_3RB) :: CVRHS  ! 
  TYPE(VARIABLE_3RB) :: CTRHS  ! 
  TYPE(VARIABLE_3RB) :: CSPDRHS  ! 
  TYPE(VARIABLE_3RB) :: CSVDRHS  ! 
  TYPE(VARIABLE_3RB) :: NHXNL  ! 
  TYPE(VARIABLE_3RB) :: CNHXNL  ! 
  TYPE(VARIABLE_2RB) :: SP  ! Surface pressure
  TYPE(VARIABLE_2RB) :: SPNL2  ! 
  TYPE(VARIABLE_2RB) :: PREHYDS  ! 
  TYPE(VARIABLE_2RB) :: DBBC  ! 
  TYPE(VARIABLE_2RB) :: CSPPT  ! 
  TYPE(VARIABLE_2RB) :: CSPNL2  ! 
  TYPE(VARIABLE_2RB) :: CSPRHS  ! 
  TYPE(VARIABLE_2RB) :: GWS  ! 
  TYPE(VARIABLE_3RB) :: Q  ! Specific humidity
  TYPE(VARIABLE_3RB) :: I  ! Ice water
  TYPE(VARIABLE_3RB) :: L  ! Liquid water
  TYPE(VARIABLE_3RB) :: LCONV  ! Liquid water (CONV. PART)
  TYPE(VARIABLE_3RB) :: ICONV  ! Ice    water (CONV. PART)
  TYPE(VARIABLE_3RB) :: RCONV  ! Rain         (CONV. PART)
  TYPE(VARIABLE_3RB) :: SCONV  ! Snow         (CONV. PART)
  TYPE(VARIABLE_3RB) :: IRAD  ! Radiative cloud Ice water
  TYPE(VARIABLE_3RB) :: LRAD  ! Radiative cloud Liquid water
  TYPE(VARIABLE_3RB) :: S  ! Snow
  TYPE(VARIABLE_3RB) :: R  ! Rain
  TYPE(VARIABLE_3RB) :: G  ! Graupel
  TYPE(VARIABLE_3RB) :: H  ! Hail
  TYPE(VARIABLE_3RB) :: TKE  ! Turbulent Kinetic Energy
  TYPE(VARIABLE_3RB) :: TTE  ! Turbulent Total Energy
  TYPE(VARIABLE_3RB) :: EFB1  ! First variable EFB scheme
  TYPE(VARIABLE_3RB) :: EFB2  ! Second variable EFB scheme
  TYPE(VARIABLE_3RB) :: EFB3  ! Third variable EFB scheme
  TYPE(VARIABLE_3RB) :: A  ! Cloud fraction
  TYPE(VARIABLE_3RB) :: O3  ! Ozone
  TYPE(VARIABLE_3RB) :: SRC  ! Second-order flux for AROME s"rc"/2Sigma_s2 multiplied by Lambda_3
  TYPE(VARIABLE_3RB) :: MXL  ! Prognostic mixing length
  TYPE(VARIABLE_3RB) :: SHTUR  ! Shear source term for turbulence.
  TYPE(VARIABLE_3RB) :: FQTUR  ! Flux form source term for turbulence - moisture.
  TYPE(VARIABLE_3RB) :: FSTUR  ! Flux form source term for turbulence - enthalpy.
  TYPE(VARIABLE_3RB) :: CPF  ! Convective precipitation flux
  TYPE(VARIABLE_3RB) :: SPF  ! Stratiform precipitation flux
  TYPE(VARIABLE_3RB) :: CVGQ  ! Moisture Convergence for french physics
  TYPE(VARIABLE_3RB) :: QVA  ! Total humidity variation
  TYPE(VARIABLE_3RB), ALLOCATABLE :: GHG_G(:)  ! Greenhouse Gases
  TYPE(VARIABLE_3RB), POINTER :: GHG(:)  ! Greenhouse Gases
  TYPE(VARIABLE_3RB), ALLOCATABLE :: CHEM_G(:)  ! Chemistry
  TYPE(VARIABLE_3RB), POINTER :: CHEM(:)  ! Chemistry
  TYPE(VARIABLE_3RB), ALLOCATABLE :: AERO_G(:)  ! Aerosols
  TYPE(VARIABLE_3RB), POINTER :: AERO(:)  ! Aerosols
  TYPE(VARIABLE_3RB) :: LRCH4  ! CH4 loss rate (instantaneous field)
  TYPE(VARIABLE_3RB), ALLOCATABLE :: FORC_G(:)  ! Large scale forcing
  TYPE(VARIABLE_3RB), POINTER :: FORC(:)  ! Large scale forcing
  TYPE(VARIABLE_3RB), ALLOCATABLE :: EZDIAG_G(:)  ! Easy diagnostics
  TYPE(VARIABLE_3RB), POINTER :: EZDIAG(:)  ! Easy diagnostics
  TYPE(VARIABLE_3RB), ALLOCATABLE :: ERA40_G(:)  ! ERA40 diagnostic fields
  TYPE(VARIABLE_3RB), POINTER :: ERA40(:)  ! ERA40 diagnostic fields
  TYPE(VARIABLE_3RB), ALLOCATABLE :: NOGW_G(:)  ! NORO GWD SCHEME
  TYPE(VARIABLE_3RB), POINTER :: NOGW(:)  ! NORO GWD SCHEME
  TYPE(VARIABLE_3RB), ALLOCATABLE :: EMIS3D_G(:)  ! 3D emission fields for composition
  TYPE(VARIABLE_3RB), POINTER :: EMIS3D(:)  ! 3D emission fields for composition
  TYPE(VARIABLE_3RB), ALLOCATABLE :: EDRP_G(:)  ! Turbulence diagnostics EDR Parameter
  TYPE(VARIABLE_3RB), POINTER :: EDRP(:)  ! Turbulence diagnostics EDR Parameter
  TYPE(VARIABLE_3RB), ALLOCATABLE :: SLDIA_G(:)  ! SL dynamics diagnostics
  TYPE(VARIABLE_3RB), POINTER :: SLDIA(:)  ! SL dynamics diagnostics
  TYPE(VARIABLE_3RB), ALLOCATABLE :: AERAOT_G(:)  ! Aerosol optical thicknesses
  TYPE(VARIABLE_3RB), POINTER :: AERAOT(:)  ! Aerosol optical thicknesses
  TYPE(VARIABLE_3RB), ALLOCATABLE :: AERLISI_G(:)  ! Aerosol lidar simulator
  TYPE(VARIABLE_3RB), POINTER :: AERLISI(:)  ! Aerosol lidar simulator
  TYPE(VARIABLE_3RB), ALLOCATABLE :: AEROUT_G(:)  ! Aerosol outputs
  TYPE(VARIABLE_3RB), POINTER :: AEROUT(:)  ! Aerosol outputs
  TYPE(VARIABLE_3RB), ALLOCATABLE :: AEROCLIM_G(:)  ! Aerosol climatology
  TYPE(VARIABLE_3RB), POINTER :: AEROCLIM(:)  ! Aerosol climatology
  TYPE(VARIABLE_3RB), ALLOCATABLE :: UVP_G(:)  ! UV-processor output
  TYPE(VARIABLE_3RB), POINTER :: UVP(:)  ! UV-processor output
  TYPE(VARIABLE_3RB), ALLOCATABLE :: PHYS_G(:)  ! PHYS output
  TYPE(VARIABLE_3RB), POINTER :: PHYS(:)  ! PHYS output
  TYPE(VARIABLE_3RB) :: PHYCTY  ! PHYS input for MassCTY
  TYPE(VARIABLE_3RB) :: RSPEC  ! Specific gas constant
  TYPE(VARIABLE_3RB) :: SDSAT  ! Standard Deviation of the saturation Depression (Sigma_s)
  TYPE(VARIABLE_3RB) :: CVV  ! Convective Vertical Velocity
  TYPE(VARIABLE_3RB) :: RKTH  ! Rasch-Kristjansson H tendency
  TYPE(VARIABLE_3RB) :: RKTQV  ! Rasch-Kristjansson Qv tendency
  TYPE(VARIABLE_3RB) :: RKTQC  ! Rasch-Kristjansson Qc tendency
  TYPE(VARIABLE_3RB) :: UOM  ! Updraught vert velocity
  TYPE(VARIABLE_3RB) :: UAL  ! Updraught mesh fraction
  TYPE(VARIABLE_3RB) :: DOM  ! Downdraught vert velocity
  TYPE(VARIABLE_3RB) :: DAL  ! Downdraught mesh fraction
  TYPE(VARIABLE_3RB) :: UEN  ! Updraught entrainment
  TYPE(VARIABLE_3RB) :: UNEBH  ! pseudo-historic convective
  TYPE(VARIABLE_3RB), ALLOCATABLE :: LIMA_G(:)  ! LIMA prognostic fields
  TYPE(VARIABLE_3RB), POINTER :: LIMA(:)  ! LIMA prognostic fields
  TYPE(VARIABLE_3RB) :: FSD  ! PHYS output
  TYPE(VARIABLE_3RB), ALLOCATABLE :: EXT_G(:)  ! Extra fields
  TYPE(VARIABLE_3RB), POINTER :: EXT(:)  ! Extra fields

  TYPE(GEOMETRY_VARIABLES) :: GEOMETRY
  TYPE(ECPHYS_VARIABLES) :: ECPHYS
  TYPE(RADIATION_VARIABLES) :: RADIATION

  TYPE(VARIABLE_3RB_PTR), ALLOCATABLE :: GFL_PTR_G (:)
  TYPE(VARIABLE_3RB_PTR), POINTER :: GFL_PTR (:)

CONTAINS
  ! PROCEDURE :: CLONE => FIELD_VARIABLES_CLONE
  PROCEDURE :: CLONE_ARRAYS => FIELD_VARIABLES_CLONE_ARRAYS
  PROCEDURE :: UPDATE_VIEW => FIELD_VARIABLES_UPDATE_VIEW
  PROCEDURE :: RESET_ARRAYS => FIELD_VARIABLES_RESET_ARRAYS
  PROCEDURE :: FINAL => FIELD_VARIABLES_FINAL
  ! Timestepping utilities
  PROCEDURE :: GFL_PH9TOT0 => FIELD_VARIABLES_GFL_PH9TOT0
  PROCEDURE :: GFL_PH9TOT9 => FIELD_VARIABLES_GFL_PH9TOT9
  PROCEDURE :: GMV_RESET_PH9 => FIELD_VARIABLES_GMV_RESET_PH9
END TYPE FIELD_VARIABLES

CONTAINS

  SUBROUTINE FIELD_VARIABLES_CLONE_ARRAYS(SELF)
    
    
    
    
    
    CLASS(FIELD_VARIABLES), TARGET :: SELF
    INTEGER(KIND=JPIM) :: I
    INTEGER(KIND=JPIM) :: IPNTR
    INTEGER(KIND=JPIM) :: JFLD

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE FIELD_VARIABLES_CLONE_ARRAYS

  SUBROUTINE FIELD_VARIABLES_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(FIELD_VARIABLES) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    

    

  END SUBROUTINE FIELD_VARIABLES_UPDATE_VIEW

  SUBROUTINE FIELD_VARIABLES_FINAL(SELF)
    
    CLASS(FIELD_VARIABLES) :: SELF
    INTEGER(KIND=JPIM) :: I

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE FIELD_VARIABLES_FINAL

  SUBROUTINE FIELD_VARIABLES_RESET_ARRAYS(SELF)
    
    
    
    
    CLASS(FIELD_VARIABLES), TARGET :: SELF
    INTEGER(KIND=JPIM) :: I

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    

  END SUBROUTINE FIELD_VARIABLES_RESET_ARRAYS

  SUBROUTINE FIELD_VARIABLES_GFL_PH9TOT0(SELF)
    
    CLASS(FIELD_VARIABLES) :: SELF
    INTEGER(KIND=JPIM) :: I

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE FIELD_VARIABLES_GFL_PH9TOT0

  SUBROUTINE FIELD_VARIABLES_GFL_PH9TOT9(SELF)
    
    CLASS(FIELD_VARIABLES) :: SELF
    INTEGER(KIND=JPIM) :: I

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE FIELD_VARIABLES_GFL_PH9TOT9

  SUBROUTINE FIELD_VARIABLES_GMV_RESET_PH9(SELF, YDGMV, YDGFL)
    
    
    
    
    CLASS(FIELD_VARIABLES) :: SELF
    TYPE(TGMV), TARGET, INTENT(INOUT) :: YDGMV
    TYPE(TGFL), TARGET, INTENT(INOUT) :: YDGFL

    
    
    
    

    
    
    
    
    
    
    
    
  END SUBROUTINE FIELD_VARIABLES_GMV_RESET_PH9

  SUBROUTINE GEOMETRY_VARIABLES_UPDATE_VIEW(SELF, BLOCK_INDEX)
    CLASS (GEOMETRY_VARIABLES) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE GEOMETRY_VARIABLES_UPDATE_VIEW
  
  SUBROUTINE GEOMETRY_VARIABLES_FINAL (SELF)
    CLASS (GEOMETRY_VARIABLES) :: SELF
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE GEOMETRY_VARIABLES_FINAL
  
END MODULE FIELD_VARIABLES_MOD
