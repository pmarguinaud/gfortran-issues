
MODULE SURFACE_VIEWS_DIAGNOSTIC_MODULE
  ! The SURFACE_VIEWS type contains namespaced access to groups of
  ! array pointers according to individual surface variable groups.
  !
  !
  ! Variable naming conventions
  ! ---------------------------
  ! The top-level type `SURFACE_VIEWS_TYPE` holds multiple group
  ! types, each prefixed with `GSP_` for prognostic and `GSD_` for
  ! diagnostic variable groups.
  !
  ! Each group type holds a list of array views (pointers to
  ! sub-sections of the gobal array), each prefixed with `P` to
  ! indicate a thread-local view pointer. The backend storage for each
  ! of these view pointers is provided by `FIELD_2D/FIELD_3D` objects,
  ! a reference to which is also stored on the group types and
  ! prefixed with `F_`.

USE PARKIND1, ONLY: JPIM, JPRB
USE FIELD_MODULE
USE SURFACE_VARIABLES_MOD

IMPLICIT NONE

TYPE SURFACE_VIEW_GROUP_VARSF
  REAL(KIND=JPRB), POINTER :: PZ0F(:)   ! gravity * surface roughness length
!>REAL(KIND=JPRB)          :: PZ0F (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALBF(:)   ! surface shortwave albedo
!>REAL(KIND=JPRB)          :: PALBF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PEMISF(:)   ! surface longwave emissivity
!>REAL(KIND=JPRB)          :: PEMISF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGETRL(:)   ! standard deviation of orography
!>REAL(KIND=JPRB)          :: PGETRL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSM(:)   ! land-sea mask
!>REAL(KIND=JPRB)          :: PLSM (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVEG(:)   ! vegetation cover
!>REAL(KIND=JPRB)          :: PVEG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVRLAN(:)   ! anisotropy of the sub-grid scale orography
!>REAL(KIND=JPRB)          :: PVRLAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVRLDI(:)   ! angle of the direction of orography with the x axis
!>REAL(KIND=JPRB)          :: PVRLDI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSIG(:)   ! characteristic orographic slope
!>REAL(KIND=JPRB)          :: PSIG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALBSF(:)   ! soil shortwave albedo
!>REAL(KIND=JPRB)          :: PALBSF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAN(:)   ! fraction of land
!>REAL(KIND=JPRB)          :: PLAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSST(:)   ! (open) sea surface temperature
!>REAL(KIND=JPRB)          :: PSST (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSS(:)   ! sea surface salinity
!>REAL(KIND=JPRB)          :: PSSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLZ0H(:)   ! logarithm of roughness length for heat
!>REAL(KIND=JPRB)          :: PLZ0H (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCVL(:)   ! low vegetation cover
!>REAL(KIND=JPRB)          :: PCVL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCO2TYP(:)   ! CO2 photosynthesis type (c3/c4) for low vegetation cover
!>REAL(KIND=JPRB)          :: PCO2TYP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCVH(:)   ! high vegetation cover
!>REAL(KIND=JPRB)          :: PCVH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCUR(:)   ! urban cover
!>REAL(KIND=JPRB)          :: PCUR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTVL(:)   ! low vegetation type
!>REAL(KIND=JPRB)          :: PTVL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTVH(:)   ! high vegetation type
!>REAL(KIND=JPRB)          :: PTVH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAIL(:)   ! low vegetation LAI
!>REAL(KIND=JPRB)          :: PLAIL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAIH(:)   ! high vegetation LAI
!>REAL(KIND=JPRB)          :: PLAIH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFWET(:)   ! wetland fraction
!>REAL(KIND=JPRB)          :: PFWET (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSOTY(:)   ! soil type
!>REAL(KIND=JPRB)          :: PSOTY (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCLK(:)   ! lake cover
!>REAL(KIND=JPRB)          :: PCLK (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDL(:)   ! lake depth
!>REAL(KIND=JPRB)          :: PDL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCI(:)   ! sea ice fraction
!>REAL(KIND=JPRB)          :: PCI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUCUR(:)   ! U-component of the ocean current
!>REAL(KIND=JPRB)          :: PUCUR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVCUR(:)   ! V-component of the ocean current
!>REAL(KIND=JPRB)          :: PVCUR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZ0RLF(:)   ! gravity * vegetation roughness length
!>REAL(KIND=JPRB)          :: PZ0RLF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCGPP(:)   ! GPP bias correction factor
!>REAL(KIND=JPRB)          :: PCGPP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCREC(:)   ! REC bias correction factor
!>REAL(KIND=JPRB)          :: PCREC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSDFOR(:)   ! SD filtered orography
!>REAL(KIND=JPRB)          :: PSDFOR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALUVP(:)   ! MODIS-derived parallel albedo for shortwave radiation
!>REAL(KIND=JPRB)          :: PALUVP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALUVD(:)   ! MODIS-derived diffuse albedo for shortwave radiation
!>REAL(KIND=JPRB)          :: PALUVD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALNIP(:)   ! MODIS-derived parallel albedo for longwave radiation
!>REAL(KIND=JPRB)          :: PALNIP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALNID(:)   ! MODIS-derived diffuse albedo for longwave radiation
!>REAL(KIND=JPRB)          :: PALNID (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFP1(:)   ! surface orography in the 2nd part of FULLPOS-927
!>REAL(KIND=JPRB)          :: PFP1 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSO2DD(:)   ! sulphate dry dep velocity
!>REAL(KIND=JPRB)          :: PSO2DD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDMSO(:)   ! oceanic DMS
!>REAL(KIND=JPRB)          :: PDMSO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PURBF(:)   ! Urban fraction
!>REAL(KIND=JPRB)          :: PURBF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFCA1(:)   ! Fraction of calcite over dust 1st bin
!>REAL(KIND=JPRB)          :: PFCA1 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFCA2(:)   ! Fraction of calcite over dust 2nd bin
!>REAL(KIND=JPRB)          :: PFCA2 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAERDEP(:)   ! dust emission potential
!>REAL(KIND=JPRB)          :: PAERDEP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAERLTS(:)   ! dust lifting threshold speed
!>REAL(KIND=JPRB)          :: PAERLTS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAERSCC(:)   ! dust soil clay content
!>REAL(KIND=JPRB)          :: PAERSCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDSF(:)   ! dust source function
!>REAL(KIND=JPRB)          :: PDSF (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PCHEMFLXO   ! total chemistry flux (emissions + deposition)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PCHEMWDFLX   ! wet deposition chemistry flux
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PCHEMDDFLX   ! dry deposition chemistry flux
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PCHEMDV   ! chemistry deposition velocity
  REAL(KIND=JPRB), POINTER :: PNUDM(:)   ! nudging mask
!>REAL(KIND=JPRB)          :: PNUDM (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PEMIS2D   ! 2D emission fields for composition
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PEMIS2DAUX   ! 2D emission auxiliary fields for composition
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VARSF), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_Z0F=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALBF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_EMISF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_GETRL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSM=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VEG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VRLAN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VRLDI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SIG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALBSF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SST=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LZ0H=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CVL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CO2TYP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CVH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CUR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TVL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TVH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAIL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAIH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FWET=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SOTY=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CLK=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_UCUR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VCUR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_Z0RLF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CGPP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CREC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SDFOR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALUVP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALUVD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALNIP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALNID=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FP1=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SO2DD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DMSO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_URBF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FCA1=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FCA2=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AERDEP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AERLTS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AERSCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DSF=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_CHEMFLXO
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_CHEMWDFLX
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_CHEMDDFLX
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_CHEMDV
  CLASS (FIELD_2RB), POINTER :: F_NUDM=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_EMIS2D
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_EMIS2DAUX
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VARSF_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VARSF_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VARSF_FINAL
END TYPE SURFACE_VIEW_GROUP_VARSF

TYPE SURFACE_VIEW_GROUP_VCLIH
  REAL(KIND=JPRB), POINTER :: PTCCH(:)   ! total convective cloudiness
!>REAL(KIND=JPRB)          :: PTCCH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSCCH(:)   ! convective cloud summit
!>REAL(KIND=JPRB)          :: PSCCH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PBCCH(:)   ! convective cloud base
!>REAL(KIND=JPRB)          :: PBCCH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPBLH(:)   ! PBL height
!>REAL(KIND=JPRB)          :: PPBLH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSPSH(:)   ! variable for prognostic convection scheme (ALARO)
!>REAL(KIND=JPRB)          :: PSPSH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PQSH(:)   ! surface moisture historic variable (used by TOUCANS)
!>REAL(KIND=JPRB)          :: PQSH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPCL(:)   ! 
!>REAL(KIND=JPRB)          :: PPCL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPSL(:)   ! 
!>REAL(KIND=JPRB)          :: PPSL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPCN(:)   ! 
!>REAL(KIND=JPRB)          :: PPCN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPSN(:)   ! 
!>REAL(KIND=JPRB)          :: PPSN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PEVA(:)   ! 
!>REAL(KIND=JPRB)          :: PEVA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIH), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_TCCH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SCCH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_BCCH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PBLH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SPSH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_QSH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PCL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PSL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PCN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PSN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_EVA=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIH_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIH_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIH_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIH

TYPE SURFACE_VIEW_GROUP_VCLIK
  REAL(KIND=JPRB), POINTER :: PUDGRO(:)   ! ud top position (accsu)
!>REAL(KIND=JPRB)          :: PUDGRO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIK), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_UDGRO=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIK_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIK_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIK_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIK

TYPE SURFACE_VIEW_GROUP_VCLIP
  REAL(KIND=JPRB), POINTER :: PTPC(:)   ! climatological deep layer temperature
!>REAL(KIND=JPRB)          :: PTPC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PWPC(:)   ! climatological deep layer moisture
!>REAL(KIND=JPRB)          :: PWPC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIP), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_TPC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_WPC=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIP_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIP_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIP_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIP

TYPE SURFACE_VIEW_GROUP_VCLIV
  REAL(KIND=JPRB), POINTER :: PARG(:)   ! silt percentage within soil
!>REAL(KIND=JPRB)          :: PARG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSAB(:)   ! percentage of sand within the soil
!>REAL(KIND=JPRB)          :: PSAB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PD2(:)   ! soil depth
!>REAL(KIND=JPRB)          :: PD2 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIVEG(:)   ! type of vegetation
!>REAL(KIND=JPRB)          :: PIVEG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PRSMIN(:)   ! stomatal minimum resistance
!>REAL(KIND=JPRB)          :: PRSMIN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAI(:)   ! leaf area index
!>REAL(KIND=JPRB)          :: PLAI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PHV(:)   ! resistance to evapotranspiration
!>REAL(KIND=JPRB)          :: PHV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZ0H(:)   ! gravity * roughness length for heat
!>REAL(KIND=JPRB)          :: PZ0H (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALS(:)   ! albedo of bare ground
!>REAL(KIND=JPRB)          :: PALS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALV(:)   ! albedo of vegetation
!>REAL(KIND=JPRB)          :: PALV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIV), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_ARG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SAB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_D2=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IVEG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_RSMIN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_HV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_Z0H=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALV=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIV_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIV_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIV_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIV

TYPE SURFACE_VIEW_GROUP_VCLIA
  REAL(KIND=JPRB), POINTER :: PSEA(:)   ! aerosol sea
!>REAL(KIND=JPRB)          :: PSEA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAN(:)   ! aerosol land
!>REAL(KIND=JPRB)          :: PLAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSOO(:)   ! aerosol soot
!>REAL(KIND=JPRB)          :: PSOO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDES(:)   ! aerosol desert
!>REAL(KIND=JPRB)          :: PDES (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSUL(:)   ! aerosol sulfate
!>REAL(KIND=JPRB)          :: PSUL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVOL(:)   ! aerosol volcano
!>REAL(KIND=JPRB)          :: PVOL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIA), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_SEA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SOO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DES=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SUL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VOL=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIA_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIA_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIA_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIA

TYPE SURFACE_VIEW_GROUP_VCLIN
  REAL(KIND=JPRB), POINTER :: PTOP(:)   ! index of convective cloud top
!>REAL(KIND=JPRB)          :: PTOP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PBAS(:)   ! index of convective cloud base
!>REAL(KIND=JPRB)          :: PBAS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PACPR(:)   ! averaged convective precipitaion rate
!>REAL(KIND=JPRB)          :: PACPR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PACCPR(:)   ! accumulated total precipitaion for assimilation
!>REAL(KIND=JPRB)          :: PACCPR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PACCPR5(:)   ! accumulated total precipitaion for assimilation (trajectory)
!>REAL(KIND=JPRB)          :: PACCPR5 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIN), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_TOP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_BAS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ACPR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ACCPR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ACCPR5=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIN_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIN_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIN_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIN

TYPE SURFACE_VIEW_GROUP_VDIAGO2
  REAL(KIND=JPRB), POINTER :: POCDEP(:)   ! bottom layer depth
!>REAL(KIND=JPRB)          :: POCDEP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUSTRC(:)   ! taux clim.
!>REAL(KIND=JPRB)          :: PUSTRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVSTRC(:)   ! tauy clim.
!>REAL(KIND=JPRB)          :: PVSTRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VDIAGO2), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_OCDEP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_USTRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VSTRC=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VDIAGO2_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VDIAGO2_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VDIAGO2_FINAL
END TYPE SURFACE_VIEW_GROUP_VDIAGO2

TYPE SURFACE_VIEW_GROUP_VDIAGO3
  REAL(KIND=JPRB), POINTER :: PDIFM(:,:)   ! viscosity
!>REAL(KIND=JPRB)          :: PDIFM (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PDIFT(:,:)   ! diff. coef. of temp
!>REAL(KIND=JPRB)          :: PDIFT (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PDIFS(:,:)   ! diff. coef. of salinity
!>REAL(KIND=JPRB)          :: PDIFS (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PADVT(:,:)   ! correction term for temp.
!>REAL(KIND=JPRB)          :: PADVT (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PADVS(:,:)   ! correction term for sal.
!>REAL(KIND=JPRB)          :: PADVS (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PTRI0(:,:)   ! coef. for solving matrix.
!>REAL(KIND=JPRB)          :: PTRI0 (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PTRI1(:,:)   ! coef. for solving matrix.
!>REAL(KIND=JPRB)          :: PTRI1 (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PSWDK(:,:)   ! radiation term
!>REAL(KIND=JPRB)          :: PSWDK (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PZO(:,:)   ! depth of layer
!>REAL(KIND=JPRB)          :: PZO (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PHO(:,:)   ! depth of interface layer
!>REAL(KIND=JPRB)          :: PHO (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PDO(:,:)   ! layer thickness
!>REAL(KIND=JPRB)          :: PDO (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PHO_INV(:,:)   ! 1 / YHO
!>REAL(KIND=JPRB)          :: PHO_INV (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PUOC(:,:)   ! U velocity clim.
!>REAL(KIND=JPRB)          :: PUOC (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PVOC(:,:)   ! V velocity clim.
!>REAL(KIND=JPRB)          :: PVOC (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: POTKE(:,:)   ! ocean turb. kin. energy
!>REAL(KIND=JPRB)          :: POTKE (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VDIAGO3), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_DIFM=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_DIFT=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_DIFS=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_ADVT=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_ADVS=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_TRI0=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_TRI1=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_SWDK=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_ZO=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_HO=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_DO=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_HO_INV=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_UOC=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_VOC=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_OTKE=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VDIAGO3_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VDIAGO3_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VDIAGO3_FINAL
END TYPE SURFACE_VIEW_GROUP_VDIAGO3

TYPE SURFACE_VIEW_GROUP_VDIAG
  REAL(KIND=JPRB), POINTER :: PLSP(:)   ! Large scale precipitation
!>REAL(KIND=JPRB)          :: PLSP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCP(:)   ! Convective precipitation
!>REAL(KIND=JPRB)          :: PCP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSF(:)   ! Snowfall
!>REAL(KIND=JPRB)          :: PSF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFZRA(:)   ! Freezing rain
!>REAL(KIND=JPRB)          :: PFZRA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PBLD(:)   ! Boundary layer dissipation
!>REAL(KIND=JPRB)          :: PBLD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSHF(:)   ! Surface sensible heat flux
!>REAL(KIND=JPRB)          :: PSSHF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSLHF(:)   ! Surface latent heat flux
!>REAL(KIND=JPRB)          :: PSLHF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PNEE(:)   ! Surface net ecosystem exchange of CO2
!>REAL(KIND=JPRB)          :: PNEE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGPP(:)   ! Surface gross primary production of CO2
!>REAL(KIND=JPRB)          :: PGPP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PREC(:)   ! Surface ecosystem respiration of CO2
!>REAL(KIND=JPRB)          :: PREC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMSL(:)   ! Mean sea level pressure
!>REAL(KIND=JPRB)          :: PMSL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSP(:)   ! Surface pressure
!>REAL(KIND=JPRB)          :: PSP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCC(:)   ! Total cloud cover
!>REAL(KIND=JPRB)          :: PTCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10U(:)   ! U-wind at 10 m
!>REAL(KIND=JPRB)          :: P10U (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10V(:)   ! V-wind at 10 m
!>REAL(KIND=JPRB)          :: P10V (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P2T(:)   ! Temperature at 2 m
!>REAL(KIND=JPRB)          :: P2T (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P2D(:)   ! Dewpoint temperature at 2 m
!>REAL(KIND=JPRB)          :: P2D (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P2SH(:)   ! Specific humidity at 2 m
!>REAL(KIND=JPRB)          :: P2SH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSR(:)   ! Surface solar radiation
!>REAL(KIND=JPRB)          :: PSSR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTR(:)   ! Surface thermal radiation
!>REAL(KIND=JPRB)          :: PSTR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTSR(:)   ! Top solar radiation
!>REAL(KIND=JPRB)          :: PTSR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTTR(:)   ! Top thermal radiation
!>REAL(KIND=JPRB)          :: PTTR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PEWSS(:)   ! Instantaneous surface U-wind stress
!>REAL(KIND=JPRB)          :: PEWSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PNSSS(:)   ! Instantaneous surface V-wind stress
!>REAL(KIND=JPRB)          :: PNSSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PE(:)   ! Water evaporation
!>REAL(KIND=JPRB)          :: PE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPEV(:)   ! Potential evaporation
!>REAL(KIND=JPRB)          :: PPEV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCCC(:)   ! Convective cloud cover
!>REAL(KIND=JPRB)          :: PCCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLCC(:)   ! Low cloud cover
!>REAL(KIND=JPRB)          :: PLCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMCC(:)   ! Medium cloud cover
!>REAL(KIND=JPRB)          :: PMCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PHCC(:)   ! High cloud cover
!>REAL(KIND=JPRB)          :: PHCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLGWS(:)   ! Zonal gravity wave stress
!>REAL(KIND=JPRB)          :: PLGWS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMGWS(:)   ! Meridian gravity wave stress
!>REAL(KIND=JPRB)          :: PMGWS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGWD(:)   ! Gravity wave dissipation
!>REAL(KIND=JPRB)          :: PGWD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMX2T(:)   ! Maximum temperature at 2 m
!>REAL(KIND=JPRB)          :: PMX2T (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMN2T(:)   ! Minimum temperature at 2 m
!>REAL(KIND=JPRB)          :: PMN2T (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMX2T6   ! Bins for maximum temperature at 2 m since last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMN2T6   ! Bins for minimum temperature at 2 m since last 6 hours
  REAL(KIND=JPRB), POINTER :: PRO(:)   ! Runoff (total)
!>REAL(KIND=JPRB)          :: PRO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSRO(:)   ! Runoff surface
!>REAL(KIND=JPRB)          :: PSRO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSRO(:)   ! Runoff sub-surface
!>REAL(KIND=JPRB)          :: PSSRO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALB(:)   ! (surface shortwave) albedo
!>REAL(KIND=JPRB)          :: PALB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIEWSS(:)   ! Instantaneous surface zonal component of stress
!>REAL(KIND=JPRB)          :: PIEWSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PINSSS(:)   ! Instantaneous surface meridian component of stress
!>REAL(KIND=JPRB)          :: PINSSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PISSHF(:)   ! Instantaneous surface heat flux
!>REAL(KIND=JPRB)          :: PISSHF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIE(:)   ! Instantaneous surface moisture flux
!>REAL(KIND=JPRB)          :: PIE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PINEE(:)   ! Instantaneous net ecosystem exchange of CO2
!>REAL(KIND=JPRB)          :: PINEE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIGPP(:)   ! Instantaneous gross primary production of CO2
!>REAL(KIND=JPRB)          :: PIGPP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIREC(:)   ! Instantaneous ecosystem respiration of CO2
!>REAL(KIND=JPRB)          :: PIREC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PICH4(:)   ! Instantaneous wetland CH4 flux
!>REAL(KIND=JPRB)          :: PICH4 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCSF(:)   ! Convective snow fall
!>REAL(KIND=JPRB)          :: PCSF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSSF(:)   ! Large scale snowfall
!>REAL(KIND=JPRB)          :: PLSSF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMXTPR(:)   ! Max precip rate since last post-processing
!>REAL(KIND=JPRB)          :: PMXTPR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMNTPR(:)   ! Min precip rate since last post-processing
!>REAL(KIND=JPRB)          :: PMNTPR (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMXTPR6   ! Max precip rate in last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMNTPR6   ! Min precip rate in last 6 hours
  REAL(KIND=JPRB), POINTER :: PTPR(:)   ! Total precipitation rate
!>REAL(KIND=JPRB)          :: PTPR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSRR(:)   ! Large scale rain rate
!>REAL(KIND=JPRB)          :: PLSRR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCRR(:)   ! Convective rain rate
!>REAL(KIND=JPRB)          :: PCRR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSSFR(:)   ! Large scale snowfall rate
!>REAL(KIND=JPRB)          :: PLSSFR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCSFR(:)   ! Convective snowfall rate
!>REAL(KIND=JPRB)          :: PCSFR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPTYPE(:)   ! Precipitation type
!>REAL(KIND=JPRB)          :: PPTYPE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PILSPF(:)   ! Large-scale precipitation fraction (inst.)
!>REAL(KIND=JPRB)          :: PILSPF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZ0F(:)   ! Gravity * surface roughness length
!>REAL(KIND=JPRB)          :: PZ0F (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLZ0H(:)   ! Logarithm of z0 times heat flux
!>REAL(KIND=JPRB)          :: PLZ0H (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVIWVE(:)   ! Vertical integral of eastward water vapour flux
!>REAL(KIND=JPRB)          :: PVIWVE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVIWVN(:)   ! Vertical integral of northward water vapour flux
!>REAL(KIND=JPRB)          :: PVIWVN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCW(:)   ! Total water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCWV(:)   ! Total water vapor content in a vertical column
!>REAL(KIND=JPRB)          :: PTCWV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCLW(:)   ! Total liquid water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCLW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCIW(:)   ! Total ice water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCIW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCRW(:)   ! Total rain water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCRW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCSW(:)   ! Total snow water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCSW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCSLW(:)   ! Total supercooled liquid water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCSLW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSRD(:)   ! Downward surface solar radiation
!>REAL(KIND=JPRB)          :: PSSRD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTRD(:)   ! Downward surface thermic radiation
!>REAL(KIND=JPRB)          :: PSTRD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSRDC(:)   ! Clear-sky downward surface solar radiation
!>REAL(KIND=JPRB)          :: PSSRDC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTRDC(:)   ! Claer-sky downward surface thermal radiation
!>REAL(KIND=JPRB)          :: PSTRDC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PBLH(:)   ! Height of boundary layer
!>REAL(KIND=JPRB)          :: PBLH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSUND(:)   ! Sunshine duration
!>REAL(KIND=JPRB)          :: PSUND (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSPAR(:)   ! Surface downward PARadiation
!>REAL(KIND=JPRB)          :: PSPAR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSUVB(:)   ! Surface downward UV-B radiation
!>REAL(KIND=JPRB)          :: PSUVB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSFDIR(:)   ! Surface total sky direct downward SW radiation
!>REAL(KIND=JPRB)          :: PSFDIR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSCDIR(:)   ! Surface clear-sky direct downward SW radiation
!>REAL(KIND=JPRB)          :: PSCDIR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSDSRP(:)   ! Surface total-sky direct beam downward SW radiation
!>REAL(KIND=JPRB)          :: PSDSRP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCAPE(:)   ! Conv.avail.potential energy (CAPE)
!>REAL(KIND=JPRB)          :: PCAPE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMUCAPE(:)   ! Maximum unstable CAPE
!>REAL(KIND=JPRB)          :: PMUCAPE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMLCAPE50(:)   ! CAPE from 50 hPa mixed-layer
!>REAL(KIND=JPRB)          :: PMLCAPE50 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMLCAPE100(:)   ! CAPE from 100 hPa mixed-layer
!>REAL(KIND=JPRB)          :: PMLCAPE100 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPDEPL(:)   ! Parcel departure level-pressure for MUCAPE
!>REAL(KIND=JPRB)          :: PPDEPL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCAPES(:)   ! CAPE-Shear
!>REAL(KIND=JPRB)          :: PCAPES (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMXCAP6   ! Bins for maximum CAPE in last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMXCAPS6   ! Bins for maximum CAPE-Shear in last 6 hours
  REAL(KIND=JPRB), POINTER :: PTROPOTP(:)   ! Pressure of thermal Tropopause
!>REAL(KIND=JPRB)          :: PTROPOTP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTSRC(:)   ! Top solar radiation clear sky
!>REAL(KIND=JPRB)          :: PTSRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTTRC(:)   ! Top thermal radiation clear sky
!>REAL(KIND=JPRB)          :: PTTRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSRC(:)   ! Surface solar radiation clear sky
!>REAL(KIND=JPRB)          :: PSSRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTRC(:)   ! Surface thermal radiation clear sky
!>REAL(KIND=JPRB)          :: PSTRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PES(:)   ! Evaporation of snow
!>REAL(KIND=JPRB)          :: PES (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSMLT(:)   ! Snow melt
!>REAL(KIND=JPRB)          :: PSMLT (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10FG(:)   ! Wind gust at 10 m (max since previous pp)
!>REAL(KIND=JPRB)          :: P10FG (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: P10FG6   ! Bins for wind gust at 10 m (max since last 6 hours)
  REAL(KIND=JPRB), POINTER :: P10FGCV(:)   ! convective wind gust at 10m for current time level (m/s)
!>REAL(KIND=JPRB)          :: P10FGCV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PI10FG(:)   ! Wind gust at 10 m ("instantaneous")
!>REAL(KIND=JPRB)          :: PI10FG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSPF(:)   ! Large scale precipitation fraction
!>REAL(KIND=JPRB)          :: PLSPF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCO3(:)   ! Total ozone content in a vertical column
!>REAL(KIND=JPRB)          :: PTCO3 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVIMD(:)   ! Vertically integrated mass divergence
!>REAL(KIND=JPRB)          :: PVIMD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSPARC(:)   ! Surface clear-sky parallel radiation
!>REAL(KIND=JPRB)          :: PSPARC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTINC(:)   ! Top of atmosphere incident solar radiation
!>REAL(KIND=JPRB)          :: PSTINC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCBASE(:)   ! Cloud base level
!>REAL(KIND=JPRB)          :: PCBASE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P0DEGL(:)   ! Zero deg. level
!>REAL(KIND=JPRB)          :: P0DEGL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PM10DEGL(:)   ! -10 deg. level
!>REAL(KIND=JPRB)          :: PM10DEGL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVISIH(:)   ! Horizontal visibility
!>REAL(KIND=JPRB)          :: PVISIH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCIN(:)   ! CIN
!>REAL(KIND=JPRB)          :: PCIN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMLCIN50(:)   ! CIN from 50 hPa mixed-layer
!>REAL(KIND=JPRB)          :: PMLCIN50 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMLCIN100(:)   ! CIN from 100 hPa mixed-layer
!>REAL(KIND=JPRB)          :: PMLCIN100 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PKINDEX(:)   ! Convective K-Index
!>REAL(KIND=JPRB)          :: PKINDEX (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTTINDEX(:)   ! Convective TT-Index
!>REAL(KIND=JPRB)          :: PTTINDEX (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCBASEA(:)   ! Cloud base aviation
!>REAL(KIND=JPRB)          :: PCBASEA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCTOPC(:)   ! Cloud top convective
!>REAL(KIND=JPRB)          :: PCTOPC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZTWETB0(:)   ! Height of 0 deg wet bulb temperature
!>REAL(KIND=JPRB)          :: PZTWETB0 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZTWETB1(:)   ! Height of 1 deg wet bulb temperature
!>REAL(KIND=JPRB)          :: PZTWETB1 (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PTCGHG   ! Total column greenhouse gases
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PTCCHEM   ! Total column chemistry
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:,:) :: PAERODIAG   ! Per-aerosol-type diagnostics
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:,:) :: PAERO_WVL_DIAG   ! Per-wavelength aerosol optical diagnostics
  REAL(KIND=JPRB), POINTER :: P100U(:)   ! 100m zonal wind
!>REAL(KIND=JPRB)          :: P100U (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P100V(:)   ! 100m meridional wind
!>REAL(KIND=JPRB)          :: P100V (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P200U(:)   ! 200m zonal wind
!>REAL(KIND=JPRB)          :: P200U (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P200V(:)   ! 200m meridional wind
!>REAL(KIND=JPRB)          :: P200V (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZUST(:)   ! Friction velocity
!>REAL(KIND=JPRB)          :: PZUST (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10NU(:)   ! 10m zonal neutral wind
!>REAL(KIND=JPRB)          :: P10NU (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10NV(:)   ! 10m meridional neutral wind
!>REAL(KIND=JPRB)          :: P10NV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDNDZN(:)   ! Minimum vertical refractivity gradient
!>REAL(KIND=JPRB)          :: PDNDZN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDNDZA(:)   ! Mean vertical refractivity gradient
!>REAL(KIND=JPRB)          :: PDNDZA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDCTB(:)   ! Duct base height
!>REAL(KIND=JPRB)          :: PDCTB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTPLB(:)   ! Trapping layer base height
!>REAL(KIND=JPRB)          :: PTPLB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTPLT(:)   ! Trapping layer top height
!>REAL(KIND=JPRB)          :: PTPLT (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODSS(:)   ! optical depth sea salt aerosols
!>REAL(KIND=JPRB)          :: PODSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODDU(:)   ! optical depth dust aerosols
!>REAL(KIND=JPRB)          :: PODDU (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODOM(:)   ! optical depth organic m. aerosols
!>REAL(KIND=JPRB)          :: PODOM (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODBC(:)   ! optical depth black C aerosols
!>REAL(KIND=JPRB)          :: PODBC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODSU(:)   ! optical depth sulphate aerosols
!>REAL(KIND=JPRB)          :: PODSU (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODNI(:)   ! optical depth nitrate aerosols
!>REAL(KIND=JPRB)          :: PODNI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODAM(:)   ! optical depth ammonium aerosols
!>REAL(KIND=JPRB)          :: PODAM (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODSOA(:)   ! optical depth secondary organic aerosols
!>REAL(KIND=JPRB)          :: PODSOA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODVFA(:)   ! optical depth volcanic flying ash
!>REAL(KIND=JPRB)          :: PODVFA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODVSU(:)   ! optical depth volcanic sulphate aerosols
!>REAL(KIND=JPRB)          :: PODVSU (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODTOACC(:)   ! optical depth total aerosol accumulated
!>REAL(KIND=JPRB)          :: PODTOACC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAEPM1(:)   ! particulate matter le 1 um
!>REAL(KIND=JPRB)          :: PAEPM1 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAEPM25(:)   ! particulate matter le 2.5um
!>REAL(KIND=JPRB)          :: PAEPM25 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAEPM10(:)   ! particulate matter le 10 um
!>REAL(KIND=JPRB)          :: PAEPM10 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUVBED(:)   ! UV biologically effective dose
!>REAL(KIND=JPRB)          :: PUVBED (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUVBEDCS(:)   ! UV biologically effective dose clear sky
!>REAL(KIND=JPRB)          :: PUVBEDCS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLITOTI(:)   ! instantaneous total lightning flash density
!>REAL(KIND=JPRB)          :: PLITOTI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLICGI(:)   ! instantaneous cloud-to-ground lightning flash density
!>REAL(KIND=JPRB)          :: PLICGI (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PLITOTA6   ! Bins for averaged total lightning over last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PLICGA6   ! Bins for averaged cloud-to-ground lightning over last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:,:) :: PPTYPEOCC6   ! Bins for accum freq of each precip type over last 6 hours
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VDIAG), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_LSP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FZRA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_BLD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSHF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SLHF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_NEE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_GPP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_REC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MSL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10U=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10V=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_2T=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_2D=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_2SH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TSR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TTR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_EWSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_NSSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_E=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PEV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_HCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LGWS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MGWS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_GWD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MX2T=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MN2T=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MX2T6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MN2T6
  CLASS (FIELD_2RB), POINTER :: F_RO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SRO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSRO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IEWSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_INSSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ISSHF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_INEE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IGPP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IREC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ICH4=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CSF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSSF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MXTPR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MNTPR=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MXTPR6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MNTPR6
  CLASS (FIELD_2RB), POINTER :: F_TPR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSRR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CRR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSSFR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CSFR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PTYPE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ILSPF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_Z0F=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LZ0H=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VIWVE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VIWVN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCWV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCLW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCIW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCRW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCSW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCSLW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSRD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STRD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSRDC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STRDC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_BLH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SUND=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SPAR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SUVB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SFDIR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SCDIR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SDSRP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CAPE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MUCAPE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MLCAPE50=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MLCAPE100=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PDEPL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CAPES=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MXCAP6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MXCAPS6
  CLASS (FIELD_2RB), POINTER :: F_TROPOTP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TSRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TTRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ES=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SMLT=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10FG=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_10FG6
  CLASS (FIELD_2RB), POINTER :: F_10FGCV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_I10FG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSPF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCO3=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VIMD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SPARC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STINC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CBASE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_0DEGL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_M10DEGL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VISIH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CIN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MLCIN50=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MLCIN100=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_KINDEX=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TTINDEX=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CBASEA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CTOPC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ZTWETB0=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ZTWETB1=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_TCGHG
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_TCCHEM
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:,:) :: F_AERODIAG
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:,:) :: F_AERO_WVL_DIAG
  CLASS (FIELD_2RB), POINTER :: F_100U=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_100V=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_200U=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_200V=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ZUST=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10NU=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10NV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DNDZN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DNDZA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DCTB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TPLB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TPLT=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODDU=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODOM=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODBC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODSU=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODNI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODAM=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODSOA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODVFA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODVSU=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODTOACC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AEPM1=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AEPM25=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AEPM10=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_UVBED=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_UVBEDCS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LITOTI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LICGI=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_LITOTA6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_LICGA6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:,:) :: F_PTYPEOCC6
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VDIAG_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VDIAG_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VDIAG_FINAL
END TYPE SURFACE_VIEW_GROUP_VDIAG

TYPE SURFACE_VIEW_GROUP_SATSIM
  REAL(KIND=JPRB), POINTER :: PCLBT(:,:)   ! Cloudy brightness temperature
!>REAL(KIND=JPRB)          :: PCLBT (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_SMD%NLEVS)
  REAL(KIND=JPRB), POINTER :: PCSBT(:,:)   ! Clear-sky brightness temperature
!>REAL(KIND=JPRB)          :: PCSBT (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_SMD%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_SATSIM), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_CLBT=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_CSBT=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_SATSIM_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_SATSIM_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_SATSIM_FINAL
END TYPE SURFACE_VIEW_GROUP_SATSIM

TYPE SURFACE_VIEW_GROUP_WAVES
  REAL(KIND=JPRB), POINTER :: PCHAR(:)   ! Charnock parameter as modified by the wave model.
!>REAL(KIND=JPRB)          :: PCHAR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCHARHQ(:)   ! Charnock for heat and moisture from the wave model.
!>REAL(KIND=JPRB)          :: PCHARHQ (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUSTOKES(:)   ! U-component of the surface Stokes drift.
!>REAL(KIND=JPRB)          :: PUSTOKES (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVSTOKES(:)   ! V-component of the surface Stokes drift.
!>REAL(KIND=JPRB)          :: PVSTOKES (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTAUOCX(:)   ! U-component of the Momentum flux to ocean.
!>REAL(KIND=JPRB)          :: PTAUOCX (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTAUOCY(:)   ! V-component of the Momentum flux to ocean.
!>REAL(KIND=JPRB)          :: PTAUOCY (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPHIOC(:)   ! Energy flux to ocean.
!>REAL(KIND=JPRB)          :: PPHIOC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PWSEMEAN(:)   ! Windsea variance.
!>REAL(KIND=JPRB)          :: PWSEMEAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PWSFMEAN(:)   ! Windsea mean frequency.
!>REAL(KIND=JPRB)          :: PWSFMEAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_WAVES), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_CHAR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CHARHQ=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_USTOKES=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VSTOKES=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TAUOCX=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TAUOCY=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PHIOC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_WSEMEAN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_WSFMEAN=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_WAVES_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_WAVES_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_WAVES_FINAL
END TYPE SURFACE_VIEW_GROUP_WAVES

TYPE SURFACE_VIEW_GROUP_WAM
  REAL(KIND=JPRB), POINTER :: PU10N(:)   ! 10m neutral wind U-component passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PU10N (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PV10N(:)   ! 10m neutral wind V-component passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PV10N (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PRHO(:)   ! surface density passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PRHO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZIL(:)   ! ZI/L passed to the wave model (used for gustiness in WAM).
!>REAL(KIND=JPRB)          :: PZIL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCIF(:)   ! Sea ice fraction passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PCIF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCLK(:)   ! Lake cover passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PCLK (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUCURW(:)   ! Ocean current    U-component passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PUCURW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVCURW(:)   ! Ocean current    V-component passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PVCURW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_WAM), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_U10N=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_V10N=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_RHO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ZIL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CIF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CLK=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_UCURW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VCURW=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_WAM_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_WAM_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_WAM_FINAL
END TYPE SURFACE_VIEW_GROUP_WAM

TYPE SURFACE_VIEW_GROUP_PRECFRAC
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_PRECFRAC), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_PRECFRAC_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_PRECFRAC_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_PRECFRAC_FINAL
END TYPE SURFACE_VIEW_GROUP_PRECFRAC

TYPE SURFACE_VIEW_GROUP_VEXTRA
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VEXTRA), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VEXTRA_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VEXTRA_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VEXTRA_FINAL
END TYPE SURFACE_VIEW_GROUP_VEXTRA

TYPE SURFACE_VIEW_GROUP_VEXTRDI
  REAL(KIND=JPRB), POINTER :: PXEDR(:,:)   ! Eddy diffusivity rate
!>REAL(KIND=JPRB)          :: PXEDR (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_DID%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VEXTRDI), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_XEDR=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VEXTRDI_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VEXTRDI_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VEXTRDI_FINAL
END TYPE SURFACE_VIEW_GROUP_VEXTRDI

TYPE SURFACE_VIEW_GROUP_VPRECIP
  REAL(KIND=JPRB), POINTER :: PPRECIP(:,:)   ! Diagnostic of precipitations type
!>REAL(KIND=JPRB)          :: PPRECIP (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_XPD%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VPRECIP), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_PRECIP=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VPRECIP_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VPRECIP_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VPRECIP_FINAL
END TYPE SURFACE_VIEW_GROUP_VPRECIP

TYPE SURFACE_VIEW_GROUP_VPRECIP2
  REAL(KIND=JPRB), POINTER :: PPRECIP2(:,:)   ! Diagnostic of precipitations type
!>REAL(KIND=JPRB)          :: PPRECIP2 (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_XP2D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VPRECIP2), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_PRECIP2=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VPRECIP2_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VPRECIP2_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VPRECIP2_FINAL
END TYPE SURFACE_VIEW_GROUP_VPRECIP2

TYPE SURFACE_VIEW_GROUP_VEXTR2
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VEXTR2), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VEXTR2_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VEXTR2_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VEXTR2_FINAL
END TYPE SURFACE_VIEW_GROUP_VEXTR2

TYPE SURFACE_VIEW_GROUP_SFORC
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_SFORC), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_SFORC_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_SFORC_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_SFORC_FINAL
END TYPE SURFACE_VIEW_GROUP_SFORC

TYPE SURFACE_VIEW_GROUP_SFLUX
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_SFLUX), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_SFLUX_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_SFLUX_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_SFLUX_FINAL
END TYPE SURFACE_VIEW_GROUP_SFLUX

TYPE SURFACE_VIEW_GROUP_VO3ABC
  REAL(KIND=JPRB), POINTER :: PA(:)   ! A climatological ozone profile
!>REAL(KIND=JPRB)          :: PA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PB(:)   ! B climatological ozone profile
!>REAL(KIND=JPRB)          :: PB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PC(:)   ! C climatological ozone profile
!>REAL(KIND=JPRB)          :: PC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VO3ABC), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_A=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_B=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_C=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VO3ABC_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VO3ABC_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VO3ABC_FINAL
END TYPE SURFACE_VIEW_GROUP_VO3ABC


CONTAINS

  SUBROUTINE SURFACE_VIEW_GROUP_VARSF_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VARSF) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VARSF), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VARSF_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VARSF_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VARSF) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VARSF_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VARSF_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VARSF) :: SELF

    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VARSF_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIH) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIH), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIH) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    
    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIH) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIK) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIK), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIK) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIK) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIP) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIP), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIP) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIP) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIV) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIV), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIV) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIV) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIA) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIA), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIA) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIA) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIN) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIN), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VCLIN) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIN) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VDIAGO2) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VDIAGO2), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VDIAGO2) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VDIAGO2) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VDIAGO3) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VDIAGO3), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VDIAGO3) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VDIAGO3) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VDIAG) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VDIAG), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VDIAG) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VDIAG) :: SELF

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_SATSIM) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_SATSIM), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_SATSIM) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_SATSIM) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_WAVES_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_WAVES) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_WAVES), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_WAVES_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_WAVES_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_WAVES) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_WAVES_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_WAVES_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_WAVES) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_WAVES_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_WAM_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_WAM) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_WAM), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_WAM_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_WAM_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_WAM) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    
    
    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_WAM_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_WAM_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_WAM) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_WAM_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_PRECFRAC) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_PRECFRAC), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_PRECFRAC) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_PRECFRAC) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VEXTRA) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VEXTRA), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VEXTRA) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VEXTRA) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VEXTRDI) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VEXTRDI), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VEXTRDI) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VEXTRDI) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VPRECIP) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VPRECIP), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VPRECIP) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VPRECIP) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VPRECIP2) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VPRECIP2), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VPRECIP2) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VPRECIP2) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VEXTR2) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VEXTR2), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VEXTR2) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VEXTR2) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_SFORC_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_SFORC) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_SFORC), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_SFORC_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_SFORC_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_SFORC) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_SFORC_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_SFORC_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_SFORC) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_SFORC_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_SFLUX) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_SFLUX), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_SFLUX) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_SFLUX) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_INIT(SELF, VARIABLE_GROUP)
    
    CLASS(SURFACE_VIEW_GROUP_VO3ABC) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VO3ABC), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    
    
    
    
    
    
  END SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(SURFACE_VIEW_GROUP_VO3ABC) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    
    
    
    

    
  END SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VO3ABC) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_FINAL

  
END MODULE SURFACE_VIEWS_DIAGNOSTIC_MODULE
