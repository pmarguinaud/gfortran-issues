
MODULE SURFACE_VARIABLES_MOD
  ! The SURFACE_VARIABLES type provides namespaced access to individual
  ! groups of surface VARIABLE objects via group-specific container
  ! types.
  !
  ! The generated group types contain the set of VARIABLE objects
  ! configured for each surface group. The VARIABLES in turn that hold
  ! metadata for surface fields and provide access to the underlying
  ! storage FIELD objects and respective data pointers for
  ! thread-parallel regions.

USE PARKIND1, ONLY: JPIM, JPRB
USE VARIABLE_MODULE
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE

IMPLICIT NONE

! Prognostic variable group types
TYPE SURFACE_VARIABLE_GROUP_SOILB
  ! Prognostic surface variable group
  TYPE(VARIABLE_3RB) :: VT   ! temperature
  TYPE(VARIABLE_3RB) :: VQ   ! liquid water content
  TYPE(VARIABLE_3RB) :: VTL   ! ice water content (for MF)

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SOILB_FINAL
END TYPE SURFACE_VARIABLE_GROUP_SOILB

TYPE SURFACE_VARIABLE_GROUP_SNOWG
  ! Prognostic surface variable group
  TYPE(VARIABLE_3RB) :: VF   ! content of surface snow
  TYPE(VARIABLE_3RB) :: VA   ! snow albedo
  TYPE(VARIABLE_3RB) :: VR   ! snow density
  TYPE(VARIABLE_3RB) :: VT   ! total albedo (diagnostic for MF for LVGSN)
  TYPE(VARIABLE_3RB) :: VW   ! Liquid water content

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SNOWG_FINAL
END TYPE SURFACE_VARIABLE_GROUP_SNOWG

TYPE SURFACE_VARIABLE_GROUP_LAKEB
  ! Prognostic surface variable group
  TYPE(VARIABLE_2RB) :: VLICT   ! lake ice temperature
  TYPE(VARIABLE_2RB) :: VLMLT   ! lake mixed-layer temperature
  TYPE(VARIABLE_2RB) :: VLTLT   ! lake total layer temperature
  TYPE(VARIABLE_2RB) :: VLBLT   ! lake bottom layer temperature
  TYPE(VARIABLE_2RB) :: VLSHF   ! lake shape factor
  TYPE(VARIABLE_2RB) :: VLICD   ! lake ice depth
  TYPE(VARIABLE_2RB) :: VLMLD   ! lake mixed-layer depth

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_LAKEB_FINAL
END TYPE SURFACE_VARIABLE_GROUP_LAKEB

TYPE SURFACE_VARIABLE_GROUP_RESVR
  ! Prognostic surface variable group
  TYPE(VARIABLE_2RB) :: VT   ! skin temperature (Ts)
  TYPE(VARIABLE_2RB) :: VW   ! skin water content (Wskin) at ECMWF superficial reservoir water content (Ws) at MF
  TYPE(VARIABLE_2RB) :: VFC   ! skin water content (Wl) at MF
  TYPE(VARIABLE_2RB) :: VIC   ! superficial reservoir ice
  TYPE(VARIABLE_2RB) :: VFP1   ! interpolated Ts for 2nd part of 927-FULLPOS

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_RESVR_FINAL
END TYPE SURFACE_VARIABLE_GROUP_RESVR

TYPE SURFACE_VARIABLE_GROUP_CLS
  ! Prognostic surface variable group
  TYPE(VARIABLE_2RB) :: VTCLS   ! 2m temperature
  TYPE(VARIABLE_2RB) :: VHUCLS   ! 2m humidity
  TYPE(VARIABLE_2RB) :: VUCLS   ! 10m U-wind
  TYPE(VARIABLE_2RB) :: VVCLS   ! 10m V-wind
  TYPE(VARIABLE_2RB) :: VNUCLS   ! 10m neutral U-wind
  TYPE(VARIABLE_2RB) :: VNVCLS   ! 10m neutral V-wind

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_CLS_FINAL
END TYPE SURFACE_VARIABLE_GROUP_CLS

TYPE SURFACE_VARIABLE_GROUP_OML
  ! Prognostic surface variable group
  TYPE(VARIABLE_3RB) :: VTO   ! temperature
  TYPE(VARIABLE_3RB) :: VSO   ! salinity
  TYPE(VARIABLE_3RB) :: VUO   ! U velocity
  TYPE(VARIABLE_3RB) :: VVO   ! V velocity

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_OML_FINAL
END TYPE SURFACE_VARIABLE_GROUP_OML

TYPE SURFACE_VARIABLE_GROUP_EXTRP
  ! Prognostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_EXTRP_FINAL
END TYPE SURFACE_VARIABLE_GROUP_EXTRP

TYPE SURFACE_VARIABLE_GROUP_XTRP2
  ! Prognostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_XTRP2_FINAL
END TYPE SURFACE_VARIABLE_GROUP_XTRP2

TYPE SURFACE_VARIABLE_GROUP_CANRI
  ! Prognostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_CANRI_FINAL
END TYPE SURFACE_VARIABLE_GROUP_CANRI


! Diagnostic variable group types
TYPE SURFACE_VARIABLE_GROUP_VARSF
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VZ0F   ! gravity * surface roughness length
  TYPE(VARIABLE_2RB) :: VALBF   ! surface shortwave albedo
  TYPE(VARIABLE_2RB) :: VEMISF   ! surface longwave emissivity
  TYPE(VARIABLE_2RB) :: VGETRL   ! standard deviation of orography
  TYPE(VARIABLE_2RB) :: VLSM   ! land-sea mask
  TYPE(VARIABLE_2RB) :: VVEG   ! vegetation cover
  TYPE(VARIABLE_2RB) :: VVRLAN   ! anisotropy of the sub-grid scale orography
  TYPE(VARIABLE_2RB) :: VVRLDI   ! angle of the direction of orography with the x axis
  TYPE(VARIABLE_2RB) :: VSIG   ! characteristic orographic slope
  TYPE(VARIABLE_2RB) :: VALBSF   ! soil shortwave albedo
  TYPE(VARIABLE_2RB) :: VLAN   ! fraction of land
  TYPE(VARIABLE_2RB) :: VSST   ! (open) sea surface temperature
  TYPE(VARIABLE_2RB) :: VSSS   ! sea surface salinity
  TYPE(VARIABLE_2RB) :: VLZ0H   ! logarithm of roughness length for heat
  TYPE(VARIABLE_2RB) :: VCVL   ! low vegetation cover
  TYPE(VARIABLE_2RB) :: VCO2TYP   ! CO2 photosynthesis type (c3/c4) for low vegetation cover
  TYPE(VARIABLE_2RB) :: VCVH   ! high vegetation cover
  TYPE(VARIABLE_2RB) :: VCUR   ! urban cover
  TYPE(VARIABLE_2RB) :: VTVL   ! low vegetation type
  TYPE(VARIABLE_2RB) :: VTVH   ! high vegetation type
  TYPE(VARIABLE_2RB) :: VLAIL   ! low vegetation LAI
  TYPE(VARIABLE_2RB) :: VLAIH   ! high vegetation LAI
  TYPE(VARIABLE_2RB) :: VFWET   ! wetland fraction
  TYPE(VARIABLE_2RB) :: VSOTY   ! soil type
  TYPE(VARIABLE_2RB) :: VCLK   ! lake cover
  TYPE(VARIABLE_2RB) :: VDL   ! lake depth
  TYPE(VARIABLE_2RB) :: VCI   ! sea ice fraction
  TYPE(VARIABLE_2RB) :: VUCUR   ! U-component of the ocean current
  TYPE(VARIABLE_2RB) :: VVCUR   ! V-component of the ocean current
  TYPE(VARIABLE_2RB) :: VZ0RLF   ! gravity * vegetation roughness length
  TYPE(VARIABLE_2RB) :: VCGPP   ! GPP bias correction factor
  TYPE(VARIABLE_2RB) :: VCREC   ! REC bias correction factor
  TYPE(VARIABLE_2RB) :: VSDFOR   ! SD filtered orography
  TYPE(VARIABLE_2RB) :: VALUVP   ! MODIS-derived parallel albedo for shortwave radiation
  TYPE(VARIABLE_2RB) :: VALUVD   ! MODIS-derived diffuse albedo for shortwave radiation
  TYPE(VARIABLE_2RB) :: VALNIP   ! MODIS-derived parallel albedo for longwave radiation
  TYPE(VARIABLE_2RB) :: VALNID   ! MODIS-derived diffuse albedo for longwave radiation
  TYPE(VARIABLE_2RB) :: VFP1   ! surface orography in the 2nd part of FULLPOS-927
  TYPE(VARIABLE_2RB) :: VSO2DD   ! sulphate dry dep velocity
  TYPE(VARIABLE_2RB) :: VDMSO   ! oceanic DMS
  TYPE(VARIABLE_2RB) :: VURBF   ! Urban fraction
  TYPE(VARIABLE_2RB) :: VFCA1   ! Fraction of calcite over dust 1st bin
  TYPE(VARIABLE_2RB) :: VFCA2   ! Fraction of calcite over dust 2nd bin
  TYPE(VARIABLE_2RB) :: VAERDEP   ! dust emission potential
  TYPE(VARIABLE_2RB) :: VAERLTS   ! dust lifting threshold speed
  TYPE(VARIABLE_2RB) :: VAERSCC   ! dust soil clay content
  TYPE(VARIABLE_2RB) :: VDSF   ! dust source function
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VCHEMFLXO   ! total chemistry flux (emissions + deposition)
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VCHEMWDFLX   ! wet deposition chemistry flux
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VCHEMDDFLX   ! dry deposition chemistry flux
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VCHEMDV   ! chemistry deposition velocity
  TYPE(VARIABLE_2RB) :: VNUDM   ! nudging mask
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VEMIS2D   ! 2D emission fields for composition
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VEMIS2DAUX   ! 2D emission auxiliary fields for composition

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VARSF_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VARSF

TYPE SURFACE_VARIABLE_GROUP_VCLIH
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VTCCH   ! total convective cloudiness
  TYPE(VARIABLE_2RB) :: VSCCH   ! convective cloud summit
  TYPE(VARIABLE_2RB) :: VBCCH   ! convective cloud base
  TYPE(VARIABLE_2RB) :: VPBLH   ! PBL height
  TYPE(VARIABLE_2RB) :: VSPSH   ! variable for prognostic convection scheme (ALARO)
  TYPE(VARIABLE_2RB) :: VQSH   ! surface moisture historic variable (used by TOUCANS)
  TYPE(VARIABLE_2RB) :: VPCL   ! 
  TYPE(VARIABLE_2RB) :: VPSL   ! 
  TYPE(VARIABLE_2RB) :: VPCN   ! 
  TYPE(VARIABLE_2RB) :: VPSN   ! 
  TYPE(VARIABLE_2RB) :: VEVA   ! 

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIH_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VCLIH

TYPE SURFACE_VARIABLE_GROUP_VCLIK
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VUDGRO   ! ud top position (accsu)

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIK_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VCLIK

TYPE SURFACE_VARIABLE_GROUP_VCLIP
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VTPC   ! climatological deep layer temperature
  TYPE(VARIABLE_2RB) :: VWPC   ! climatological deep layer moisture

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIP_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VCLIP

TYPE SURFACE_VARIABLE_GROUP_VCLIV
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VARG   ! silt percentage within soil
  TYPE(VARIABLE_2RB) :: VSAB   ! percentage of sand within the soil
  TYPE(VARIABLE_2RB) :: VD2   ! soil depth
  TYPE(VARIABLE_2RB) :: VIVEG   ! type of vegetation
  TYPE(VARIABLE_2RB) :: VRSMIN   ! stomatal minimum resistance
  TYPE(VARIABLE_2RB) :: VLAI   ! leaf area index
  TYPE(VARIABLE_2RB) :: VHV   ! resistance to evapotranspiration
  TYPE(VARIABLE_2RB) :: VZ0H   ! gravity * roughness length for heat
  TYPE(VARIABLE_2RB) :: VALS   ! albedo of bare ground
  TYPE(VARIABLE_2RB) :: VALV   ! albedo of vegetation

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIV_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VCLIV

TYPE SURFACE_VARIABLE_GROUP_VCLIA
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VSEA   ! aerosol sea
  TYPE(VARIABLE_2RB) :: VLAN   ! aerosol land
  TYPE(VARIABLE_2RB) :: VSOO   ! aerosol soot
  TYPE(VARIABLE_2RB) :: VDES   ! aerosol desert
  TYPE(VARIABLE_2RB) :: VSUL   ! aerosol sulfate
  TYPE(VARIABLE_2RB) :: VVOL   ! aerosol volcano

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIA_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VCLIA

TYPE SURFACE_VARIABLE_GROUP_VCLIN
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VTOP   ! index of convective cloud top
  TYPE(VARIABLE_2RB) :: VBAS   ! index of convective cloud base
  TYPE(VARIABLE_2RB) :: VACPR   ! averaged convective precipitaion rate
  TYPE(VARIABLE_2RB) :: VACCPR   ! accumulated total precipitaion for assimilation
  TYPE(VARIABLE_2RB) :: VACCPR5   ! accumulated total precipitaion for assimilation (trajectory)

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIN_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VCLIN

TYPE SURFACE_VARIABLE_GROUP_VDIAGO2
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VOCDEP   ! bottom layer depth
  TYPE(VARIABLE_2RB) :: VUSTRC   ! taux clim.
  TYPE(VARIABLE_2RB) :: VVSTRC   ! tauy clim.

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VDIAGO2_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VDIAGO2

TYPE SURFACE_VARIABLE_GROUP_VDIAGO3
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VDIFM   ! viscosity
  TYPE(VARIABLE_3RB) :: VDIFT   ! diff. coef. of temp
  TYPE(VARIABLE_3RB) :: VDIFS   ! diff. coef. of salinity
  TYPE(VARIABLE_3RB) :: VADVT   ! correction term for temp.
  TYPE(VARIABLE_3RB) :: VADVS   ! correction term for sal.
  TYPE(VARIABLE_3RB) :: VTRI0   ! coef. for solving matrix.
  TYPE(VARIABLE_3RB) :: VTRI1   ! coef. for solving matrix.
  TYPE(VARIABLE_3RB) :: VSWDK   ! radiation term
  TYPE(VARIABLE_3RB) :: VZO   ! depth of layer
  TYPE(VARIABLE_3RB) :: VHO   ! depth of interface layer
  TYPE(VARIABLE_3RB) :: VDO   ! layer thickness
  TYPE(VARIABLE_3RB) :: VHO_INV   ! 1 / YHO
  TYPE(VARIABLE_3RB) :: VUOC   ! U velocity clim.
  TYPE(VARIABLE_3RB) :: VVOC   ! V velocity clim.
  TYPE(VARIABLE_3RB) :: VOTKE   ! ocean turb. kin. energy

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VDIAGO3_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VDIAGO3

TYPE SURFACE_VARIABLE_GROUP_VDIAG
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VLSP   ! Large scale precipitation
  TYPE(VARIABLE_2RB) :: VCP   ! Convective precipitation
  TYPE(VARIABLE_2RB) :: VSF   ! Snowfall
  TYPE(VARIABLE_2RB) :: VFZRA   ! Freezing rain
  TYPE(VARIABLE_2RB) :: VBLD   ! Boundary layer dissipation
  TYPE(VARIABLE_2RB) :: VSSHF   ! Surface sensible heat flux
  TYPE(VARIABLE_2RB) :: VSLHF   ! Surface latent heat flux
  TYPE(VARIABLE_2RB) :: VNEE   ! Surface net ecosystem exchange of CO2
  TYPE(VARIABLE_2RB) :: VGPP   ! Surface gross primary production of CO2
  TYPE(VARIABLE_2RB) :: VREC   ! Surface ecosystem respiration of CO2
  TYPE(VARIABLE_2RB) :: VMSL   ! Mean sea level pressure
  TYPE(VARIABLE_2RB) :: VSP   ! Surface pressure
  TYPE(VARIABLE_2RB) :: VTCC   ! Total cloud cover
  TYPE(VARIABLE_2RB) :: V10U   ! U-wind at 10 m
  TYPE(VARIABLE_2RB) :: V10V   ! V-wind at 10 m
  TYPE(VARIABLE_2RB) :: V2T   ! Temperature at 2 m
  TYPE(VARIABLE_2RB) :: V2D   ! Dewpoint temperature at 2 m
  TYPE(VARIABLE_2RB) :: V2SH   ! Specific humidity at 2 m
  TYPE(VARIABLE_2RB) :: VSSR   ! Surface solar radiation
  TYPE(VARIABLE_2RB) :: VSTR   ! Surface thermal radiation
  TYPE(VARIABLE_2RB) :: VTSR   ! Top solar radiation
  TYPE(VARIABLE_2RB) :: VTTR   ! Top thermal radiation
  TYPE(VARIABLE_2RB) :: VEWSS   ! Instantaneous surface U-wind stress
  TYPE(VARIABLE_2RB) :: VNSSS   ! Instantaneous surface V-wind stress
  TYPE(VARIABLE_2RB) :: VE   ! Water evaporation
  TYPE(VARIABLE_2RB) :: VPEV   ! Potential evaporation
  TYPE(VARIABLE_2RB) :: VCCC   ! Convective cloud cover
  TYPE(VARIABLE_2RB) :: VLCC   ! Low cloud cover
  TYPE(VARIABLE_2RB) :: VMCC   ! Medium cloud cover
  TYPE(VARIABLE_2RB) :: VHCC   ! High cloud cover
  TYPE(VARIABLE_2RB) :: VLGWS   ! Zonal gravity wave stress
  TYPE(VARIABLE_2RB) :: VMGWS   ! Meridian gravity wave stress
  TYPE(VARIABLE_2RB) :: VGWD   ! Gravity wave dissipation
  TYPE(VARIABLE_2RB) :: VMX2T   ! Maximum temperature at 2 m
  TYPE(VARIABLE_2RB) :: VMN2T   ! Minimum temperature at 2 m
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMX2T6   ! Bins for maximum temperature at 2 m since last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMN2T6   ! Bins for minimum temperature at 2 m since last 6 hours
  TYPE(VARIABLE_2RB) :: VRO   ! Runoff (total)
  TYPE(VARIABLE_2RB) :: VSRO   ! Runoff surface
  TYPE(VARIABLE_2RB) :: VSSRO   ! Runoff sub-surface
  TYPE(VARIABLE_2RB) :: VALB   ! (surface shortwave) albedo
  TYPE(VARIABLE_2RB) :: VIEWSS   ! Instantaneous surface zonal component of stress
  TYPE(VARIABLE_2RB) :: VINSSS   ! Instantaneous surface meridian component of stress
  TYPE(VARIABLE_2RB) :: VISSHF   ! Instantaneous surface heat flux
  TYPE(VARIABLE_2RB) :: VIE   ! Instantaneous surface moisture flux
  TYPE(VARIABLE_2RB) :: VINEE   ! Instantaneous net ecosystem exchange of CO2
  TYPE(VARIABLE_2RB) :: VIGPP   ! Instantaneous gross primary production of CO2
  TYPE(VARIABLE_2RB) :: VIREC   ! Instantaneous ecosystem respiration of CO2
  TYPE(VARIABLE_2RB) :: VICH4   ! Instantaneous wetland CH4 flux
  TYPE(VARIABLE_2RB) :: VCSF   ! Convective snow fall
  TYPE(VARIABLE_2RB) :: VLSSF   ! Large scale snowfall
  TYPE(VARIABLE_2RB) :: VMXTPR   ! Max precip rate since last post-processing
  TYPE(VARIABLE_2RB) :: VMNTPR   ! Min precip rate since last post-processing
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMXTPR6   ! Max precip rate in last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMNTPR6   ! Min precip rate in last 6 hours
  TYPE(VARIABLE_2RB) :: VTPR   ! Total precipitation rate
  TYPE(VARIABLE_2RB) :: VLSRR   ! Large scale rain rate
  TYPE(VARIABLE_2RB) :: VCRR   ! Convective rain rate
  TYPE(VARIABLE_2RB) :: VLSSFR   ! Large scale snowfall rate
  TYPE(VARIABLE_2RB) :: VCSFR   ! Convective snowfall rate
  TYPE(VARIABLE_2RB) :: VPTYPE   ! Precipitation type
  TYPE(VARIABLE_2RB) :: VILSPF   ! Large-scale precipitation fraction (inst.)
  TYPE(VARIABLE_2RB) :: VZ0F   ! Gravity * surface roughness length
  TYPE(VARIABLE_2RB) :: VLZ0H   ! Logarithm of z0 times heat flux
  TYPE(VARIABLE_2RB) :: VVIWVE   ! Vertical integral of eastward water vapour flux
  TYPE(VARIABLE_2RB) :: VVIWVN   ! Vertical integral of northward water vapour flux
  TYPE(VARIABLE_2RB) :: VTCW   ! Total water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCWV   ! Total water vapor content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCLW   ! Total liquid water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCIW   ! Total ice water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCRW   ! Total rain water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCSW   ! Total snow water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCSLW   ! Total supercooled liquid water content in a vertical column
  TYPE(VARIABLE_2RB) :: VSSRD   ! Downward surface solar radiation
  TYPE(VARIABLE_2RB) :: VSTRD   ! Downward surface thermic radiation
  TYPE(VARIABLE_2RB) :: VSSRDC   ! Clear-sky downward surface solar radiation
  TYPE(VARIABLE_2RB) :: VSTRDC   ! Claer-sky downward surface thermal radiation
  TYPE(VARIABLE_2RB) :: VBLH   ! Height of boundary layer
  TYPE(VARIABLE_2RB) :: VSUND   ! Sunshine duration
  TYPE(VARIABLE_2RB) :: VSPAR   ! Surface downward PARadiation
  TYPE(VARIABLE_2RB) :: VSUVB   ! Surface downward UV-B radiation
  TYPE(VARIABLE_2RB) :: VSFDIR   ! Surface total sky direct downward SW radiation
  TYPE(VARIABLE_2RB) :: VSCDIR   ! Surface clear-sky direct downward SW radiation
  TYPE(VARIABLE_2RB) :: VSDSRP   ! Surface total-sky direct beam downward SW radiation
  TYPE(VARIABLE_2RB) :: VCAPE   ! Conv.avail.potential energy (CAPE)
  TYPE(VARIABLE_2RB) :: VMUCAPE   ! Maximum unstable CAPE
  TYPE(VARIABLE_2RB) :: VMLCAPE50   ! CAPE from 50 hPa mixed-layer
  TYPE(VARIABLE_2RB) :: VMLCAPE100   ! CAPE from 100 hPa mixed-layer
  TYPE(VARIABLE_2RB) :: VPDEPL   ! Parcel departure level-pressure for MUCAPE
  TYPE(VARIABLE_2RB) :: VCAPES   ! CAPE-Shear
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMXCAP6   ! Bins for maximum CAPE in last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMXCAPS6   ! Bins for maximum CAPE-Shear in last 6 hours
  TYPE(VARIABLE_2RB) :: VTROPOTP   ! Pressure of thermal Tropopause
  TYPE(VARIABLE_2RB) :: VTSRC   ! Top solar radiation clear sky
  TYPE(VARIABLE_2RB) :: VTTRC   ! Top thermal radiation clear sky
  TYPE(VARIABLE_2RB) :: VSSRC   ! Surface solar radiation clear sky
  TYPE(VARIABLE_2RB) :: VSTRC   ! Surface thermal radiation clear sky
  TYPE(VARIABLE_2RB) :: VES   ! Evaporation of snow
  TYPE(VARIABLE_2RB) :: VSMLT   ! Snow melt
  TYPE(VARIABLE_2RB) :: V10FG   ! Wind gust at 10 m (max since previous pp)
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: V10FG6   ! Bins for wind gust at 10 m (max since last 6 hours)
  TYPE(VARIABLE_2RB) :: V10FGCV   ! convective wind gust at 10m for current time level (m/s)
  TYPE(VARIABLE_2RB) :: VI10FG   ! Wind gust at 10 m ("instantaneous")
  TYPE(VARIABLE_2RB) :: VLSPF   ! Large scale precipitation fraction
  TYPE(VARIABLE_2RB) :: VTCO3   ! Total ozone content in a vertical column
  TYPE(VARIABLE_2RB) :: VVIMD   ! Vertically integrated mass divergence
  TYPE(VARIABLE_2RB) :: VSPARC   ! Surface clear-sky parallel radiation
  TYPE(VARIABLE_2RB) :: VSTINC   ! Top of atmosphere incident solar radiation
  TYPE(VARIABLE_2RB) :: VCBASE   ! Cloud base level
  TYPE(VARIABLE_2RB) :: V0DEGL   ! Zero deg. level
  TYPE(VARIABLE_2RB) :: VM10DEGL   ! -10 deg. level
  TYPE(VARIABLE_2RB) :: VVISIH   ! Horizontal visibility
  TYPE(VARIABLE_2RB) :: VCIN   ! CIN
  TYPE(VARIABLE_2RB) :: VMLCIN50   ! CIN from 50 hPa mixed-layer
  TYPE(VARIABLE_2RB) :: VMLCIN100   ! CIN from 100 hPa mixed-layer
  TYPE(VARIABLE_2RB) :: VKINDEX   ! Convective K-Index
  TYPE(VARIABLE_2RB) :: VTTINDEX   ! Convective TT-Index
  TYPE(VARIABLE_2RB) :: VCBASEA   ! Cloud base aviation
  TYPE(VARIABLE_2RB) :: VCTOPC   ! Cloud top convective
  TYPE(VARIABLE_2RB) :: VZTWETB0   ! Height of 0 deg wet bulb temperature
  TYPE(VARIABLE_2RB) :: VZTWETB1   ! Height of 1 deg wet bulb temperature
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VTCGHG   ! Total column greenhouse gases
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VTCCHEM   ! Total column chemistry
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:,:) :: VAERODIAG   ! Per-aerosol-type diagnostics
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:,:) :: VAERO_WVL_DIAG   ! Per-wavelength aerosol optical diagnostics
  TYPE(VARIABLE_2RB) :: V100U   ! 100m zonal wind
  TYPE(VARIABLE_2RB) :: V100V   ! 100m meridional wind
  TYPE(VARIABLE_2RB) :: V200U   ! 200m zonal wind
  TYPE(VARIABLE_2RB) :: V200V   ! 200m meridional wind
  TYPE(VARIABLE_2RB) :: VZUST   ! Friction velocity
  TYPE(VARIABLE_2RB) :: V10NU   ! 10m zonal neutral wind
  TYPE(VARIABLE_2RB) :: V10NV   ! 10m meridional neutral wind
  TYPE(VARIABLE_2RB) :: VDNDZN   ! Minimum vertical refractivity gradient
  TYPE(VARIABLE_2RB) :: VDNDZA   ! Mean vertical refractivity gradient
  TYPE(VARIABLE_2RB) :: VDCTB   ! Duct base height
  TYPE(VARIABLE_2RB) :: VTPLB   ! Trapping layer base height
  TYPE(VARIABLE_2RB) :: VTPLT   ! Trapping layer top height
  TYPE(VARIABLE_2RB) :: VODSS   ! optical depth sea salt aerosols
  TYPE(VARIABLE_2RB) :: VODDU   ! optical depth dust aerosols
  TYPE(VARIABLE_2RB) :: VODOM   ! optical depth organic m. aerosols
  TYPE(VARIABLE_2RB) :: VODBC   ! optical depth black C aerosols
  TYPE(VARIABLE_2RB) :: VODSU   ! optical depth sulphate aerosols
  TYPE(VARIABLE_2RB) :: VODNI   ! optical depth nitrate aerosols
  TYPE(VARIABLE_2RB) :: VODAM   ! optical depth ammonium aerosols
  TYPE(VARIABLE_2RB) :: VODSOA   ! optical depth secondary organic aerosols
  TYPE(VARIABLE_2RB) :: VODVFA   ! optical depth volcanic flying ash
  TYPE(VARIABLE_2RB) :: VODVSU   ! optical depth volcanic sulphate aerosols
  TYPE(VARIABLE_2RB) :: VODTOACC   ! optical depth total aerosol accumulated
  TYPE(VARIABLE_2RB) :: VAEPM1   ! particulate matter le 1 um
  TYPE(VARIABLE_2RB) :: VAEPM25   ! particulate matter le 2.5um
  TYPE(VARIABLE_2RB) :: VAEPM10   ! particulate matter le 10 um
  TYPE(VARIABLE_2RB) :: VUVBED   ! UV biologically effective dose
  TYPE(VARIABLE_2RB) :: VUVBEDCS   ! UV biologically effective dose clear sky
  TYPE(VARIABLE_2RB) :: VLITOTI   ! instantaneous total lightning flash density
  TYPE(VARIABLE_2RB) :: VLICGI   ! instantaneous cloud-to-ground lightning flash density
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VLITOTA6   ! Bins for averaged total lightning over last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VLICGA6   ! Bins for averaged cloud-to-ground lightning over last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:,:) :: VPTYPEOCC6   ! Bins for accum freq of each precip type over last 6 hours

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VDIAG_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VDIAG

TYPE SURFACE_VARIABLE_GROUP_SATSIM
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VCLBT   ! Cloudy brightness temperature
  TYPE(VARIABLE_3RB) :: VCSBT   ! Clear-sky brightness temperature

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SATSIM_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_SATSIM

TYPE SURFACE_VARIABLE_GROUP_WAVES
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VCHAR   ! Charnock parameter as modified by the wave model.
  TYPE(VARIABLE_2RB) :: VCHARHQ   ! Charnock for heat and moisture from the wave model.
  TYPE(VARIABLE_2RB) :: VUSTOKES   ! U-component of the surface Stokes drift.
  TYPE(VARIABLE_2RB) :: VVSTOKES   ! V-component of the surface Stokes drift.
  TYPE(VARIABLE_2RB) :: VTAUOCX   ! U-component of the Momentum flux to ocean.
  TYPE(VARIABLE_2RB) :: VTAUOCY   ! V-component of the Momentum flux to ocean.
  TYPE(VARIABLE_2RB) :: VPHIOC   ! Energy flux to ocean.
  TYPE(VARIABLE_2RB) :: VWSEMEAN   ! Windsea variance.
  TYPE(VARIABLE_2RB) :: VWSFMEAN   ! Windsea mean frequency.

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_WAVES_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_WAVES

TYPE SURFACE_VARIABLE_GROUP_WAM
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VU10N   ! 10m neutral wind U-component passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VV10N   ! 10m neutral wind V-component passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VRHO   ! surface density passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VZIL   ! ZI/L passed to the wave model (used for gustiness in WAM).
  TYPE(VARIABLE_2RB) :: VCIF   ! Sea ice fraction passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VCLK   ! Lake cover passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VUCURW   ! Ocean current    U-component passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VVCURW   ! Ocean current    V-component passed to the wave model (WAM).

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_WAM_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_WAM

TYPE SURFACE_VARIABLE_GROUP_PRECFRAC
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_PRECFRAC_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_PRECFRAC

TYPE SURFACE_VARIABLE_GROUP_VEXTRA
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VEXTRA_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VEXTRA

TYPE SURFACE_VARIABLE_GROUP_VEXTRDI
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VXEDR   ! Eddy diffusivity rate

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VEXTRDI_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VEXTRDI

TYPE SURFACE_VARIABLE_GROUP_VPRECIP
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VPRECIP   ! Diagnostic of precipitations type

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VPRECIP_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VPRECIP

TYPE SURFACE_VARIABLE_GROUP_VPRECIP2
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VPRECIP2   ! Diagnostic of precipitations type

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VPRECIP2_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VPRECIP2

TYPE SURFACE_VARIABLE_GROUP_VEXTR2
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VEXTR2_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VEXTR2

TYPE SURFACE_VARIABLE_GROUP_SFORC
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SFORC_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_SFORC

TYPE SURFACE_VARIABLE_GROUP_SFLUX
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SFLUX_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_SFLUX

TYPE SURFACE_VARIABLE_GROUP_VO3ABC
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VA   ! A climatological ozone profile
  TYPE(VARIABLE_2RB) :: VB   ! B climatological ozone profile
  TYPE(VARIABLE_2RB) :: VC   ! C climatological ozone profile

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VO3ABC_FINAL 
END TYPE SURFACE_VARIABLE_GROUP_VO3ABC


TYPE SURFACE_VARIABLES
  ! Global variable and field storage for surface variables

  ! Prognostic variable groups
  TYPE(SURFACE_VARIABLE_GROUP_SOILB) :: GSP_SB
  TYPE(SURFACE_VARIABLE_GROUP_SNOWG) :: GSP_SG
  TYPE(SURFACE_VARIABLE_GROUP_LAKEB) :: GSP_SL
  TYPE(SURFACE_VARIABLE_GROUP_RESVR) :: GSP_RR
  TYPE(SURFACE_VARIABLE_GROUP_CLS) :: GSP_CL
  TYPE(SURFACE_VARIABLE_GROUP_OML) :: GSP_OM
  TYPE(SURFACE_VARIABLE_GROUP_EXTRP) :: GSP_EP
  TYPE(SURFACE_VARIABLE_GROUP_XTRP2) :: GSP_X2
  TYPE(SURFACE_VARIABLE_GROUP_CANRI) :: GSP_CI

  ! Diagnostic variable groups
  TYPE(SURFACE_VARIABLE_GROUP_VARSF) :: GSD_VF
  TYPE(SURFACE_VARIABLE_GROUP_VCLIH) :: GSD_VH
  TYPE(SURFACE_VARIABLE_GROUP_VCLIK) :: GSD_VK
  TYPE(SURFACE_VARIABLE_GROUP_VCLIP) :: GSD_VP
  TYPE(SURFACE_VARIABLE_GROUP_VCLIV) :: GSD_VV
  TYPE(SURFACE_VARIABLE_GROUP_VCLIA) :: GSD_VA
  TYPE(SURFACE_VARIABLE_GROUP_VCLIN) :: GSD_VN
  TYPE(SURFACE_VARIABLE_GROUP_VDIAGO2) :: GSD_V2
  TYPE(SURFACE_VARIABLE_GROUP_VDIAGO3) :: GSD_V3
  TYPE(SURFACE_VARIABLE_GROUP_VDIAG) :: GSD_VD
  TYPE(SURFACE_VARIABLE_GROUP_SATSIM) :: GSD_SM
  TYPE(SURFACE_VARIABLE_GROUP_WAVES) :: GSD_WS
  TYPE(SURFACE_VARIABLE_GROUP_WAM) :: GSD_WW
  TYPE(SURFACE_VARIABLE_GROUP_PRECFRAC) :: GSD_PF
  TYPE(SURFACE_VARIABLE_GROUP_VEXTRA) :: GSD_XA
  TYPE(SURFACE_VARIABLE_GROUP_VEXTRDI) :: GSD_DI
  TYPE(SURFACE_VARIABLE_GROUP_VPRECIP) :: GSD_XP
  TYPE(SURFACE_VARIABLE_GROUP_VPRECIP2) :: GSD_XP2
  TYPE(SURFACE_VARIABLE_GROUP_VEXTR2) :: GSD_X2
  TYPE(SURFACE_VARIABLE_GROUP_SFORC) :: GSD_SFO
  TYPE(SURFACE_VARIABLE_GROUP_SFLUX) :: GSD_SFL
  TYPE(SURFACE_VARIABLE_GROUP_VO3ABC) :: GSD_VC
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLES_FINAL
END TYPE SURFACE_VARIABLES

CONTAINS

SUBROUTINE SURFACE_VARIABLE_GROUP_SOILB_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SOILB) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_SNOWG_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SNOWG) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_LAKEB_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_LAKEB) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_RESVR_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_RESVR) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_CLS_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_CLS) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_OML_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_OML) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_EXTRP_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_EXTRP) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_XTRP2_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_XTRP2) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_CANRI_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_CANRI) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  

END SUBROUTINE


SUBROUTINE SURFACE_VARIABLE_GROUP_VARSF_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VARSF) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIH_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIH) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIK_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIK) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIP_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIP) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIV_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIV) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIA_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIA) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIN_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIN) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VDIAGO2_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO2) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VDIAGO3_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO3) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VDIAG_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VDIAG) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_SATSIM_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SATSIM) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_WAVES_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_WAVES) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_WAM_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_WAM) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  
  
  
  
  
  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_PRECFRAC_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_PRECFRAC) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VEXTRA_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VEXTRA) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VEXTRDI_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VEXTRDI) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VPRECIP_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VPRECIP2_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP2) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  

  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VEXTR2_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VEXTR2) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_SFORC_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SFORC) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_SFLUX_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SFLUX) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VO3ABC_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VO3ABC) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  
  
  

  

END SUBROUTINE



SUBROUTINE SURFACE_VARIABLES_FINAL (SELF)
CLASS (SURFACE_VARIABLES) :: SELF

  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
END SUBROUTINE

END MODULE SURFACE_VARIABLES_MOD
