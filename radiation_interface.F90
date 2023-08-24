! radiation_interface.F90 - Public interface to radiation scheme
!
! Copyright (C) 2014-2017 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Changes to enable generalized surface description
!   2017-09-08  R. Hogan  Reverted some changes
!
! To use the radiation scheme, create a configuration_type object,
! call "setup_radiation" on it once to load the look-up-tables and
! data describing how gas and hydrometeor absorption/scattering are to
! be represented, and call "radiation" multiple times on different
! input profiles.

! Several parts of this module are only activated if the HAVE_PSRAD
! preprocessor variable is defined, which means that we have the
! implementation of the RRTMG gas absorption model that forms part of
! the PS-Rad (Pincus & Stevens) package.

module radiation_interface

  implicit none

  public  :: setup_radiation, set_gas_units, radiation
  private :: radiation_reverse

contains

  !---------------------------------------------------------------------
  ! Load the look-up-tables and data describing how gas and
  ! hydrometeor absorption/scattering are to be represented
  subroutine setup_radiation(YDERDI, config)

    use parkind1,         only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : config_type, ISolverMcICA, &
         &   IGasModelMonochromatic, IGasModelPSRRTMG, IGasModelIFSRRTMG

    
    
    use radiation_monochromatic,  only : &
         &   setup_gas_optics_mono     => setup_gas_optics, &
         &   setup_cloud_optics_mono   => setup_cloud_optics, &
         &   setup_aerosol_optics_mono => setup_aerosol_optics
    use radiation_ifs_rrtm,       only :  setup_gas_optics

    use radiation_cloud_optics,   only :  setup_cloud_optics
    use radiation_aerosol_optics, only :  setup_aerosol_optics
    USE YOERDI, ONLY : TERDI

    TYPE(TERDI), INTENT(INOUT) :: YDERDI
    type(config_type), intent(inout) :: config

    real(jphook) :: hook_handle

    

    
    
    

    
    

    
    
    
    
    

    
    
    
    
    

    
    
    
    
    

    
    
    
    

    

    

    
    

    

  end subroutine setup_radiation


  !---------------------------------------------------------------------
  ! Scale the gas mixing ratios so that they have the units (and
  ! possibly scale factors) required by the specific gas absorption
  ! model.  This subroutine simply passes the gas object on to the
  ! module of the currently active gas model.
  subroutine set_gas_units(config, gas)
    
    use radiation_config
    use radiation_gas,           only : gas_type
    use radiation_monochromatic, only : set_gas_units_mono  => set_gas_units
    use radiation_ifs_rrtm,      only : set_gas_units_ifs   => set_gas_units



    type(config_type), intent(in)    :: config
    type(gas_type),    intent(inout) :: gas

    

  end subroutine set_gas_units


  !---------------------------------------------------------------------
  ! Run the radiation scheme according to the configuration in the
  ! config object. There are ncol profiles of which only istartcol to
  ! iendcol are to be processed, and there are nlev model levels.  The
  ! output fluxes are written to the flux object, and all other
  ! objects contain the input variables.  The variables may be defined
  ! either in order of increasing or decreasing pressure, but if in
  ! order of decreasing pressure then radiation_reverse will be called
  ! to reverse the order for the computation and then reverse the
  ! order of the output fluxes to match the inputs.
  subroutine radiation(ncol, nlev, istartcol, iendcol, config, &
       &  single_level, thermodynamics, gas, cloud, aerosol, flux)

    use parkind1,                 only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,         only : config_type, &
         &   IGasModelMonochromatic, IGasModelIFSRRTMG, IGasModelPSRRTMG, &
         &   ISolverMcICA, ISolverSpartacus, ISolverHomogeneous, &
         &   ISolverTripleclouds
    use radiation_single_level,   only : single_level_type
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_gas,            only : gas_type
    use radiation_cloud,          only : cloud_type
    use radiation_aerosol,        only : aerosol_type
    use radiation_flux,           only : flux_type
    use radiation_tripleclouds_sw,only : solver_tripleclouds_sw
    use radiation_tripleclouds_lw,only : solver_tripleclouds_lw
    use radiation_mcica_sw,       only : solver_mcica_sw
    use radiation_mcica_lw,       only : solver_mcica_lw
    use radiation_cloudless_sw,   only : solver_cloudless_sw
    use radiation_cloudless_lw,   only : solver_cloudless_lw
    use radiation_homogeneous_sw, only : solver_homogeneous_sw
    use radiation_homogeneous_lw, only : solver_homogeneous_lw
    use radiation_save,           only : save_radiative_properties

    
    use radiation_monochromatic,  only : &
         &   gas_optics_mono         => gas_optics, &
         &   cloud_optics_mono       => cloud_optics, &
         &   add_aerosol_optics_mono => add_aerosol_optics

    use radiation_ifs_rrtm,       only : gas_optics
    use radiation_cloud_optics,   only : cloud_optics
    use radiation_aerosol_optics, only : add_aerosol_optics

    
    integer, intent(in) :: ncol               
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type),        intent(in)   :: config
    type(single_level_type),  intent(in)   :: single_level
    type(thermodynamics_type),intent(in)   :: thermodynamics
    type(gas_type),           intent(in)   :: gas
    type(cloud_type),         intent(inout):: cloud
    type(aerosol_type),       intent(in)   :: aerosol
    
    type(flux_type),          intent(inout):: flux


    

    
    
    
    
    real(jprb), dimension(config%n_g_lw,nlev,istartcol:iendcol) :: od_lw
    real(jprb), dimension(config%n_g_lw_if_scattering,nlev,istartcol:iendcol) :: &
         &  ssa_lw, g_lw

    
    
    
    
    
    real(jprb), dimension(config%n_bands_lw,nlev,istartcol:iendcol) :: od_lw_cloud
    real(jprb), dimension(config%n_bands_lw_if_scattering,nlev,istartcol:iendcol) :: &
         &  ssa_lw_cloud, g_lw_cloud

    
    
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol) :: od_sw, ssa_sw, g_sw

    
    
    real(jprb), dimension(config%n_bands_sw,nlev,istartcol:iendcol)   :: &
         &  od_sw_cloud, ssa_sw_cloud, g_sw_cloud

    
    
    real(jprb), dimension(config%n_g_lw,nlev+1,istartcol:iendcol) :: planck_hl

    
    
    
    real(jprb), dimension(config%n_g_lw, istartcol:iendcol) :: lw_emission
    real(jprb), dimension(config%n_g_lw, istartcol:iendcol) :: lw_albedo

    
    
    
    real(jprb), dimension(config%n_g_sw, istartcol:iendcol) :: sw_albedo_direct
    real(jprb), dimension(config%n_g_sw, istartcol:iendcol) :: sw_albedo_diffuse

    
    
    
    real(jprb), dimension(config%n_g_sw,istartcol:iendcol) :: incoming_sw

    character(len=100) :: rad_prop_file_name
    character(*), parameter :: rad_prop_base_file_name = "radiative_properties"

    real(jphook) :: hook_handle

    

    
    
    

  end subroutine radiation


  !---------------------------------------------------------------------
  ! If the input arrays are arranged in order of decreasing pressure /
  ! increasing height then this subroutine reverses them, calls the
  ! radiation scheme and then reverses the returned fluxes. Since this
  ! subroutine calls, and is called by "radiation", it must be in this
  ! module to avoid circular dependencies.
  subroutine radiation_reverse(ncol, nlev, istartcol, iendcol, config, &
       &  single_level, thermodynamics, gas, cloud, aerosol, flux)
 
    use parkind1, only : jprb

    use radiation_config,         only : config_type
    use radiation_single_level,   only : single_level_type
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_gas,            only : gas_type
    use radiation_cloud,          only : cloud_type
    use radiation_aerosol,        only : aerosol_type
    use radiation_flux,           only : flux_type

    
    integer, intent(in) :: ncol               
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type),        intent(in) :: config
    type(single_level_type),  intent(in) :: single_level
    type(thermodynamics_type),intent(in) :: thermodynamics
    type(gas_type),           intent(in) :: gas
    type(cloud_type),         intent(in) :: cloud
    type(aerosol_type),       intent(in) :: aerosol
    
    type(flux_type),          intent(inout):: flux

    
    type(thermodynamics_type) :: thermodynamics_rev
    type(gas_type)            :: gas_rev
    type(cloud_type)          :: cloud_rev
    type(aerosol_type)        :: aerosol_rev
    type(flux_type)           :: flux_rev

    
    integer :: istartlev, iendlev

    

    
    
    
    
    

    
    
    

    
    

    

    

    
    

    
    
    

    
    
    
    
    
    

  end subroutine radiation_reverse

end module radiation_interface
