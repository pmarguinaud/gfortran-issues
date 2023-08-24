! radiation_interface.F90 - Monochromatic gas/cloud optics for testing
!
! Copyright (C) 2014-2018 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Receive "surface" dummy argument
!   2017-09-13  R. Hogan  Revert
!   2018-08-29  R. Hogan  Particulate single-scattering albedo / asymmetry from namelist

module radiation_monochromatic

  implicit none

  public  :: setup_gas_optics, gas_optics, set_gas_units, &
       &     setup_cloud_optics, cloud_optics

contains

  ! Provides elemental function "delta_eddington"

  !---------------------------------------------------------------------
  ! Setup the arrays in the config object corresponding to the
  ! monochromatic gas optics model.  The directory argument is not
  ! used, since no look-up tables need to be loaded.
  subroutine setup_gas_optics(config, directory)

    use radiation_config, only : config_type
    
    type(config_type), intent(inout) :: config
    character(len=*),  intent(in)    :: directory

    
    
    
    
    
    

    
    
    
    
    
    
    

    
    
    
    
    
    
    

  end subroutine setup_gas_optics


  !---------------------------------------------------------------------
  ! Dummy routine for scaling gas mixing ratios
  subroutine set_gas_units(gas)

    use radiation_gas,           only : gas_type
    type(gas_type),    intent(inout) :: gas

  end subroutine set_gas_units


  !---------------------------------------------------------------------
  ! Dummy setup routine for cloud optics: in fact, no setup is
  ! required for monochromatic case
  subroutine setup_cloud_optics(config)

    use radiation_config, only : config_type
    type(config_type), intent(inout) :: config

  end subroutine setup_cloud_optics


  !---------------------------------------------------------------------
  ! Dummy subroutine since no aerosols are represented in
  ! monochromatic case
  subroutine setup_aerosol_optics(config)

    use radiation_config,              only : config_type
    type(config_type), intent(inout) :: config

  end subroutine setup_aerosol_optics


  !---------------------------------------------------------------------
  ! Compute gas optical depths, shortwave scattering, Planck function
  ! and incoming shortwave radiation at top-of-atmosphere
  subroutine gas_optics(ncol,nlev,istartcol,iendcol, &
       config, single_level, thermodynamics, gas, lw_albedo, & 
       od_lw, od_sw, ssa_sw, planck_hl, lw_emission, &
       incoming_sw)

    use parkind1,                 only : jprb
    use radiation_config,         only : config_type
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_single_level,   only : single_level_type
    use radiation_gas,            only : gas_type
    use radiation_constants,      only : Pi, StefanBoltzmann

    
    integer, intent(in) :: ncol               
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type),        intent(in) :: config
    type(single_level_type),  intent(in) :: single_level
    type(thermodynamics_type),intent(in) :: thermodynamics
    type(gas_type),           intent(in) :: gas

    
    real(jprb), dimension(config%n_g_lw,istartcol:iendcol), &
         &  intent(in) :: lw_albedo

    

    
    
    
    real(jprb), dimension(config%n_g_lw,nlev,istartcol:iendcol), intent(out) :: &
         &   od_lw
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), intent(out) :: &
         &   od_sw, ssa_sw

    
    
    real(jprb), dimension(config%n_g_lw,nlev+1,istartcol:iendcol), intent(out) :: &
         &   planck_hl
    real(jprb), dimension(config%n_g_lw,istartcol:iendcol), intent(out) :: &
         &   lw_emission

    
    
    
    real(jprb), dimension(config%n_g_sw,istartcol:iendcol), intent(out) :: &
         &   incoming_sw
    
    
    
    real(jprb), dimension(istartcol:iendcol) :: extinction_fraction

    
    
    
    
    
    

    integer :: jlev

    

    
    
    

    
    

    

  end subroutine gas_optics


  !---------------------------------------------------------------------
  ! Compute cloud optical depth, single-scattering albedo and
  ! g factor in the longwave and shortwave
  subroutine cloud_optics(nlev,istartcol,iendcol, &
       &   config, thermodynamics, cloud, & 
       &   od_lw_cloud, ssa_lw_cloud, g_lw_cloud, &
       &   od_sw_cloud, ssa_sw_cloud, g_sw_cloud)

    use parkind1,                 only : jprb
    use radiation_config,         only : config_type
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_cloud,          only : cloud_type
    use radiation_constants,      only : AccelDueToGravity, &
         &   DensityLiquidWater, DensitySolidIce

    
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in) :: config
    type(thermodynamics_type),intent(in) :: thermodynamics
    type(cloud_type),   intent(in) :: cloud

    

    
    
    
    
    real(jprb), dimension(config%n_bands_lw,nlev,istartcol:iendcol), intent(out) :: &
         &   od_lw_cloud
    real(jprb), dimension(config%n_bands_lw_if_scattering,nlev,istartcol:iendcol), &
         &   intent(out) :: ssa_lw_cloud, g_lw_cloud

    
    
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), intent(out) :: &
         &   od_sw_cloud, ssa_sw_cloud, g_sw_cloud

    
    real(jprb), dimension(nlev,istartcol:iendcol) :: lwp_kg_m2, iwp_kg_m2

    integer  :: jlev, jcol

    
    
    real(jprb) :: factor

    
    
    

    
    
    
    

    

    
    

    
    

    

  end subroutine cloud_optics


  !---------------------------------------------------------------------
  ! Dummy subroutine since no aerosols are represented in
  ! monochromatic case
  subroutine add_aerosol_optics(nlev,istartcol,iendcol, &
       &  config, thermodynamics, gas, aerosol, & 
       &  od_lw, ssa_lw, g_lw, od_sw, ssa_sw, g_sw)

    use parkind1,                      only : jprb

    use radiation_config,              only : config_type
    use radiation_thermodynamics,      only : thermodynamics_type
    use radiation_gas,                 only : gas_type
    use radiation_aerosol,             only : aerosol_type

    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in), target :: config
    type(thermodynamics_type),intent(in)  :: thermodynamics
    type(gas_type),           intent(in)  :: gas
    type(aerosol_type),       intent(in)  :: aerosol
    
    
    
    
    
    real(jprb), dimension(config%n_g_lw,nlev,istartcol:iendcol), intent(inout) :: od_lw
    real(jprb), dimension(config%n_g_lw_if_scattering,nlev,istartcol:iendcol), &
         &  intent(out) :: ssa_lw, g_lw
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), intent(inout) &
         &  :: od_sw, ssa_sw
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), intent(out) :: g_sw

    

    

  end subroutine add_aerosol_optics

  !---------------------------------------------------------------------
  ! Planck function in terms of wavelength
  elemental function planck_function(wavelength, temperature)

    use parkind1,            only : jprb

    use radiation_constants, only : BoltzmannConstant, PlanckConstant, &
         &                          SpeedOfLight

    real(jprb), intent(in) :: wavelength  
    real(jprb), intent(in) :: temperature 

    
    real(jprb)             :: planck_function

    

  end function planck_function

end module radiation_monochromatic
