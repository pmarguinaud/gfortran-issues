! radiation_aerosol_optics.F90 - Computing aerosol optical properties
!
! Copyright (C) 2015-2018 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2018-04-15  R Hogan  Add "direct" option

module radiation_aerosol_optics

  implicit none

contains

  ! Provides the elemental function "delta_eddington_extensive"

  !---------------------------------------------------------------------
  ! Load aerosol scattering data; this subroutine delegates to one
  ! in radiation_aerosol_optics_data.F90
  subroutine setup_aerosol_optics(config)

    use parkind1,                      only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,              only : config_type
    use radiation_aerosol_optics_data, only : aerosol_optics_type

    type(config_type), intent(inout) :: config

    real(jphook) :: hook_handle

    

    

    

    

  end subroutine setup_aerosol_optics


  !---------------------------------------------------------------------
  ! Compute aerosol optical properties and add to existing gas optical
  ! depth and scattering properties
  subroutine add_aerosol_optics(nlev,istartcol,iendcol, &
       &  config, thermodynamics, gas, aerosol, & 
       &  od_lw, ssa_lw, g_lw, od_sw, ssa_sw, g_sw)

    use parkind1,                      only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,              only : config_type
    use radiation_thermodynamics,      only : thermodynamics_type
    use radiation_gas,                 only : gas_type, IH2O, IMassMixingRatio
    use radiation_aerosol,             only : aerosol_type
    use radiation_constants,           only : AccelDueToGravity
    use radiation_aerosol_optics_data, only : aerosol_optics_type, &
         &  IAerosolClassUndefined,   IAerosolClassIgnored, &
         &  IAerosolClassHydrophobic, IAerosolClassHydrophilic

    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in), target :: config
    type(thermodynamics_type),intent(in)  :: thermodynamics
    type(gas_type),           intent(in)  :: gas
    type(aerosol_type),       intent(in)  :: aerosol
    
    
    
    
    
    real(jprb), dimension(config%n_g_lw,nlev,istartcol:iendcol), &
         &   intent(inout) :: od_lw
    real(jprb), dimension(config%n_g_lw_if_scattering,nlev,istartcol:iendcol), &
         &   intent(out)   :: ssa_lw, g_lw
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), &
         &   intent(inout) :: od_sw, ssa_sw
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), &
         &   intent(out)   :: g_sw

    
    
    
    
    real(jprb), dimension(config%n_bands_sw) &
         & :: od_sw_aerosol, scat_sw_aerosol, scat_g_sw_aerosol, local_od_sw
    real(jprb), dimension(config%n_bands_lw) :: od_lw_aerosol, local_od_lw
    real(jprb), dimension(config%n_bands_lw_if_scattering) &
         & :: scat_lw_aerosol, scat_g_lw_aerosol

    real(jprb) :: h2o_mmr(istartcol:iendcol,nlev)

    real(jprb) :: rh 

    
    
    real(jprb) :: factor

    
    
    real(jprb) :: local_od, local_scat

    
    integer :: jcol, jlev, jg, jtype

    
    integer :: istartlev, iendlev

    
    integer :: iband, irh

    
    type(aerosol_optics_type), pointer :: ao

    real(jphook) :: hook_handle

    

    

    

  end subroutine add_aerosol_optics


  !---------------------------------------------------------------------
  ! Add precomputed optical properties to gas optical depth and
  ! scattering properties
  subroutine add_aerosol_optics_direct(nlev,istartcol,iendcol, &
       &  config, aerosol, & 
       &  od_lw, ssa_lw, g_lw, od_sw, ssa_sw, g_sw)

    use parkind1,                      only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,              only : config_type
    use radiation_aerosol,             only : aerosol_type

    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in), target :: config
    type(aerosol_type),       intent(in)  :: aerosol
    
    
    
    
    
    real(jprb), dimension(config%n_g_lw,nlev,istartcol:iendcol), &
         &   intent(inout) :: od_lw
    real(jprb), dimension(config%n_g_lw_if_scattering,nlev,istartcol:iendcol), &
         &   intent(out)   :: ssa_lw, g_lw
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), &
         &   intent(inout) :: od_sw, ssa_sw
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), &
         &   intent(out)   :: g_sw

    
    
    real(jprb) :: local_od, local_scat

    
    
    
    
    real(jprb), dimension(config%n_bands_sw) &
         & :: od_sw_aerosol, scat_sw_aerosol, scat_g_sw_aerosol
    real(jprb), dimension(config%n_bands_lw) :: od_lw_aerosol
    real(jprb), dimension(config%n_bands_lw_if_scattering) &
         & :: scat_lw_aerosol, scat_g_lw_aerosol

    
    integer :: jcol, jlev, jg

    
    integer :: istartlev, iendlev

    
    integer :: iband

    real(jphook) :: hook_handle

    

    

    


    

  end subroutine add_aerosol_optics_direct
 

  !---------------------------------------------------------------------
  ! Sometimes it is useful to specify aerosol in terms of its optical
  ! depth at a particular wavelength.  This function returns the dry
  ! shortwave mass-extinction coefficient, i.e. the extinction cross
  ! section per unit mass, for aerosol of type "itype" at shortwave
  ! band "iband". For hydrophilic types, the value at the first
  ! relative humidity bin is taken.
  function dry_aerosol_sw_mass_extinction(config, itype, iband)

    use parkind1,                      only : jprb
    use radiation_config,              only : config_type
    use radiation_aerosol_optics_data, only : aerosol_optics_type, &
         &  IAerosolClassUndefined,   IAerosolClassIgnored, &
         &  IAerosolClassHydrophobic, IAerosolClassHydrophilic

    type(config_type), intent(in), target :: config

    
    integer, intent(in) :: itype, iband
    
    real(jprb) dry_aerosol_sw_mass_extinction

    
    type(aerosol_optics_type), pointer :: ao

    

    

  end function dry_aerosol_sw_mass_extinction


  !---------------------------------------------------------------------
  ! Compute aerosol extinction coefficient at a particular shortwave
  ! band and a single height - this is useful for visibility
  ! diagnostics
  subroutine aerosol_sw_extinction(ncol,istartcol,iendcol, &
       &  config, iband, mixing_ratio, relative_humidity, extinction)

    use parkind1,                      only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,              only : config_type
    use radiation_aerosol_optics_data, only : aerosol_optics_type, &
         &  IAerosolClassUndefined,   IAerosolClassIgnored, &
         &  IAerosolClassHydrophobic, IAerosolClassHydrophilic

    integer, intent(in) :: ncol               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in), target :: config
    integer, intent(in)     :: iband 
    real(jprb), intent(in)  :: mixing_ratio(ncol,config%n_aerosol_types)
    real(jprb), intent(in)  :: relative_humidity(ncol)
    real(jprb), intent(out) :: extinction(ncol)

    
    real(jprb) :: ext

    
    type(aerosol_optics_type), pointer :: ao
    
    
    integer :: jcol, jtype

    
    integer :: irh

    real(jphook) :: hook_handle

    

    

    

    
    

    

  end subroutine aerosol_sw_extinction

end module radiation_aerosol_optics
