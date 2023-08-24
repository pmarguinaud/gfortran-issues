! radiation_cloud_optics.F90 - Computing cloud optical properties
!
! Copyright (C) 2014-2017 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-07-22  R. Hogan  Added Yi et al. ice optics model

module radiation_cloud_optics

  implicit none

contains

  ! Provides elemental function "delta_eddington_scat_od"

  !---------------------------------------------------------------------
  ! Load cloud scattering data; this subroutine delegates to one
  ! in radiation_cloud_optics_data.F90, but checks the size of
  ! what is returned
  subroutine setup_cloud_optics(config)

    use parkind1,         only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : config_type, IIceModelFu, IIceModelBaran, &
         &                       IIceModelBaran2016, IIceModelBaran2017, &
         &                       IIceModelYi, &
         &                       ILiquidModelSOCRATES, ILiquidModelSlingo
    use radiation_cloud_optics_data, only  : cloud_optics_type
    use radiation_ice_optics_fu, only    : NIceOpticsCoeffsFuSW, &
         &                                 NIceOpticsCoeffsFuLW
    use radiation_ice_optics_baran, only : NIceOpticsCoeffsBaran, &
         &                                 NIceOpticsCoeffsBaran2016
    use radiation_ice_optics_baran2017, only : NIceOpticsCoeffsBaran2017, &
         &                                 NIceOpticsGeneralCoeffsBaran2017
    use radiation_ice_optics_yi, only    : NIceOpticsCoeffsYiSW, &
         &                                 NIceOpticsCoeffsYiLW
    use radiation_liquid_optics_socrates, only : NLiqOpticsCoeffsSOCRATES
    use radiation_liquid_optics_slingo, only : NLiqOpticsCoeffsSlingoSW, &
         &                                     NLiqOpticsCoeffsLindnerLiLW

    type(config_type), intent(inout) :: config

    real(jphook) :: hook_handle

    

    

    
    
    

    

    
    
    

    

    

  end subroutine setup_cloud_optics


  !---------------------------------------------------------------------
  ! Compute cloud optical properties
  subroutine cloud_optics(nlev,istartcol,iendcol, &
       &  config, thermodynamics, cloud, & 
       &  od_lw_cloud, ssa_lw_cloud, g_lw_cloud, &
       &  od_sw_cloud, ssa_sw_cloud, g_sw_cloud)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : config_type, IIceModelFu, IIceModelBaran, &
         &                       IIceModelBaran2016, IIceModelBaran2017, &
         &                       IIceModelYi, &
         &                       ILiquidModelSOCRATES, ILiquidModelSlingo
    use radiation_thermodynamics, only    : thermodynamics_type
    use radiation_cloud, only             : cloud_type
    use radiation_constants, only         : AccelDueToGravity
    use radiation_cloud_optics_data, only : cloud_optics_type
    use radiation_ice_optics_fu, only     : calc_ice_optics_fu_sw, &
         &                                  calc_ice_optics_fu_lw
    use radiation_ice_optics_baran, only  : calc_ice_optics_baran, &
         &                                  calc_ice_optics_baran2016
    use radiation_ice_optics_baran2017, only  : calc_ice_optics_baran2017
    use radiation_ice_optics_yi, only     : calc_ice_optics_yi_sw, &
         &                                  calc_ice_optics_yi_lw
    use radiation_liquid_optics_socrates, only:calc_liq_optics_socrates
    use radiation_liquid_optics_slingo, only:calc_liq_optics_slingo, &
         &                                   calc_liq_optics_lindner_li

    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in), target :: config
    type(thermodynamics_type),intent(in)  :: thermodynamics
    type(cloud_type),   intent(in)        :: cloud

    
    
    
    
    real(jprb), dimension(config%n_bands_lw,nlev,istartcol:iendcol), intent(out) :: &
         &   od_lw_cloud
    real(jprb), dimension(config%n_bands_lw_if_scattering,nlev,istartcol:iendcol), &
         &   intent(out) :: ssa_lw_cloud, g_lw_cloud

    
    
    real(jprb), dimension(config%n_bands_sw,nlev,istartcol:iendcol), intent(out) :: &
         &   od_sw_cloud, ssa_sw_cloud, g_sw_cloud

    
    
    
    real(jprb), dimension(config%n_bands_lw) :: &
         &  od_lw_liq, scat_od_lw_liq, g_lw_liq, &
         &  od_lw_ice, scat_od_lw_ice, g_lw_ice
    real(jprb), dimension(config%n_bands_sw) :: &
         &  od_sw_liq, scat_od_sw_liq, g_sw_liq, &
         &  od_sw_ice, scat_od_sw_ice, g_sw_ice

    
    
    real(jprb) :: lwp_in_cloud, iwp_in_cloud

    
    real(jprb) :: temperature

    
    
    real(jprb) :: factor

    
    
    type(cloud_optics_type), pointer :: ho

    integer    :: jcol, jlev

    real(jphook) :: hook_handle

    

    

    

    
    
    
    
    
    

     

    

  end subroutine cloud_optics

end module radiation_cloud_optics
