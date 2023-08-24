! radiation_mcica_sw.F90 - Monte-Carlo Independent Column Approximation shortwave solver
!
! Copyright (C) 2015-2017 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Receive albedos at g-points
!   2017-04-22  R. Hogan  Store surface fluxes at all g-points
!   2017-10-23  R. Hogan  Renamed single-character variables

module radiation_mcica_sw

contains

  ! Provides elemental function "delta_eddington"

  !---------------------------------------------------------------------
  ! Shortwave Monte Carlo Independent Column Approximation
  ! (McICA). This implementation performs a clear-sky and a cloudy-sky
  ! calculation, and then weights the two to get the all-sky fluxes
  ! according to the total cloud cover. This method reduces noise for
  ! low cloud cover situations, and exploits the clear-sky
  ! calculations that are usually performed for diagnostic purposes
  ! simultaneously. The cloud generator has been carefully written
  ! such that the stochastic cloud field satisfies the prescribed
  ! overlap parameter accounting for this weighting.
  subroutine solver_mcica_sw(nlev,istartcol,iendcol, &
       &  config, single_level, cloud, & 
       &  od, ssa, g, od_cloud, ssa_cloud, g_cloud, &
       &  albedo_direct, albedo_diffuse, incoming_sw, &
       &  flux)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only         : config_type
    use radiation_single_level, only   : single_level_type
    use radiation_cloud, only          : cloud_type
    use radiation_flux, only           : flux_type
    use radiation_two_stream, only     : calc_two_stream_gammas_sw, &
         &                               calc_reflectance_transmittance_sw
    use radiation_adding_ica_sw, only  : adding_ica_sw
    use radiation_cloud_generator, only: cloud_generator

    

    
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type),        intent(in) :: config
    type(single_level_type),  intent(in) :: single_level
    type(cloud_type),         intent(in) :: cloud

    
    
    real(jprb), intent(in), dimension(config%n_g_sw, nlev, istartcol:iendcol) :: &
         &  od, ssa, g

    
    
    real(jprb), intent(in), dimension(config%n_bands_sw,nlev,istartcol:iendcol)   :: &
         &  od_cloud, ssa_cloud, g_cloud

    
    
    
    real(jprb), intent(in), dimension(config%n_g_sw,istartcol:iendcol) :: &
         &  albedo_direct, albedo_diffuse, incoming_sw

    
    type(flux_type), intent(inout):: flux

    

    
    real(jprb)                                 :: cos_sza

    
    
    real(jprb), dimension(config%n_g_sw, nlev) :: ref_clear, trans_clear, reflectance, transmittance

    
    
    real(jprb), dimension(config%n_g_sw, nlev) :: ref_dir_clear, trans_dir_diff_clear, ref_dir, trans_dir_diff

    
    real(jprb), dimension(config%n_g_sw, nlev) :: trans_dir_dir_clear, trans_dir_dir

    
    real(jprb), dimension(config%n_g_sw, nlev+1) :: flux_up, flux_dn_diffuse, flux_dn_direct

    
    
    real(jprb), dimension(config%n_g_sw) :: od_total, ssa_total, g_total

    
    real(jprb), dimension(config%n_g_sw) :: gamma1, gamma2, gamma3

    
    
    real(jprb), dimension(config%n_g_sw,nlev) :: od_scaling

    
    
    real(jprb), dimension(config%n_g_sw) :: od_cloud_new

    
    real(jprb) :: total_cloud_cover

    
    integer :: ng

    
    integer :: jlev, jcol

    real(jphook) :: hook_handle

    

    

    

    
     

    
    
  end subroutine solver_mcica_sw

end module radiation_mcica_sw
