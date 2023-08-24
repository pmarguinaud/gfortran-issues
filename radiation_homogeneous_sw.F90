! radiation_homogeneous_sw.F90 - Shortwave homogeneous-column (no cloud fraction) solver
!
! Copyright (C) 2016-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Receive albedos at g-points
!   2017-04-22  R. Hogan  Store surface fluxes at all g points
!   2017-10-23  R. Hogan  Renamed single-character variables
!   2019-01-14  R. Hogan  Save spectral flux profile if required

module radiation_homogeneous_sw

contains

  ! Provides elemental function "delta_eddington"

  !---------------------------------------------------------------------
  ! Shortwave homogeneous solver, in which clouds are assumed to fill
  ! the gridbox horizontally
  subroutine solver_homogeneous_sw(nlev,istartcol,iendcol, &
       &  config, single_level, cloud, & 
       &  od, ssa, g, od_cloud, ssa_cloud, g_cloud, &
       &  albedo_direct, albedo_diffuse, incoming_sw, &
       &  flux)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only         : config_type
    use radiation_single_level, only   : single_level_type
    use radiation_cloud, only          : cloud_type
    use radiation_flux, only           : flux_type, &
         &                               indexed_sum_profile, add_indexed_sum_profile
    use radiation_two_stream, only     : calc_two_stream_gammas_sw, &
         &                       calc_reflectance_transmittance_sw
    use radiation_constants, only      : Pi, GasConstantDryAir, &
         &                               AccelDueToGravity
    use radiation_adding_ica_sw, only  : adding_ica_sw

    

    
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

    
    real(jprb), dimension(config%n_g_sw, nlev) :: reflectance, transmittance

    
    
    real(jprb), dimension(config%n_g_sw, nlev) :: ref_dir, trans_dir_diff

    
    real(jprb), dimension(config%n_g_sw, nlev) :: trans_dir_dir

    
    real(jprb), dimension(config%n_g_sw, nlev+1) :: flux_up, flux_dn_diffuse, flux_dn_direct

    
    
    real(jprb), dimension(config%n_g_sw) :: od_total, ssa_total, g_total

    
    real(jprb), dimension(config%n_g_sw) :: gamma1, gamma2, gamma3

    
    real(jprb), dimension(config%n_g_sw) :: od_cloud_g

    
    logical :: is_cloudy_profile

    
    integer :: ng

    
    integer :: jlev, jcol

    real(jphook) :: hook_handle

    

    

    
    

    

  end subroutine solver_homogeneous_sw

end module radiation_homogeneous_sw
