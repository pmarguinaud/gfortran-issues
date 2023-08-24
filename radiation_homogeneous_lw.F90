! radiation_homogeneous_lw.F90 - Longwave homogeneous-column (no cloud fraction) solver
!
! Copyright (C) 2016-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Receive emission/albedo rather than planck/emissivity
!   2017-04-22  R. Hogan  Store surface fluxes at all g-points
!   2017-10-23  R. Hogan  Renamed single-character variables
!   2019-01-14  R. Hogan  Save spectral flux profile if required

module radiation_homogeneous_lw

contains

  !---------------------------------------------------------------------
  ! Longwave homogeneous solver, in which clouds are assumed to fill
  ! the gridbox horizontally
  subroutine solver_homogeneous_lw(nlev,istartcol,iendcol, &
       &  config, cloud, & 
       &  od, ssa, g, od_cloud, ssa_cloud, g_cloud, planck_hl, &
       &  emission, albedo, &
       &  flux)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only         : config_type
    use radiation_cloud, only          : cloud_type
    use radiation_flux, only           : flux_type, indexed_sum_profile
    use radiation_two_stream, only     : calc_two_stream_gammas_lw, &
         &                               calc_reflectance_transmittance_lw, &
         &                               calc_no_scattering_transmittance_lw
    use radiation_adding_ica_lw, only  : adding_ica_lw, calc_fluxes_no_scattering_lw
    use radiation_lw_derivatives, only : calc_lw_derivatives_ica

    

    
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type),        intent(in) :: config
    type(cloud_type),         intent(in) :: cloud

    
    
    real(jprb), intent(in), dimension(config%n_g_lw, nlev, istartcol:iendcol) :: &
         &  od
    real(jprb), intent(in), dimension(config%n_g_lw_if_scattering, nlev, istartcol:iendcol) :: &
         &  ssa, g

    
    
    real(jprb), intent(in), dimension(config%n_bands_lw,nlev,istartcol:iendcol)   :: &
         &  od_cloud
    real(jprb), intent(in), dimension(config%n_bands_lw_if_scattering, &
         &  nlev,istartcol:iendcol) :: ssa_cloud, g_cloud

    
    real(jprb), intent(in), dimension(config%n_g_lw,nlev+1,istartcol:iendcol) :: &
         &  planck_hl
  
    
    
    real(jprb), intent(in), dimension(config%n_g_lw, istartcol:iendcol) &
         &  :: emission, albedo

    
    type(flux_type), intent(inout):: flux

    

    
    
    real(jprb), dimension(config%n_g_lw, nlev) :: reflectance, transmittance

    
    
    real(jprb), dimension(config%n_g_lw, nlev) :: source_up, source_dn

    
    real(jprb), dimension(config%n_g_lw, nlev+1) :: flux_up, flux_dn

    
    
    real(jprb), dimension(config%n_g_lw) :: od_total, ssa_total, g_total

    
    real(jprb), dimension(config%n_g_lw) :: gamma1, gamma2

    
    real(jprb), dimension(config%n_g_lw) :: od_cloud_g

    
    logical :: is_cloudy_profile

    
    integer :: ng

    
    integer :: jlev, jcol

    real(jphook) :: hook_handle

    

    

    
    

    
    
  end subroutine solver_homogeneous_lw

end module radiation_homogeneous_lw
