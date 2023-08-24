! radiation_adding_ica_lw.f90 - Longwave adding method in independent column approximation
!
! Copyright (C) 2015-2017 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Receive emission/albedo rather than planck/emissivity
!   2017-07-12  R. Hogan  Fast adding method for if only clouds scatter
!   2017-10-23  R. Hogan  Renamed single-character variables

module radiation_adding_ica_lw

contains

  !---------------------------------------------------------------------
  ! Use the scalar "adding" method to compute longwave flux profiles,
  ! including scattering, by successively adding the contribution of
  ! layers starting from the surface to compute the total albedo and
  ! total upward emission of the increasingly larger block of
  ! atmospheric layers.
  subroutine adding_ica_lw(ncol, nlev, &
       &  reflectance, transmittance, source_up, source_dn, emission_surf, albedo_surf, &
       &  flux_up, flux_dn)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer, intent(in) :: ncol 
    integer, intent(in) :: nlev 

    
    real(jprb), intent(in),  dimension(ncol) :: emission_surf, albedo_surf

    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: reflectance, transmittance

    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: source_up, source_dn

    
    
    real(jprb), intent(out), dimension(ncol, nlev+1) :: flux_up, flux_dn
    
    
    
    real(jprb), dimension(ncol, nlev+1) :: albedo

    
    
    real(jprb), dimension(ncol, nlev+1) :: source

    
    real(jprb), dimension(ncol, nlev)   :: inv_denominator

    
    integer :: jlev, jcol

    real(jphook) :: hook_handle

    

    

    
    

    
    
    
    
    

    
    

    
    
    

    
    
    

    

  end subroutine adding_ica_lw


  !---------------------------------------------------------------------
  ! Use the scalar "adding" method to compute longwave flux profiles,
  ! including scattering in cloudy layers only.
  subroutine fast_adding_ica_lw(ncol, nlev, &
       &  reflectance, transmittance, source_up, source_dn, emission_surf, albedo_surf, &
       &  is_clear_sky_layer, i_cloud_top, flux_dn_clear, &
       &  flux_up, flux_dn)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer, intent(in) :: ncol 
    integer, intent(in) :: nlev 

    
    real(jprb), intent(in),  dimension(ncol) :: emission_surf, albedo_surf

    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: reflectance, transmittance

    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: source_up, source_dn

    
    logical, intent(in) :: is_clear_sky_layer(nlev)

    
    integer, intent(in) :: i_cloud_top

    
    real(jprb), intent(in), dimension(ncol, nlev+1)  :: flux_dn_clear

    
    
    real(jprb), intent(out), dimension(ncol, nlev+1) :: flux_up, flux_dn
    
    
    
    real(jprb), dimension(ncol, nlev+1) :: albedo

    
    
    real(jprb), dimension(ncol, nlev+1) :: source

    
    real(jprb), dimension(ncol, nlev)   :: inv_denominator

    
    integer :: jlev, jcol

    real(jphook) :: hook_handle

    

    
    

    
    
    
    

    
    
    
    
    

    
    
    

    
    
    

    

  end subroutine fast_adding_ica_lw


  !---------------------------------------------------------------------
  ! If there is no scattering then fluxes may be computed simply by
  ! passing down through the atmosphere computing the downwelling
  ! fluxes from the transmission and emission of each layer, and then
  ! passing back up through the atmosphere to compute the upwelling
  ! fluxes in the same way.
  subroutine calc_fluxes_no_scattering_lw(ncol, nlev, &
       &  transmittance, source_up, source_dn, emission_surf, albedo_surf, flux_up, flux_dn)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer, intent(in) :: ncol 
    integer, intent(in) :: nlev 

    
    real(jprb), intent(in),  dimension(ncol) :: emission_surf, albedo_surf

    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: transmittance

    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: source_up, source_dn

    
    
    real(jprb), intent(out), dimension(ncol, nlev+1) :: flux_up, flux_dn
    
    
    integer :: jlev

    real(jphook) :: hook_handle

    

    
    

    
    
    

    
    

    
    
    
    
    

  end subroutine calc_fluxes_no_scattering_lw

end module radiation_adding_ica_lw
