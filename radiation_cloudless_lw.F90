! radiation_cloudless_lw.F90 - Longwave homogeneous cloudless solver
!
! Copyright (C) 2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!

module radiation_cloudless_lw

contains

  !---------------------------------------------------------------------
  ! Longwave homogeneous solver containing no clouds
  subroutine solver_cloudless_lw(nlev,istartcol,iendcol, &
       &  config, od, ssa, g, planck_hl, emission, albedo, flux)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only         : config_type
    use radiation_flux, only           : flux_type, indexed_sum_profile
    use radiation_two_stream, only     : calc_two_stream_gammas_lw, &
         &                               calc_reflectance_transmittance_lw, &
         &                               calc_no_scattering_transmittance_lw
    use radiation_adding_ica_lw, only  : adding_ica_lw, calc_fluxes_no_scattering_lw
    use radiation_lw_derivatives, only : calc_lw_derivatives_ica

    

    
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type),        intent(in) :: config

    
    
    real(jprb), intent(in), dimension(config%n_g_lw, nlev, istartcol:iendcol) :: &
         &  od
    real(jprb), intent(in), dimension(config%n_g_lw_if_scattering, nlev, istartcol:iendcol) :: &
         &  ssa, g

    
    real(jprb), intent(in), dimension(config%n_g_lw,nlev+1,istartcol:iendcol) :: &
         &  planck_hl
  
    
    
    real(jprb), intent(in), dimension(config%n_g_lw, istartcol:iendcol) &
         &  :: emission, albedo

    
    type(flux_type), intent(inout):: flux

    

    
    
    real(jprb), dimension(config%n_g_lw, nlev) :: reflectance, transmittance

    
    
    real(jprb), dimension(config%n_g_lw, nlev) :: source_up, source_dn

    
    real(jprb), dimension(config%n_g_lw, nlev+1) :: flux_up, flux_dn

    
    
    real(jprb), dimension(config%n_g_lw) :: ssa_total, g_total

    
    real(jprb), dimension(config%n_g_lw) :: gamma1, gamma2

    
    integer :: ng

    
    integer :: jlev, jcol

    real(jphook) :: hook_handle

    

    

    
    

    
    
  end subroutine solver_cloudless_lw

end module radiation_cloudless_lw
