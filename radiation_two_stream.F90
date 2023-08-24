! radiation_two_stream.F90 - Compute two-stream coefficients
!
! Copyright (C) 2014-2021 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-05-04  P Dueben/R Hogan  Use JPRD where double precision essential
!   2017-07-12  R Hogan  Optimized LW coeffs in low optical depth case
!   2017-07-26  R Hogan  Added calc_frac_scattered_diffuse_sw routine
!   2017-10-23  R Hogan  Renamed single-character variables
!   2021-02-19  R Hogan  Security for shortwave singularity

module radiation_two_stream

  use parkind1, only : jprb, jprd

  implicit none

  ! Elsasser's factor: the effective factor by which the zenith
  ! optical depth needs to be multiplied to account for longwave
  ! transmission at all angles through the atmosphere.  Alternatively
  ! think of acos(1/lw_diffusivity) to be the effective zenith angle
  ! of longwave radiation.
  real(jprd), parameter :: LwDiffusivity = 1.66_jprd

  ! Shortwave diffusivity factor assumes hemispheric isotropy, assumed
  ! by Zdunkowski's scheme and most others; note that for efficiency
  ! this parameter is not used in the calculation of the gamma values,
  ! but is used in the SPARTACUS solver.
  real(jprb), parameter :: SwDiffusivity = 2.00_jprb

  ! The routines in this module can be called millions of times, so
  !calling Dr Hook for each one may be a significant overhead.
  !Uncomment the following to turn Dr Hook on.
!#define DO_DR_HOOK_TWO_STREAM

contains

  !---------------------------------------------------------------------
  ! Calculate the two-stream coefficients gamma1 and gamma2 for the
  ! longwave
  subroutine calc_two_stream_gammas_lw(ng, ssa, g, &
       &                               gamma1, gamma2)


    use yomhook,  only           : lhook, dr_hook, jphook


    integer, intent(in) :: ng
    
    real(jprb), intent(in),  dimension(:) :: ssa, g
    real(jprb), intent(out), dimension(:) :: gamma1, gamma2

    real(jprb) :: factor

    integer    :: jg


    real(jphook) :: hook_handle

    


    


    


  end subroutine calc_two_stream_gammas_lw


  !---------------------------------------------------------------------
  ! Calculate the two-stream coefficients gamma1-gamma4 in the
  ! shortwave
  subroutine calc_two_stream_gammas_sw(ng, mu0, ssa, g, &
       &                               gamma1, gamma2, gamma3)


    use yomhook,  only           : lhook, dr_hook, jphook


    integer, intent(in) :: ng
    
    
    real(jprb), intent(in)                :: mu0
    real(jprb), intent(in),  dimension(:) :: ssa, g
    real(jprb), intent(out), dimension(:) :: gamma1, gamma2, gamma3

    real(jprb) :: factor

    integer    :: jg


    real(jphook) :: hook_handle

    


    
    
    


    


  end subroutine calc_two_stream_gammas_sw


  !---------------------------------------------------------------------
  ! Compute the longwave reflectance and transmittance to diffuse
  ! radiation using the Meador & Weaver formulas, as well as the
  ! upward flux at the top and the downward flux at the base of the
  ! layer due to emission from within the layer assuming a linear
  ! variation of Planck function within the layer.
  subroutine calc_reflectance_transmittance_lw(ng, &
       &    od, gamma1, gamma2, planck_top, planck_bot, &
       &    reflectance, transmittance, source_up, source_dn)


    use yomhook,  only           : lhook, dr_hook, jphook


    integer, intent(in) :: ng

    
    real(jprb), intent(in), dimension(ng) :: od

    
    
    
    real(jprb), intent(in), dimension(ng) :: gamma1, gamma2

    
    
    real(jprb), intent(in), dimension(ng) :: planck_top, planck_bot

    
    
    
    real(jprb), intent(out), dimension(ng) :: reflectance, transmittance

    
    
    real(jprb), intent(out), dimension(ng) :: source_up, source_dn

    real(jprd) :: k_exponent, reftrans_factor
    real(jprd) :: exponential  
    real(jprd) :: exponential2 

    real(jprd) :: coeff, coeff_up_top, coeff_up_bot, coeff_dn_top, coeff_dn_bot

    integer :: jg


    real(jphook) :: hook_handle

    


    
    

    

  
  end subroutine calc_reflectance_transmittance_lw
  


  !---------------------------------------------------------------------
  ! As calc_reflectance_transmittance_lw but for an isothermal layer
  subroutine calc_reflectance_transmittance_isothermal_lw(ng, &
       &    od, gamma1, gamma2, planck, &
       &    reflectance, transmittance, source)


    use yomhook,  only           : lhook, dr_hook, jphook


    integer, intent(in) :: ng

    
    real(jprb), intent(in), dimension(ng) :: od

    
    
    
    real(jprb), intent(in), dimension(ng) :: gamma1, gamma2

    
    
    real(jprb), intent(in), dimension(ng) :: planck

    
    
    
    real(jprb), intent(out), dimension(ng) :: reflectance, transmittance

    
    
    real(jprb), intent(out), dimension(ng) :: source

    real(jprd) :: k_exponent, reftrans_factor
    real(jprd) :: exponential  
    real(jprd) :: exponential2 

    integer :: jg


    real(jphook) :: hook_handle

    


    
    

    

  
  end subroutine calc_reflectance_transmittance_isothermal_lw
  


  !---------------------------------------------------------------------
  ! Compute the longwave transmittance to diffuse radiation in the
  ! no-scattering case, as well as the upward flux at the top and the
  ! downward flux at the base of the layer due to emission from within
  ! the layer assuming a linear variation of Planck function within
  ! the layer.
  subroutine calc_no_scattering_transmittance_lw(ng, &
       &    od, planck_top, planck_bot, transmittance, source_up, source_dn)


    use yomhook,  only           : lhook, dr_hook, jphook


    integer, intent(in) :: ng

    
    real(jprb), intent(in), dimension(ng) :: od

    
    
    real(jprb), intent(in), dimension(ng) :: planck_top, planck_bot

    
    
    
    real(jprb), intent(out), dimension(ng) :: transmittance

    
    
    real(jprb), intent(out), dimension(ng) :: source_up, source_dn

    real(jprd) :: coeff, coeff_up_top, coeff_up_bot, coeff_dn_top, coeff_dn_bot 

    integer :: jg


    real(jphook) :: hook_handle

    


    

    
    
    
    
    
    
    
    


    


  end subroutine calc_no_scattering_transmittance_lw
   
   
  !---------------------------------------------------------------------
  ! Compute the shortwave reflectance and transmittance to diffuse
  ! radiation using the Meador & Weaver formulas, as well as the
  ! "direct" reflection and transmission, which really means the rate
  ! of transfer of direct solar radiation (into a plane perpendicular
  ! to the direct beam) into diffuse upward and downward streams at
  ! the top and bottom of the layer, respectively.  Finally,
  ! trans_dir_dir is the transmittance of the atmosphere to direct
  ! radiation with no scattering.
  subroutine calc_reflectance_transmittance_sw(ng, mu0, od, ssa, &
       &      gamma1, gamma2, gamma3, ref_diff, trans_diff, &
       &      ref_dir, trans_dir_diff, trans_dir_dir)
    

    use yomhook,  only           : lhook, dr_hook, jphook


    integer, intent(in) :: ng

    
    real(jprb), intent(in) :: mu0

    
    real(jprb), intent(in), dimension(ng) :: od, ssa

    
    
    real(jprb), intent(in), dimension(ng) :: gamma1, gamma2, gamma3

    
    
    
    
    real(jprb), intent(out), dimension(ng) :: ref_dir, trans_dir_diff

    
    
    
    real(jprb), intent(out), dimension(ng) :: ref_diff, trans_diff

    
    real(jprb), intent(out), dimension(ng) :: trans_dir_dir

    real(jprd) :: gamma4, alpha1, alpha2, k_exponent, reftrans_factor
    real(jprd) :: exponential0 
    real(jprd) :: exponential  
    real(jprd) :: exponential2 
    real(jprd) :: k_mu0, k_gamma3, k_gamma4
    real(jprd) :: k_2_exponential, od_over_mu0
    integer    :: jg

    
    
    
    real(jprb) :: mu0_local


    real(jphook) :: hook_handle

    


    
    

    

 
  end subroutine calc_reflectance_transmittance_sw
  
  !---------------------------------------------------------------------
  ! As above but with height as a vertical coordinate rather than
  ! optical depth
  subroutine calc_reflectance_transmittance_z_sw(ng, mu0, depth, &
       &      gamma0, gamma1, gamma2, gamma3, gamma4, &
       &      ref_diff, trans_diff, ref_dir, trans_dir_diff, trans_dir_dir)
    

    use yomhook,  only           : lhook, dr_hook, jphook


    integer, intent(in) :: ng

    
    real(jprb), intent(in) :: mu0

    
    real(jprb), intent(in) :: depth

    
    
    real(jprb), intent(in), dimension(ng) :: gamma1, gamma2, gamma3, gamma4

    
    
    real(jprb), intent(in), dimension(ng) :: gamma0

    
    
    
    
    real(jprb), intent(out), dimension(ng) :: ref_dir, trans_dir_diff

    
    
    
    real(jprb), intent(out), dimension(ng) :: ref_diff, trans_diff

    
    real(jprb), intent(out), dimension(ng) :: trans_dir_dir

    real(jprd) :: alpha1, alpha2, k_exponent, reftrans_factor
    real(jprd) :: exponential0 
    real(jprd) :: exponential  
    real(jprd) :: exponential2 
    real(jprd) :: k_mu0, k_gamma3, k_gamma4
    real(jprd) :: k_2_exponential, od_over_mu0
    integer    :: jg


    real(jphook) :: hook_handle

    


    
    

    

 
  end subroutine calc_reflectance_transmittance_z_sw
  

  !---------------------------------------------------------------------
  ! Compute the fraction of shortwave transmitted diffuse radiation
  ! that is scattered during its transmission, used to compute
  ! entrapment in SPARTACUS
  subroutine calc_frac_scattered_diffuse_sw(ng, od, &
       &      gamma1, gamma2, frac_scat_diffuse)
    

    use yomhook,  only           : lhook, dr_hook, jphook


    integer, intent(in) :: ng

    
    real(jprb), intent(in), dimension(ng) :: od

    
    
    real(jprb), intent(in), dimension(ng) :: gamma1, gamma2

    
    
    real(jprb), intent(out), dimension(ng) :: frac_scat_diffuse

    real(jprd) :: k_exponent, reftrans_factor
    real(jprd) :: exponential  
    real(jprd) :: exponential2 
    real(jprd) :: k_2_exponential
    integer    :: jg


    real(jphook) :: hook_handle

    


    
    

    

 
  end subroutine calc_frac_scattered_diffuse_sw

end module radiation_two_stream
