! radiation_liquid_optics_slingo.F90 - Slingo SW & Lindner-Li LW parameterization of liquid droplet optics
!
! Copyright (C) 2016 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!

module radiation_liquid_optics_slingo

  implicit none

  integer, parameter :: NLiqOpticsCoeffsSlingoSW = 6
  integer, parameter :: NLiqOpticsCoeffsLindnerLiLW = 13

contains

  !---------------------------------------------------------------------
  ! Compute liquid-droplet scattering properties in the shortwave from
  ! Slingo (1989). WARNING: this parameterization is known not to be
  ! very accurate: see Nielsen et al. (GMD 2014).
  subroutine calc_liq_optics_slingo(nb, coeff, lwp, re, od, scat_od, g)

    use parkind1, only : jprb
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: lwp, re
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    
    real(jprb) :: lwp_gm_2
    
    real(jprb) :: re_um, inv_re_um

    

    

    
    
    
    

    
    
    

    

  end subroutine calc_liq_optics_slingo


  !---------------------------------------------------------------------
  ! Compute liquid-droplet scattering properties in the longwave from
  ! Lindner & Li (2000)
  subroutine calc_liq_optics_lindner_li(nb, coeff, lwp, re, od, scat_od, g)

    use parkind1, only : jprb
    use yomhook,  only : lhook, dr_hook, jphook

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: lwp, re
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    
    real(jprb) :: lwp_gm_2
    
    real(jprb) :: re_um, inv_re_um

    real(jphook) :: hook_handle

    

    
    
    
    

    
    
    

    

  end subroutine calc_liq_optics_lindner_li

end module radiation_liquid_optics_slingo
