! radiation_liquid_optics_socrates.f90 - SOCRATES method for parameterizing liquid droplet optics
!
! Copyright (C) 2014-2016 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!

module radiation_liquid_optics_socrates

  implicit none

  ! SOCRATES (Edwards-Slingo) parameterizes info on the dependence of
  ! the scattering properties in each band on effective radius in
  ! terms of 16 coefficients
  integer, parameter :: NLiqOpticsCoeffsSOCRATES = 16

contains

  !---------------------------------------------------------------------
  ! Compute liquid-droplet scattering properties using a
  ! parameterization consisting of Pade approximants from the
  ! SOCRATES (Edwards-Slingo) code
  subroutine calc_liq_optics_socrates(nb, coeff, lwp, re, od, scat_od, g)

    use parkind1, only : jprb
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: lwp, re
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    

    

    
    
    

    

  end subroutine calc_liq_optics_socrates

end module radiation_liquid_optics_socrates
