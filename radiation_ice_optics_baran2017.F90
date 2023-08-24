! radiation_ice_optics_baran2017.f90 - 2017 parameterization of Baran's ice optical properties
!
! Copyright (C) 2017 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!

module radiation_ice_optics_baran2017

  implicit none

  ! The number of ice coefficients depends on the parameterization
  integer, parameter :: NIceOpticsCoeffsBaran2017 = 9
  integer, parameter :: NIceOpticsGeneralCoeffsBaran2017 = 5

contains

  
  !---------------------------------------------------------------------
  ! Compute ice-particle scattering properties using a
  ! parameterization as a function of ice water mixing ratio and
  ! temperature
  subroutine calc_ice_optics_baran2017(nb, coeff_gen, coeff, ice_wp, &
       &  qi, temperature, od, scat_od, g)

    use parkind1, only : jprb
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff_gen(:)
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: ice_wp, qi
    
    real(jprb), intent(in) :: temperature
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)
    
    
    real(jprb) :: qi_mod, qi_mod_od, qi_mod_ssa, qi_mod_g
    
    

    

    
    
    
    

    
    
    

    

  end subroutine calc_ice_optics_baran2017

end module radiation_ice_optics_baran2017
