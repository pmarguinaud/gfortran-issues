! radiation_ice_optics_fu.f90 - Scheme for ice optical properties adapted from Baran's data
!
! Copyright (C) 2014-2016 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!

module radiation_ice_optics_baran

  implicit none

  ! The number of ice coefficients depends on the parameterization
  integer, parameter :: NIceOpticsCoeffsBaran = 9
  integer, parameter :: NIceOpticsCoeffsBaran2016 = 5

contains

  
  !---------------------------------------------------------------------
  ! Compute ice-particle scattering properties using a
  ! parameterization as a function of ice water mixing ratio only
  subroutine calc_ice_optics_baran(nb, coeff, ice_wp, &
       &  qi, od, scat_od, g)

    use parkind1, only : jprb
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: ice_wp, qi
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    

    

    
    
    
    
    
    
    

    

  end subroutine calc_ice_optics_baran


  !---------------------------------------------------------------------
  ! Compute ice-particle scattering properties using a
  ! parameterization as a function of ice water mixing ratio and
  ! temperature
  subroutine calc_ice_optics_baran2016(nb, coeff, ice_wp, &
       &  qi, temperature, od, scat_od, g)

    use parkind1, only : jprb
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: ice_wp, qi
    
    real(jprb), intent(in) :: temperature
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)
    
    
    real(jprb) :: qi_T, T2, qi_over_T4
    
    

    

    

    

    
    
    

    

  end subroutine calc_ice_optics_baran2016

end module radiation_ice_optics_baran
