! radiation_ice_optics_fu.F90 - Fu's scheme for ice optical properties
!
! Copyright (C) 2014-2016 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!

module radiation_ice_optics_fu

  implicit none

  ! The number of ice coefficients depends on the parameterization
  integer, parameter :: NIceOpticsCoeffsFuSW  = 10
  integer, parameter :: NIceOpticsCoeffsFuLW  = 11


contains

  !---------------------------------------------------------------------
  ! Compute shortwave ice-particle scattering properties using Fu
  ! (1996) parameterization
  subroutine calc_ice_optics_fu_sw(nb, coeff, ice_wp, &
       &  re, od, scat_od, g)

    use parkind1, only : jprb
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: ice_wp
    
    real(jprb), intent(in) :: re
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    
    real(jprb) :: de_um, inv_de_um
    
    real (jprb) :: iwp_gm_2

    

    

    
    
    
    
    

    
    
    

    

  end subroutine calc_ice_optics_fu_sw


  !---------------------------------------------------------------------
  ! Compute longwave ice-particle scattering properties using Fu et
  ! al. (1998) parameterization
  subroutine calc_ice_optics_fu_lw(nb, coeff, ice_wp, &
       &  re, od, scat_od, g)

    use parkind1, only : jprb
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: ice_wp
    
    real(jprb), intent(in) :: re
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    
    real(jprb) :: de_um, inv_de_um
    
    real (jprb) :: iwp_gm_2

    

    

    
    
    

    
    

    
    
    

    

  end subroutine calc_ice_optics_fu_lw

end module radiation_ice_optics_fu
