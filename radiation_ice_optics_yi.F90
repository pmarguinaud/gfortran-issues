! radiation_ice_optics_yi.F90 - Yi et al. (2013) ice optical properties
!
! Copyright (C) 2017 ECMWF
!
! Authors: Mark Fielding and Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! The reference for this ice optics parameterization is Yi, B.,
! P. Yang, B.A. Baum, T. L'Ecuyer, L. Oreopoulos, E.J. Mlawer,
! A.J. Heymsfield, and K. Liou, 2013: Influence of Ice Particle
! Surface Roughening on the Global Cloud Radiative
! Effect. J. Atmos. Sci., 70, 2794–2807,
! https://doi.org/10.1175/JAS-D-13-020.1

module radiation_ice_optics_yi

  implicit none

  ! The number of ice coefficients depends on the parameterization
  integer, parameter :: NIceOpticsCoeffsYiSW  = 69
  integer, parameter :: NIceOpticsCoeffsYiLW  = 69

  integer, parameter :: NSingleCoeffs = 23

contains

  !---------------------------------------------------------------------
  ! Compute shortwave ice-particle scattering properties using Yi et
  ! al. (2013) parameterization
  subroutine calc_ice_optics_yi_sw(nb, coeff, ice_wp, &
       &  re, od, scat_od, g)

    use parkind1, only : jprb, jpim
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: ice_wp
    
    real(jprb), intent(in) :: re
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    
    real(jprb) :: de_um
    
    real (jprb) :: iwp_gm_2
    
    real(jprb) :: wts_1, wts_2
    integer(jpim) :: lu_idx
    real(kind=jprb), parameter    :: lu_scale  = 0.2_jprb
    real(kind=jprb), parameter    :: lu_offset = 1.0_jprb
    

    

    
    
    

    
    
     

    

    
    
    
    
    
    

    

  end subroutine calc_ice_optics_yi_sw


  !---------------------------------------------------------------------
  ! Compute longwave ice-particle scattering properties using Yi et
  ! al. (2013) parameterization
  subroutine calc_ice_optics_yi_lw(nb, coeff, ice_wp, &
       &  re, od, scat_od, g)

    use parkind1, only : jprb, jpim
    

    
    integer, intent(in)  :: nb
    
    real(jprb), intent(in) :: coeff(:,:)
    
    real(jprb), intent(in) :: ice_wp
    
    real(jprb), intent(in) :: re
    
    real(jprb), intent(out) :: od(nb), scat_od(nb), g(nb)

    
    real(jprb) :: de_um
    
    real (jprb) :: iwp_gm_2
    
    real(jprb) :: wts_1, wts_2
    integer(jpim) :: lu_idx
    real(kind=jprb), parameter    :: lu_scale  = 0.2_jprb
    real(kind=jprb), parameter    :: lu_offset = 1.0_jprb
    

    

    
    
    

    
    
     

    

    
    
    
    
    
    

     

  end subroutine calc_ice_optics_yi_lw

end module radiation_ice_optics_yi
