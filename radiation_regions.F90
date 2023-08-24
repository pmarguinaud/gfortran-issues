! radiation_regions.F90 -- Properties of horizontal regions in Tripleclouds & SPARTACUS
!
! Copyright (C) 2016-2018 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-07-14  R. Hogan  Incorporate gamma distribution option
!   2018-10-06  R. Hogan  Merged from radiation_optical_depth_scaling.h and radiation_overlap.F90

module radiation_regions

  implicit none

  public :: calc_region_properties

contains

  !---------------------------------------------------------------------
  ! Compute the optical depth scalings for the optically "thick" and
  ! "thin" regions of a Tripleclouds representation of a sub-grid PDF
  ! of cloud optical depth. Following Shonk and Hogan (2008), the 16th
  ! percentile is used for the thin region, and the formulas estimate
  ! this for both lognormal and gamma distributions. However, an
  ! adjustment is needed for the gamma distribution at large
  ! fractional standard deviations.
  subroutine calc_region_properties(nlev, nreg, istartcol, iendcol, do_gamma, &
       &  cloud_fraction, frac_std, reg_fracs, od_scaling, cloud_fraction_threshold)

    use parkind1,     only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook

    
    real(jprb), parameter :: MinGammaODScaling = 0.025_jprb

    
    
    
    
    
    
    
    real(jprb), parameter :: MinLowerFrac      = 0.5_jprb
    real(jprb), parameter :: MaxLowerFrac      = 0.9_jprb
    real(jprb), parameter :: FSDAtMinLowerFrac = 1.5_jprb
    real(jprb), parameter :: FSDAtMaxLowerFrac = 3.725_jprb
    
    
    real(jprb), parameter :: LowerFracFSDGradient &
         &  = (MaxLowerFrac-MinLowerFrac) / (FSDAtMaxLowerFrac-FSDAtMinLowerFrac)
    real(jprb), parameter :: LowerFracFSDIntercept &
         &  = MinLowerFrac - FSDAtMinLowerFrac*LowerFracFSDGradient

    
    integer, intent(in) :: nlev, nreg

    
    integer, intent(in) :: istartcol, iendcol

    
    logical, intent(in) :: do_gamma

    
    
    real(jprb), intent(in), dimension(:,:)  :: cloud_fraction 

    
    real(jprb), intent(in), dimension(:,:)  :: frac_std       

    
    real(jprb), intent(out) :: reg_fracs(1:nreg,nlev,istartcol:iendcol)

    
    real(jprb), intent(out) :: od_scaling(2:nreg,nlev,istartcol:iendcol)

    
    real(jprb), intent(in), optional :: cloud_fraction_threshold

    
    
    real(jprb) :: frac_threshold

    
    integer :: jcol, jlev

    real(jphook) :: hook_handle

    

    

    

    

  end subroutine calc_region_properties

end module radiation_regions

