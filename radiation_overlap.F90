! radiation_overlap.F90 - Module to compute cloud overlap quantities
!
! Copyright (C) 2014-2018 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-10-23  R. Hogan  Renamed single-character variables
!   2018-10-05  R. Hogan  Generalized alpha overlap for non-equal regions
!   2018-10-08  R. Hogan  Removed calc_region_fractions

module radiation_overlap

  implicit none

  public :: calc_overlap_matrices

contains


  ! This function now superceded by calc_region_properties in module
  ! radiation_regions
  ! !---------------------------------------------------------------------
  ! ! Return an array of length nreg containing the fraction of the
  ! ! gridbox occupied by each region for the specified cloud fraction.
  ! pure function calc_region_fractions(nreg, cloud_fraction)

  !   use parkind1, only : jprb

  !   integer,    intent(in)      :: nreg
  !   real(jprb), intent(in)      :: cloud_fraction

  !   real(jprb), dimension(nreg) :: calc_region_fractions
  !   integer :: jreg

  !   if (nreg == 1) then
  !     ! Only one region: must occupy all of gridbox
  !     calc_region_fractions(1) = 1.0_jprb
  !   else
  !     ! Two or more regions: the first is the cloud-free region
  !     calc_region_fractions(1) = 1.0_jprb - cloud_fraction

  !     do jreg = 2,nreg
  !       ! The cloudy regions are assumed to each have the same
  !       ! fraction - see Shonk and Hogan (2008) for justification
  !       calc_region_fractions(jreg) = cloud_fraction / (nreg - 1.0_jprb)
  !     end do
  !   end if

  ! end function calc_region_fractions

  !---------------------------------------------------------------------
  ! Calculate a matrix expressing the overlap of regions in adjacent
  ! layers, using the method of Shonk et al. (2010) in terms of their
  ! "beta" overlap parameter
  pure function calc_beta_overlap_matrix(nreg, op, frac_upper, frac_lower, &
       &  frac_threshold) result(overlap_matrix)

    use parkind1, only : jprb
    
    integer, intent(in) :: nreg 

    
    
    real(jprb), intent(in), dimension(nreg) :: op, frac_upper, frac_lower

    
    
    real(jprb), intent(in) :: frac_threshold

    
    real(jprb) :: overlap_matrix(nreg,nreg)

    
    
    real(jprb) :: denominator, factor

    
    
    real(jprb) :: op_x_frac_min(nreg)

    integer :: jupper, jlower, jreg

    
    
    
    
    
    
    
    
    
    

  end function calc_beta_overlap_matrix


  !---------------------------------------------------------------------
  ! Calculate a matrix expressing the overlap of regions in adjacent
  ! layers, using the Hogan and Illingworth (2000) "alpha" overlap
  ! parameter, but allowing for the two cloudy regions in the
  ! Tripleclouds assumption to have different areas
  pure function calc_alpha_overlap_matrix(nreg, op, op_inhom, &
       &  frac_upper, frac_lower) result(overlap_matrix)

    use parkind1, only : jprb
    
    integer, intent(in) :: nreg 

    
    
    real(jprb), intent(in) :: op, op_inhom

    
    
    real(jprb), intent(in), dimension(nreg) :: frac_upper, frac_lower

    
    real(jprb) :: overlap_matrix(nreg,nreg)

    
    real(jprb) :: pair_cloud_cover

    
    real(jprb) :: cf_upper, cf_lower

    
    real(jprb) :: one_over_cf

    
    real(jprb) :: frac_both

    
    

    

    
    
    

  end function calc_alpha_overlap_matrix


  !---------------------------------------------------------------------
  ! Calculate a matrix expressing the overlap of regions in adjacent
  ! layers, using the Hogan and Illingworth (2000) "alpha" overlap
  ! parameter, and assuming the two cloudy regions in the Tripleclouds
  ! assumption have the same area
  pure function calc_alpha_overlap_matrix_simple(nreg, op, op_inhom, &
       &  cf_upper, cf_lower) result(overlap_matrix)

    use parkind1, only : jprb
    
    integer, intent(in) :: nreg 

    
    
    real(jprb), intent(in) :: op, op_inhom

    
    real(jprb), intent(in) :: cf_upper, cf_lower

    
    real(jprb) :: overlap_matrix(nreg,nreg)

    
    real(jprb) :: pair_cloud_cover

    real(jprb) :: cloud_unit

    

    
    
    

  end function calc_alpha_overlap_matrix_simple


  !---------------------------------------------------------------------
  ! Compute the upward and downward overlap matrices u_matrix and
  ! v_matrix, respectively, where u_matrix is defined such that
  ! y=u_matrix*x, where x is a vector of upwelling fluxes in each
  ! region just below an interface, and y is a vector of upwelling
  ! fluxes in each region just above that interface. For nlev model
  ! levels there are nlev+1 interfaces including the ground and
  ! top-of-atmosphere, and so that is one of the dimensions of
  ! u_matrix and v_matrix.
  subroutine calc_overlap_matrices(nlev,nreg,istartcol,iendcol, &
       &     region_fracs, overlap_param, u_matrix, v_matrix, decorrelation_scaling, &
       &     cloud_fraction_threshold, cloud_cover, use_beta_overlap)

    use parkind1,     only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    
    integer,  intent(in) :: nlev, nreg

    
    
    integer, intent(in) :: istartcol, iendcol

    
    
    
    real(jprb), intent(in), dimension(1:nreg,nlev,istartcol:iendcol)  :: region_fracs

    
    
    real(jprb), intent(in), dimension(:,:)  :: overlap_param  

    
    real(jprb), intent(out), dimension(nreg,nreg,nlev+1,istartcol:iendcol) &
         &  :: u_matrix, v_matrix

    
    
    
    
    
    real(jprb), intent(in), optional :: decorrelation_scaling

    
    real(jprb), intent(in), optional :: cloud_fraction_threshold

    
    real(jprb), intent(out), optional :: cloud_cover(:)

    
    logical, intent(in), optional :: use_beta_overlap

    
    
    integer  :: jcol, jlev, jupper, jlower

    
    real(jprb) :: overlap_matrix(nreg,nreg)

    
    
    real(jprb) :: frac_upper(nreg), frac_lower(nreg)

    
    real(jprb) :: op(nreg)

    
    
    real(jprb) :: frac_threshold

    
    
    real(jprb) :: used_decorrelation_scaling

    logical :: use_beta_overlap_param

    real(jphook) :: hook_handle

    

    

    

    

    
     

    

  end subroutine calc_overlap_matrices
  
end module radiation_overlap
