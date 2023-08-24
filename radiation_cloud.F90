! radiation_cloud.F90 - Derived type to store cloud/precip properties
!
! Copyright (C) 2014-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2019-01-14  R. Hogan  Added inv_inhom_effective_size variable
!   2019-01-14  R. Hogan  Added out_of_physical_bounds routine

module radiation_cloud

  use parkind1, only : jprb

  implicit none

  !---------------------------------------------------------------------
  ! The intention is that all variables describing clouds and
  ! radiatively-active precipitation are contained in this derived
  ! type, and if cloud variables are to be added in future, they can
  ! be added to this type without requiring extra variables to be
  ! passed between subroutines elsewhere in the program.
  type cloud_type
    ! For maximum flexibility, an arbitrary number "ntype" of
    ! cloud types could be stored, as follows:
    !     integer :: ntype     ! number of cloud types
    !     integer :: nfraction ! number of cloud fractions
    !     real(jprb), allocatable, dimension(:,:,:) :: &
    !          mixing_ratio, & ! (ncol,nwetlev,ntype) mass mixing ratio (kg/kg)
    !          particle_size,& ! (ncol,nwetlev,ntype) effective radius/size (m)
    !          fraction        ! (ncol,nwetlev,nfraction) areal (i.e. cloud) fraction
    ! However, for practical purposes at the moment we consider two
    ! cloud types, liquid cloud droplets and ice cloud
    ! particles.  The following variables are dimensioned (ncol,nlev)
    real(jprb), allocatable, dimension(:,:) :: &
         &  q_liq,  q_ice,  & ! mass mixing ratio (kg/kg)
         &  re_liq, re_ice, & ! effective radius (m)
         &  fraction          ! (0-1) Assume liq & ice completely mixed

    ! The fractional standard deviation of cloud optical depth in the
    ! cloudy part of the gridbox.  In the Tripleclouds representation
    ! of cloud inhomogeneity, this is implemented by splitting the
    ! cloudy part of the gridbox into two equal-area regions, one
    ! with the cloud optical depth scaled by 1+fractional_std and the
    ! other scaled by 1-fractional_std. This variable is dimensioned
    ! (ncol,nlev)
    real(jprb), allocatable, dimension(:,:) :: fractional_std

    ! The inverse of the effective horizontal size of the clouds in
    ! the gridbox, used to compute the cloud edge length per unit
    ! gridbox area for use in representing 3D effects. This variable
    ! is dimensioned (ncol,nlev).
    real(jprb), allocatable, dimension(:,:) :: inv_cloud_effective_size ! m-1

    ! Similarly for the in-cloud heterogeneities, used to compute the
    ! edge length between the optically thin and thick cloudy regions
    ! of the gridbox.
    real(jprb), allocatable, dimension(:,:) :: inv_inhom_effective_size ! m-1

    ! The following variable describes the overlap of cloud boundaries
    ! in adjacent layers, with dimensions (ncol,nlev-1): 1 corresponds
    ! to maximum overlap and 0 to random overlap. Depending on the
    ! ecRad configuration, it may be the "alpha" overlap parameter of
    ! Hogan and Illingworth (2000) or the "beta" overlap parameter of
    ! Shonk et al. (2010).
    real(jprb), allocatable, dimension(:,:) :: overlap_param

  contains
    procedure :: allocate   => allocate_cloud_arrays
    procedure :: deallocate => deallocate_cloud_arrays
    procedure :: set_overlap_param
    procedure :: set_overlap_param_approx
    procedure :: create_fractional_std
    procedure :: create_inv_cloud_effective_size
    procedure :: create_inv_cloud_effective_size_eta
    procedure :: param_cloud_effective_separation_eta
    procedure :: crop_cloud_fraction
    procedure :: out_of_physical_bounds

  end type cloud_type

contains

  !---------------------------------------------------------------------
  ! Allocate arrays for describing clouds and precipitation, although
  ! in the offline code these are allocated when they are read from
  ! the NetCDF file
  subroutine allocate_cloud_arrays(this, ncol, nlev, use_inhom_effective_size)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(cloud_type), intent(inout) :: this
    integer, intent(in)              :: ncol  
    integer, intent(in)              :: nlev  
    logical, intent(in), optional    :: use_inhom_effective_size

    real(jphook)                     :: hook_handle

    

    
    
    
    
    
    
    
    

    

    

  end subroutine allocate_cloud_arrays


  !---------------------------------------------------------------------
  ! Deallocate arrays
  subroutine deallocate_cloud_arrays(this)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(cloud_type), intent(inout) :: this

    real(jphook)                     :: hook_handle

    

    
    
    
    
    
    
    
    
    

    

  end subroutine deallocate_cloud_arrays


  !---------------------------------------------------------------------
  ! Compute and store the overlap parameter from the provided overlap
  ! decorrelation length (in metres).  If istartcol and/or iendcol are
  ! provided then only columns in this range are computed.  If the
  ! overlap_param array has not been allocated then it will be
  ! allocated to be of the correct size relative to the pressure
  ! field. 
  subroutine set_overlap_param(this, thermodynamics, decorrelation_length, &
       &  istartcol, iendcol)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_constants,      only : GasConstantDryAir, AccelDueToGravity

    class(cloud_type),         intent(inout) :: this
    type(thermodynamics_type), intent(in)    :: thermodynamics
    real(jprb),                intent(in)    :: decorrelation_length 
    integer,         optional, intent(in)    :: istartcol, iendcol

    
    real(jprb), parameter :: R_over_g = GasConstantDryAir / AccelDueToGravity

    
    
    integer :: i1, i2

    integer :: ncol, nlev

    integer :: jlev

    real(jphook)      :: hook_handle

    

    
    
    
    

    

    

    

    

    

  end subroutine set_overlap_param


  !---------------------------------------------------------------------
  ! Compute and store the overlap parameter from the provided overlap
  ! decorrelation length (in metres).  If istartcol and/or iendcol are
  ! provided then only columns in this range are computed.  If the
  ! overlap_param array has not been allocated then it will be
  ! allocated to be of the correct size relative to the pressure
  ! field. This is the APPROXIMATE method as it assumes a fixed
  ! atmospheric scale height, which leads to differences particularly
  ! in low cloud.
  subroutine set_overlap_param_approx(this, thermodynamics, decorrelation_length, &
       &  istartcol, iendcol)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_thermodynamics, only : thermodynamics_type

    class(cloud_type),         intent(inout) :: this
    type(thermodynamics_type), intent(in)    :: thermodynamics
    real(jprb),                intent(in)    :: decorrelation_length 
    integer,         optional, intent(in)    :: istartcol, iendcol

    
    
    
    
    
    real(jprb), parameter :: scale_height = 8000.0_jprb

    
    
    integer :: i1, i2

    integer :: ncol, nlev

    real(jphook)      :: hook_handle

    

    
    
    
    

    

    

    

    

    

  end subroutine set_overlap_param_approx


  !---------------------------------------------------------------------
  ! Create a matrix of constant fractional standard deviations
  ! (dimensionless)
  subroutine create_fractional_std(this, ncol, nlev, frac_std)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(cloud_type), intent(inout) :: this
    integer,           intent(in)    :: ncol, nlev
    real(jprb),        intent(in)    :: frac_std

    real(jphook)           :: hook_handle

    

    
    
    

    

    

  end subroutine create_fractional_std


  !---------------------------------------------------------------------
  ! Create a matrix of constant inverse cloud effective size (m-1)
  subroutine create_inv_cloud_effective_size(this, ncol, nlev, inv_eff_size)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(cloud_type), intent(inout) :: this
    integer,           intent(in)    :: ncol, nlev
    real(jprb),        intent(in)    :: inv_eff_size

    real(jphook)           :: hook_handle

    

    
    
    

    

    

  end subroutine create_inv_cloud_effective_size


  !---------------------------------------------------------------------
  ! Create a matrix of inverse cloud effective size (m-1) according to
  ! the value of eta (=pressure divided by surface pressure)
  subroutine create_inv_cloud_effective_size_eta(this, ncol, nlev, &
       &  pressure_hl, inv_eff_size_low, inv_eff_size_mid, inv_eff_size_high, &
       &  eta_low_mid, eta_mid_high, istartcol, iendcol)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(cloud_type), intent(inout) :: this
    integer,           intent(in)    :: ncol, nlev
    
    real(jprb),        intent(in)    :: pressure_hl(:,:)
    
    real(jprb),        intent(in)    :: inv_eff_size_low
    real(jprb),        intent(in)    :: inv_eff_size_mid
    real(jprb),        intent(in)    :: inv_eff_size_high
    
    real(jprb),        intent(in)    :: eta_low_mid, eta_mid_high
    integer, optional, intent(in)    :: istartcol, iendcol

    
    real(jprb) :: eta(nlev)

    
    integer :: jcol, isurf

    
    integer :: i1, i2

    real(jphook)           :: hook_handle

    

    
    
    

    

    

    
    

    

    

  end subroutine create_inv_cloud_effective_size_eta


  !---------------------------------------------------------------------
  ! Create a matrix of inverse cloud and inhomogeneity effective size
  ! (m-1) parameterized according to the value of eta (=pressure
  ! divided by surface pressure): effective_separation =
  ! coeff_a + coeff_b*exp(-(eta**power)).  
  subroutine param_cloud_effective_separation_eta(this, ncol, nlev, &
       &  pressure_hl, separation_surf, separation_toa, power, &
       &  inhom_separation_factor, istartcol, iendcol)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(cloud_type), intent(inout) :: this
    integer,           intent(in)    :: ncol, nlev
    
    real(jprb),        intent(in)    :: pressure_hl(:,:)
    
    
    real(jprb),           intent(in) :: separation_surf 
    real(jprb),           intent(in) :: separation_toa 
    real(jprb),           intent(in) :: power
    real(jprb), optional, intent(in) :: inhom_separation_factor
    integer,    optional, intent(in) :: istartcol, iendcol

    
    real(jprb) :: eta(nlev)

    
    real(jprb) :: eff_separation(nlev)

    
    real(jprb) :: coeff_e, coeff_a, coeff_b, inhom_sep_factor

    
    integer :: jcol, isurf

    
    integer :: i1, i2

    real(jphook) :: hook_handle

    

    

    
    
    

    
     
   
    
    

    

    

    
    

    

    

  end subroutine param_cloud_effective_separation_eta


  !---------------------------------------------------------------------
  ! Remove "ghost" clouds: those with a cloud fraction that is too
  ! small to treat sensibly (e.g. because it implies that the
  ! "in-cloud" water content is too high), or with a cloud water
  ! content that is too small.  We do this in one place to ensure that
  ! all subsequent subroutines can assume that if cloud_fraction > 0.0
  ! then cloud is really present and should be treated.
  subroutine crop_cloud_fraction(this, istartcol, iendcol, &
       &    cloud_fraction_threshold, cloud_mixing_ratio_threshold)
    
    use yomhook,  only           : lhook, dr_hook, jphook
    class(cloud_type), intent(inout) :: this
    integer,           intent(in)    :: istartcol, iendcol

    integer :: nlev
    integer :: jcol, jlev

    real(jprb) :: cloud_fraction_threshold, cloud_mixing_ratio_threshold

    real(jphook) :: hook_handle

    

    

    

    

  end subroutine crop_cloud_fraction


  !---------------------------------------------------------------------
  ! Return .true. if variables are out of a physically sensible range,
  ! optionally only considering columns between istartcol and iendcol
  function out_of_physical_bounds(this, istartcol, iendcol, do_fix) result(is_bad)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : out_of_bounds_2d

    class(cloud_type), intent(inout) :: this
    integer,  optional,intent(in) :: istartcol, iendcol
    logical,  optional,intent(in) :: do_fix
    logical                       :: is_bad

    logical    :: do_fix_local

    real(jphook) :: hook_handle

    

    

    

    

  end function out_of_physical_bounds
  
end module radiation_cloud
