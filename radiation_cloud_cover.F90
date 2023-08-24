! radiation_cloud_cover.F90 - Compute cumulative cloud cover for McICA
!
! Copyright (C) 2016 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Generate profiles of the cumulative cloud cover as seen from TOA,
! used in the McICA cloud generator.
!
! Modifications
!   2020-10-07  R. Hogan  Ensure iobj1 initialized in case of alpha_obj==0

module radiation_cloud_cover

  use parkind1, only           : jprb

  ! Three overlap schemes.  Note that "Exponential" means that
  ! clear-sky regions have no special significance for computing the
  ! cumulative cloud cover: non-contiguous clouds are exponentially
  ! rather than randomly overlapped. This is the situaition in the
  ! McRad radiation scheme at ECMWF.
  enum, bind(c)
    enumerator IOverlapMaximumRandom, IOverlapExponentialRandom, &
         &     IOverlapExponential
  end enum
  character(len=*), parameter :: OverlapName(0:2) = (/ 'Max-Ran', &
       &                                               'Exp-Ran', &
       &                                               'Exp-Exp' /)

  ! Maximum cloud fraction distinguishable from 1
  real(jprb), parameter :: MaxCloudFrac = 1.0_jprb-epsilon(1.0_jprb)*10.0_jprb


contains

  !---------------------------------------------------------------------
  ! Convert "beta" overlap parameter of Shonk et al. (2010) to "alpha"
  ! overlap parameter of Hogan and Illingworth (2000)
  elemental function beta2alpha(beta, frac1, frac2)

    

    
    
    real(jprb), intent(in) :: beta, frac1, frac2

    real(jprb)             :: beta2alpha

    
    real(jprb)             :: frac_diff

    

  end function beta2alpha


  !---------------------------------------------------------------------
  ! Compute total cloud cover according to the specified overlap
  ! rule. This can be used to compute the high, mid and low cloud
  ! cover by passing in subsets of the cloud fraction array
  function cloud_cover(nlev, i_overlap_scheme, frac, overlap_param, &
       &               is_beta_overlap)

    
    
    
    integer, intent(in)    :: nlev, i_overlap_scheme

    
    
    real(jprb), intent(in) :: frac(nlev), overlap_param(nlev-1)

    
    
    logical, intent(in), optional :: is_beta_overlap

    
    real(jprb)             :: cloud_cover

    
    real(jprb) :: cum_cloud_cover(nlev)

    
    real(jprb) :: pair_cloud_cover(nlev-1)

    

    

  end function cloud_cover


  !---------------------------------------------------------------------
  ! Maximum-random overlap: Geleyn & Hollingsworth formula
  subroutine cum_cloud_cover_max_ran(nlev, frac, &
       & cum_cloud_cover, pair_cloud_cover)

    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer, intent(in)     :: nlev  

    
    real(jprb), intent(in)  :: frac(nlev)

    

    
    real(jprb), intent(out) :: cum_cloud_cover(nlev)

    
    real(jprb), intent(out) :: pair_cloud_cover(nlev-1)

    

    
    real(jprb) :: cum_product

    
    integer :: jlev

    real(jphook) :: hook_handle

    

    

    
    
    
    

    

  end subroutine cum_cloud_cover_max_ran
  

  !---------------------------------------------------------------------
  ! Exponential-random overlap: exponential overlap for contiguous
  ! clouds, random overlap for non-contiguous clouds
  subroutine cum_cloud_cover_exp_ran(nlev, frac, overlap_param, &
       & cum_cloud_cover, pair_cloud_cover, is_beta_overlap)

    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer, intent(in)     :: nlev  

    
    real(jprb), intent(in)  :: frac(nlev)

    
    
    
    real(jprb), intent(in)  :: overlap_param(nlev-1)

    
    
    
    
    
    logical, intent(in), optional :: is_beta_overlap

    

    
    real(jprb), intent(out) :: cum_cloud_cover(nlev)

    
    real(jprb), intent(out) :: pair_cloud_cover(nlev-1)

    

    
    real(jprb) :: cum_product

    
    real(jprb) :: overlap_alpha
    logical    :: do_overlap_conversion

    
    integer :: jlev

    real(jphook) :: hook_handle

    

    
    

    

    
    
    
    


    

  end subroutine cum_cloud_cover_exp_ran



  !---------------------------------------------------------------------
  ! Exponential-exponential overlap: exponential overlap for both
  ! contiguous and non-contiguous clouds. This is the result of the
  ! simple Raisanen cloud generator, but unfortunately it has no
  ! (known) analytic formula for the total cloud cover, or the
  ! cumulative cloud cover.  In partially cloudy columns, The McICA
  ! scheme needs this info in order to devote all the cloudy g-points
  ! to columns containing cloud, which reduces McICA noise. The
  ! following routine provides an approximate estimate of cumulative
  ! cloud cover consistent with the exponential-exponential scheme.
  subroutine cum_cloud_cover_exp_exp(nlev, frac, overlap_param, &
       & cum_cloud_cover, pair_cloud_cover, is_beta_overlap)

    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer, intent(in)     :: nlev  

    
    real(jprb), intent(in)  :: frac(nlev)

    
    
    
    real(jprb), intent(in)  :: overlap_param(nlev-1)

    
    
    
    
    
    logical, intent(in), optional :: is_beta_overlap

    

    
    real(jprb), intent(out) :: cum_cloud_cover(nlev)

    
    real(jprb), intent(out) :: pair_cloud_cover(nlev-1)

    

    
    
    
    
    real(jprb), parameter :: min_frac = 1.0e-6_jprb

    
    real(jprb) :: overlap_alpha(nlev-1)
    logical    :: do_overlap_conversion

    
    
    

    
    integer    :: nobj

    
    
    integer    :: i_top_obj(nlev)
    integer    :: i_max_obj(nlev)
    integer    :: i_base_obj(nlev)
    
    
    integer    :: i_next_obj(nlev)

    
    real(jprb) :: cc_obj(nlev)

    
    real(jprb) :: alpha_obj(nlev)

    
    integer :: jlev

    
    integer :: jobj

    
    real(jprb) :: alpha_max

    
    integer    :: iobj1, iobj2

    
    
    real(jprb) :: cc_pair, scaling

    real(jphook) :: hook_handle

    

    
    

    
    
    
    
    

    
    
    

     

    

  end subroutine cum_cloud_cover_exp_exp

end module radiation_cloud_cover
