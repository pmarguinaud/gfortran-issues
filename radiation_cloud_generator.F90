! radiation_cloud_generator.F90 - Generate water-content or optical-depth scalings for McICA
!
! Copyright (C) 2015-2018 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Generate clouds for McICA using a method modified from Raisanen et
! al. (2002)
!
! Modifications
!   2018-02-22  R. Hogan  Call masked version of PDF sampler for speed

module radiation_cloud_generator

contains

  !---------------------------------------------------------------------
  ! Generate scaling factors for the cloud optical depth to represent
  ! cloud overlap, the overlap of internal cloud inhomogeneities and
  ! the fractional standard deviation of these inhomogeneities, for
  ! use in a Monte Carlo Independent Column Approximation radiation
  ! scheme. All returned profiles contain cloud, and the total cloud
  ! cover is also returned, so the calling function can then do a
  ! weighted average of clear and cloudy skies; this is a way to
  ! reduce the Monte Carlo noise in profiles with low cloud cover.
  subroutine cloud_generator(ng, nlev, i_overlap_scheme, &
       &  iseed, frac_threshold, &
       &  frac, overlap_param, decorrelation_scaling, &
       &  fractional_std, pdf_sampler, &
       &  od_scaling, total_cloud_cover, &
       &  is_beta_overlap)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use random_numbers_mix, only : randomnumberstream, &
         initialize_random_numbers, uniform_distribution
    use radiation_pdf_sampler, only : pdf_sampler_type
    use radiation_cloud_cover, only : cum_cloud_cover_exp_ran, &
         &       cum_cloud_cover_max_ran, cum_cloud_cover_exp_exp, &
         &       IOverlapMaximumRandom, IOverlapExponentialRandom, &
         &       IOverlapExponential

    

    
    integer, intent(in)     :: ng    
    integer, intent(in)     :: nlev  
    integer, intent(in)     :: i_overlap_scheme
    integer, intent(in)     :: iseed 

    
    
    real(jprb), intent(in)  :: frac_threshold

    
    real(jprb), intent(in)  :: frac(nlev)

    
    
    
    real(jprb), intent(in)  :: overlap_param(nlev-1)

    
    real(jprb), intent(in)  :: decorrelation_scaling

    
    real(jprb), intent(in)  :: fractional_std(nlev)

    
    type(pdf_sampler_type), intent(in) :: pdf_sampler

    
    
    
    
    
    logical, intent(in), optional :: is_beta_overlap

    

    
    real(jprb), intent(out) :: od_scaling(ng,nlev)

    
    real(jprb), intent(out) :: total_cloud_cover

    

    
    real(jprb) :: cum_cloud_cover(nlev)

    
    real(jprb) :: trigger

    
    real(jprb) :: rand_top(ng)

    
    real(jprb) :: overlap_param_inhom(nlev-1)

    
    
    type(randomnumberstream) :: random_stream
    
    
    integer :: ibegin, iend

    integer :: itrigger

    
    integer :: jlev, jg

    
    
    real(jprb), dimension(nlev-1) :: pair_cloud_cover, overhang

    real(jphook) :: hook_handle

    

    

    
    

    


    

  end subroutine cloud_generator


  !---------------------------------------------------------------------
  ! Generate a column of optical depth scalings using
  ! exponential-random overlap (which includes maximum-random overlap
  ! as a limiting case)
  subroutine generate_column_exp_ran(ng, nlev, ig, random_stream, pdf_sampler, &
       &  frac, pair_cloud_cover, &
       &  cum_cloud_cover, overhang, fractional_std, overlap_param_inhom, &
       &  itrigger, iend, od_scaling)

    use parkind1,              only : jprb
    use radiation_pdf_sampler, only : pdf_sampler_type
    use random_numbers_mix,    only : randomnumberstream, &
         initialize_random_numbers, uniform_distribution


    

    
    integer, intent(in) :: ng, ig

    
    integer, intent(in) :: nlev

    
    type(randomnumberstream), intent(inout) :: random_stream

    
    type(pdf_sampler_type), intent(in) :: pdf_sampler

    
    
    real(jprb), intent(in), dimension(nlev) :: frac, cum_cloud_cover, fractional_std

    
    
    real(jprb), intent(in), dimension(nlev-1) :: pair_cloud_cover, overhang

    
    real(jprb), intent(in), dimension(nlev-1) :: overlap_param_inhom

    
    
    integer, intent(in) :: itrigger, iend

    
    real(jprb), intent(inout), dimension(ng,nlev) :: od_scaling

    
    integer :: jlev, jcloud

    
    
    integer :: n_layers_to_scale

    integer :: iy

    
    logical :: do_fill_od_scaling

    real(jprb) :: rand_cloud(nlev)
    real(jprb) :: rand_inhom1(nlev), rand_inhom2(nlev)

    
    
    

    
    
    

    
    
    

  end subroutine generate_column_exp_ran



  !---------------------------------------------------------------------
  ! Generate a column of optical depth scalings using
  ! exponential-exponential overlap
  subroutine generate_column_exp_exp(ng, nlev, ig, random_stream, pdf_sampler, &
       &  frac, pair_cloud_cover, &
       &  cum_cloud_cover, overhang, fractional_std, overlap_param_inhom, &
       &  itrigger, iend, od_scaling)

    use parkind1,              only : jprb
    use radiation_pdf_sampler, only : pdf_sampler_type
    use random_numbers_mix,    only : randomnumberstream, &
         initialize_random_numbers, uniform_distribution

    

    
    integer, intent(in) :: ng, ig

    
    integer, intent(in) :: nlev

    
    type(randomnumberstream), intent(inout) :: random_stream

    
    type(pdf_sampler_type), intent(in) :: pdf_sampler

    
    
    real(jprb), intent(in), dimension(nlev) :: frac, cum_cloud_cover, fractional_std

    
    
    real(jprb), intent(in), dimension(nlev-1) :: pair_cloud_cover, overhang

    
    real(jprb), intent(in), dimension(nlev-1) :: overlap_param_inhom

    
    
    integer, intent(in) :: itrigger, iend

    
    real(jprb), intent(inout), dimension(ng,nlev) :: od_scaling

    
    integer :: jlev, jcloud

    integer :: iy

    real(jprb) :: rand_cloud(nlev)
    real(jprb) :: rand_inhom1(nlev), rand_inhom2(nlev)

    
    
    logical :: is_cloudy(nlev)

    
    
    integer :: n_layers_to_scale

    

    
    

    
    
    

    
    
    

    
    

    
    
        
    
    
        
    
    
        
    
    
    
    
    
    

    
    
    
    
    
    
        
  end subroutine generate_column_exp_exp

    !---------------------------------------------------------------------
  ! Extract the value of a lognormal distribution with fractional
  ! standard deviation "fsd" corresponding to the cumulative
  ! distribution function value "cdf", and return it in x. Since this
  ! is an elemental subroutine, fsd, cdf and x may be arrays. SIMD version.
  subroutine sample_from_pdf_simd(this, fsd, cdf, x)
    use parkind1,              only : jprb
    use radiation_pdf_sampler, only : pdf_sampler_type
    


    
    

    type(pdf_sampler_type), intent(in)  :: this

    
    
    real(jprb),              intent(in)  :: fsd, cdf

    
    real(jprb),              intent(out) :: x

    
    integer    :: ifsd, icdf

    
    real(jprb) :: wfsd, wcdf

    
    
    
    

    
    
    

    

  end subroutine sample_from_pdf_simd

end module radiation_cloud_generator
