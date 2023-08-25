! radiation_pdf_sampler.f90 - Get samples from a lognormal distribution for McICA
!
! Copyright (C) 2015 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!

module radiation_pdf_sampler

  use parkind1, only : jprb

  implicit none

  !---------------------------------------------------------------------
  ! Derived type for sampling from a lognormal distribution, used to
  ! generate water content or optical depth scalings for use in the
  ! Monte Carlo Independent Column Approximation (McICA)
  type pdf_sampler_type
    ! Number of points in look-up table for cumulative distribution
    ! function (CDF) and fractional standard deviation (FSD)
    ! dimensions
    integer :: ncdf, nfsd

    ! First value of FSD and the reciprocal of the interval between
    ! FSD values (which are assumed to be uniformly distributed)
    real(jprb) :: fsd1, inv_fsd_interval

    ! Value of the distribution for each CDF and FSD bin
    real(jprb), allocatable, dimension(:,:) :: val

  contains

    procedure :: setup => setup_pdf_sampler
    procedure :: sample => sample_from_pdf
    procedure :: masked_sample => sample_from_pdf_masked
    procedure :: deallocate => deallocate_pdf_sampler

  end type pdf_sampler_type

contains

  !---------------------------------------------------------------------
  ! Load look-up table from a file 
  subroutine setup_pdf_sampler(this, file_name, iverbose)
    
    use yomhook,  only           : lhook, dr_hook, jphook

    class(pdf_sampler_type), intent(inout) :: this
    character(len=*),        intent(in)    :: file_name
    integer, optional,       intent(in)    :: iverbose

    integer            :: iverb
    real(jprb), allocatable :: fsd(:)

    real(jphook)       :: hook_handle

    

    

    

    

    
    

    

    
    
    
    

    

    

  end subroutine setup_pdf_sampler

  !---------------------------------------------------------------------
  ! Deallocate data in pdf_sampler_type derived type
  subroutine deallocate_pdf_sampler(this)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(pdf_sampler_type), intent(inout) :: this
    real(jphook)       :: hook_handle

    

    

    
    
  end subroutine deallocate_pdf_sampler


  !---------------------------------------------------------------------
  ! Extract the value of a lognormal distribution with fractional
  ! standard deviation "fsd" corresponding to the cumulative
  ! distribution function value "cdf", and return it in val. Since this
  ! is an elemental subroutine, fsd, cdf and val may be arrays.
  elemental subroutine sample_from_pdf(this, fsd, cdf, val)
    
    class(pdf_sampler_type), intent(in)  :: this

    
    
    real(jprb),              intent(in)  :: fsd, cdf

    
    real(jprb),              intent(out) :: val

    
    integer    :: ifsd, icdf

    
    real(jprb) :: wfsd, wcdf

    
    
    
    

    
    
    

    

  end subroutine sample_from_pdf


  !---------------------------------------------------------------------
  ! For true elements of mask, extract the values of a lognormal
  ! distribution with fractional standard deviation "fsd"
  ! corresponding to the cumulative distribution function values
  ! "cdf", and return in val. For false elements of mask, return zero
  ! in val.
  subroutine sample_from_pdf_masked(this, nsamp, fsd, cdf, val, mask)
    
    class(pdf_sampler_type), intent(in)  :: this

    
    integer,    intent(in) :: nsamp

    
    
    real(jprb), intent(in)  :: fsd(nsamp), cdf(nsamp)

    
    real(jprb), intent(out) :: val(:)

    
    logical,    intent(in) :: mask(nsamp)

    
    integer    :: jsamp

    
    integer    :: ifsd, icdf

    
    real(jprb) :: wfsd, wcdf

    
  end subroutine sample_from_pdf_masked

end module radiation_pdf_sampler
