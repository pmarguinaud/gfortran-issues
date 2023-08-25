! radiation_flux.F90 - Derived type to store the output fluxes
!
! Copyright (C) 2014-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-09-08  R. Hogan  Store g-point fluxes
!   2017-10-23  R. Hogan  Renamed single-character variables
!   2019-01-08  R. Hogan  Added "indexed_sum_profile"
!   2019-01-14  R. Hogan  out_of_physical_bounds calls routine in radiation_config

module radiation_flux

  use parkind1, only : jprb

  implicit none

  !---------------------------------------------------------------------
  ! This derived type contains the output from the radiation
  ! calculation.  Currently this is solely flux profiles, but in
  ! future surface fluxes in each band may be stored in order that the
  ! calling program can compute surface-radiation such as
  ! photosynthetically active radiation and UV index.
  type flux_type
     ! All the following are broad-band fluxes in W m-2 with
     ! dimensions (ncol,nlev+1).  Note that only those fluxes that are
     ! requested will be used, so clear-sky and direct-beam arrays may
     ! not be allocated
     real(jprb), allocatable, dimension(:,:) :: &
          &  lw_up, lw_dn, &   ! Upwelling and downwelling longwave
          &  sw_up, sw_dn, &   ! Upwelling and downwelling shortwave
          &  sw_dn_direct, &   ! Direct-beam shortwave into a horizontal plane
          &  lw_up_clear, lw_dn_clear, & ! Clear-sky quantities...
          &  sw_up_clear, sw_dn_clear, &
          &  sw_dn_direct_clear
     ! As above but fluxes in each spectral band in W m-2 with
     ! dimensions (nband,ncol,nlev+1).  These are only allocated if
     ! config%do_save_spectral_flux==.true.
     real(jprb), allocatable, dimension(:,:,:) :: &
          &  lw_up_band, lw_dn_band, &   ! Upwelling and downwelling longwave
          &  sw_up_band, sw_dn_band, &   ! Upwelling and downwelling shortwave
          &  sw_dn_direct_band, &        ! Direct-beam shortwave
          &  lw_up_clear_band, lw_dn_clear_band, & ! Clear-sky quantities...
          &  sw_up_clear_band, sw_dn_clear_band, &
          &  sw_dn_direct_clear_band
     ! Surface downwelling quantaties at each g point, dimensioned
     ! (ng,ncol), that are always saved by the solver, except for the
     ! clear-sky ones that are only produced if
     ! config%do_clear==.true.
     real(jprb), allocatable, dimension(:,:) :: &
          &  lw_dn_surf_g, lw_dn_surf_clear_g, &
          &  sw_dn_diffuse_surf_g, sw_dn_direct_surf_g, &
          &  sw_dn_diffuse_surf_clear_g, sw_dn_direct_surf_clear_g
     ! Shortwave downwelling spectral fluxes in W m-2 at the surface,
     ! from which quantities such as photosynthetically active and UV
     ! radiation can be computed. Only allocated in
     ! config%do_surface_sw_spectral_flux==.true.  Note that the
     ! clear-sky quantities are only computed if
     ! config%do_clear==.true., but direct fluxes are computed whether
     ! or not do_direct==.true.. The dimensions are (nband,ncol).
     real(jprb), allocatable, dimension(:,:) :: &
          &  sw_dn_surf_band, sw_dn_direct_surf_band, &
          &  sw_dn_surf_clear_band, sw_dn_direct_surf_clear_band
     ! Surface downwelling fluxes in W m-2 at the spectral resolution
     ! needed by any subsequent canopy radiative transfer.  If
     ! config%use_canopy_full_spectrum_[sw|lw] then these will be at
     ! g-point resolution; otherwise they will be at
     ! config%n_albedo_bands and config%n_emiss_bands resolution.
     real(jprb), allocatable, dimension(:,:) :: &
          &  lw_dn_surf_canopy, &
          &  sw_dn_diffuse_surf_canopy, sw_dn_direct_surf_canopy

     ! Diagnosed cloud cover from the short- and long-wave solvers
     real(jprb), allocatable, dimension(:) :: &
          &  cloud_cover_lw, cloud_cover_sw
     ! Longwave derivatives needed by Hogan and Bozzo (2015) method
     ! for approximate longwave updates in between the full radiation
     ! calls: rate of change of upwelling broad-band flux with respect
     ! to surface value, dimensioned (ncol,nlev+1)
     real(jprb), allocatable, dimension(:,:) :: &
          &  lw_derivatives

   contains
     procedure :: allocate   => allocate_flux_type
     procedure :: deallocate => deallocate_flux_type
     procedure :: calc_surface_spectral
     procedure :: out_of_physical_bounds
  end type flux_type

contains

  !---------------------------------------------------------------------
  ! Allocate arrays for flux profiles, using config to define which
  ! fluxes are needed.  The arrays are dimensioned for columns between
  ! istartcol, iendcol and levels from 1 to nlev+1
  subroutine allocate_flux_type(this, config, istartcol, iendcol, nlev)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : config_type

    integer, intent(in)             :: istartcol, iendcol, nlev
    class(flux_type), intent(inout) :: this
    type(config_type), intent(in)   :: config

    real(jphook)                    :: hook_handle

    

    
    
    
    
    
    
    
    
    

    
    
    
    

    
    
  end subroutine allocate_flux_type


  !---------------------------------------------------------------------
  ! Deallocate flux arrays
  subroutine deallocate_flux_type(this)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(flux_type), intent(inout) :: this
    real(jphook)                    :: hook_handle

    

    

    

    
    
    

    
    

    
    
    

    
    

    

    

  end subroutine deallocate_flux_type

  !---------------------------------------------------------------------
  ! Calculate surface downwelling fluxes in each band using the
  ! downwelling surface fluxes at each g point
  subroutine calc_surface_spectral(this, config, istartcol, iendcol)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : config_type

    class(flux_type),  intent(inout) :: this
    type(config_type), intent(in)    :: config
    integer,           intent(in)    :: istartcol, iendcol

    integer :: jcol, jband, jalbedoband, nalbedoband

    
    
    real(jprb) :: lw_dn_surf_band(config%n_bands_lw,istartcol:iendcol)

    real(jphook)                     :: hook_handle

    

     

    
     

    

    

  end subroutine calc_surface_spectral


  !---------------------------------------------------------------------
  ! Return .true. if the most important flux variables are out of a
  ! physically sensible range, optionally only considering columns
  ! between istartcol and iendcol
  function out_of_physical_bounds(this, istartcol, iendcol) result(is_bad)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : out_of_bounds_2d

    class(flux_type), intent(inout) :: this
    integer, optional,intent(in) :: istartcol, iendcol
    logical                      :: is_bad

    real(jphook)                 :: hook_handle

    

    

    

  end function out_of_physical_bounds
  

  !---------------------------------------------------------------------
  ! Sum elements of "source" into "dest" according to index "ind".
  ! "source" and "ind" should have the same size and bounds, and no
  ! element of "ind" should refer outside the bounds of "dest".  This
  ! version increments existing contents of "dest".
  pure subroutine add_indexed_sum(source, ind, dest)

    real(jprb), intent(in)    :: source(:)
    integer,    intent(in)    :: ind(:)
    real(jprb), intent(inout) :: dest(:)

    integer :: ig, jg, istart, iend

    
    

    

  end subroutine add_indexed_sum


  !---------------------------------------------------------------------
  ! As "add_indexed_sum" but this version overwrites existing contents
  ! of "dest"
  pure subroutine indexed_sum(source, ind, dest)

    real(jprb), intent(in)  :: source(:)
    integer,    intent(in)  :: ind(:)
    real(jprb), intent(out) :: dest(:)

    integer :: ig, jg, istart, iend

    

    
    

    

  end subroutine indexed_sum


  !---------------------------------------------------------------------
  ! As "add_indexed_sum" but a whole vertical profiles
  pure subroutine add_indexed_sum_profile(source, ind, dest)

    real(jprb), intent(in)  :: source(:,:)
    integer,    intent(in)  :: ind(:)
    real(jprb), intent(out) :: dest(:,:)

    integer :: ig, jg, istart, iend, jlev, nlev

    
    
    

    

  end subroutine add_indexed_sum_profile


  !---------------------------------------------------------------------
  ! As "indexed_sum" but a whole vertical profiles
  pure subroutine indexed_sum_profile(source, ind, dest)

    real(jprb), intent(in)  :: source(:,:)
    integer,    intent(in)  :: ind(:)
    real(jprb), intent(out) :: dest(:,:)

    integer :: ig, jg, istart, iend, jlev, nlev

    

    
    
    

    

  end subroutine indexed_sum_profile
  
end module radiation_flux
