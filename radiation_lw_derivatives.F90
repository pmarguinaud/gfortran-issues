! radiation_lw_derivatives.f90 - Compute longwave derivatives for Hogan and Bozzo (2015) method
!
! Copyright (C) 2016-2017 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! This module provides routines to compute the rate of change of
! broadband upwelling longwave flux at each half level with respect to
! the surface broadband upwelling flux.  This is done from the surface
! spectral fluxes and the spectral transmittance of each atmospheric
! layer, assuming no longwave scattering. The result may be used to
! perform approximate updates to the longwave flux profile in between
! calls to the full radiation scheme, accounting for the change in
! skin temperature, following the method of Hogan and Bozzo (JAMES
! 2015).  Separate routines are provided for each solver.
!
! Note that currently a more approximate calculation is performed from
! the exact one in Hogan and Bozzo (2015); here we assume that a
! change in temperature increases the spectral fluxes in proportion,
! when in reality there is a change in shape of the Planck function in
! addition to an overall increase in the total emission.
!
! Modifications
!   2017-10-23  R. Hogan  Renamed single-character variables

module radiation_lw_derivatives

contains

  !---------------------------------------------------------------------
  ! Calculation for the Independent Column Approximation
  subroutine calc_lw_derivatives_ica(ng, nlev, icol, transmittance, flux_up_surf, lw_derivatives)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer,    intent(in) :: ng   
    integer,    intent(in) :: nlev 
    integer,    intent(in) :: icol 
    real(jprb), intent(in) :: transmittance(ng,nlev)
    real(jprb), intent(in) :: flux_up_surf(ng) 
    
    
    real(jprb), intent(out) :: lw_derivatives(:,:) 

    
    
    real(jprb) :: lw_derivatives_g(ng)

    integer    :: jlev

    real(jphook) :: hook_handle

    

    
    
    

    
    
    

    

  end subroutine calc_lw_derivatives_ica


  !---------------------------------------------------------------------
  ! Calculation for the Independent Column Approximation
  subroutine modify_lw_derivatives_ica(ng, nlev, icol, transmittance, &
       &                               flux_up_surf, weight, lw_derivatives)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer,    intent(in) :: ng   
    integer,    intent(in) :: nlev 
    integer,    intent(in) :: icol 
    real(jprb), intent(in) :: transmittance(ng,nlev)
    real(jprb), intent(in) :: flux_up_surf(ng) 
    real(jprb), intent(in) :: weight 
    
    
    real(jprb), intent(inout) :: lw_derivatives(:,:) 

    
    
    real(jprb) :: lw_derivatives_g(ng)

    integer    :: jlev

    real(jphook) :: hook_handle

    

    
    
    
    

    
    
    

    

  end subroutine modify_lw_derivatives_ica



  !---------------------------------------------------------------------
  ! Calculation for solvers involving multiple regions and matrices
  subroutine calc_lw_derivatives_matrix(ng, nlev, nreg, icol, transmittance, &
       &                                u_matrix, flux_up_surf, lw_derivatives)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_matrix

    

    
    integer,    intent(in) :: ng   
    integer,    intent(in) :: nlev 
    integer,    intent(in) :: nreg 
    integer,    intent(in) :: icol 
    real(jprb), intent(in) :: transmittance(ng,nreg,nreg,nlev)
    real(jprb), intent(in) :: u_matrix(nreg,nreg,nlev+1) 
    real(jprb), intent(in) :: flux_up_surf(ng) 
    
    
    real(jprb), intent(out) :: lw_derivatives(:,:) 

    
    
    real(jprb) :: lw_derivatives_g_reg(ng,nreg)

    integer    :: jlev

    real(jphook) :: hook_handle

    

    
    
    
    
    
    

    
    
    

    

  end subroutine calc_lw_derivatives_matrix


  !---------------------------------------------------------------------
  ! Calculation for solvers involving multiple regions but no 3D
  ! effects: the difference from calc_lw_derivatives_matrix is that transmittance
  ! has one less dimensions
  subroutine calc_lw_derivatives_region(ng, nlev, nreg, icol, transmittance, &
       &                                u_matrix, flux_up_surf, lw_derivatives)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_matrix

    

    
    integer,    intent(in) :: ng   
    integer,    intent(in) :: nlev 
    integer,    intent(in) :: nreg 
    integer,    intent(in) :: icol 
    real(jprb), intent(in) :: transmittance(ng,nreg,nlev)
    real(jprb), intent(in) :: u_matrix(nreg,nreg,nlev+1) 
    real(jprb), intent(in) :: flux_up_surf(ng) 
    
    
    real(jprb), intent(out) :: lw_derivatives(:,:) 

    
    
    real(jprb) :: lw_derivatives_g_reg(ng,nreg)

    integer    :: jlev

    real(jphook) :: hook_handle

    

    
    
    
    
    
    

    
    
    

    

  end subroutine calc_lw_derivatives_region


end module radiation_lw_derivatives
