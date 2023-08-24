! radiation_adding_ica_sw.f90 - Shortwave adding method in independent column approximation
!
! Copyright (C) 2015-2017 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-10-23  R. Hogan  Renamed single-character variables

module radiation_adding_ica_sw

contains

  subroutine adding_ica_sw(ncol, nlev, incoming_toa, &
       &  albedo_surf_diffuse, albedo_surf_direct, cos_sza, &
       &  reflectance, transmittance, ref_dir, trans_dir_diff, trans_dir_dir, &
       &  flux_up, flux_dn_diffuse, flux_dn_direct)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    

    
    integer, intent(in) :: ncol 
    integer, intent(in) :: nlev 

    
    real(jprb), intent(in),  dimension(ncol)         :: incoming_toa

    
    real(jprb), intent(in),  dimension(ncol)         :: albedo_surf_diffuse, &
         &                                              albedo_surf_direct

    
    real(jprb), intent(in),  dimension(ncol)         :: cos_sza

    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: reflectance, transmittance

    
    
    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: ref_dir, trans_dir_diff

    
    
    real(jprb), intent(in),  dimension(ncol, nlev)   :: trans_dir_dir

    
    
    real(jprb), intent(out), dimension(ncol, nlev+1) :: flux_up, flux_dn_diffuse, &
         &                                              flux_dn_direct
    
    
    
    real(jprb), dimension(ncol, nlev+1) :: albedo

    
    
    real(jprb), dimension(ncol, nlev+1) :: source

    
    real(jprb), dimension(ncol, nlev)   :: inv_denominator

    
    integer :: jlev, jcol

    real(jphook) :: hook_handle

    

    
    
    
    

    

    
    
    

    
    
    
    
    

    
    

    
    
    

    
    
    
    

    

  end subroutine adding_ica_sw

end module radiation_adding_ica_sw
