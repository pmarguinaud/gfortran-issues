! radiation_tripleclouds_lw.F90 - Longwave "Tripleclouds" solver
!
! Copyright (C) 2016-2018 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-28  R. Hogan  Receive emission/albedo rather than planck/emissivity
!   2017-04-22  R. Hogan  Store surface fluxes at all g-points
!   2017-10-23  R. Hogan  Renamed single-character variables
!   2018-10-08  R. Hogan  Call calc_region_properties

module radiation_tripleclouds_lw

contains
  ! Small routine for scaling cloud optical depth in the cloudy
  ! regions

  ! This module contains just one subroutine, the longwave
  ! "Tripleclouds" solver in which cloud inhomogeneity is treated by
  ! dividing each model level into three regions, one clear and two
  ! cloudy (with differing optical depth). This approach was described
  ! by Shonk and Hogan (2008).

  subroutine solver_tripleclouds_lw(nlev,istartcol,iendcol, &
       &  config, cloud, & 
       &  od, ssa, g, od_cloud, ssa_cloud, g_cloud, planck_hl, &
       &  emission, albedo, &
       &  flux)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook

    use radiation_config, only         : config_type, IPdfShapeGamma
    use radiation_cloud, only          : cloud_type
    use radiation_regions, only        : calc_region_properties
    use radiation_overlap, only        : calc_overlap_matrices
    use radiation_flux, only           : flux_type, &
         &                               indexed_sum, add_indexed_sum
    use radiation_matrix, only         : singlemat_x_vec
    use radiation_two_stream, only     : calc_two_stream_gammas_lw, &
         &                               calc_reflectance_transmittance_lw, &
         &                               calc_no_scattering_transmittance_lw
    use radiation_lw_derivatives, only : calc_lw_derivatives_region

    

    
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type),        intent(in) :: config
    type(cloud_type),         intent(in) :: cloud

    
    
    real(jprb), intent(in), dimension(config%n_g_lw,nlev,istartcol:iendcol) :: od

    
    
    real(jprb), intent(in), &
         &  dimension(config%n_g_lw_if_scattering,nlev,istartcol:iendcol) :: ssa, g

    
    
    real(jprb), intent(in) :: od_cloud(config%n_bands_lw,nlev,istartcol:iendcol)

    
    
    
    real(jprb), intent(in), dimension(config%n_bands_lw_if_scattering, &
         &                            nlev,istartcol:iendcol) :: ssa_cloud, g_cloud

    
    
    real(jprb), intent(in), dimension(config%n_g_lw,nlev+1,istartcol:iendcol) :: planck_hl

    
    
    real(jprb), intent(in), dimension(config%n_g_lw, istartcol:iendcol) :: emission, albedo

    
    
    real(jprb), dimension(config%n_g_lw) :: od_total, ssa_total, g_total

    
    
    real(jprb), dimension(config%n_g_lw) :: od_cloud_new

    
    type(flux_type), intent(inout):: flux

    
    integer, parameter :: nregions = 3

    
    integer :: nreg

    
    
    
    real(jprb) :: region_fracs(1:nregions,nlev,istartcol:iendcol)

    
    real(jprb) :: od_scaling(2:nregions,nlev,istartcol:iendcol)

    
    
    real(jprb), dimension(nregions,nregions,nlev+1, &
         &                istartcol:iendcol) :: u_matrix, v_matrix

    
    real(jprb), dimension(config%n_g_lw) :: gamma1, gamma2

    
    real(jprb), dimension(config%n_g_lw, nregions, nlev) :: reflectance, transmittance

    
    
    real(jprb), dimension(config%n_g_lw, nregions, nlev) &
         &  :: Sup, Sdn

    
    real(jprb), dimension(config%n_g_lw, nlev) &
         &  :: Sup_clear, Sdn_clear

    
    
    
    
    real(jprb), dimension(config%n_g_lw, nregions, nlev+1) :: total_albedo

    
    
    
    real(jprb), dimension(config%n_g_lw, nregions, nlev+1) :: total_source

    
    real(jprb), dimension(config%n_g_lw, nlev+1) :: total_albedo_clear, total_source_clear

    
    real(jprb), dimension(config%n_g_lw, nregions) &
         &  :: total_albedo_below, total_source_below

    
    
    real(jprb), dimension(config%n_g_lw, nregions) &
         &  :: flux_dn, flux_dn_below, flux_up

    
    real(jprb), dimension(config%n_g_lw) &
         &  :: flux_dn_clear, flux_up_clear

    
    
    real(jprb), dimension(config%n_g_lw, nregions) :: inv_denom

    
    
    logical :: is_clear_sky_layer(0:nlev+1)

    integer :: jcol, jlev, jg, jreg, jreg2, ng

    real(jphook) :: hook_handle

    

    
    
    
    
    

    
    
    

    
    

    
     

    

  end subroutine solver_tripleclouds_lw

end module radiation_tripleclouds_lw
