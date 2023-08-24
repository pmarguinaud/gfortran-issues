! radiation_tripleclouds_sw.F90 - Shortwave "Tripleclouds" solver
!
! Copyright (C) 2016-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Receive albedos at g-points
!   2017-04-22  R. Hogan  Store surface fluxes at all g-points
!   2017-10-23  R. Hogan  Renamed single-character variables
!   2018-10-08  R. Hogan  Call calc_region_properties
!   2019-01-02  R. Hogan  Fixed problem of do_save_spectral_flux .and. .not. do_sw_direct

module radiation_tripleclouds_sw

contains
  ! Provides elemental function "delta_eddington"

  ! Small routine for scaling cloud optical depth in the cloudy
  ! regions

  ! This module contains just one subroutine, the shortwave
  ! "Tripleclouds" solver in which cloud inhomogeneity is treated by
  ! dividing each model level into three regions, one clear and two
  ! cloudy (with differing optical depth). This approach was described
  ! by Shonk and Hogan (2008).

  subroutine solver_tripleclouds_sw(nlev,istartcol,iendcol, &
       &  config, single_level, cloud, & 
       &  od, ssa, g, od_cloud, ssa_cloud, g_cloud, &
       &  albedo_direct, albedo_diffuse, incoming_sw, &
       &  flux)

    use parkind1, only           : jprb
    use yomhook,  only           : lhook, dr_hook, jphook

    use radiation_config, only         : config_type, IPdfShapeGamma
    use radiation_single_level, only   : single_level_type
    use radiation_cloud, only          : cloud_type
    use radiation_regions, only        : calc_region_properties
    use radiation_overlap, only        : calc_overlap_matrices
    use radiation_flux, only           : flux_type, &
         &                               indexed_sum, add_indexed_sum
    use radiation_matrix, only         : singlemat_x_vec
    use radiation_two_stream, only     : calc_two_stream_gammas_sw, &
         &                       calc_reflectance_transmittance_sw

    

    
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type),        intent(in) :: config
    type(single_level_type),  intent(in) :: single_level
    type(cloud_type),         intent(in) :: cloud

    
    

    real(jprb), intent(in), dimension(config%n_g_sw,nlev,istartcol:iendcol) :: &
         &  od, ssa, g

    
    
    real(jprb), intent(in), dimension(config%n_bands_sw,nlev,istartcol:iendcol) :: &
         &  od_cloud, ssa_cloud, g_cloud

    
    
    real(jprb), dimension(config%n_g_sw) :: od_total, ssa_total, g_total

    
    
    
    real(jprb), intent(in), dimension(config%n_g_sw,istartcol:iendcol) :: &
         &  albedo_direct, albedo_diffuse, incoming_sw

    
    type(flux_type), intent(inout):: flux

    
    integer, parameter :: nregions = 3

    
    integer :: nreg

    
    
    
    real(jprb) :: region_fracs(1:nregions,nlev,istartcol:iendcol)

    
    real(jprb) :: od_scaling(2:nregions,nlev,istartcol:iendcol)

    
    
    real(jprb), dimension(nregions,nregions,nlev+1, &
         &                istartcol:iendcol) :: u_matrix, v_matrix

    
    real(jprb), dimension(config%n_g_sw) :: gamma1, gamma2, gamma3

    
    real(jprb), dimension(config%n_g_sw, nregions, nlev) &
         &  :: reflectance, transmittance

    
    
    
    
    real(jprb), dimension(config%n_g_sw, nregions, nlev) &
         &  :: ref_dir, trans_dir_diff, trans_dir_dir

    
    
    
    
    real(jprb), dimension(config%n_g_sw, nregions, nlev+1) &
         &  :: total_albedo, total_albedo_direct

    
    real(jprb), dimension(config%n_g_sw, nlev+1) &
         &  :: total_albedo_clear, total_albedo_clear_direct

    
    real(jprb), dimension(config%n_g_sw, nregions) &
         &  :: total_albedo_below, total_albedo_below_direct

    
    
    real(jprb), dimension(config%n_g_sw, nregions) &
         &  :: direct_dn, direct_dn_below
    
    real(jprb), dimension(config%n_g_sw, nregions) &
         &  :: flux_dn, flux_dn_below, flux_up

    
    real(jprb), dimension(config%n_g_sw) &
         &  :: direct_dn_clear, flux_dn_clear, flux_up_clear

    
    
    real(jprb), dimension(config%n_g_sw, nregions) :: inv_denom

    
    
    logical :: is_clear_sky_layer(0:nlev+1)

    
    real(jprb) :: scat_od, scat_od_cloud

    real(jprb) :: mu0

    integer :: jcol, jlev, jg, jreg, iband, jreg2, ng

    real(jphook) :: hook_handle

    

    
    
    
    
    

    
    
    

    
    
    

    
     

    

  end subroutine solver_tripleclouds_sw

end module radiation_tripleclouds_sw
