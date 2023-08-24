! radiation_spartacus_lw.F90 - SPARTACUS longwave solver
!
! Copyright (C) 2014-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Receive emission/albedo rather than planck/emissivity
!   2017-04-22  R. Hogan  Store surface fluxes at all g-points
!   2018-09-03  R. Hogan  Security via min_cloud_effective_size
!   2018-10-08  R. Hogan  Call calc_region_properties
!   2019-01-12  R. Hogan  Use inv_inhom_effective_size if allocated

module radiation_spartacus_lw

contains

  ! Small routine for scaling cloud optical depth in the cloudy
  ! regions

  ! This module contains just one subroutine, the longwave solver
  ! using the Speedy Algorithm for Radiative Transfer through Cloud
  ! Sides (SPARTACUS), which can represent 3D effects using a matrix
  ! form of the two-stream equations.
  !
  ! Sections:
  !   1: Prepare general variables and arrays
  !   2: Prepare column-specific variables and arrays
  !   3: First loop over layers
  !     3.1: Layer-specific initialization
  !     3.2: Compute gamma variables
  !       3.2a: Clear-sky case
  !       3.2b: Cloudy case
  !     3.3: Compute reflection, transmission and emission
  !       3.3a: g-points with 3D effects
  !       3.3b: g-points without 3D effects
  !   4: Compute total sources and albedos
  !   5: Compute fluxes
  subroutine solver_spartacus_lw(nlev,istartcol,iendcol, &
       &  config, thermodynamics, cloud, & 
       &  od, ssa, g, od_cloud, ssa_cloud, g_cloud, planck_hl, &
       &  emission, albedo, &
       &  flux)

    use parkind1,                 only : jprb
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,         only : config_type, IPdfShapeGamma
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_cloud,          only : cloud_type
    use radiation_regions,        only : calc_region_properties
    use radiation_overlap,        only : calc_overlap_matrices
    use radiation_flux,           only : flux_type, indexed_sum
    use radiation_matrix
    use radiation_two_stream,     only : calc_two_stream_gammas_lw, &
         calc_reflectance_transmittance_lw, LwDiffusivity
    use radiation_lw_derivatives, only : calc_lw_derivatives_matrix
    use radiation_constants,      only : Pi, GasConstantDryAir, &
         AccelDueToGravity

    

    
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in)        :: config
    type(thermodynamics_type),intent(in) :: thermodynamics
    type(cloud_type),   intent(in)       :: cloud

    
    
    real(jprb), intent(in), dimension(config%n_g_lw,nlev,istartcol:iendcol) :: od

    
    
    real(jprb), intent(in), &
         &  dimension(config%n_g_lw_if_scattering,nlev,istartcol:iendcol) :: ssa, g

    
    
    real(jprb), intent(in) :: od_cloud(config%n_bands_lw,nlev,istartcol:iendcol)

    
    
    
    real(jprb), intent(in), dimension(config%n_bands_lw_if_scattering, &
         &                            nlev,istartcol:iendcol) :: ssa_cloud, g_cloud

    
    real(jprb), intent(in), dimension(config%n_g_lw,nlev+1,istartcol:iendcol) &
         &  :: planck_hl

    
    
    real(jprb), intent(in), dimension(config%n_g_lw, istartcol:iendcol) &
         &  :: emission, albedo

    
    type(flux_type),          intent(inout):: flux

    integer :: nreg, ng
    integer :: nregActive 
    integer :: jcol, jlev, jg, jreg, iband, jreg2
    integer :: ng3D 
                    

    
    real(jprb), parameter &
         &  :: R_over_g = GasConstantDryAir / AccelDueToGravity

    
    real(jprb), parameter :: four_over_pi = 4.0_jprb / Pi

    
    
    real(jprb), parameter :: tan_diffuse_angle_3d = Pi * 0.5_jprb
    
    

    
    
    real(jprb), dimension(config%n_g_lw, config%nregions) &
         &  :: od_region, ssa_region, g_region

    
    real(jprb) :: scat_od, scat_od_cloud

    
    real(jprb) :: region_fracs(1:config%nregions,nlev,istartcol:iendcol)

    
    real(jprb) :: od_scaling(2:config%nregions,nlev,istartcol:iendcol)

    
    
    
    
    
    real(jprb) :: edge_length(3)

    
    
    
    real(jprb) :: transfer_rate(config%nregions,config%nregions)

    
    
    real(jprb), dimension(config%nregions,config%nregions,nlev+1,istartcol:iendcol) &
         &  :: u_matrix, v_matrix

    
    real(jprb), dimension(config%n_g_lw, config%nregions) &
         &  :: gamma1, gamma2

    
    
    
    real(jprb), dimension(config%n_g_lw, 2*config%nregions, &
         &  2*config%nregions) :: Gamma_z1

    
    real(jprb), dimension(config%n_g_lw, config%nregions, &
         &  config%nregions, nlev) :: reflectance, transmittance

    
    
    real(jprb), dimension(config%n_g_lw, nlev) :: ref_clear, trans_clear

    
    
    real(jprb), dimension(config%n_g_lw,2*config%nregions) :: planck_top

    
    
    
    real(jprb), dimension(config%n_g_lw,2*config%nregions) :: planck_diff

    
    
    
    real(jprb), dimension(config%n_g_lw,2*config%nregions) &
         &  :: solution0, solution_diff

    
    real(jprb), dimension(config%n_g_lw,config%nregions) &
         &  :: tmp_vectors

    
    
    
    real(jprb), dimension(config%n_g_lw, config%nregions, nlev) &
         &  :: source_up, source_dn
    
    real(jprb), dimension(config%n_g_lw, nlev) &
         &  :: source_up_clear, source_dn_clear

    
    
    
    real(jprb), dimension(config%n_g_lw, config%nregions, nlev+1) &
         &  :: total_source
    
    real(jprb), dimension(config%n_g_lw, nlev+1) :: total_source_clear

    
    real(jprb), dimension(config%n_g_lw, config%nregions) &
         &  :: total_source_below

    
    
    
    
    real(jprb), dimension(config%n_g_lw, config%nregions, &
         &  config%nregions, nlev+1) :: total_albedo
    
    real(jprb), dimension(config%n_g_lw, nlev+1) :: total_albedo_clear

    
    real(jprb), dimension(config%n_g_lw, config%nregions, config%nregions) &
         &  :: total_albedo_below

    
    
    real(jprb), dimension(config%n_g_lw, config%nregions, config%nregions) &
         &  :: denominator
    
    
    real(jprb), dimension(config%n_g_lw) :: inv_denom_scalar

    
    real(jprb) :: dz

    
    real(jprb), dimension(config%n_g_lw, config%nregions) &
         &  :: flux_up_above, flux_dn_above, flux_dn_below
    
    
    
    real(jprb), dimension(config%n_g_lw) :: flux_up_clear, flux_dn_clear

    
    
    
    
    
    real(jprb), dimension(config%n_g_lw) :: lateral_od, sqrt_1_minus_ssa
    real(jprb), dimension(config%n_g_lw) :: side_emiss_thick, side_emiss

    
    
    real(jprb), parameter :: side_emiss_thin = 1.4107

    real(jprb) :: aspect_ratio

    
    
    integer :: n_calls_expm, n_calls_meador_weaver

    
    
    logical :: is_clear_sky_layer(0:nlev+1)

    real(jphook) :: hook_handle

    

    
    
    

    
    
    

    
    
    
    

    
    

    
    
    

    
    

    

    
     

    

    
    
    

    

  end subroutine solver_spartacus_lw

end module radiation_spartacus_lw
