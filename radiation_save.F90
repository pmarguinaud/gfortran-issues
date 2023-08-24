! radiation_save.F90 - Save data to NetCDF files
!
! Copyright (C) 2014-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-22  R. Hogan  Adapt for new way of describing longwave properties
!   2019-01-02  R. Hogan  Only save cloud properties if do_clouds==.true.

module radiation_save

  use parkind1, only : jprb

  implicit none

  ! Two available subroutines: save final fluxes and save intermediate
  ! radiative properties
  public :: save_fluxes, save_radiative_properties, save_inputs

contains

  !---------------------------------------------------------------------
  ! Save fluxes in "flux" to NetCDF file_name, plus pressure from the
  ! thermodynamics object
  subroutine save_fluxes(file_name, config, thermodynamics, flux, &
       &                 iverbose)

    use yomhook,  only           : lhook, dr_hook, jphook

    use radiation_config,         only : config_type, IGasModelMonochromatic
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_flux,           only : flux_type

    character(len=*), intent(in)         :: file_name
    type(config_type), intent(in)        :: config
    type(thermodynamics_type), intent(in):: thermodynamics
    type(flux_type), intent(in)          :: flux

    integer                              :: ncol, n_lev_plus1

    character(5), parameter :: default_lw_units_str = 'W m-2'
    character(5)            :: lw_units_str

    integer, optional, intent(in) :: iverbose
    integer                       :: i_local_verbose

    real(jphook) :: hook_handle

    
    
    

    
    

    

    
    

    
    
    

    
    
    
    

    
    
    

    

    
    

    
    

    
    

    

    
   
    
    

    

    

    

    

    
    

    
    

    

  end subroutine save_fluxes
  

  !---------------------------------------------------------------------
  ! Save intermediate radiative properties, specifically the
  ! scattering and absorption properties at each g-point/band
  subroutine save_radiative_properties(file_name, nlev, &
       &  istartcol, iendcol, &
       &  config, single_level, thermodynamics, cloud, &
       &  planck_hl, lw_emission, lw_albedo, &
       &  sw_albedo_direct, sw_albedo_diffuse, &
       &  incoming_sw, &
       &  od_lw, ssa_lw, g_lw, &
       &  od_sw, ssa_sw, g_sw, &
       &  od_lw_cloud, ssa_lw_cloud, g_lw_cloud, &
       &  od_sw_cloud, ssa_sw_cloud, g_sw_cloud)

    use radiation_config,        only : config_type
    use radiation_single_level,  only : single_level_type
    use radiation_thermodynamics,only : thermodynamics_type
    use radiation_cloud,         only : cloud_type

    character(len=*),         intent(in) :: file_name
    type(config_type),        intent(in) :: config
    type(single_level_type),  intent(in) :: single_level
    type(thermodynamics_type),intent(in) :: thermodynamics
    type(cloud_type),         intent(in) :: cloud

    integer, intent(in) :: nlev, istartcol, iendcol

    

    
    
    real(jprb), intent(in), dimension(config%n_g_sw,nlev,istartcol:iendcol) :: od_sw, ssa_sw, g_sw

   
    
    real(jprb), intent(in), dimension(config%n_bands_sw,nlev,istartcol:iendcol)   :: &
         &  od_sw_cloud, ssa_sw_cloud, g_sw_cloud

    
    
    
    real(jprb), intent(in), dimension(config%n_g_sw,istartcol:iendcol) &
         &  :: sw_albedo_direct, sw_albedo_diffuse, incoming_sw

    
    
    
    
    real(jprb), intent(in), dimension(config%n_g_lw,nlev,istartcol:iendcol) :: od_lw
    real(jprb), intent(in), dimension(config%n_g_lw_if_scattering,nlev,istartcol:iendcol) :: &
         &  ssa_lw, g_lw

    
    
    
    
    real(jprb), intent(in), dimension(config%n_bands_lw,nlev,istartcol:iendcol) :: od_lw_cloud
    real(jprb), intent(in), dimension(config%n_bands_lw_if_scattering,nlev,istartcol:iendcol) :: &
         &  ssa_lw_cloud, g_lw_cloud

    
    
    real(jprb), intent(in), dimension(config%n_g_lw,nlev+1,istartcol:iendcol) :: planck_hl

    
    
    real(jprb), dimension(config%n_g_lw, istartcol:iendcol) :: lw_emission, lw_albedo

    integer :: n_col_local 

    

    

    
    

    

    
    

    
    

    
    
    

    
    
     
    
    
    

    
    

    
    

    

    

    

     
    
    
   
    

    
    

    

    

    

    
    
    
    
    

    
    

    
    
  end subroutine save_radiative_properties
  

  !---------------------------------------------------------------------
  ! Save inputs to the radiation scheme
  subroutine save_inputs(file_name, config, single_level, thermodynamics, &
       &                 gas, cloud, aerosol, lat, lon, iverbose)
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,         only : config_type
    use radiation_single_level,   only : single_level_type
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_gas
    use radiation_cloud,          only : cloud_type
    use radiation_aerosol,        only : aerosol_type

    character(len=*),             intent(in)   :: file_name
    type(config_type),            intent(in)   :: config
    type(single_level_type),      intent(in)   :: single_level
    type(thermodynamics_type),    intent(in)   :: thermodynamics
    type(gas_type),               intent(inout):: gas
    type(cloud_type),             intent(in)   :: cloud
    type(aerosol_type), optional, intent(in)   :: aerosol
    real(jprb),         optional, intent(in)   :: lat(:), lon(:)
    integer,            optional, intent(in)   :: iverbose

    real(jprb), allocatable :: mixing_ratio(:,:)
    real(jprb), allocatable :: aerosol_mmr(:,:,:)
    real(jprb), allocatable :: seed(:)
    integer       :: i_local_verbose
    integer       :: ncol, nlev
    integer       :: jgas
    character(32) :: var_name, long_name

    

    logical :: do_aerosol

    real(jphook) :: hook_handle

    

    

    
    
    
    
    
    

    
    

    
    
    

    
    
    
     

    
    
    
    
    
    
    

    

    
    

    
    
    
    
    
    

    
    

    

    
    
    

    
    
    
    

     

    
    

    
    
    
    
    
    
    
    
    
    

    
    

    
    
    
    
    
    
    

    

    

    
    

    
    
  end subroutine save_inputs

end module radiation_save
