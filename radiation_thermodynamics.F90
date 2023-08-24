! radiation_thermodynamics.F90 - Derived type for pressure & temperature
!
! Copyright (C) 2014-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-05-11  R. Hogan  Fix startcol/endcol for get_layer_mass
!   2019-01-14  R. Hogan  Added out_of_physical_bounds routine
!   2019-01-14  R. Hogan  Capped h2o_sat_liq at 1

module radiation_thermodynamics

  use parkind1, only : jprb

  implicit none

  !---------------------------------------------------------------------
  ! Derived type for storing pressure and temperature at half levels
  type thermodynamics_type
     real(jprb), allocatable, dimension(:,:) :: &
          &  pressure_hl, &   ! (ncol,nlev+1) pressure (Pa)
          &  temperature_hl   ! (ncol,nlev+1) temperature (K)

     ! The following is a function of pressure and temperature: you
     ! can calculate it according to your favourite formula, or the
     ! calc_saturation_wrt_liquid subroutine can be used to do this
     ! approximately
     real(jprb), allocatable, dimension(:,:) :: &
          &  h2o_sat_liq ! (ncol,nlev) specific humidity at liquid
                         ! saturation (kg/kg)
   contains
     procedure :: allocate   => allocate_thermodynamics_arrays
     procedure :: deallocate => deallocate_thermodynamics_arrays
     procedure :: calc_saturation_wrt_liquid
     procedure :: get_layer_mass
     procedure :: get_layer_mass_column
     procedure :: out_of_physical_bounds

  end type thermodynamics_type

contains


  !---------------------------------------------------------------------
  ! Allocate variables with specified dimensions
  subroutine allocate_thermodynamics_arrays(this, ncol, nlev, &
       &                                    use_h2o_sat)

    use yomhook,  only : lhook, dr_hook, jphook

    class(thermodynamics_type), intent(inout) :: this
    integer, intent(in)           :: ncol  
    integer, intent(in)           :: nlev  
    logical, intent(in), optional :: use_h2o_sat 

    logical :: use_h2o_sat_local

    real(jphook) :: hook_handle

    

    
    

    
    
    
        

    

  end subroutine allocate_thermodynamics_arrays


  !---------------------------------------------------------------------
  ! Deallocate variables
  subroutine deallocate_thermodynamics_arrays(this)

    use yomhook,  only : lhook, dr_hook, jphook

    class(thermodynamics_type), intent(inout) :: this

    real(jphook) :: hook_handle

    

    
    
    

    
  
  end subroutine deallocate_thermodynamics_arrays


  !---------------------------------------------------------------------
  ! Calculate approximate saturation with respect to liquid
  subroutine calc_saturation_wrt_liquid(this,istartcol,iendcol)

    use yomhook,  only : lhook, dr_hook, jphook

    class(thermodynamics_type), intent(inout) :: this
    integer, intent(in)                       :: istartcol, iendcol

    
    real(jprb) :: pressure, temperature

    
    real(jprb) :: e_sat

    integer :: ncol, nlev 
    integer :: jcol, jlev 

    real(jphook) :: hook_handle

    

    
    

    

    

    

  end subroutine calc_saturation_wrt_liquid


  !---------------------------------------------------------------------
  ! Calculate the dry mass of each layer, neglecting humidity effects.
  ! The first version is for all columns.
  subroutine get_layer_mass(this,istartcol,iendcol,layer_mass)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_constants,  only : AccelDueToGravity

    class(thermodynamics_type), intent(in)  :: this
    integer,                    intent(in)  :: istartcol, iendcol
    real(jprb),                 intent(out) :: layer_mass(:,:)

    integer    :: nlev
    real(jprb) :: inv_g

    real(jphook) :: hook_handle

    

    
    

     
    
    

  end subroutine get_layer_mass

  !---------------------------------------------------------------------
  ! Calculate the dry mass of each layer, neglecting humidity effects.
  ! The second version is for one column, the one numbered "icol".
  subroutine get_layer_mass_column(this, icol, layer_mass)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_constants,  only : AccelDueToGravity

    class(thermodynamics_type), intent(in)  :: this
    integer,                    intent(in)  :: icol
    real(jprb),                 intent(out) :: layer_mass(:)

    integer    :: nlev
    real(jprb) :: inv_g

    real(jphook) :: hook_handle

    

    
    

    
    
    

  end subroutine get_layer_mass_column


  !---------------------------------------------------------------------
  ! Estimate the separation between the mid-points of model layers
  ! given the half-level pressure and temperature.  This is not in
  ! terms of the "thermodynamics" type as it is useful for computing
  ! overlap decorrelation lengths and hence cloud cover outside the
  ! radiation scheme.
  subroutine get_layer_separation(pressure_hl, temperature_hl, layer_separation)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_constants,  only : GasConstantDryAir, AccelDueToGravity

    
    
    
    real(jprb), dimension(:,:), intent(in)  :: pressure_hl, temperature_hl

    
    real(jprb), dimension(:,:), intent(out) :: layer_separation

    
    real(jprb), parameter :: R_over_g = GasConstantDryAir / AccelDueToGravity

    
    integer    :: jlev
    integer    :: i1, i2, nlev

    real(jphook) :: hook_handle

    

    
    
    

    

        

  end subroutine get_layer_separation


  !---------------------------------------------------------------------
  ! Return .true. if variables are out of a physically sensible range,
  ! optionally only considering columns between istartcol and iendcol
  function out_of_physical_bounds(this, istartcol, iendcol, do_fix) result(is_bad)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : out_of_bounds_2d

    class(thermodynamics_type), intent(inout) :: this
    integer,           optional,intent(in) :: istartcol, iendcol
    logical,           optional,intent(in) :: do_fix
    logical                                :: is_bad

    logical    :: do_fix_local

    real(jphook) :: hook_handle

    

    

    
    

    

  end function out_of_physical_bounds
  
end module radiation_thermodynamics
