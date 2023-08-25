! radiation_aerosol.F90 - Derived type describing aerosol
!
! Copyright (C) 2014-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2018-04-15  R. Hogan  Add "direct" option
!   2019-01-14  R. Hogan  Added out_of_physical_bounds routine

module radiation_aerosol

  use parkind1, only : jprb

  implicit none

  !---------------------------------------------------------------------
  ! Type describing the aerosol content in the atmosphere
  type aerosol_type
     ! The mass mixing ratio of config%n_aerosol_types different
     ! aerosol types dimensioned
     ! (ncol,istartlev:iendlev,config%n_aerosol_types), where ncol is
     ! the number of columns, istartlev:iendlev is the range of model
     ! levels where aerosols are present
     real(jprb), allocatable, dimension(:,:,:) :: &
          &  mixing_ratio  ! mass mixing ratio (kg/kg)

     ! Alternatively, if is_direct=true, the optical properties are
     ! provided directly and are dimensioned
     ! (nband,istartlev:iendlev,ncol)
     real(jprb), allocatable, dimension(:,:,:) :: &
          &  od_sw, ssa_sw, g_sw, & ! Shortwave optical properties
          &  od_lw, ssa_lw, g_lw    ! Longwave optical properties

     ! Range of levels in which the aerosol properties are provided
     integer :: istartlev, iendlev

     ! Are the optical properties going to be provided directly by the
     ! user?
     logical :: is_direct = .false.

   contains
     procedure :: allocate        => allocate_aerosol_arrays
     procedure :: allocate_direct => allocate_aerosol_arrays_direct
     procedure :: deallocate      => deallocate_aerosol_arrays
     procedure :: out_of_physical_bounds
  end type aerosol_type

contains

  !---------------------------------------------------------------------
  ! Allocate array for describing aerosols, although in the offline
  ! code these are allocated when they are read from the NetCDF file
  subroutine allocate_aerosol_arrays(this, ncol, istartlev, iendlev, ntype)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(aerosol_type), intent(inout) :: this
    integer, intent(in)                :: ncol  
    integer, intent(in)                :: istartlev, iendlev 
    integer, intent(in)                :: ntype 
    real(jphook)                       :: hook_handle

    

    
    
    
    

    

  end subroutine allocate_aerosol_arrays


  !---------------------------------------------------------------------
  ! Allocate arrays for describing aerosol optical properties
  subroutine allocate_aerosol_arrays_direct(this, config, &
       &                                    ncol, istartlev, iendlev)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : config_type

    class(aerosol_type), intent(inout) :: this
    type(config_type),   intent(in)    :: config
    integer, intent(in)                :: ncol  
    integer, intent(in)                :: istartlev, iendlev 

    real(jphook)                       :: hook_handle

    

    
    
    

    

    

    

  end subroutine allocate_aerosol_arrays_direct


  !---------------------------------------------------------------------
  ! Deallocate array
  subroutine deallocate_aerosol_arrays(this)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(aerosol_type), intent(inout) :: this

    real(jphook)                       :: hook_handle

    

    
    
    
    
    
    
    
 
    

  end subroutine deallocate_aerosol_arrays


  !---------------------------------------------------------------------
  ! Return .true. if variables are out of a physically sensible range,
  ! optionally only considering columns between istartcol and iendcol
  function out_of_physical_bounds(this, istartcol, iendcol, do_fix) result(is_bad)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : out_of_bounds_3d

    class(aerosol_type),   intent(inout) :: this
    integer,      optional,intent(in) :: istartcol, iendcol
    logical,      optional,intent(in) :: do_fix
    logical                           :: is_bad

    logical    :: do_fix_local

    real(jphook) :: hook_handle

    

    

    

    

  end function out_of_physical_bounds
  
end module radiation_aerosol
