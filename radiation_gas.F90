! radiation_gas.F90 - Derived type to store the gas mixing ratios
!
! Copyright (C) 2014-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2019-01-14  R. Hogan  Added out_of_physical_bounds routine


module radiation_gas

  use parkind1, only : jprb

  implicit none

  ! Gas codes; these indices match those of RRTM-LW up to 7
  integer, parameter :: IGasNotPresent = 0
  integer, parameter :: IH2O   = 1
  integer, parameter :: ICO2   = 2
  integer, parameter :: IO3    = 3
  integer, parameter :: IN2O   = 4
  integer, parameter :: ICO    = 5
  integer, parameter :: ICH4   = 6
  integer, parameter :: IO2    = 7
  integer, parameter :: ICFC11 = 8
  integer, parameter :: ICFC12 = 9
  integer, parameter :: IHCFC22= 10
  integer, parameter :: ICCl4  = 11 
  integer, parameter :: INO2   = 12
  integer, parameter :: NMaxGases = 12

  ! Molar masses (g mol-1) of dry air and the various gases above
  real(jprb), parameter :: IAirMolarMass = 28.970
  real(jprb), parameter, dimension(0:NMaxGases) :: IGasMolarMass = (/ &
       & 0.0_jprb,        & ! Gas not present
       & 18.0152833_jprb, & ! H2O
       & 44.011_jprb,     & ! CO2
       & 47.9982_jprb,    & ! O3
       & 44.013_jprb,     & ! N2O
       & 28.0101_jprb,    & ! CO
       & 16.043_jprb,     & ! CH4
       & 31.9988_jprb,    & ! O2
       & 137.3686_jprb,   & ! CFC11
       & 120.914_jprb,    & ! CFC12
       & 86.469_jprb,     & ! HCFC22
       & 153.823_jprb,    & ! CCl4    
       & 46.0055_jprb /)    ! NO2

  ! The corresponding names of the gases in upper and lower case, used
  ! for reading variables from the input file
  character*6, dimension(NMaxGases), parameter :: GasName &
       &  = (/'H2O   ','CO2   ','O3    ','N2O   ','CO    ','CH4   ', &
       &      'O2    ','CFC11 ','CFC12 ','HCFC22','CCl4  ','NO2   '/)
  character*6, dimension(NMaxGases), parameter :: GasLowerCaseName &
       &  = (/'h2o   ','co2   ','o3    ','n2o   ','co    ','ch4   ', &
       &      'o2    ','cfc11 ','cfc12 ','hcfc22','ccl4  ','no2   '/)

  ! Available units
  enum, bind(c)
    enumerator IMassMixingRatio, IVolumeMixingRatio
  end enum

  !---------------------------------------------------------------------
  ! This derived type describes the gaseous composition of the
  ! atmosphere; gases may be stored as part of a 3D array (if their
  ! variation with height/column is to be represented) or one 1D array
  ! (if they are to be assumed globally well-mixed).
  type gas_type
    ! Units of each stored gas (or 0 if not present)
    integer :: iunits(NMaxGases) = 0

    ! Scaling factor that should be applied to each stored gas to get
    ! a dimensionless result, e.g. if iunits=IVolumeMixingRatio then
    ! 1.0e-6 is used to indicate the units are actually PPMV: need to
    ! multiply by 1e-6 to get mol/mol.
    real(jprb) :: scale_factor(NMaxGases) = 1.0_jprb
    
    ! Mixing ratios of variable gases, dimensioned (ncol, nlev,
    ! NMaxGases)
    real(jprb), allocatable, dimension(:,:,:) :: mixing_ratio

    ! Flag to indicate whether a gas is present
    logical :: is_present(NMaxGases) = .false.

    ! Flag to indicate whether a gas is well mixed
    logical :: is_well_mixed(NMaxGases) = .false.

    integer :: ntype          = 0 ! Number of gas types described

    integer :: ncol           = 0 ! Number of columns in mixing_ratio
    integer :: nlev           = 0 ! Number of levels  in mixing_ratio

    ! A list of length ntype of gases whose volume mixing ratios have
    ! been provided
    integer :: icode(NMaxGases) = 0
    
   contains
     procedure :: allocate   => allocate_gas
     procedure :: deallocate => deallocate_gas
     procedure :: put        => put_gas
     procedure :: put_well_mixed => put_well_mixed_gas
     procedure :: scale      => scale_gas
     procedure :: set_units  => set_units_gas
     procedure :: assert_units => assert_units_gas
     procedure :: get        => get_gas
     procedure :: reverse    => reverse_gas
     procedure :: out_of_physical_bounds
  end type gas_type

contains


  !---------------------------------------------------------------------
  subroutine allocate_gas(this, ncol, nlev)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(gas_type), intent(inout) :: this
    integer,         intent(in)    :: ncol, nlev

    real(jphook)        :: hook_handle

    

    

    
    

    
    

    

  end subroutine allocate_gas


  !---------------------------------------------------------------------
  ! Deallocate memory and reset arrays
  subroutine deallocate_gas(this)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(gas_type), intent(inout) :: this

    real(jphook)        :: hook_handle

    

    
    
    
    
    
    
    
    
    
    

    

  end subroutine deallocate_gas


  !---------------------------------------------------------------------
  ! Put gas mixing ratio corresponding to gas ID "igas" with units
  ! "iunits"
  subroutine put_gas(this, igas, iunits, mixing_ratio, scale_factor, &
       istartcol)

    use yomhook,  only           : lhook, dr_hook, jphook

    class(gas_type),      intent(inout) :: this
    integer,              intent(in)    :: igas
    integer,              intent(in)    :: iunits
    real(jprb),           intent(in)    :: mixing_ratio(:,:)
    real(jprb), optional, intent(in)    :: scale_factor
    integer,    optional, intent(in)    :: istartcol

    integer :: i1, i2

    real(jphook)                        :: hook_handle

    

    
    
    

    

    

    

    

    

    
    
    
    
    

    

    

  end subroutine put_gas


  !---------------------------------------------------------------------
  ! Put well-mixed gas mixing ratio corresponding to gas ID "igas"
  ! with units "iunits"
  subroutine put_well_mixed_gas(this, igas, iunits, mixing_ratio, &
       scale_factor, istartcol, iendcol)

    use yomhook,  only           : lhook, dr_hook, jphook

    class(gas_type),      intent(inout) :: this
    integer,              intent(in)    :: igas
    integer,              intent(in)    :: iunits
    real(jprb),           intent(in)    :: mixing_ratio
    real(jprb), optional, intent(in)    :: scale_factor
    integer,    optional, intent(in)    :: istartcol, iendcol

    real(jphook)                        :: hook_handle

    integer :: i1, i2

    

    
    
    

    

    

    

    

    
    
    
    
    
    

    

    

  end subroutine put_well_mixed_gas


  !---------------------------------------------------------------------
  ! Scale gas concentrations, e.g. igas=ICO2 and set scale_factor=2 to
  ! double CO2.  Note that this does not perform the scaling
  ! immediately, but changes the scale factor for the specified gas,
  ! ready to be used in set_units_gas.

  subroutine scale_gas(this, igas, scale_factor, lverbose)


    class(gas_type),      intent(inout) :: this
    integer,              intent(in)    :: igas
    real(jprb),           intent(in)    :: scale_factor
    logical,    optional, intent(in)    :: lverbose

    

  end subroutine scale_gas


  !---------------------------------------------------------------------
  ! Scale the gas concentrations so that they have the units "iunits"
  ! and are therefore ready to be used by the gas optics model within
  ! ecRad with no further scaling.  The existing scale_factor for each
  ! gas is applied.  If "igas" is present then apply only to gas with
  ! ID "igas", otherwise to all gases. Optional argument scale_factor
  ! specifies scaling that any subsequent access would need to apply
  ! to get a dimensionless result (consistent with definition of
  ! gas_type). So say that your gas optics model requires gas
  ! concentrations in PPMV, specify iunits=IVolumeMixingRatio and
  ! scale_factor=1.0e-6. If the gas concentrations were currently
  ! dimensionless volume mixing ratios, then the values would be
  ! internally divided by 1.0e-6.
  recursive subroutine set_units_gas(this, iunits, igas, scale_factor)
    class(gas_type),      intent(inout) :: this
    integer,              intent(in)    :: iunits
    integer,    optional, intent(in)    :: igas
    real(jprb), optional, intent(in)    :: scale_factor    

    integer :: ig

    
    real(jprb) :: sf

    
    real(jprb) :: new_sf

    

    

  end subroutine set_units_gas


  !---------------------------------------------------------------------
  ! Assert that gas mixing ratio units are "iunits", applying to gas
  ! with ID "igas" if present, otherwise to all gases. Otherwise the
  ! program will exit. Otional argument scale factor specifies any
  ! subsequent multiplication to apply; for PPMV one would use
  ! iunits=IVolumeMixingRatio and scale_factor=1.0e6.
  recursive subroutine assert_units_gas(this, iunits, igas, scale_factor)
    

    class(gas_type),      intent(in) :: this
    integer,              intent(in) :: iunits
    integer,    optional, intent(in) :: igas
    real(jprb), optional, intent(in) :: scale_factor    

    integer :: ig

    real(jprb) :: sf

    

    

  end subroutine assert_units_gas


  !---------------------------------------------------------------------
  ! Get gas mixing ratio corresponding to gas ID "igas" with units
  ! "iunits" and return as a 2D array of dimensions (ncol,nlev).  The
  ! array will contain zeros if the gas is not stored.
  subroutine get_gas(this, igas, iunits, mixing_ratio, scale_factor, &
       &   istartcol)

    use yomhook,  only           : lhook, dr_hook, jphook

    class(gas_type),      intent(in)  :: this
    integer,              intent(in)  :: igas
    integer,              intent(in)  :: iunits
    real(jprb),           intent(out) :: mixing_ratio(:,:)
    real(jprb), optional, intent(in)  :: scale_factor
    integer,    optional, intent(in)  :: istartcol

    real(jprb)                        :: sf
    integer                           :: i1, i2

    real(jphook)                      :: hook_handle

    

    

    

    

    

    

    

    

  end subroutine get_gas


  !---------------------------------------------------------------------
  ! Copy data to "gas_rev", reversing the height ordering of the gas
  ! data
  subroutine reverse_gas(this, istartcol, iendcol, gas_rev)

    class(gas_type)             :: this
    integer,        intent(in)  :: istartcol, iendcol
    type(gas_type), intent(out) :: gas_rev

    
    
    
    
    
    
    
    

    

    

  end subroutine reverse_gas

  !---------------------------------------------------------------------
  ! Return .true. if variables are out of a physically sensible range,
  ! optionally only considering columns between istartcol and iendcol
  function out_of_physical_bounds(this, istartcol, iendcol, do_fix) result(is_bad)

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config, only : out_of_bounds_3d

    class(gas_type),   intent(inout) :: this
    integer,  optional,intent(in) :: istartcol, iendcol
    logical,  optional,intent(in) :: do_fix
    logical                       :: is_bad

    logical    :: do_fix_local

    real(jphook) :: hook_handle

    

    

    

    

  end function out_of_physical_bounds
  
end module radiation_gas
