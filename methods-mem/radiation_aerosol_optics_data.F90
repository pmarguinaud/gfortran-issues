! radiation_aerosol_optics_data.F90 - Type to store aerosol optical properties
!
! Copyright (C) 2015-2018 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-10-23  R. Hogan  Renamed single-character variables
!   2018-04-20  A. Bozzo  Read optical properties at selected wavelengths


module radiation_aerosol_optics_data

  use parkind1,      only : jprb

  implicit none

  private :: get_line

  ! The following provide possible values for
  ! aerosol_optics_type%iclass, which is used to map the user's type
  ! index to the hydrophobic or hydrophilic types loaded from the
  ! aerosol optics file. Initially iclass is equal to
  ! AerosolClassUndefined, which will throw an error if ever the user
  ! tries to use this aerosol type. The user may specify that an
  ! aerosol type is to be ignored in the radiation calculation, in
  ! which case iclass will be set equal to AerosolClassIgnored.
  enum, bind(c) 
     enumerator IAerosolClassUndefined,   IAerosolClassIgnored, &
          &     IAerosolClassHydrophobic, IAerosolClassHydrophilic
  end enum

  integer, parameter :: NMaxStringLength = 2000
  integer, parameter :: NMaxLineLength   = 200

  !---------------------------------------------------------------------
  ! This type holds the configuration information to compute
  ! aerosol optical properties
  type aerosol_optics_type
     ! A vector of length ntype, iclass maps user-defined types on to
     ! the hydrophilic or hydrophobic aerosol classes using the
     ! enumerators above
     integer, allocatable, dimension(:) :: iclass

     ! Also a vector of length ntype, itype maps user-defined types on
     ! to specific hydrophilic or hydrophobic aerosol types
     integer, allocatable, dimension(:) :: itype

     ! Scattering properties are provided separately in the shortwave
     ! and longwave for hydrophobic and hydrophilic aerosols.
     ! Hydrophobic aerosols are dimensioned (nband,n_type_phobic):
     real(jprb), allocatable, dimension(:,:) :: &
          &  mass_ext_sw_phobic, & ! Mass-extinction coefficient (m2 kg-1)
          &  ssa_sw_phobic,      & ! Single scattering albedo
          &  g_sw_phobic,        & ! Asymmetry factor
          &  mass_ext_lw_phobic, & ! Mass-extinction coefficient (m2 kg-1)
          &  ssa_lw_phobic,      & ! Single scattering albedo
          &  g_lw_phobic           ! Asymmetry factor

     ! Hydrophilic aerosols are dimensioned (nband, nrh, n_type_philic):
     real(jprb), allocatable, dimension(:,:,:) :: &
          &  mass_ext_sw_philic, & ! Mass-extinction coefficient (m2 kg-1)
          &  ssa_sw_philic,      & ! Single scattering albedo
          &  g_sw_philic,        & ! Asymmetry factor
          &  mass_ext_lw_philic, & ! Mass-extinction coefficient (m2 kg-1)
          &  ssa_lw_philic,      & ! Single scattering albedo
          &  g_lw_philic           ! Asymmetry factor

     ! Scattering properties at selected wavelengths
     ! (n_mono_wl,n_type_phobic/philic)
     real(jprb), allocatable, dimension(:,:) :: &
          &  mass_ext_mono_phobic, & ! Mass-extinction coefficient (m2 kg-1)
          &  ssa_mono_phobic,      & ! Single scattering albedo
          &  g_mono_phobic,        & ! Asymmetry factor
          &  lidar_ratio_mono_phobic ! Lidar Ratio
     real(jprb), allocatable, dimension(:,:,:) :: &
          &  mass_ext_mono_philic, & ! Mass-extinction coefficient (m2 kg-1)
          &  ssa_mono_philic,      & ! Single scattering albedo
          &  g_mono_philic,        & ! Asymmetry factor
          &  lidar_ratio_mono_philic ! Lidar Ratio

     ! For hydrophilic aerosols, the lower bounds of the relative
     ! humidity bins is a vector of length nrh:
     real(jprb), allocatable, dimension(:) :: &
          &  rh_lower    ! Dimensionless (1.0 = 100% humidity)

     ! Strings describing the aerosol types
     character(len=NMaxStringLength) :: description_phobic_str = ' '
     character(len=NMaxStringLength) :: description_philic_str = ' '

     ! The number of user-defined aerosol types
     integer :: ntype

     ! The number of hydrophobic and hydrophilic types read from the
     ! aerosol optics file
     integer :: n_type_phobic, n_type_philic

     ! Number of relative humidity bins
     integer :: nrh

     ! Number of longwave and shortwave bands of the data in the file,
     ! and monochromatic wavelengths
     integer :: n_bands_lw = 0, n_bands_sw = 0, n_mono_wl = 0

     ! Do we have any hydrophilic types?
     logical :: use_hydrophilic = .true.

     ! Do we have monochromatic optical properties
     logical :: use_monochromatic = .false.

   contains
     procedure :: setup => setup_aerosol_optics
     procedure :: set_hydrophobic_type
     procedure :: set_hydrophilic_type
     procedure :: set_empty_type
     procedure :: set_types
     procedure :: calc_rh_index
     procedure :: print_description

  end type aerosol_optics_type

contains


  !---------------------------------------------------------------------
  ! Setup aerosol optics coefficients by reading them from a file
  subroutine setup_aerosol_optics(this, file_name, ntype, iverbose)

    use yomhook,  only           : lhook, dr_hook, jphook

    class(aerosol_optics_type), intent(inout) :: this
    character(len=*), intent(in)              :: file_name
    integer, intent(in)                       :: ntype
    integer, intent(in), optional             :: iverbose

    
    integer            :: iverb
    real(jphook)       :: hook_handle

    

    

    
    
    

    

    
    
    
    
    
    
    

    

    

    
    
    

    
    

    
    
    
    
    

    

    
    
    
    

    
    

    

  end subroutine setup_aerosol_optics


  !---------------------------------------------------------------------
  ! Map user type "itype" onto stored hydrophobic type "i_type_phobic"
  subroutine set_hydrophobic_type(this, itype, i_type_phobic)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(aerosol_optics_type), intent(inout) :: this
    integer, intent(in)                       :: itype, i_type_phobic
    real(jphook)                              :: hook_handle

    

    
    

    
    

    

  end subroutine set_hydrophobic_type


  !---------------------------------------------------------------------
  ! Map user type "itype" onto stored hydrophilic type "i_type_philic"
  subroutine set_hydrophilic_type(this, itype, i_type_philic)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(aerosol_optics_type), intent(inout) :: this
    integer, intent(in)                       :: itype, i_type_philic
    real(jphook)                              :: hook_handle

    

    

    
    

    
    

    

  end subroutine set_hydrophilic_type


  !---------------------------------------------------------------------
  ! Set a user type "itype" to be ignored in the radiation scheme
  subroutine set_empty_type(this, itype)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(aerosol_optics_type), intent(inout) :: this
    integer, intent(in)                       :: itype
    real(jphook)                              :: hook_handle

    

    

    

    

  end subroutine set_empty_type


  !---------------------------------------------------------------------
  ! Set user types "itypes" to map onto the stored hydrophobic and
  ! hydrophilic types according to its sign and value, with a value of
  ! 0 indicating that this type is to be ignored.  Thus if itypes=(/
  ! 3, 4, -6, 0 /) then user types 1 and 2 map on to hydrophobic types
  ! 3 and 4, user type 3 maps on to hydrophilic type 6 and user type 4
  ! is ignored.
  subroutine set_types(this, itypes)

    use yomhook,  only           : lhook, dr_hook, jphook
    class(aerosol_optics_type), intent(inout) :: this
    integer, dimension(:), intent(in)         :: itypes

    integer :: jtype
    integer :: istart, iend
    real(jphook)                              :: hook_handle

    

    
    

    

    

  end subroutine set_types


  !---------------------------------------------------------------------
  ! Return an index to the relative-humdity array, or zero if no
  ! hydrophilic types are present. This function does so little that
  ! it is best to remove the Dr Hook call.
  function calc_rh_index(this, rh)

    
    class(aerosol_optics_type), intent(inout) :: this
    real(jprb),                 intent(in)    :: rh
    integer                                   :: calc_rh_index
    

    

    

    

  end function calc_rh_index


  !---------------------------------------------------------------------
  ! Print a description of the aerosol types to nulout
  subroutine print_description(this, i_type_map)


    class(aerosol_optics_type), intent(in) :: this
    integer,                    intent(in) :: i_type_map(:)

    integer :: jtype

    

    
    
  end subroutine print_description


  !---------------------------------------------------------------------
  ! Private helper function for print_description
  pure function get_line(str,iline) result(line_str)
    character(len=*), intent(in)  :: str
    integer,          intent(in)  :: iline
    character(len=NMaxLineLength) :: line_str
    
    integer :: istart, iend, i_start_new, ioffset, ilength, i_line_current
    logical :: is_fail
    
    
    
    
    
    

    
    
    
    
    
  end function get_line
  
end module radiation_aerosol_optics_data
