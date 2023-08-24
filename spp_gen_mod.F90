module spp_gen_mod

USE PARKIND1 , ONLY : JPIM, JPRB

implicit none

integer(kind=jpim), parameter :: jp_lab_len=16      ! string length for perturbation labels
integer(kind=jpim), parameter :: jp_ver_len=32      ! string length for version
integer(kind=jpim), parameter :: jp_mag  =4         ! max size of xmag
integer(kind=jpim), parameter :: jp_cli  =4         ! max size of xclipmin/xclipmax
integer(kind=jpim), parameter :: jp_off  =2         ! max size of nseed_off
integer(kind=jpim), parameter :: jpmaxperts=128     ! max number of perturbations in the context of namelist inputs
character(len=jp_lab_len), parameter :: cp_undefined='????????????????'

type spp_pert
  !
  !   define variables for an individual perturbation of 1 variable or 1 parameter
  !
  character(len=jp_lab_len) :: label=cp_undefined
  logical                   :: on=.false.       ! on/off
  integer(kind=jpim)        :: idistr           ! Determines the distribution; 
                                                !  0: normal, 
                                                !  1: log-normal, 
                                                !  2: uniform
  real(kind=jprb), dimension(jp_mag)   :: xmag=0._jprb        ! amplitude
  real(kind=jprb), dimension(jp_mag)   :: mu=0._jprb          ! mean of Gaussian
  real(kind=jprb)           :: xclipmin=-HUGE(1.0_jprb) ! lower clipping limit
  real(kind=jprb)           :: xclipmax= HUGE(1.0_jprb) ! upper clipping limit
  real(kind=jprb)           :: xuniform_offset   ! Offset of distribution if idistr = 2
  integer(kind=jpim)        :: nmag=1           !   number of used magnitudes
  logical                   :: ln1=.true.       ! T implies the mean of the log-normal distribution is 1
  integer(kind=jpim),dimension(jp_off) :: nseed_off=0       ! random number seed offsets
  integer(kind=jpim)        :: noff=1           !   number of used seed offsets

  integer(kind=jpim)        :: nrf=1            ! number of random fields for this perturbation
                                                !   [e.b. would be 2 for CUDUDV but 1 for RTAU and most others]
  integer(kind=jpim)        :: nrf_radgrid=0    ! number of random fields on the radiation grid
  logical                   :: radgrid=.false.  ! perturbation requires random field on radiation grid
  character(len=jp_lab_len) :: rf_pert_label=cp_undefined ! point to random field of perturbation rf_pert_label
  integer(kind=jpim)        :: mp               ! pointer to first random field
                                                !   [correspond to MP* IN TSPP_DATA, e.g. MPCFM ... MPZHSVDAERO]
  integer(kind=jpim)        :: mp_radgrid       ! pointer to first random field on radiation grid

  real(kind=jprb)                      :: tau, sdev, xlcor ! random field characteristics
end type spp_pert

type spp_model
  !
  !    define a model of perturbations
  !
  character(len=jp_ver_len)  :: version
  integer(kind=jpim)         :: nmax  ! max. number of perturbations for array dimensioning
  character(len=jp_lab_len), dimension(:), allocatable :: defined_perts
  character(len=jp_lab_len), dimension(:), allocatable :: active_perts
  integer(kind=jpim) :: ndef=0 ! number of defined perturbations
  integer(kind=jpim) :: nact ! number of active perturbations
  integer(kind=jpim) :: nrftotal=0 ! total number of random fields
  integer(kind=jpim) :: nrftotal_radgrid=0 ! total number of random fields on the radiation grid
  integer(kind=jpim), dimension(:), allocatable :: nseed_off ! seed offsets for random fields 1..nrftotal
  type(spp_pert), dimension(:), allocatable :: pndef  ! configurations of defined individual perturbations
  type(spp_pert), dimension(:), allocatable :: pn   ! configurations of active individual perturbations
  real(kind=jprb)    :: tau    ! global default decorrelation time
  real(kind=jprb)    :: xlcor  ! global default correlation length scale
  real(kind=jprb)    :: sdev   ! global default standard deviation
  integer(kind=jpim) :: kseed_off ! random number seed offset for current perturbation
end type spp_model


contains

subroutine allocate_spp_model( sm, knmax)
  
  
  
  type(spp_model), intent(inout) :: sm
  integer(kind=jpim), intent(in) :: knmax

  
  
  
  
  
  

end subroutine allocate_spp_model

subroutine deallocate_spp_model( sm )
  
  
  
  type(spp_model), intent(inout) :: sm

  
  
  
  
  

end subroutine deallocate_spp_model

subroutine update_mu_spp_pertn( pn )
  
  
  
  
  type(spp_pert), intent(inout) :: pn

  integer(kind=jpim) :: jmag

  

end subroutine update_mu_spp_pertn

subroutine implement_spp_pertn( sm, cdlabel, pxmag, xclipmin, xclipmax, &
                              & kidistr, knseed_off, knrf, ln1, radgrid, cd_rf_pert_label, &
                              & xuniform_offset )
  
  
  
  
  type(spp_model),           intent(inout) :: sm
  character(len=*),          intent(in)    :: cdlabel
  real(kind=jprb), dimension(:)        , intent(in)    :: pxmag
  
  integer(kind=jpim), optional        , intent(in)    :: kidistr
  integer(kind=jpim), dimension(:), optional        , intent(in)    :: knseed_off
  integer(kind=jpim), optional        , intent(in)    :: knrf
  logical,            optional        , intent(in)    :: ln1
  logical,            optional        , intent(in)    :: radgrid
  character(len=*),   optional        , intent(in)    :: cd_rf_pert_label
  real(kind=jprb),    optional        , intent(in)    :: xuniform_offset
  real(kind=jprb),    optional        , intent(in)    :: xclipmin
  real(kind=jprb),    optional        , intent(in)    :: xclipmax

  type(spp_pert) :: pn

      
      

  
  
  
  
  
  

  
  
  

  

  

  

  

  
  

  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
end subroutine implement_spp_pertn





subroutine get_active_spp_perts( sm, ku_nam, ku_out )
  
  
  
  
  type(spp_model), intent(inout) :: sm
  integer(kind=jpim), intent(in) :: ku_nam
  integer(kind=jpim), intent(in) :: ku_out

  integer(kind=jpim) :: inreq 
  integer(kind=jpim) :: jjpert, kf0, kf1

  character(len=jp_lab_len), dimension(jpmaxperts) :: requested_perts

  

  

  
  

  
  
  

  
  
  
  
end subroutine get_active_spp_perts


subroutine map_indices_spp_nml( sm, ku_nam, ku_out, kidx_map )
  
  
  
  type(spp_model), intent(inout) :: sm
  integer(kind=jpim), intent(in) :: ku_nam
  integer(kind=jpim), intent(in) :: ku_out
  integer(kind=jpim), dimension(jpmaxperts), intent(out):: kidx_map

  integer(kind=jpim) :: jjpert
  type(spp_pert), dimension(jpmaxperts) :: pnx   

  

  
  

  
  

  

end subroutine map_indices_spp_nml


subroutine modify_spp_perts( sm, kidx_map, ku_nam, ku_out )
  
  
  
  
  
  
  
  type(spp_model), intent(inout) :: sm
  integer(kind=jpim), dimension(jpmaxperts), intent(in):: kidx_map
  integer(kind=jpim), intent(in) :: ku_nam
  integer(kind=jpim), intent(in) :: ku_out

  integer(kind=jpim) :: jjpert, kf0, kf1

  integer(kind=jpim) :: i_rf_donor

  type(spp_pert), dimension(jpmaxperts) :: pnx   

  

  

  

  

  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
end subroutine modify_spp_perts

integer(kind=jpim) function get_spp_idx( cd_label, sm )
  
  
  
  character(len=*), intent(in)      :: cd_label
  type(spp_model)          , intent(in)      :: sm

  integer(kind=jpim) :: ipn
  integer(kind=jpim) :: jj
  
  
  
  
end function get_spp_idx

integer(kind=jpim) function get_spp_idx_def( cd_label, sm )
  
  
  
  character(len=*), intent(in)      :: cd_label
  type(spp_model)          , intent(in)      :: sm

  integer(kind=jpim) :: ipn
  integer(kind=jpim) :: jj
  
  
  
  
end function get_spp_idx_def


logical function active_spp_pn(cd_label, sm )
  
  
  
  character(len=*), intent(in)      :: cd_label
  type(spp_model)          , intent(in)      :: sm

  
end function active_spp_pn


subroutine write_spp_pn_infoline( p, ku)
  
  
  
  type(spp_pert),               intent(in) :: p
  integer(kind=jpim), optional, intent(in) :: ku

  integer(kind=jpim) :: iunit
  
  

end subroutine write_spp_pn_infoline

subroutine write_spp_model_table( sm, ku, ldefined )
  
  
  
  
  type(spp_model), intent(in) :: sm
  integer(kind=jpim), optional, intent(in) :: ku
  logical, optional, intent(in) :: ldefined

  logical :: lldef

  integer(kind=jpim) :: jjpert, nperts, iunit
  

  

  
  
  

  
  
  

end subroutine write_spp_model_table



end module spp_gen_mod
