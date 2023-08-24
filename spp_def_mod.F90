module spp_def_mod
  use parkind1, only: jpim, jprb
  use spp_gen_mod

  type spp_pert_pointer
    !
    !   perturbation pointer type
    !
    !   A value of -1 implies that the perturbation is off, otherwise
    !     it points to the spp_pert perturbation in the spp_model
    !

    ! IFS

    integer(kind=jpim) :: cfm        = -1
    integer(kind=jpim) :: cfm1       = -1
    integer(kind=jpim) :: cfm2       = -1
    integer(kind=jpim) :: cfm3       = -1
    integer(kind=jpim) :: rkap       = -1
    integer(kind=jpim) :: rkap1      = -1
    integer(kind=jpim) :: rkap2      = -1
    integer(kind=jpim) :: rkap3      = -1
    integer(kind=jpim) :: tofdc      = -1
    integer(kind=jpim) :: hsdt       = -1
    integer(kind=jpim) :: vdexc      = -1
    integer(kind=jpim) :: entrorg    = -1
    integer(kind=jpim) :: entshalp   = -1
    integer(kind=jpim) :: entstpc1   = -1
    integer(kind=jpim) :: detrpen    = -1
    integer(kind=jpim) :: rprcon     = -1
    integer(kind=jpim) :: rtau       = -1
    integer(kind=jpim) :: cududv     = -1
    integer(kind=jpim) :: cududvs    = -1
    integer(kind=jpim) :: ramid      = -1
    integer(kind=jpim) :: rcldiff    = -1
    integer(kind=jpim) :: rclcrit    = -1
    integer(kind=jpim) :: rlcritsnow = -1
    integer(kind=jpim) :: rainevap   = -1
    integer(kind=jpim) :: snowsublim = -1
    integer(kind=jpim) :: cloudinhom = -1
    integer(kind=jpim) :: qsatvervel = -1
    integer(kind=jpim) :: zdecorr    = -1
    integer(kind=jpim) :: zsigqcw    = -1
    integer(kind=jpim) :: zradeffl   = -1
    integer(kind=jpim) :: zradeffi   = -1
    integer(kind=jpim) :: phr        = -1
    integer(kind=jpim) :: zhs_vdaero = -1
    integer(kind=jpim) :: delta_aero = -1

    ! HARMONIE-AROME
    integer(kind=jpim) :: psigqsat   = -1
    integer(kind=jpim) :: clddpth    = -1
    integer(kind=jpim) :: clddpthdp  = -1
    integer(kind=jpim) :: ice_cld_wgt= -1
    integer(kind=jpim) :: icenu      = -1
    integer(kind=jpim) :: kgn_acon   = -1
    integer(kind=jpim) :: kgn_sbgr   = -1
    integer(kind=jpim) :: radgr      = -1
    integer(kind=jpim) :: radsn      = -1
    integer(kind=jpim) :: rfac_twoc  = -1
    integer(kind=jpim) :: rzc_h      = -1
    integer(kind=jpim) :: rzl_inf    = -1
    integer(kind=jpim) :: rswinhf    = -1
    integer(kind=jpim) :: rlwinhf    = -1

  contains
    procedure :: set => set_spp_pert_pointer

  end type spp_pert_pointer


contains

  subroutine set_spp_pert_pointer( this, cd_pert, kptr )
    
    
    

    class(spp_pert_pointer)   :: this
    character(len=jp_lab_len), intent(in) :: cd_pert   
    integer(kind=jpim),        intent(in) :: kptr      

    

  end subroutine set_spp_pert_pointer


subroutine define_spp_arpege( sm, nulout )


  type(spp_model), intent(inout) :: sm
  integer(kind=jpim),intent(in)  :: nulout
  integer(kind=jpim) :: indef

  
  
  
  

  
  
  
   
  
    

  
  

  
  
  
  
  

  
  
  
  

  
  
  
  
  
  
  
  
  


end subroutine define_spp_arpege

subroutine define_spp_ifs( sm, nulout  )
  
  
  
  



  type(spp_model), intent(inout) :: sm
  integer(kind=jpim),intent(in)  :: nulout
  integer(kind=jpim) :: indef

  
  
  
  

  
  
  
   
  
    

  
  
  
  
  
  
  
  
  
  
   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
  
  
  
   
  
  
  
   
   
    
  
  

  
  
  
end subroutine define_spp_ifs

subroutine define_spp_lam( sm, nulout  )
  type(spp_model), intent(inout) :: sm
  integer(kind=jpim),intent(in)  :: nulout
  integer(kind=jpim) :: indef

  
  
  
  

  
  
  
        
          
    

  
  

  
  
  
  
  

  
  
  
  
   
   
   
   
   
   
   
   
   
   
   
   
   
   

  
  
  
  
  
  
  
  
  


end subroutine define_spp_lam


end module spp_def_mod
