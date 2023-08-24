! radiation_ifs_rrtm.F90 - Interface to IFS implementation of RRTM-G
!
! Copyright (C) 2015-2019 ECMWF
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
! License: see the COPYING file for details
!
! Modifications
!   2017-04-11  R. Hogan  Receive "surface" dummy argument
!   2017-09-08  R. Hogan  Reverted some changes
!   2017-10-18  R. Hogan  Added planck_function public function
!   2018-01-11  R. Hogan  Added optional spectral scaling of incoming solar radiation
!   2018-02-22  R. Hogan  Optimized reverse indexing of heights
!   2018-05-05  R. Hogan  gas_optics can be called for reduced number of levels
!   2019-01-02  R. Hogan  Initialize shortwave props to zero in case sun below horizon

module radiation_ifs_rrtm

  implicit none

  public  :: setup_gas_optics, gas_optics, planck_function

contains

  !---------------------------------------------------------------------
  ! Setup the IFS implementation of RRTM-G gas absorption model
  subroutine setup_gas_optics(YDERDI, config, directory)

    use yoerrtm,   only : jpglw
    use yoesrtm,   only : jpgsw
    use yoerrtftr, only : ngb_lw => ngb
    use yoesrtm,   only : ngb_sw => ngbsw
    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config
    USE YOERDI   , ONLY : TERDI

    TYPE(TERDI)       ,INTENT(INOUT):: YDERDI
    type(config_type), intent(inout), target :: config
    character(len=*), intent(in)     :: directory

    integer :: irep 

    integer, parameter :: RRTM_GPOINT_REORDERING_LW(140) = (/ &
          &   89, 90, 139, 77, 137, 69, 131, 97, 91, 70, 78, 71, 53, 72, 123, 54, 79, 98,  &
          &   92, 55, 80, 132, 124, 81, 73, 56, 99, 82, 57, 23, 125, 100, 24, 74, 93, 58, 25,  &
          &   83, 126, 75, 26, 11, 101, 133, 59, 27, 76, 140, 12, 84, 102, 94, 28, 127, 85,  &
          &   13, 39, 60, 86, 103, 87, 109, 14, 29, 115, 40, 95, 15, 61, 88, 41, 110, 104, 1,  &
          &   116, 42, 30, 134, 128, 138, 96, 62, 16, 43, 117, 63, 111, 44, 2, 64, 31, 65,  &
          &   105, 17, 45, 66, 118, 32, 3, 33, 67, 18, 129, 135, 46, 112, 34, 106, 68, 35, 4,  &
          &   119, 36, 47, 107, 19, 37, 38, 113, 48, 130, 5, 120, 49, 108, 20, 50, 51, 114,  &
          &   21, 121, 52, 136, 122, 6, 22, 7, 8, 9, 10 &
          & /)
    integer, parameter :: RRTM_GPOINT_REORDERING_SW(112) = (/ &
          &   35, 45, 19, 27, 36, 57, 20, 46, 58, 21, 28, 67, 55, 68, 37, 1, 69, 22, 29, 59,  &
          &   78, 101, 79, 77, 70, 76, 47, 75, 30, 81, 60, 102, 80, 82, 23, 2, 83, 84, 85,  &
          &   86, 103, 61, 31, 87, 56, 38, 71, 48, 88, 3, 62, 89, 24, 7, 49, 32, 104, 72, 90,  &
          &   63, 39, 4, 8, 50, 91, 64, 40, 33, 25, 51, 95, 96, 73, 65, 9, 41, 97, 92, 105,  &
          &   52, 5, 98, 10, 42, 99, 100, 66, 11, 74, 34, 53, 26, 6, 106, 12, 43, 13, 54, 93,  &
          &   44, 107, 94, 14, 108, 15, 16, 109, 17, 18, 110, 111, 112 &
          & /)
    
    real(jphook) :: hook_handle








    

    
    
    
    

    
    
    
    

    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    

    
    
    

    

    

    

    

    
    
    

    

  end subroutine setup_gas_optics


  !---------------------------------------------------------------------
  ! Scale gas mixing ratios according to required units
  subroutine set_gas_units(gas)

    use radiation_gas,           only : gas_type, IMassMixingRatio
    type(gas_type),    intent(inout) :: gas

    

  end subroutine set_gas_units


  !---------------------------------------------------------------------
  ! Compute gas optical depths, shortwave scattering, Planck function
  ! and incoming shortwave radiation at top-of-atmosphere
  subroutine gas_optics(ncol,nlev,istartcol,iendcol, &
       &  config, single_level, thermodynamics, gas, & 
       &  od_lw, od_sw, ssa_sw, lw_albedo, planck_hl, lw_emission, &
       &  incoming_sw)

    use parkind1,                 only : jprb, jpim

    USE PARRRTM  , ONLY : JPBAND, JPXSEC, JPINPX 
    USE YOERRTM  , ONLY : JPGPT_LW => JPGPT
    USE YOESRTM  , ONLY : JPGPT_SW => JPGPT  
    USE YOMDIMV  , ONLY : TDIMV
    use yomhook  , only : lhook, dr_hook, jphook

    use radiation_config,         only : config_type, ISolverSpartacus
    use radiation_thermodynamics, only : thermodynamics_type
    use radiation_single_level,   only : single_level_type
    use radiation_gas

    integer, intent(in) :: ncol               
    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in) :: config
    type(single_level_type),  intent(in) :: single_level
    type(thermodynamics_type),intent(in) :: thermodynamics
    type(gas_type),           intent(in) :: gas

    
    real(jprb), dimension(config%n_g_lw,istartcol:iendcol), &
         &  intent(in), optional :: lw_albedo

    
    
    
    real(jprb), dimension(config%n_g_lw,nlev,istartcol:iendcol), intent(out) :: &
         &   od_lw
    real(jprb), dimension(config%n_g_sw,nlev,istartcol:iendcol), intent(out) :: &
         &   od_sw, ssa_sw

    
    
    real(jprb), dimension(config%n_g_lw,nlev+1,istartcol:iendcol), &
         &   intent(out), optional :: planck_hl
    
    real(jprb), dimension(config%n_g_lw,istartcol:iendcol), &
         &   intent(out), optional :: lw_emission

    
    
    
    real(jprb), dimension(config%n_g_sw,istartcol:iendcol), &
         &   intent(out), optional :: incoming_sw

    
    TYPE(TDIMV) :: YDDIMV

    real(jprb) :: incoming_sw_scale(istartcol:iendcol)

    
    

    real(jprb) :: ZOD_LW(JPGPT_LW,nlev,istartcol:iendcol) 
    real(jprb) :: ZOD_SW(istartcol:iendcol,nlev,JPGPT_SW)
    real(jprb) :: ZSSA_SW(istartcol:iendcol,nlev,JPGPT_SW)
    real(jprb) :: ZINCSOL(istartcol:iendcol,JPGPT_SW)

    real(jprb) :: ZCOLMOL(istartcol:iendcol,nlev)
    real(jprb) :: ZCOLDRY(istartcol:iendcol,nlev)
    real(jprb) :: ZWBRODL(istartcol:iendcol,nlev) 
    real(jprb) :: ZCOLBRD(istartcol:iendcol,nlev) 
    real(jprb) :: ZWKL(istartcol:iendcol,JPINPX,nlev)

    real(jprb) :: ZWX(istartcol:iendcol,JPXSEC,nlev) 
    
    real(jprb) :: ZFLUXFAC, ZPI

    
    real(jprb) :: ZTAUAERL(istartcol:iendcol,nlev,JPBAND)

    
    real(jprb) :: ZFAC00(istartcol:iendcol,nlev)
    real(jprb) :: ZFAC01(istartcol:iendcol,nlev)
    real(jprb) :: ZFAC10(istartcol:iendcol,nlev)
    real(jprb) :: ZFAC11(istartcol:iendcol,nlev)
    
    
    real(jprb) :: ZFORFAC(istartcol:iendcol,nlev)
    real(jprb) :: ZFORFRAC(istartcol:iendcol,nlev)
    integer    :: INDFOR(istartcol:iendcol,nlev) 

    
    integer    :: INDMINOR(istartcol:iendcol,nlev) 
    real(jprb) :: ZSCALEMINOR(istartcol:iendcol,nlev) 
    real(jprb) :: ZSCALEMINORN2(istartcol:iendcol,nlev) 
    real(jprb) :: ZMINORFRAC(istartcol:iendcol,nlev) 
    
    real(jprb)     :: &                 
         &  ZRAT_H2OCO2(istartcol:iendcol,nlev),ZRAT_H2OCO2_1(istartcol:iendcol,nlev), &
         &  ZRAT_H2OO3(istartcol:iendcol,nlev) ,ZRAT_H2OO3_1(istartcol:iendcol,nlev), & 
         &  ZRAT_H2ON2O(istartcol:iendcol,nlev),ZRAT_H2ON2O_1(istartcol:iendcol,nlev), &
         &  ZRAT_H2OCH4(istartcol:iendcol,nlev),ZRAT_H2OCH4_1(istartcol:iendcol,nlev), &
         &  ZRAT_N2OCO2(istartcol:iendcol,nlev),ZRAT_N2OCO2_1(istartcol:iendcol,nlev), &
         &  ZRAT_O3CO2(istartcol:iendcol,nlev) ,ZRAT_O3CO2_1(istartcol:iendcol,nlev)
    
    
    integer :: JP(istartcol:iendcol,nlev)
    integer :: JT(istartcol:iendcol,nlev)
    integer :: JT1(istartcol:iendcol,nlev)

    
    real(jprb) :: ZONEMINUS, ZONEMINUS_ARRAY(istartcol:iendcol)

    
    real(jprb) :: ZCOLH2O(istartcol:iendcol,nlev)
    real(jprb) :: ZCOLCO2(istartcol:iendcol,nlev)
    real(jprb) :: ZCOLO3(istartcol:iendcol,nlev)
    real(jprb) :: ZCOLN2O(istartcol:iendcol,nlev)
    real(jprb) :: ZCOLCH4(istartcol:iendcol,nlev)
    real(jprb) :: ZCOLO2(istartcol:iendcol,nlev)
    real(jprb) :: ZCO2MULT(istartcol:iendcol,nlev)
    integer    :: ILAYTROP(istartcol:iendcol)
    integer    :: ILAYSWTCH(istartcol:iendcol)
    integer    :: ILAYLOW(istartcol:iendcol)

    
    real(jprb) :: ZPAVEL(istartcol:iendcol,nlev)
    real(jprb) :: ZTAVEL(istartcol:iendcol,nlev)
    real(jprb) :: ZPZ(istartcol:iendcol,0:nlev)
    real(jprb) :: ZTZ(istartcol:iendcol,0:nlev)
    
    
    real(jprb) :: ZSELFFAC(istartcol:iendcol,nlev)
    real(jprb) :: ZSELFFRAC(istartcol:iendcol,nlev)
    integer :: INDSELF(istartcol:iendcol,nlev)

    
    real(jprb) :: ZPFRAC(istartcol:iendcol,JPGPT_LW,nlev)
    
    
    integer :: IREFLECT(istartcol:iendcol)

    real(jprb) :: pressure_fl(ncol, nlev), temperature_fl(ncol, nlev)

    
    
    
    
    
    integer :: istartlev, iendlev

    integer :: jlev, jgreorder, jg, ig, iband, jcol

    real(jphook) :: hook_handle







    

    
    
    
    

    
    
    
    

    
    

    
    
    
    
    

      

       

    

          

    

    
    
      
    
    
    
    
    
    

    
    
    
    

    
    
    
    

    
    
    
    
  end subroutine gas_optics
  

  !---------------------------------------------------------------------
  ! Compute Planck function of the atmosphere
  subroutine planck_function_atmos(nlev,istartcol,iendcol, &
       config, thermodynamics, PFRAC, &
       planck_hl)

    use parkind1,                 only : jprb, jpim

    USE YOERRTM  , ONLY : JPGPT_LW => JPGPT
    use yoerrtwn, only : totplnk, delwave

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,         only : config_type, ISolverSpartacus
    use radiation_thermodynamics, only : thermodynamics_type
    

    integer, intent(in) :: nlev               
    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in) :: config
    type(thermodynamics_type),intent(in) :: thermodynamics
    real(jprb), intent(in) :: PFRAC(istartcol:iendcol,JPGPT_LW,nlev)

    
    
    real(jprb), dimension(config%n_g_lw,nlev+1,istartcol:iendcol), intent(out) :: &
         &   planck_hl

    
    real(jprb), dimension(istartcol:iendcol, config%n_bands_lw) :: planck_store

    
    real(jprb), dimension(istartcol:iendcol) :: frac
    integer,    dimension(istartcol:iendcol) :: ind

    
    real(jprb) :: temperature

    real(jprb) :: factor
    real(jprb) :: ZFLUXFAC

    integer :: jlev, jgreorder, jg, ig, iband, jband, jcol, ilevoffset

    real(jphook) :: hook_handle

    

    
    
    
    
    
    

    
    
    
    

    

  end subroutine planck_function_atmos


  !---------------------------------------------------------------------
  ! Compute Planck function of the surface
  subroutine planck_function_surf(istartcol, iendcol, config, temperature, PFRAC, &
       &  planck_surf)

    use parkind1,                 only : jprb, jpim

    USE YOERRTM  , ONLY : JPGPT_LW => JPGPT
    use yoerrtwn, only : totplnk, delwave

    use yomhook,  only           : lhook, dr_hook, jphook
    use radiation_config,         only : config_type, ISolverSpartacus
    

    integer, intent(in) :: istartcol, iendcol 
    type(config_type), intent(in) :: config
    real(jprb), intent(in) :: temperature(:)

    real(jprb), intent(in) :: PFRAC(istartcol:iendcol,JPGPT_LW)

    
    real(jprb), dimension(config%n_g_lw,istartcol:iendcol), &
         &  intent(out) :: planck_surf

    
    real(jprb), dimension(istartcol:iendcol, config%n_bands_lw) :: planck_store

    
    real(jprb), dimension(istartcol:iendcol) :: frac
    integer,    dimension(istartcol:iendcol) :: ind

    
    real(jprb) :: Tsurf

    real(jprb) :: factor
    real(jprb) :: ZFLUXFAC

    integer :: jgreorder, jg, ig, iband, jband, jcol

    real(jphook) :: hook_handle

    

    

    
    

    
    

    

    
    
  end subroutine planck_function_surf


  !---------------------------------------------------------------------
  ! Externally facing function for computing the Planck function
  ! without reference to any gas profile; typically this would be used
  ! for computing the emission by facets of a complex surface.  Note
  ! that this uses fixed "PFRAC" values, obtained by averaging over
  ! those derived from RRTM-G for near-surface conditions over a line
  ! of meridian from the ECMWF model.
  subroutine planck_function(config, temperature, planck_surf)

    use parkind1,                 only : jprb, jpim

    use radiation_config,         only : config_type

    type(config_type), intent(in) :: config
    real(jprb), intent(in) :: temperature

    
    real(jprb), dimension(config%n_g_lw), &
         &  intent(out) :: planck_surf

    
    
    real(jprb), parameter, dimension(1,140) :: frac &
         = reshape( (/ 0.21227E+00, 0.18897E+00, 0.25491E+00, 0.17864E+00, 0.11735E+00, 0.38298E-01, 0.57871E-02, &
         &    0.31753E-02, 0.53169E-03, 0.76476E-04, 0.16388E+00, 0.15241E+00, 0.14290E+00, 0.12864E+00, &
         &    0.11615E+00, 0.10047E+00, 0.80013E-01, 0.60445E-01, 0.44918E-01, 0.63395E-02, 0.32942E-02, &
         &    0.54541E-03, 0.15380E+00, 0.15194E+00, 0.14339E+00, 0.13138E+00, 0.11701E+00, 0.10081E+00, &
         &    0.82296E-01, 0.61735E-01, 0.41918E-01, 0.45918E-02, 0.37743E-02, 0.30121E-02, 0.22500E-02, &
         &    0.14490E-02, 0.55410E-03, 0.78364E-04, 0.15938E+00, 0.15146E+00, 0.14213E+00, 0.13079E+00, &
         &    0.11672E+00, 0.10053E+00, 0.81566E-01, 0.61126E-01, 0.41150E-01, 0.44488E-02, 0.36950E-02, &
         &    0.29101E-02, 0.21357E-02, 0.19609E-02, 0.14134E+00, 0.14390E+00, 0.13913E+00, 0.13246E+00, &
         &    0.12185E+00, 0.10596E+00, 0.87518E-01, 0.66164E-01, 0.44862E-01, 0.49402E-02, 0.40857E-02, &
         &    0.32288E-02, 0.23613E-02, 0.15406E-02, 0.58258E-03, 0.82171E-04, 0.29127E+00, 0.28252E+00, &
         &    0.22590E+00, 0.14314E+00, 0.45494E-01, 0.71792E-02, 0.38483E-02, 0.65712E-03, 0.29810E+00, &
         &    0.27559E+00, 0.11997E+00, 0.10351E+00, 0.84515E-01, 0.62253E-01, 0.41050E-01, 0.44217E-02, &
         &    0.36946E-02, 0.29113E-02, 0.34290E-02, 0.55993E-03, 0.31441E+00, 0.27586E+00, 0.21297E+00, &
         &    0.14064E+00, 0.45588E-01, 0.65665E-02, 0.34232E-02, 0.53199E-03, 0.19811E+00, 0.16833E+00, &
         &    0.13536E+00, 0.11549E+00, 0.10649E+00, 0.93264E-01, 0.75720E-01, 0.56405E-01, 0.41865E-01, &
         &    0.59331E-02, 0.26510E-02, 0.40040E-03, 0.32328E+00, 0.26636E+00, 0.21397E+00, 0.14038E+00, &
         &    0.52142E-01, 0.38852E-02, 0.14601E+00, 0.13824E+00, 0.27703E+00, 0.22388E+00, 0.15446E+00, &
         &    0.48687E-01, 0.98054E-02, 0.18870E-02, 0.11961E+00, 0.12106E+00, 0.13215E+00, 0.13516E+00, &
         &    0.25249E+00, 0.16542E+00, 0.68157E-01, 0.59725E-02, 0.49258E+00, 0.33651E+00, 0.16182E+00, &
         &    0.90984E-02, 0.95202E+00, 0.47978E-01, 0.91716E+00, 0.82857E-01, 0.77464E+00, 0.22536E+00 /), (/ 1,140 /) )

    

  end subroutine planck_function

end module radiation_ifs_rrtm
