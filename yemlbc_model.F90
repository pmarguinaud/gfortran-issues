MODULE YEMLBC_MODEL

! Purpose :
! -------
!    Forcing a LAM model by another model: part 0B
!    - forcing by lateral boundary conditions
!    - pressure tendency coupling
!    - spectral nudging

! Interface :
! ---------
!    Empty.

! External :
! --------
!    None.

! Method :
! ------
!    See Documentation.

! Reference :
! ---------

! Author :
! ------
!    K. YESSAD (CNRM/GMAP) after YEMBICU, YEMDYN, YEMGT3B, SUEBICU, SUEDYN, SUESC2.
! Original : December 2010

! Modifications :
! -------------
! Daan Degrauwe: Feb 2012 Boyd biperiodization         
! T. Wilhelmsson and K. Yessad (Oct 2013) Geometry and setup refactoring.
! B. Bochenek (Oct 2013): Weights for LBC interpolation
! K. Yessad (July 2014): Move some variables.
! F. Taillefer (Aug 2015)  Add control of no coupling at all in canari by namelist
! M. Hortal (Dec 2014): Upper boundary relaxation
! P. Marguinaud (Oct 2016) : Port to single precision
! J. Vivoda (Mar 2017): Fixing bug in options LQCPL and LCCPL
! WARNING! The bug is in swapping LBC buffers P*CPL entering ESC2R. Fix does
! not remove it, but adjusts interpolation weights EWB accordingly. Clean
! solution is to correct swapping of LBC buffers, otherwise the code will
! remain difficult to understand.
! H. Dhouioui (Sep 2017) renamed from elbc0b_mod.F90
! O. Vignes (Feb 2020): Upper boundary relaxation fixes
! M. Hamrud (Oct 2021) incorporated YEMLBC_INIT. Forced by move of YOMDYNA 
! into model object
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB, JPRD
USE MODEL_GENERAL_CONF_MOD , ONLY : MODEL_GENERAL_CONF_TYPE
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMDFI   , ONLY : NSTDFI, NSTDFIA, RTDFI, RTDFIA
USE YOMCT0   , ONLY : LRPLANE, LALLOPR, LCANARI,NCONF,LELAM
USE YOMGMV   , ONLY : TGMV
USE YOMINI   , ONLY : LDFI
USE YOMMP0   , ONLY : MYSETB, MYSETV, LOUTPUT

IMPLICIT NONE
SAVE

!=============================================================================

!      1.    TYPE DEFINITION
!            ---------------

! Moved form YEMLBC_FIELDS
! Structure for GMVS coupled fields in LTENC option.
TYPE TGMVSTENC
INTEGER(KIND=JPIM) :: MSP        ! surface pressure variable
INTEGER(KIND=JPIM) :: MSPL       ! zonal component of grad(surface pressure variable)
INTEGER(KIND=JPIM) :: MSPM       ! meridian component of grad(surface pressure variable)
INTEGER(KIND=JPIM) :: NDIM       ! number of coupled fields (includes derivatives)
INTEGER(KIND=JPIM) :: NDIMT      ! number of temporally interpolated fields (includes derivatives)
END TYPE TGMVSTENC

TYPE TGMVCPL
! coupled GMV
INTEGER(KIND=JPIM) :: MU         ! U-wind
INTEGER(KIND=JPIM) :: MV         ! V-wind
INTEGER(KIND=JPIM) :: MT         ! temperature
INTEGER(KIND=JPIM) :: MSPD       ! pressure departure variable
INTEGER(KIND=JPIM) :: MSVD       ! vertical divergence variable
INTEGER(KIND=JPIM) :: MNHX       ! NHX term
! derivatives required for linear terms calculation (in ESEIMPLS)
INTEGER(KIND=JPIM) :: MDIV       ! horizontal divergence
INTEGER(KIND=JPIM) :: MTL        ! zonal component of grad(temperature)
INTEGER(KIND=JPIM) :: MTM        ! meridian component of grad(temperature)
INTEGER(KIND=JPIM) :: MSPDL      ! zonal component of grad(pressure departure variable)
INTEGER(KIND=JPIM) :: MSPDM      ! meridian component of grad(pressure departure variable)
INTEGER(KIND=JPIM) :: NDIM       ! number of coupled fields (does not include derivatives)
INTEGER(KIND=JPIM) :: NDIMT      ! number of temporally interpolated fields (includes derivatives)
END TYPE TGMVCPL

TYPE TGMVSCPL
INTEGER(KIND=JPIM) :: MSP        ! surface pressure variable
INTEGER(KIND=JPIM) :: MSPL       ! zonal component of grad(surface pressure variable)
INTEGER(KIND=JPIM) :: MSPM       ! meridian component of grad(surface pressure variable)
INTEGER(KIND=JPIM) :: NDIM       ! number of coupled fields (does not include derivatives)
INTEGER(KIND=JPIM) :: NDIMT      ! number of temporally interpolated fields (includes derivatives)
END TYPE TGMVSCPL

! ** IMPORTED FROM YEMLBC_INIT
INTEGER(KIND=JPIM), PARAMETER :: JPLSGT=20
INTEGER(KIND=JPIM), PARAMETER :: JPALFNM=31

TYPE :: TELBC_MODEL

!      1.1   Coupling of surface pressure tendency

! LTENC   : TRUE if tendency coupling of surface pressure is switched on
! LALLTC  : used together with LTENC when LTENC=.T.
!           - no meaning for quadratic tendency coupling, where just t1 coupling
!             is applied at every NEFRCL time step
!           - for lin. tendency coupling:
!             TRUE if tendency coupling of surf. pres. at every step
!             FALSE if tend. coupl., except at every NEFRCL time steps
!             when just t1 coupling

LOGICAL :: LTENC
LOGICAL :: LALLTC
LOGICAL :: LRFIRST   ! Force reading of first coupling file (usually, it is the
                     ! same as the initial conditions file)

!      1.2   Lateral forcing

!   NBICOU  : controls coupling of wind components (GMV)
!   NBICOT  : controls coupling of temperature (GMV)
!   NBICPD  : controls coupling of pressure departure variable (GMV)
!   NBICVD  : controls coupling of vertical divergence variable (GMV)
!   NBICNHX : controls coupling of "NHX" term (GMV)
!   NBICOP  : controls coupling of surface pressure (GMVS)
!   Possible value for the NBIC[X] variables:
!   * 0: no coupling
!   * 1: default coupling
!   * 2: specific coupling function

!   NECRIPL : controls timelevel of coupling
!   * 0: coupling at t
!   * 1: coupling at t+dt
!   * 2: coupling at t and t+dt
!   LQCPL   : if T (resp. F), quadratic (resp. linear) temporal interpolation
!   LCCPL   : if T (resp. F), cubic (resp. linear) temporal interpolation
!   NECOTL  : Controls the coupling in the tangent linear model:
!             0   - no coupling
!             < 0 - coupling with the Davies relaxation
!             -1  - the linear time interpolation is switched on
!             > 0 - coupling other than Davies relaxation (to be implemented)
!   NECOAD  : cf. NECOTL but for the adjoint model.
!   LE0COTA : TRUE if the relaxation in the I+E zone is towards
!             nullified boundary conditions (assumed exact);
!             FALSE if the relaxation is towards a predefined forcing.
!             LE0COTA has the same meaning in the TL and AD models
!             (respectively, relaxation to 0 perturbation or sensitivity)
!   LEREADINI: TRUE if initial historical file has to be read
!              (used for E501, E801 - not a namelist parameter)
!   N1LSG   : if =1, the gradient with respect to the large scale
!             coupling data has to be written into a file.
!   NFRLSG  : frequency of writing large scale gradients (time or steps)
!   NLSGTS  : array containing large scale gradients timesteps
!             * if NLSGTS(0)=0 action if MOD(JSTEP,NFRLSG)=0
!             * if NLSGTS(0)>0 NLSGTS(0) significant numbers in NLSGTS
!               are then considered and action for JSTEP=NLSGTS(.)*NFRLSG
!             * if NLSGTS(0)<0 action for JSTEP=(NLSGTS(.)/DELTAT)*NFRLSG
!   LRDLSG  : switch for using boundary data perturbation in conf 1
!             for sensitivity forecast run.
!   JPALFNM : Dimension for reading alpha function parameters

INTEGER(KIND=JPIM) :: NBICOU
INTEGER(KIND=JPIM) :: NBICOT
INTEGER(KIND=JPIM) :: NBICPD
INTEGER(KIND=JPIM) :: NBICVD
INTEGER(KIND=JPIM) :: NBICNHX
INTEGER(KIND=JPIM) :: NBICOP
INTEGER(KIND=JPIM) :: NECRIPL
LOGICAL :: LQCPL 
LOGICAL :: LCCPL
INTEGER(KIND=JPIM) :: NECOTL
INTEGER(KIND=JPIM) :: NECOAD
LOGICAL :: LE0COTA
LOGICAL :: LEREADINI
INTEGER(KIND=JPIM) :: N1LSG
INTEGER(KIND=JPIM) :: NFRLSG
INTEGER(KIND=JPIM) :: NLSGTS(0:JPLSGT)
LOGICAL :: LRDLSG

!      1.3   Spectral nudging

!   LESPCPL  : control of spectral nudging

LOGICAL :: LESPCPL
LOGICAL :: LSPTENC

!      1.4   Upper nesting boundary conditions

!   LUNBC    : controls upper nesting boundary conditions

LOGICAL :: LUNBC


!      2.1   LECOBI.

! LECOBI  : T if there is coupling and biperiodicisation

LOGICAL :: LECOBI

! ** END OF IMPORTED FROM YEMLBC_INIT

! Moved from YEMLBC_FIELDS
!      1.1   Number of coupled fields, structures for coupled fields.

! YYTGMVSTENC  : contains pointers and number of coupled fields for GMVS in LTENC option
! YYTGMVCPL    : contains pointers and number of coupled fields for GMV
! YYTGMVSCPL   : contains pointers and number of coupled fields for GMVS
! NDIMCPL      : number of GFL fields with true LCOUPLING attribute.
! NGALEF       : total number of coupled fields.

TYPE(TGMVSTENC) :: YYTGMVSTENC
TYPE(TGMVCPL) :: YYTGMVCPL
TYPE(TGMVSCPL) :: YYTGMVSCPL
INTEGER(KIND=JPIM) :: NDIMCPL
INTEGER(KIND=JPIM) :: NGALEF
! End ofMoved from YEMLBC_FIELDS

!      2.    DECLARATIONS
!            ------------

!      2.0   Control frequency of LBC.

! NEFRCL   : frequency of updating the lateral boundary coupling fields.
!            The LBC fields will be updated every NEFRCL time steps.
! NETLS1   : Time step of the first set of lateral boundary  fields.
! TEFRCL   : time interval between two updatings of the lateral boundary fields

INTEGER(KIND=JPIM) :: NEFRCL
INTEGER(KIND=JPIM) :: NETLS1
REAL(KIND=JPRB) :: TEFRCL

!      2.2   Namelist variables for relaxation coefficients (resp for GMV, GMVS, GFL).

REAL(KIND=JPRB) :: EPA_GMV(JPALFNM)
REAL(KIND=JPRB) :: EPA_GMVS(JPALFNM)
REAL(KIND=JPRB) :: EPA_GFL(JPALFNM)

!      2.3   Relaxation coefficients.

! EALFA_GMV    : relaxation coefficients alpha for GMV.
! EALFA_GMVS   : relaxation coefficients alpha for GMVS.
! EALFA_GFL    : relaxation coefficients alpha for GFL.
! EALFA_TENC   : relaxation coefficients alpha for LTENC (GMVS only).
! EALFAGT3GMV  : ALFA (relax. coef.) of coupling points for GMV
! EALFAGT3GMVS : ALFA (relax. coef.) of coupling points for GMVS
! EALFAGT3GFL  : ALFA (relax. coef.) of coupling points for GFL
! EALFAU_GMV   : relaxation coefficients alpha for GMV (upper boundary).
! EALFAU_GFL   : relaxation coefficients alpha for GFL (upper boundary).

REAL(KIND=JPRB),ALLOCATABLE:: EALFA_GMV(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFA_GMVS(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFA_GFL(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFA_TENC(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAGT3GMV(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAGT3GMVS(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAGT3GFL(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAU_GMV(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAU_GFL(:,:)

!      2.4   Other variables for grid-point coupling.

! GMGT3        : GM array of coupling points
! GMGT4        : GM array of coupling points (upper boundary).
! EWB          : weights for couplings
! EWBDFIFW     : weights for forward DFI
! EWBDFIBW     : weights for backward DFI
! RTENC        : multiplier of EALFA in the tendency coupling scheme
!                for stability reasons (RTENC<=1. close to 1)

REAL(KIND=JPRB),ALLOCATABLE:: GMGT3(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: GMGT4(:)
REAL(KIND=JPRB),ALLOCATABLE:: EWB(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EWBDFIFW(:,:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EWBDFIBW(:,:,:,:)
REAL(KIND=JPRB) :: RTENC

!      2.5   Other variables for spectral nudging.

! LSPNUSPDL  : .TRUE. if spectral nudging on Ps is relevant on this MPI task
! RNUDTFRAC  : Time fraction for spectral nudging
! NEFRSPCPL  : frequency of spectral nudging
! NEK0,NEK1  : lower and upper limits for total wavenumber for spectral nudging
! NEN1,NEN2  : lower and upper model levels for spectral nudging
! SPNUDVOR   : spectral nudging coeficient for vorticity
! SPNUDDIV   : spectral nudging coeficient for divergence
! SPNUDT     : spectral nudging coeficient for temperature
! SPNUDQ     : spectral nudging coeficient for specific humidity
! SPNUDSP    : spectral nudging coeficient for surface pressure
! LNUDSPGFL  : An array to control if any spectral GFL, for nudging

LOGICAL, ALLOCATABLE :: LNUDSPGFL(:)
LOGICAL :: LSPNUSPDL
REAL(KIND=JPRB) :: RNUDTFRAC
INTEGER(KIND=JPIM) :: NEFRSPCPL
INTEGER(KIND=JPIM) :: NEK0
INTEGER(KIND=JPIM) :: NEK1
INTEGER(KIND=JPIM) :: NEN1
INTEGER(KIND=JPIM) :: NEN2
REAL(KIND=JPRB) :: SPNUDVOR
REAL(KIND=JPRB) :: SPNUDDIV
REAL(KIND=JPRB) :: SPNUDT
REAL(KIND=JPRB) :: SPNUDQ
REAL(KIND=JPRB) :: SPNUDSP
REAL(KIND=JPRB) :: RNUTENC

END TYPE TELBC_MODEL


!=============================================================================

CONTAINS

SUBROUTINE SUELBC_FIELDS_DIM(YDML_LBC,YDGEOMETRY,YDDYNA,YGFL,KNFD2D,KNS3D)






USE GEOMETRY_MOD , ONLY : GEOMETRY
USE YOM_YGFL     , ONLY : TYPE_GFLD
USE YOMDYNA      , ONLY : TDYNA
TYPE(TELBC_MODEL)     , INTENT(INOUT) :: YDML_LBC 
TYPE(GEOMETRY)    , INTENT(IN)    :: YDGEOMETRY
TYPE(TDYNA)       , INTENT(IN)    :: YDDYNA
TYPE(TYPE_GFLD)   , INTENT(INOUT) :: YGFL
INTEGER(KIND=JPIM), INTENT(IN)    :: KNFD2D
INTEGER(KIND=JPIM), INTENT(IN)    :: KNS3D
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
INTEGER(KIND=JPIM) :: IWS_TENC, ICPL, JGFL, IW, IW_TENC














































END SUBROUTINE SUELBC_FIELDS_DIM 
!=====================================================================================
SUBROUTINE SUELBC_INIT(YDDYNA,YDML_LBC)
USE YOMDYNA      , ONLY : TDYNA




TYPE(TDYNA)       , INTENT(IN)    :: YDDYNA
TYPE(TELBC_MODEL),TARGET,INTENT(INOUT):: YDML_LBC



INTEGER(KIND=JPIM) :: J
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

LOGICAL,POINTER :: LTENC,LALLTC,LRFIRST
INTEGER(KIND=JPIM),POINTER :: NBICOU,NBICOT,NBICPD,NBICVD,NBICNHX,NBICOP,NECRIPL
LOGICAL,POINTER :: LQCPL,LCCPL
INTEGER(KIND=JPIM),POINTER :: NECOTL,NECOAD
LOGICAL,POINTER :: LE0COTA,LEREADINI
INTEGER(KIND=JPIM),POINTER :: N1LSG,NFRLSG,NLSGTS(:)
LOGICAL,POINTER :: LRDLSG
LOGICAL,POINTER :: LESPCPL,LSPTENC,LUNBC


















































































END SUBROUTINE SUELBC_INIT

SUBROUTINE SUELBC_MODEL(YDML_LBC,YDGEOMETRY,YDDYNA,YDGMV,YDML_GCONF)







USE GEOMETRY_MOD           , ONLY : GEOMETRY
USE MODEL_GENERAL_CONF_MOD , ONLY : MODEL_GENERAL_CONF_TYPE
USE YOMDYNA                , ONLY : TDYNA

TYPE(TELBC_MODEL) ,TARGET,INTENT(INOUT)    :: YDML_LBC 
TYPE(GEOMETRY),INTENT(IN)                  :: YDGEOMETRY
TYPE(TDYNA)   , INTENT(IN)                 :: YDDYNA
TYPE(TGMV)    ,INTENT(INOUT)               :: YDGMV
TYPE(MODEL_GENERAL_CONF_TYPE),INTENT(INOUT):: YDML_GCONF

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
REAL(KIND=JPRB),ALLOCATABLE :: ZB(:,:),ZBU(:,:),ZEALP(:),ZREPA(:),ZEALFA(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: ZGP(:,:,:),ZSPM(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE :: INEAL(:),INNAL(:),INMAL(:)
INTEGER(KIND=JPIM),ALLOCATABLE :: IBICO(:)

INTEGER(KIND=JPIM) :: IENDLON, ISTLON, IBZONGL, IGPTOT
INTEGER(KIND=JPIM) :: IA, IGLG, IIA, IJA, IROF, ISUP, ICPL
INTEGER(KIND=JPIM) :: JFLD, JK, JA, JIA, JJA, JLON, JGL
INTEGER(KIND=JPIM) :: INSTEP, ICANCPL, JLEV
INTEGER(KIND=JPIM) :: IBL, JKGLO, JROF, IBLOCKCPL

REAL(KIND=JPRB) :: ZA, ZE, ZO, ZRZONG, ZRZONL, ZRZONU, ZXA, ZYA, ZDIV, ZREM, ZYAOXA
REAL(KIND=JPRB) :: ZT, ZT1, ZT2, ZT3, ZT4
REAL(KIND=JPRB) :: ZEPS=1.E-4_JPRB
REAL(KIND=JPRB) :: ZEPS2=1.E-10_JPRB
INTEGER(KIND=JPIM) :: INEFRCLDFI, INEFRCLDFIA, ISWP


INTEGER(KIND=JPIM),POINTER :: NEFRSPCPL,NEK0,NEK1,NEN1,NEN2
REAL(KIND=JPRB),POINTER :: EPA_GMV(:),EPA_GMVS(:),EPA_GFL(:)
REAL(KIND=JPRB),POINTER :: TEFRCL,SPNUDVOR,SPNUDDIV,SPNUDT,SPNUDQ,SPNUDSP,RNUTENC,RTENC




































END SUBROUTINE SUELBC_MODEL

SUBROUTINE DEALLOCATE_ELBC0B(YDML_LBC)







TYPE(TELBC_MODEL) ,INTENT(INOUT) :: YDML_LBC 
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

























END SUBROUTINE DEALLOCATE_ELBC0B

!=============================================================================

END MODULE YEMLBC_MODEL
