MODULE STOPH_MIX

USE PARKIND1 , ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------

!*    control parameters for stochastic physics

!      Variables for forced S.V.s
!      LFORCENL   : add perturbations to model tendencies during forecast
!                   period NFORCESTART and NFORCEEND (in hours)

!      Variables for stochastic physics: option PDT
!      NFRSTOPH_SPBS   : time frequency (number of time steps) between
!                        each call to spectral backscatter
!      NFRSTOPH_SPBS_PAT   : time frequency (number of time steps) between
!                        each call to spectral pattern update
!      NFRSTOPH_VC     : time frequency (number of time steps) between
!                        each call to vorticity confinement
!      NSTOCHOPT  : =1 dissipation rate based on massflux-formulation;
!      NSTOCHOPT  : =2 dissipation rate based on updraft-formulation
!      NSTOCHOPT  : =3 dissipation rate based on revised updraft-formulation
!      SQRTCORR   : sqrt of vertical correlation matrix for SPBS perturbations
!      Variables for stochastic physics: common to CASBS and SPBS
!      GPSTREAM        : Gridpoint field for streamfunction perturbations
!      GPTOTDISS       : Gridpoint field for total dissipation rates
!      GPVORTGRAD      : gridpoint field for grad(vorticity) (used in numerical
!                        dissipation rate calculation and vorticity confinement
!      RSMOOTH         : Spectral filter for dissipation rate field
!      ALPHA_DEEP_CONV : Entrainment cloud fraction for deep convection
!      ALPHA_SHAL_CONV : Entrainment cloud fraction for shallow and mid-level convection
!      LEXTRAFIELDS    : .T. to write extrafields
!      LSPBSNORM       ; .T. to calculate and print out energy input
!      LSPBSDISS       ; .T. to calculate and print out global mean dissipationrate
!      SPDP(:)         : average level thickness in Pa (used for energy input calculation)

!      Variables for stochastic physics: option SPBS
!      SPSTREAM        : Spectral field for streamfunction perturbations
!      SPSTREAM_FORC   : Spectral field for streamfunction forcing field
!      SPVELPOT        : Spectral field for velocity potential pattern field
!      SPVELPOT_FORC   : Spectral field for velocity potential forcing field
!      SPG_AMP         : Noise amplitude of streamfunction forcing 
!      ALPHA_STO       : Autoregressive parameter 
!      ONEMINALPHA_NFRSPBS : (1-Autoregressive parameter) if pattern is only updated every NFRSTOPH_SPBS timestep
!      RATIO_BACKSCAT  : Backscatter ratio
!      RSPBS_TAU       : decorrelation time of pattern
!      LSTOPH_SPBS     : .T. to add spectral backscatter streamfunction perturbations
!      LSTOPH_SPBS_FAST: .T. pattern is only updated every NFRSTOPH_SPBS timesteps 
!                        (bigger steps -> should be equivalent statistically)
!      LSTOPH_SPBS_VORT: .T. use vorticity-based forcing  ansatz
!      LSTOPH_TAPER    : .T. to suppress stream function forcing in the PBL
!      LSTOPH_JBCOR    : .T. to make SPBS perturbations vertically correlated similar to Jb statistics for vorticity
!      LSTOPH_UNCORR   : .T. to make SPBS perturbations vertically uncorrelated 
!      LSTOPH_UNIFORM  : .T. to make SPBS perturbations uniformly distributed, instead 
!                        of Gaussian distributed
!      LSTOPH_RVP      : .T. to use random vertical profiles in SPBS
!      LSTOPH_RVPOLD   : .T. to use old RVP (p-dependency only appropriate for 91 and 62 levels)
!      LSTOPH_VARALPHA : .T. to use scale-dependent decorrelation time in SPBS
!      LSTOPH_GAUSS    : .T. to use gaussian for dissipation smoothing
!      LSPBS_DISSGW    : .T. orographic GWD contributes to dissipation rate estimate
!      LSPBS_DISSNUM   : .T. horizontal diffusion contributes to dissipation rate estimate
!      LSPBS_DISSNUM_CT: .T. use horizontal diffusion in SPBS consistent with hor. diff in model
!      LSPBS_DISSCU    : .T. deep convection contributes to dissipation rate estimate
!      RSMOOTHSCALE    : "decorrelation" scale for smoothing
!      RSIGMA2_EPS     : variance of complex random numbers (cancels out for gaussian distributed random numbers in the AR1 process
!                        needs to be 2/12 when uniform distributed random numbers are used)
!      NIMRAN          : address in a spectral array of (m, n=m) for m in increasing order
!                        ([m=0,n=0], [0,1], [0,2],...,[1,1], [1,2],...,[2,2], [2,3],..,[NSMAX,NSMAX])
!      NSMAXSPBS       : maximum wavenumber forced by SPBS

!      Variables for stochastic physics: option SPBS with random vertical profiles
!      RVP_MULMIN      : parameter for wavemode dependent vertical correlation
!      RVP_MULMAX      : parameter for wavemode dependent vertical correlation
!      RVP_MULEXP      : parameter for wavemode dependent vertical correlation
!      RVP_MULNSMAX    : parameter for wavemode dependent vertical correlation
!      RVP_MUL_A       : parameter for pressure dependent vertical correlation
!      RVP_MUL_B       : parameter for pressure dependent vertical correlation
!      RVP_MUL_C       : parameter for pressure dependent vertical correlation
!      RVP_MUL_D       : parameter for pressure dependent vertical correlation
!      RVP_MUL_1       : parameter for new pressure dependent vertical correlation
!      RVP_MUL_2       : parameter for new pressure dependent vertical correlation
!      RVP_MUL(:)      : Multiplicator for pressure dependent vertical correlation
     
!      Variables for stochastic physics: option CASBS
!      LSTOPH_CASBS    : .T. to add cellular automaton streamfunction perturbations
!      MCELL           : Lat-lon grid holding the 'number of lives' for each CA cell
!      RWGHT           : Weighted and smoothed CA pattern
!      AMAGSTOPH_CASBS : Magnitude of forcing 
!      ADLATSTOPH_CA   : Gridsize of cellular automaton in zonal direction
!      ADLONSTOPH_CA   : Gridsize of cellular automaton in meridional direction
!      RFLUX_DET_CLIP  : upper limit for convective mass flux detrainment rate

!      Variables for vorticity confinement 
!      LVORTCON        : .T. to switch on vorticity confinement
!      VC_CON          :  value of the 'epsilon' parameter in Steinhoff's type 1 vorticity confinement

TYPE :: TSTOPH
REAL(KIND=JPRB),ALLOCATABLE :: RSTOPHCA(:)

REAL(KIND=JPRB),ALLOCATABLE :: SQRTCORR(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SPSTREAM(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SPVELPOT(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SPSTREAM_FORC(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SPVELPOT_FORC(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SPG_AMP(:)
REAL(KIND=JPRB),ALLOCATABLE :: ALPHA_STO(:)
REAL(KIND=JPRB),ALLOCATABLE :: ONEMINALPHA_NFRSPBS(:)
REAL(KIND=JPRB),ALLOCATABLE :: RSMOOTH(:)

REAL(KIND=JPRB),ALLOCATABLE :: GPSTREAM(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: GPVELPOT(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: GPTOTDISS(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: GPTOTDISS_SMOOTH(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: GPVORTGRAD(:,:,:)

INTEGER(KIND=JPIM),ALLOCATABLE :: MCELL(:,:)
REAL(KIND=JPRB),   ALLOCATABLE :: RWGHT(:,:)

INTEGER(KIND=JPIM) :: NFRSTOPH_SPBS
INTEGER(KIND=JPIM) :: NFRSTOPH_SPBS_PAT
INTEGER(KIND=JPIM) :: NFRSTOPH_VC
INTEGER(KIND=JPIM) :: NFORCESTART
INTEGER(KIND=JPIM) :: NFORCEEND
INTEGER(KIND=JPIM) :: NSTOCHOPT

REAL(KIND=JPRB) :: ALPHA_DEEP_CONV
REAL(KIND=JPRB) :: ALPHA_SHAL_CONV
REAL(KIND=JPRB) :: SLDISSFAC
REAL(KIND=JPRB) :: RATIO_BACKSCAT
REAL(KIND=JPRB) :: RSPBS_TAU
REAL(KIND=JPRB) :: RATIO_BACKSCAT_CON2NUM
REAL(KIND=JPRB) :: REXPONENT
REAL(KIND=JPRB) :: VC_CON
REAL(KIND=JPRB) :: RFLUX_DET_CLIP
REAL(KIND=JPRB) :: BIHARM
REAL(KIND=JPRB) :: RSMOOTHSCALE
REAL(KIND=JPRB) :: RSIGMA2_EPS

INTEGER(KIND=JPIM) :: INCUT

REAL(KIND=JPRB) :: AMAGSTOPH_CASBS
REAL(KIND=JPRB) :: ADLATSTOPH_CA
REAL(KIND=JPRB) :: ADLONSTOPH_CA

LOGICAL :: LSTOPH_SPBS,LSTOPH_SPBS_FAST,LEXTRAFIELDS,LSTOPH_JBCOR,LSTOPH_UNCORR,LSTOPH_UNIFORM
LOGICAL :: LSTOPH_SPBS_VORT
LOGICAL :: LSTOPH_RVP,LSTOPH_RVPOLD
LOGICAL :: LSTOPH_TAPER
LOGICAL :: LSTOPH_INI
LOGICAL :: LSTOPH_CASBS
LOGICAL :: LVORTCON
LOGICAL :: LFORCENL
LOGICAL :: LSTOPH_VARALPHA
LOGICAL :: LSTOPH_GAUSS
LOGICAL :: LSPBS_DISSGW, LSPBS_DISSNUM, LSPBS_DISSCU 
LOGICAL :: LSPBS_DISSNUM_CT
LOGICAL :: LSPBSNORM
LOGICAL :: LSPBSDISS
REAL(KIND=JPRB),ALLOCATABLE::   SPDP(:)

INTEGER(KIND=JPIM),ALLOCATABLE:: NIMRAN(:)

INTEGER(KIND=JPIM) :: NSMAXSPBS

REAL(KIND=JPRB) ::    RVP_MULMIN
REAL(KIND=JPRB) ::    RVP_MULMAX
REAL(KIND=JPRB) ::    RVP_MULEXP
REAL(KIND=JPRB) ::    RVP_MULNSMAX
REAL(KIND=JPRB),ALLOCATABLE ::  RVP_MULFACT(:)
REAL(KIND=JPRB) ::    RVP_MUL_A, RVP_MUL_B, RVP_MUL_C, RVP_MUL_D
REAL(KIND=JPRB) ::    RVP_MUL_1, RVP_MUL_2
REAL(KIND=JPRB),ALLOCATABLE ::   RVP_MUL(:)
REAL(KIND=JPRB) ::    TAPER_SIGMATOP, TAPER_SIGMABOT, TAPER0, TAPER1,&
                    & TAPER2, TAPER3
REAL(KIND=JPRB),ALLOCATABLE::   TAPER_FACT(:)

!variables for T-Backscatter

LOGICAL :: LSTOPH_SPBS_T
REAL(KIND=JPRB) :: REXPONENT_T
REAL(KIND=JPRB) :: RATIO_APE2KE
REAL(KIND=JPRB),ALLOCATABLE :: SPTEMP(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SPTEMP_FORC(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SPG_AMP_T(:)
REAL(KIND=JPRB),ALLOCATABLE :: ALPHA_STO_T(:)
REAL(KIND=JPRB),ALLOCATABLE :: ONEMINALPHA_NFRSPBS_T(:)
REAL(KIND=JPRB),ALLOCATABLE :: GPTEMP(:,:,:)

REAL(KIND=JPRB) ::    RVP_MULMIN_T
REAL(KIND=JPRB) ::    RVP_MULMAX_T
REAL(KIND=JPRB) ::    RVP_MULEXP_T
REAL(KIND=JPRB) ::    RVP_MULNSMAX_T
REAL(KIND=JPRB),ALLOCATABLE ::    RVP_MULFACT_T(:)
REAL(KIND=JPRB) ::    RVP_MUL_A_T, RVP_MUL_B_T, RVP_MUL_C_T, RVP_MUL_D_T
REAL(KIND=JPRB) ::    RVP_MUL_1_T, RVP_MUL_2_T
REAL(KIND=JPRB),ALLOCATABLE ::   RVP_MUL_T(:)
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TSTOPH
!============================================================================

!     ------------------------------------------------------

CONTAINS

SUBROUTINE INITIALIZE_CELLS(YD_RANDOM_STREAMS,KCELL,PWGHT,KIIP,KJJP)
USE YOMRANDOM_STREAMS , ONLY : TRANDOM_STREAMS
USE YOMHOOK           , ONLY : LHOOK,   DR_HOOK, JPHOOK 


TYPE(TRANDOM_STREAMS) , INTENT(INOUT) :: YD_RANDOM_STREAMS
INTEGER(KIND=JPIM)    , INTENT(IN)    :: KIIP
INTEGER(KIND=JPIM)    , INTENT(IN)    :: KJJP
INTEGER(KIND=JPIM)    , INTENT(INOUT) :: KCELL(KIIP*4,KJJP*4)
REAL(KIND=JPRB)       , INTENT(INOUT) :: PWGHT(KIIP,KJJP)

INTEGER(KIND=JPIM)            :: IC,JC
INTEGER(KIND=JPIM), PARAMETER :: INCLUMPS=200
INTEGER(KIND=JPIM), PARAMETER :: ILIVES=10
INTEGER(KIND=JPIM)            :: I,ICLUMP,JCLUMP
REAL(KIND=JPRB)               :: ZRAND_NOS(2*INCLUMPS+10)   
REAL(KIND=JPHOOK)               :: ZHOOK_HANDLE








    





  


END SUBROUTINE INITIALIZE_CELLS

!     ------------------------------------------------------
SUBROUTINE WEIGHTING_FIELD(KCELL,PWGHT,KIIP,KJJP)





USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK 

INTEGER(KIND=JPIM),INTENT(IN)    :: KIIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KJJP
INTEGER(KIND=JPIM),INTENT(INOUT) :: KCELL(KIIP*4,KJJP*4)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PWGHT(KIIP,KJJP)
INTEGER(KIND=JPIM), PARAMETER    :: ILIVES=10
INTEGER(KIND=JPIM), PARAMETER    :: ISMOOTH=1
INTEGER(KIND=JPIM)               :: I,J,II,JJ
REAL(KIND=JPRB)                  :: ZSUM
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE











END SUBROUTINE WEIGHTING_FIELD

SUBROUTINE SMOOTH121(PFLD,KIIP,KJJP)




USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK 

INTEGER(KIND=JPIM),INTENT(IN)    :: KIIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KJJP
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFLD(KIIP,KJJP)
INTEGER(KIND=JPIM)               :: I,J,IM1,IP1,JM1,JP1
REAL(KIND=JPRB), DIMENSION(KIIP,KJJP) :: ZFLD_SMOOTH
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE







END SUBROUTINE SMOOTH121

SUBROUTINE UPDATE_CELLS(KCELL,KCELLS,KIIP,KJJP)
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK 

INTEGER(KIND=JPIM),INTENT(IN)    :: KIIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KJJP
INTEGER(KIND=JPIM),INTENT(INOUT) :: KCELL(KIIP*4,KJJP*4)
INTEGER(KIND=JPIM),INTENT(INOUT) :: KCELLS(0:KIIP*4+1,0:KJJP*4+1)
INTEGER(KIND=JPIM)               :: IC,JC
INTEGER(KIND=JPIM), PARAMETER    :: ILIVES=10
INTEGER(KIND=JPIM)               :: I,J
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE










END SUBROUTINE UPDATE_CELLS

SUBROUTINE WRAP_CELLS(KCELLS,KIIP,KJJP)
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK 

INTEGER(KIND=JPIM),INTENT(IN)    :: KIIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KJJP
INTEGER(KIND=JPIM),INTENT(INOUT) :: KCELLS(0:KIIP*4+1,0:KJJP*4+1)
INTEGER(KIND=JPIM)               :: IC,JC
INTEGER(KIND=JPIM), PARAMETER    :: ILIVES=10
INTEGER(KIND=JPIM)               :: I,J
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END SUBROUTINE WRAP_CELLS

FUNCTION NEIGHBOUR_COUNT(KCOL,KROW,KCELLS,KIIP,KJJP)
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK 

INTEGER(KIND=JPIM),INTENT(IN)    :: KIIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KJJP
INTEGER(KIND=JPIM),INTENT(IN)    :: KCOL
INTEGER(KIND=JPIM),INTENT(IN)    :: KROW
INTEGER(KIND=JPIM),INTENT(INOUT) :: KCELLS(0:KIIP*4+1,0:KJJP*4+1)
INTEGER(KIND=JPIM), PARAMETER    :: ILIVES=10
INTEGER(KIND=JPIM)               :: ICOUNT,II,JJ
INTEGER(KIND=JPIM)               :: NEIGHBOUR_COUNT
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE








END FUNCTION NEIGHBOUR_COUNT

FUNCTION FERTILE_NEIGHBOUR_COUNT(KCOL,KROW,KCELLS,KIIP,KJJP)



USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK 

INTEGER(KIND=JPIM),INTENT(IN)    :: KIIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KJJP
INTEGER(KIND=JPIM),INTENT(IN)    :: KCOL
INTEGER(KIND=JPIM),INTENT(IN)    :: KROW
INTEGER(KIND=JPIM),INTENT(INOUT) :: KCELLS(0:KIIP*4+1,0:KJJP*4+1)
INTEGER(KIND=JPIM), PARAMETER    :: ILIVES=10
INTEGER(KIND=JPIM)               :: ICOUNT,II,JJ
INTEGER(KIND=JPIM)               :: FERTILE_NEIGHBOUR_COUNT
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE








END FUNCTION FERTILE_NEIGHBOUR_COUNT


FUNCTION SIGTOFACT(PSIG)


















































USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK 
USE YOMCST   , ONLY : RPI

REAL(KIND=JPRB),INTENT(IN) :: PSIG
REAL(KIND=JPRB) :: SIGTOFACT
REAL(KIND=JPRB) :: ZA1, ZVAR, ZOFF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE





  




END FUNCTION SIGTOFACT



SUBROUTINE SPNORMBS(YDGEOMETRY,YDSTOPH,YDRIP,PVOR,PDIV,PVORFORC,PDIVFORC,LDETAIL)













































  USE GEOMETRY_MOD , ONLY : GEOMETRY
  USE PARKIND1     , ONLY : JPIM, JPRB
  USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
  USE YOMCT3       , ONLY : NSTEP
  USE YOMMP0       , ONLY : MYPROC, NPROC
  USE YOMRIP       , ONLY : TRIP
  USE YOMCST       , ONLY : RG
  
TYPE(GEOMETRY)   , INTENT(IN) :: YDGEOMETRY
TYPE(TSTOPH)     , INTENT(IN) :: YDSTOPH
TYPE(TRIP)       , INTENT(IN) :: YDRIP
  REAL(KIND=JPRB), INTENT(IN) :: PVOR(:,:)
  REAL(KIND=JPRB), INTENT(IN) :: PDIV(:,:)
  REAL(KIND=JPRB), INTENT(IN) :: PVORFORC(:,:)
  REAL(KIND=JPRB), INTENT(IN) :: PDIVFORC(:,:)
  LOGICAL        , INTENT(IN) :: LDETAIL

  REAL(KIND=JPRB) :: ZSMVOR(YDGEOMETRY%YRDIMV%NFLEVL,YDGEOMETRY%YRDIM%NUMP),&
 &  ZSMDIV(YDGEOMETRY%YRDIMV%NFLEVL,YDGEOMETRY%YRDIM%NUMP)
  REAL(KIND=JPRB) :: ZSMG(YDGEOMETRY%YRDIMV%NFLEVG,0:YDGEOMETRY%YRDIM%NSMAX)
  INTEGER(KIND=JPIM) :: JLEV, JML, JROC, JMLOC, JL

  REAL(KIND=JPRB) :: ZSENDBUF(YDGEOMETRY%YRDIMV%NFLEVL*YDGEOMETRY%YRDIM%NUMP)
  REAL(KIND=JPRB),ALLOCATABLE :: ZRECVBUF(:)
  INTEGER(KIND=JPIM) :: ISZSP, ISTOTAL, IBUFLENR
  INTEGER(KIND=JPIM) :: IOMASTER
  INTEGER(KIND=JPIM) :: IA,IB,IRECVSETA,IRECVSETB,IRECVLEV,IRECVNUMP,&
                      & ISTLEV,ISZSPR,ITOTAL,IOFFB,IM,ILEV
  INTEGER(KIND=JPIM) :: IRECVCOUNTS(NPROC)

  REAL(KIND=JPRB) :: ZTOTALLEV(YDGEOMETRY%YRDIMV%NFLEVG),ZTOTAL

  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE






END SUBROUTINE SPNORMBS

SUBROUTINE SPNORMEI(YDLAP,YDDIM,PX,PXFORC,PSM,KLEV)











































USE YOMLAP   , ONLY : TLAP
USE YOMDIM   , ONLY : TDIM
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK

TYPE(TLAP)        ,INTENT(IN)  :: YDLAP
TYPE(TDIM)        ,INTENT(IN)  :: YDDIM
REAL(KIND=JPRB)   ,INTENT(IN)  :: PX(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)  :: PXFORC(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT) :: PSM(:,:)
INTEGER(KIND=JPIM),INTENT(IN)  :: KLEV

INTEGER(KIND=JPIM) :: INM, ISP, JN, JLEV, JNML
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE




END SUBROUTINE SPNORMEI


SUBROUTINE GMTOTDISS(YDGEOMETRY,YDSTOPH,YDRIP,PDISS,LDETAIL)










































USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST    , ONLY : RG
USE YOMRIP    , ONLY : TRIP
USE YOMCT3    , ONLY : NSTEP
USE YOMMP0    , ONLY : NPRCIDS, MYPROC, NPROC

TYPE(GEOMETRY)  , INTENT(IN) :: YDGEOMETRY
TYPE(TSTOPH)    , INTENT(IN) :: YDSTOPH
TYPE(TRIP)      , INTENT(IN) :: YDRIP
REAL(KIND=JPRB) , INTENT(IN) :: PDISS(:,:)
LOGICAL         , INTENT(IN) :: LDETAIL

REAL(KIND=JPRB),ALLOCATABLE :: ZSPAS(:)
REAL(KIND=JPRB),ALLOCATABLE :: ZSPAR(:)

REAL(KIND=JPRB) :: ZDISSG(YDGEOMETRY%YRDIMV%NFLEVG,YDGEOMETRY%YRDIM%NSPEC2G), ZTOTDISSGLOBAL

INTEGER(KIND=JPIM) :: ISPINDX(YDGEOMETRY%YRDIM%NSPEC2MX),ISENDREQ(NPROC)

INTEGER(KIND=JPIM) :: ILEN,IBUFLEN,I,IM,ISP,ITAG,IRECV,ILREC&
 & ,IRECVID,IRECVSETA,IRECVSETB,IRECVLEV,IRECVNUMP,ISPEC2RCV&
 & ,ILEV,ISPG,ISEND,IA,IB,JMLOC,JIR,JLEV,JROC,JN,JMN

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE







END SUBROUTINE GMTOTDISS


SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)

CLASS(TSTOPH), INTENT(IN) :: SELF
INTEGER      , INTENT(IN) :: KDEPTH
INTEGER      , INTENT(IN) :: KOUTNO

INTEGER :: IDEPTHLOC













































































































END SUBROUTINE PRINT_CONFIGURATION

END MODULE STOPH_MIX
