MODULE TYPE_FPUSERGEO

USE PARKIND1  ,ONLY : JPIM     ,JPRB   ,JPRD
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE

SAVE
PUBLIC TFPUSERGEO, ALLOC_FPUSERGEO, DEALLOC_FPUSERGEO, PRINT_FPUSERGEO, CHECK_FPUSERGEO

!========== USER PARAMETERS TO DEFINE A GEOMETRY  ======

!========== FOR ALL KINDS OF DOMAINS =====================================

! CFPDOM : domain label

! CFPGRID : kind of geometry : 'GAUSS', 'LELAM' or 'LALON'

! NLAT  : number of latitudes or gridpoints along y axis
! NLON  : number of longitudes or gridpoints along x axis

!========== DEFINITION OF GAUSSIAN GRID ================================

! NFPHTYP : 0 = regular grid
!         : 1 = number of points proportional to sqrt(1-mu**2)
!         : 2 = number of points read on namelist namfpg
! NFPTTYP : 1 = POLE OF STRETCHING, POLE OF THE COLLOCATION GRID AT THE NORTHERN POLE OF THE REAL EARTH.
!           2 = POLE OF STRETCHING, POLE OF THE COLLOCATION GRID ANYWHERE ON THE REAL EARTH.
! FPMUCEN : MU OF THE POLE OF INTEREST
! FPLOCEN : LONGITUDE OF THE POLE OF INTEREST
! FPSTRET : STRETCHING FACTOR
! NFPRGRI : number of active points on a parallel
! FPMU    : Sine of the latitudes (**)
! NFPMEN  : Wavenumbers on each latitude (***)

! (**) : Though this variable is temporary and could be retrieved at any time
! from the spectral transforms data (it is used only to fill the output files headers
! and the interpolations weights), it is defined in the geometry because it could be used one day
! to define a more general grid than a gaussian one. REK

! (**) : Though this variable is temporary and could be retrieved at any time from
! the spectral transforms data (it is used only to fill the output files headers), it is defined
! in the geometry because it could be used one day to define a more general spectral geometry (?). REK

!========== DEFINITION OF LIMITED AREA DOMAIN ===================================

! RDELY : horizontal resolution along latitude or y axis (in metres if LAM ; in degrees otherwise)
! RDELX : horizontal resolution along longitude or x axis (in metres if LAM ; in degrees otherwise)

! NFPRLX : number of safety rows against missing data for interpolation along x dir, lower side
! NFPRLY : number of safety rows against missing data for interpolation along y dir, lower side
! NFPRUX : number of safety rows against missing data for interpolation along x dir, upper side
! NFPRUY : number of safety rows against missing data for interpolation along y dir, upper side

!========== DEFINITION OF LAM DOMAIN ======================================

! NFPGUX : Number of gridpoints along y axis, excluding extension zone
! NFPLUX : Number of gridpoints along x axis, excluding extension zone
! NFPBWX : width of Boyd window in x direction
! NFPBWY : width of Boyd window in y direction
! RLATC  : Center of domain along y axis (in degrees)
! RLONC  : Center of domain along x axis (in degrees)
! FPLON0 : geographical longitude of reference for the projection (in degrees)
! FPLAT0 : geographical latitude of reference for the projection (in degrees)
! LFPMRT : If .TRUE. in Mercator case => use of Rotated/Tilted option
! LFPMAP : .T./.F. if the domain is defined by its coordinates/wavelengths
! FPLX   : wavelength in x (if LFPMAP=.FALSE. only)
! FPLY   : wavelength in y (if LFPMAP=.FALSE. only
! NFPBZONL : half-width of relaxation zone (I) zonal dimension (*)
! NFPBZONG : half-width of relaxation zone (I) meridional dimension (*)
! NFPEDOM  : "Kind" of geometry (spectral, gridpoint C+I+E, or C+I only) ; actually for I/Os (weird !)

! (*) Though these variables are needed today only to fill the output files headers,
! they are defined in the geometry because they could be used one day to define the width
! of a gridpoint frame. REK.

!========== SPECTRAL SPACE DEFINITIONS (Gaussian grid & LAM) ================

! NFPMAX  : spectral truncation (along y axis for LAM)
! NMFPMAX : spectral truncation (along x axis for LAM)
! NFPNOEXTZL : alternative extension zone (E') in x direction - LAM only
! NFPNOEXTZG : alternative extension zone (E') in y direction - LAM only



!========== SELF-DETERMINED VARIABLES FOR A GIVEN GEOMETRY  ======

!========== SPECTRAL SPACE DEFINITIONS ================

! LFPMODELSPEC : .TRUE. if the spectral dimensions are equal to the model ones
! NSPEC2  : local  number of spectral coefficients (MPI distribution)
! NSPEC2G : global number of spectral coefficients

!========== GRIDPOINT SPACE DEFINITIONS ================

! LFPMODELGRID : .TRUE. if the gridpoint dimensions are equal to the model ones for the whole zone (C+I+E)
! LFPMODELCORE : .TRUE. if the gridpoint dimensions are equal to the model ones for core zone only (C+I)
! LFPCOORD     : .TRUE. if actual horizontal interpolation on this grid (new gridpoint coodinates)
! LFPBIPER     : .TRUE. if actual biperiodicization on this grid (new extension zone)
! LFPLAMCOREXT : .TRUE. if LAM core extraction only
! LFPMAPF      : .TRUE. if map factor is smoothed in spectral space (and geometry is plane) for spectral outputs
! LFPOSBUFSHAPE: .TRUE. if Fullpos buffer shape should be used (otherwise model buffer shape can be used)
! NGPTOT  : local  number of grid points (MPI distribution)
! NGPTOTX : maximum local number of grid points (MPI distribution)
! NFPSIZEG: global number of grid points of the output grid
! NFPSIZEG_DEP : global number of grid points of the interpolation grid

!========== FOR ALL KINDS OF DOMAINS ====================

! NFPRESOL : resolution tag of the spectral transforms
! NFPDIST  : Kind of additional transposition between departure geometry and arrival geometry :
!  0 : no additional transposition 
!  1 : transposition based on a straightforward load balancing (can't enable spectral transforms afterwards)
!  2 : transposition based on spectral transforms package distribution


TYPE TFPUSERGEO

CHARACTER(LEN=32) :: CFPDOM = ' '

CHARACTER(LEN=5) ::  CFPGRID

INTEGER(KIND=JPIM) :: NLAT = 0
INTEGER(KIND=JPIM) :: NLON = 0

INTEGER(KIND=JPIM) :: NFPHTYP = 0
INTEGER(KIND=JPIM) :: NFPTTYP = 1
REAL(KIND=JPRB)    :: FPMUCEN = 0._JPRB
REAL(KIND=JPRB)    :: FPLOCEN = 0._JPRB
REAL(KIND=JPRB)    :: FPSTRET = 0._JPRB
INTEGER(KIND=JPIM), ALLOCATABLE :: NFPRGRI(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NFPMEN(:)
REAL(KIND=JPRD),    ALLOCATABLE :: FPMU(:)

REAL(KIND=JPRB) :: RDELY = 0._JPRB
REAL(KIND=JPRB) :: RDELX = 0._JPRB

INTEGER(KIND=JPIM) :: NFPRLX = 0
INTEGER(KIND=JPIM) :: NFPRUX = 0
INTEGER(KIND=JPIM) :: NFPRLY = 0
INTEGER(KIND=JPIM) :: NFPRUY = 0

INTEGER(KIND=JPIM) :: NFPGUX = 0
INTEGER(KIND=JPIM) :: NFPLUX = 0
INTEGER(KIND=JPIM) :: NFPBWX = 0
INTEGER(KIND=JPIM) :: NFPBWY = 0
REAL(KIND=JPRB)    :: RLATC  = 0._JPRB
REAL(KIND=JPRB)    :: RLONC  = 0._JPRB
REAL(KIND=JPRB)    :: FPLON0 = 0._JPRB
REAL(KIND=JPRB)    :: FPLAT0 = 0._JPRB
LOGICAL            :: LFPMRT = .FALSE.
LOGICAL            :: LFPMAP = .FALSE.
REAL(KIND=JPRB)    :: FPLX   = 0._JPRB
REAL(KIND=JPRB)    :: FPLY   = 0._JPRB
INTEGER(KIND=JPIM) :: NFPBZONL = 0
INTEGER(KIND=JPIM) :: NFPBZONG = 0
INTEGER(KIND=JPIM) :: NFPEDOM  = 1

INTEGER(KIND=JPIM) :: NFPMAX  = 0
INTEGER(KIND=JPIM) :: NMFPMAX = 0
INTEGER(KIND=JPIM) :: NFPNOEXTZL = 0
INTEGER(KIND=JPIM) :: NFPNOEXTZG = 0

LOGICAL :: LFPMODELSPEC
LOGICAL :: LFPMODELGRID
LOGICAL :: LFPMODELCORE
LOGICAL :: LFPCOORD
LOGICAL :: LFPBIPER
LOGICAL :: LFPLAMCOREXT
LOGICAL :: LFPMAPF
LOGICAL :: LFPOSBUFSHAPE

INTEGER(KIND=JPIM) :: NFPDIST = 0

INTEGER(KIND=JPIM) :: NFPRESOL = 0
INTEGER(KIND=JPIM) :: NSPEC2   = 0
INTEGER(KIND=JPIM) :: NSPEC2G  = 0 
INTEGER(KIND=JPIM) :: NGPTOT   = 0 
INTEGER(KIND=JPIM) :: NGPTOTX  = 0
INTEGER(KIND=JPIM) :: NFPSIZEG = 0 
INTEGER(KIND=JPIM) :: NFPSIZEG_DEP = 0 

END TYPE TFPUSERGEO

CONTAINS

!-----------------------------------------------------------------------------

SUBROUTINE ALLOC_FPUSERGEO(YD)

TYPE(TFPUSERGEO), INTENT(INOUT) :: YD

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE












END SUBROUTINE ALLOC_FPUSERGEO

SUBROUTINE DEALLOC_FPUSERGEO(YD)

TYPE(TFPUSERGEO), INTENT(INOUT) :: YD

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE









END SUBROUTINE DEALLOC_FPUSERGEO

SUBROUTINE PRINT_FPUSERGEO(YD,KULOUT)

TYPE(TFPUSERGEO), INTENT(IN) :: YD
INTEGER(KIND=JPIM), INTENT(IN) :: KULOUT

INTEGER(KIND=JPIM) :: J

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE














END SUBROUTINE PRINT_FPUSERGEO

SUBROUTINE CHECK_FPUSERGEO(YD,KULOUT)

TYPE(TFPUSERGEO), INTENT(INOUT) :: YD
INTEGER(KIND=JPIM), INTENT(IN) :: KULOUT

INTEGER(KIND=JPIM) :: IERR, J
REAL(KIND=JPRB) :: ZEPS

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


























END SUBROUTINE CHECK_FPUSERGEO

!     ------------------------------------------------------------------
END MODULE TYPE_FPUSERGEO
