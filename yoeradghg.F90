MODULE YOERADGHG

!     -------------------------------------------------------------------
!     *YOERADGHG* - STORAGE OF GAS CLIMATOLOGY INTERPOLATED TO MODEL TIME
!     -------------------------------------------------------------------
! Modifications
!   2019-01-25  R. Hogan  Completely rewrote

USE PARKIND1  ,ONLY : JPRB, JPIM

IMPLICIT NONE

! Type to store greenhouse-gas climatology interpolated to model time
TYPE :: TRADGHG
  ! Sine of latitude where monthly mean climatologies are provided
  REAL(KIND=JPRB), ALLOCATABLE :: SINLAT(:)
  ! Pressure (Pa) at half levels *indexed from 0* up to the number of layers
  REAL(KIND=JPRB), ALLOCATABLE :: PRESSURE_HL(:)
  ! Layer-integrated gas mass concentrations (Pa, i.e. mass mixing
  ! ratio multiplied by pressure difference across the layer),
  ! dimensioned (NLATITUDE,NPRESSURE), having already been
  ! interpolated in time
  REAL(KIND=JPRB), ALLOCATABLE :: MASS_CH4(:,:),  MASS_CO2(:,:),    MASS_N2O(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: MASS_NO2(:,:),  MASS_CFC11(:,:),  MASS_CFC12(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: MASS_CCL4(:,:), MASS_HCFC22(:,:), MASS_O3(:,:)

  ! Number of pressures and latitudes
  INTEGER(KIND=JPIM) :: NLATITUDE = 0, NPRESSURE = 0

CONTAINS

  PROCEDURE :: INIT => INIT_RAD_GHG

END TYPE TRADGHG

TYPE(TRADGHG), POINTER :: YRADGHG => NULL()

CONTAINS

!-----------------------------------------------------------------------
! Initialize pressure and latitude, and allocate space for gases
SUBROUTINE INIT_RAD_GHG(SELF, PLATITUDE, PPRESSURE)

  USE YOMHOOK   ,ONLY : LHOOK, DR_HOOK, JPHOOK
  USE YOMCST    ,ONLY : RPI

  CLASS(TRADGHG),  INTENT(INOUT) :: SELF
  REAL(KIND=JPRB), INTENT(IN)    :: PLATITUDE(:), PPRESSURE(:)

  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  

  
  

  

END SUBROUTINE INIT_RAD_GHG

END MODULE YOERADGHG


