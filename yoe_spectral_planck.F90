! (C) Copyright 2019- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE YOE_SPECTRAL_PLANCK

! YOE_SPECTRAL_PLANCK
!
! PURPOSE
! -------
!   Calculate Planck function integrated across user-specified
!   spectral intervals, used in RADHEATN by approximate longwave
!   update scheme to modify longwave fluxes to account for the
!   spectral emissivity on the high-resolution model grid (rather than
!   the lower resolution grid seen by the radiation scheme).
!
! INTERFACE
! ---------
!   Call the INIT member routine to configure the look-up table of the
!   TSPECRALPLANCK type, followed by any number of CALC calls with the
!   temperatures at which the Planck function is required. FREE then
!   deallocates memory.
!
! AUTHOR
! ------
!   Robin Hogan, ECMWF
!   Original: 2019-02-04
!
! MODIFICATIONS
! -------------
!   A Dawson 2019-08-05 avoid single precision overflow in INIT

!-----------------------------------------------------------------------

USE PARKIND1, ONLY : JPRB,JPRD,JPIM
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
IMPLICIT NONE
SAVE

!-----------------------------------------------------------------------
! Type for storing Planck function look-up table
TYPE TSPECTRALPLANCK
  ! Number of intervals over which the integrated Planck function is
  ! required. Note that an interval need not be contiguous in
  ! wavelength.
  INTEGER(KIND=JPIM) :: NINTERVALS

  ! Number of temperatures in look-up table
  INTEGER(KIND=JPIM) :: NTEMPS

  ! Start temperature and temperature spacing of look-up table
  REAL(KIND=JPRB) :: TEMP1, DTEMP

  ! Integrated Planck functions in look-up table, dimensioned
  ! (NINTERVALS,NTEMPS)
  REAL(KIND=JPRB),    ALLOCATABLE :: PLANCK_LUT(:,:)

  ! Store interval data
  REAL(KIND=JPRB),    ALLOCATABLE :: WAVLEN_BOUND(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: INTERVAL_MAP(:)

CONTAINS
  PROCEDURE :: INIT
  PROCEDURE :: CALC
  PROCEDURE :: PRINT=> PRINT_SPECTRAL_PLANCK
  PROCEDURE :: FREE => FREE_SPECTRAL_PLANCK

END TYPE TSPECTRALPLANCK

CONTAINS

!-----------------------------------------------------------------------
! Generate a Planck function look-up table consisting of KINTERVALS
! spectral intervals (which need not be contiguous in wavelength),
! whose wavelength bounds are defined by PWAVLEN_BOUND and mapping
! on to KINTERVALS described by KINTERVAL_MAP.
SUBROUTINE INIT(SELF, KINTERVALS, PWAVLEN_BOUND, KINTERVAL_MAP)

  USE YOMCST,   ONLY : RPI, RKBOL, RHPLA, RCLUM

  CLASS(TSPECTRALPLANCK), INTENT(INOUT) :: SELF
  INTEGER(KIND=JPIM)    , INTENT(IN)    :: KINTERVALS
  REAL(KIND=JPRB)       , INTENT(IN)    :: PWAVLEN_BOUND(:)
  INTEGER(KIND=JPIM)    , INTENT(IN)    :: KINTERVAL_MAP(:)

  
  REAL(KIND=JPRB) :: ZTEMP

  
  REAL(KIND=JPRB) :: ZCOEFF1, ZCOEFF2

  
  REAL(KIND=JPRB) :: ZWAVLEN1, ZWAVLEN2, DWAVLEN

  
  REAL(KIND=JPRB) :: ZWAVLEN, ZWAVLEN_SQR

  
  REAL(KIND=JPRB) :: ZSUM, ZWEIGHT

  
  
  REAL(KIND=JPRD) :: ZPLANCKEXP

  
  
  INTEGER(KIND=JPIM) :: NRANGES, NWAVLEN

  INTEGER(KIND=JPIM) :: JT, JI, JW, JR

  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE



  

  

  

END SUBROUTINE INIT


!-----------------------------------------------------------------------
! Calculate Planck function in spectral intervals from temperature
SUBROUTINE CALC(SELF, KIDIA, KFDIA, KLON, PTEMPERATURE, PPLANCK)

  USE YOMCST,   ONLY : RSIGMA

  CLASS(TSPECTRALPLANCK), INTENT(IN)  :: SELF
  
  INTEGER(KIND=JPIM)    , INTENT(IN)  :: KIDIA, KFDIA, KLON
  
  REAL(KIND=JPRB)       , INTENT(IN)  :: PTEMPERATURE(KLON)
  
  REAL(KIND=JPRB)       , INTENT(OUT) :: PPLANCK(KLON,SELF%NINTERVALS)

  
  INTEGER(KIND=JPRB) :: JL, ITEMP

  
  REAL(KIND=JPRB) :: ZWEIGHT, ZTEMP2

  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  

  

  

END SUBROUTINE CALC


!-----------------------------------------------------------------------
! Print look-up table to a unit
SUBROUTINE PRINT_SPECTRAL_PLANCK(SELF, IUNIT)

  CLASS(TSPECTRALPLANCK), INTENT(IN) :: SELF
  INTEGER(KIND=JPIM),     INTENT(IN) :: IUNIT

  INTEGER(KIND=JPIM) :: JT

  CHARACTER(len=24)  :: MY_FORMAT
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  
  


END SUBROUTINE PRINT_SPECTRAL_PLANCK


!-----------------------------------------------------------------------
! Free allocated memory
SUBROUTINE FREE_SPECTRAL_PLANCK(SELF)

  CLASS(TSPECTRALPLANCK), INTENT(INOUT) :: SELF
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  
  
  
  


END SUBROUTINE FREE_SPECTRAL_PLANCK


END MODULE YOE_SPECTRAL_PLANCK
