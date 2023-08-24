! (C) Copyright 2011- ECMWF.
! (C) Copyright 2011- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!> Handle model configuration for the IFS model

MODULE TYPE_MODEL

USE PARKIND1                      , ONLY : JPIM
USE MODEL_GENERAL_CONF_MOD        , ONLY : MODEL_GENERAL_CONF_TYPE
USE MODEL_ATMOS_OCEAN_COUPLING_MOD, ONLY : MODEL_ATMOS_OCEAN_COUPLING_TYPE
!USE MODEL_LAM_COUPLING_MOD        , ONLY : MODEL_LAM_COUPLING_TYPE
USE MODEL_DYNAMICS_MOD            , ONLY : MODEL_DYNAMICS_TYPE
USE MODEL_PHYSICS_GENERAL_MOD     , ONLY : MODEL_PHYSICS_GENERAL_TYPE
USE MODEL_PHYSICS_ECMWF_MOD       , ONLY : MODEL_PHYSICS_ECMWF_TYPE
USE MODEL_PHYSICS_SIMPLINEAR_MOD  , ONLY : MODEL_PHYSICS_SIMPLINEAR_TYPE
USE MODEL_PHYSICS_AEROSOL_MOD     , ONLY : MODEL_PHYSICS_AEROSOL_TYPE
USE MODEL_PHYSICS_RADIATION_MOD   , ONLY : MODEL_PHYSICS_RADIATION_TYPE
USE MODEL_PHYSICS_STOCHAST_MOD    , ONLY : MODEL_PHYSICS_STOCHAST_TYPE
USE MODEL_PHYSICS_MF_MOD          , ONLY : MODEL_PHYSICS_MF_TYPE
USE MODEL_CHEM_MOD                , ONLY : MODEL_CHEM_TYPE
USE MODEL_DIAGNOSTICS_MOD         , ONLY : MODEL_DIAGNOSTICS_TYPE
USE YOEWCOU                       , ONLY : TEWCOU
USE YOMCST                        , ONLY : TCST
USE YOMSPSDT                      , ONLY : TSPPT_DATA
USE SPP_MOD                       , ONLY : TSPP_DATA
!
USE YEMLBC_MODEL                  , ONLY : TELBC_MODEL

IMPLICIT NONE

TYPE, PUBLIC :: MODEL
!!  PRIVATE
  LOGICAL                               :: LINEAR_MODEL=.FALSE. ! As seen by OOPS
  TYPE(MODEL_GENERAL_CONF_TYPE)         :: YRML_GCONF
  TYPE(MODEL_ATMOS_OCEAN_COUPLING_TYPE) :: YRML_AOC
!  TYPE(MODEL_LAM_COUPLING_TYPE)         :: YRML_LC   ! Removed
  TYPE(MODEL_DYNAMICS_TYPE)             :: YRML_DYN
  TYPE(MODEL_PHYSICS_GENERAL_TYPE)      :: YRML_PHY_G
  TYPE(MODEL_PHYSICS_ECMWF_TYPE)        :: YRML_PHY_EC
  TYPE(MODEL_PHYSICS_SIMPLINEAR_TYPE)   :: YRML_PHY_SLIN
  TYPE(MODEL_PHYSICS_AEROSOL_TYPE)      :: YRML_PHY_AER
  TYPE(MODEL_PHYSICS_RADIATION_TYPE)    :: YRML_PHY_RAD
  TYPE(MODEL_PHYSICS_STOCHAST_TYPE)     :: YRML_PHY_STOCH
  TYPE(MODEL_PHYSICS_MF_TYPE)           :: YRML_PHY_MF
  TYPE(MODEL_CHEM_TYPE)                 :: YRML_CHEM
  TYPE(MODEL_DIAGNOSTICS_TYPE)          :: YRML_DIAG
  TYPE(TSPPT_DATA)                      :: YRML_SPPT
  TYPE(TSPP_DATA)                       :: YRML_SPP
  TYPE(TEWCOU)                          :: YREWCOU
  TYPE(TCST), POINTER                   :: YRCST => NULL()
  TYPE(TELBC_MODEL)                     :: YRML_LBC
  INTEGER(KIND=JPIM)                    :: MOBJECT_ID=0 ! Object identifier
  CHARACTER(LEN=17)                     :: COBJECT_ID='' ! Object identifier string
  !-------

  CONTAINS

    PROCEDURE, PASS :: PRINT => PRINT_MODEL_CONFIGURATION
    FINAL :: MODEL_FINAL

END TYPE MODEL

!------------------------------------------------------------------------
CONTAINS

SUBROUTINE PRINT_MODEL_CONFIGURATION(SELF,CDSTRING,KOUTNO)
  USE PARKIND1, ONLY : JPIM, JPRB
  USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
  
  CLASS(MODEL),       INTENT(IN)           :: SELF
  CHARACTER(LEN=*),   INTENT(IN)           :: CDSTRING
  INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: KOUTNO

  INTEGER(KIND=JPIM) :: IOUTNO,ITOPDEPTH
  LOGICAL :: LLOPENED = .FALSE.
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  
  

  

  
  
  
  
  


  
  
  
  
  
  
  
  

  
  
  
  
  

  
  
  
  


  
  

END SUBROUTINE PRINT_MODEL_CONFIGURATION

! ------------------------------------------------------------------------------

SUBROUTINE MODEL_FINAL(THIS)
  TYPE(MODEL) :: THIS
  
END SUBROUTINE MODEL_FINAL

END MODULE TYPE_MODEL
