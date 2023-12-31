
MODULE MODEL_PHYSICS_AEROSOL_MOD
  USE YOEAERLID, ONLY : TEAERLID
  USE YOEAERMAP, ONLY : TEAERMAP
  USE YOEAERSNK, ONLY : TEAERSNK
  USE YOEAERSRC, ONLY : TEAERSRC
  USE YOEAERVOL, ONLY : TEAERVOL
  USE YOEDBUG, ONLY : TEDBUG
  IMPLICIT NONE
          
  TYPE MODEL_PHYSICS_AEROSOL_TYPE

  TYPE(TEAERLID)  :: YREAERLID  !! LIDAR simulator of 
                                !! aerosol effects
  TYPE(TEAERMAP)  :: YREAERMAP
  TYPE(TEAERSNK)  :: YREAERSNK  !! sinks
  TYPE(TEAERSRC)  :: YREAERSRC  !! sources
  TYPE(TEAERVOL)  :: YREAERVOL  !! volcanic aerosols
  TYPE(TEDBUG)    :: YREDBUG    !! aerosol debugging help

    CONTAINS

    PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 

  END TYPE MODEL_PHYSICS_AEROSOL_TYPE

  !---------------------------------------------------------------------

  CONTAINS 

  SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  
  CLASS(MODEL_PHYSICS_AEROSOL_TYPE), INTENT(IN) :: SELF
  INTEGER                          , INTENT(IN) :: KDEPTH
  INTEGER                          , INTENT(IN) :: KOUTNO

  
  
  
  
  
  

  END SUBROUTINE PRINT_CONFIGURATION

END MODULE MODEL_PHYSICS_AEROSOL_MOD
