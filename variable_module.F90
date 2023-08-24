! Rank and shape definitions for simple templating

MODULE VARIABLE_MODULE
  ! Base class definition of VARIABLE types that manages configuration
  ! and metadata for individual variables and associates them with the
  ! respective FIELDS objects that store the data.

USE PARKIND1, ONLY: JPIM, JPRB, JPRD
USE FIELD_MODULE, ONLY: FIELD_2RB, FIELD_3RB, FIELD_4RB
USE FIELD_MODULE, ONLY: FIELD_2RD, FIELD_3RD, FIELD_4RD
USE YOM_YGFL, ONLY: TYPE_GFL_COMP

IMPLICIT NONE

TYPE, ABSTRACT :: VARIABLE_BASE
  ! Description and definition of a scientific variable that stores
  ! its associated data in one or more underlying fields, eg. for timestepping.

  ! Generic metadata like names and IDs
  CHARACTER(LEN=16)  :: NAME                  ! Primary name used for indexing
  CHARACTER(LEN=16)  :: CNAME     = ''        ! ARPEGE field name
  INTEGER(KIND=JPIM) :: IGRBCODE  = -999      ! GRIB code

  ! Flags that define the behaviour of the field variable
  LOGICAL            :: LACTIVE   = .FALSE.   ! Field in use
  LOGICAL            :: LT1       = .FALSE.   ! Field in t+dt GFL
  LOGICAL            :: LT9       = .FALSE.   ! Field in t-dt GFL
  LOGICAL            :: LPH9      = .FALSE.   ! Field in t-dt physics
  LOGICAL            :: LDL       = .FALSE.   ! Field has zontal derivative
  LOGICAL            :: LDM       = .FALSE.   ! Field has meridional derivative
  LOGICAL            :: LDL9      = .FALSE.   ! Field has zontal derivative at t-dt
  LOGICAL            :: LDM9      = .FALSE.   ! Field has meridional derivative at t-dt
  LOGICAL            :: LADV      = .FALSE.   ! Field advected or not
  LOGICAL            :: LGP       = .FALSE.   
  LOGICAL            :: LWATER    = .FALSE.   
  LOGICAL            :: LTHERMACT = .FALSE.   
  LOGICAL            :: LCDERS    = .FALSE.   
  REAL (KIND=JPRB)   :: RCP       = 0._JPRB
  REAL (KIND=JPRB)   :: R         = 0._JPRB

  TYPE (TYPE_GFL_COMP) :: YCOMP

CONTAINS
  PROCEDURE(VARIABLE_BASE_FINAL), DEFERRED :: FINAL
END TYPE VARIABLE_BASE

ABSTRACT INTERFACE
  SUBROUTINE VARIABLE_BASE_FINAL(SELF)
    IMPORT :: VARIABLE_BASE
    CLASS(VARIABLE_BASE) :: SELF
  END SUBROUTINE VARIABLE_BASE_FINAL
END INTERFACE


TYPE, EXTENDS(VARIABLE_BASE) :: VARIABLE_2RB
  ! TODO: Allocation-specific metadata, like shapes and dimensions
  ! Note that storing things like NLEV would break templating

  ! Array view pointers, to be set up from associated fields
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: P(:)  => NULL()  ! Basic field at t
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T0(:) => NULL()  ! Basic field at t (alias of P)
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T1(:) => NULL()  ! Basic field at t+dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T9(:) => NULL()  ! Basic field at t-dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: PH9(:)=> NULL()  ! Basic field for physics
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DL(:) => NULL()  ! Zonal derivative field
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DM(:) => NULL()  ! Meridional derivative field
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DL9(:) => NULL()  ! Zonal derivative field at t-dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DM9(:) => NULL()  ! Meridional derivative field at t-dt

  ! Pointers to associated FIELD objects
  CLASS (FIELD_2RB), POINTER :: FT0 => NULL()  ! Basic field at t
  CLASS (FIELD_2RB), POINTER :: FT1 => NULL()  ! Basic field at t+dt
  CLASS (FIELD_2RB), POINTER :: FT9 => NULL()  ! Basic field at t-dt
  CLASS (FIELD_2RB), POINTER :: FPH9 => NULL() ! Basic field for physics
  CLASS (FIELD_2RB), POINTER :: FDL => NULL()  ! Zonal derivative field
  CLASS (FIELD_2RB), POINTER :: FDM => NULL()  ! Meridional derivative field
  CLASS (FIELD_2RB), POINTER :: FDL9 => NULL() ! Zonal derivative field at t-dt
  CLASS (FIELD_2RB), POINTER :: FDM9 => NULL() ! Meridional derivative field at t-dt

CONTAINS
  PROCEDURE :: UPDATE_VIEW => VARIABLE_2RB_UPDATE_VIEW
  PROCEDURE :: CLONE => VARIABLE_2RB_CLONE
  PROCEDURE :: FINAL => VARIABLE_2RB_FINAL
  ! Timestepping utilities
  PROCEDURE :: PH9TOT0 => VARIABLE_2RB_PH9TOT0
  PROCEDURE :: PH9TOT9 => VARIABLE_2RB_PH9TOT9
END TYPE VARIABLE_2RB


TYPE :: VARIABLE_2RB_PTR
  TYPE (TYPE_GFL_COMP) :: YCOMP
  TYPE (VARIABLE_2RB), POINTER :: YV => NULL ()
END TYPE 

TYPE, EXTENDS(VARIABLE_BASE) :: VARIABLE_3RB
  ! TODO: Allocation-specific metadata, like shapes and dimensions
  ! Note that storing things like NLEV would break templating

  ! Array view pointers, to be set up from associated fields
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: P(:,:)  => NULL()  ! Basic field at t
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T0(:,:) => NULL()  ! Basic field at t (alias of P)
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T1(:,:) => NULL()  ! Basic field at t+dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T9(:,:) => NULL()  ! Basic field at t-dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: PH9(:,:)=> NULL()  ! Basic field for physics
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DL(:,:) => NULL()  ! Zonal derivative field
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DM(:,:) => NULL()  ! Meridional derivative field
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DL9(:,:) => NULL()  ! Zonal derivative field at t-dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DM9(:,:) => NULL()  ! Meridional derivative field at t-dt

  ! Pointers to associated FIELD objects
  CLASS (FIELD_3RB), POINTER :: FT0 => NULL()  ! Basic field at t
  CLASS (FIELD_3RB), POINTER :: FT1 => NULL()  ! Basic field at t+dt
  CLASS (FIELD_3RB), POINTER :: FT9 => NULL()  ! Basic field at t-dt
  CLASS (FIELD_3RB), POINTER :: FPH9 => NULL() ! Basic field for physics
  CLASS (FIELD_3RB), POINTER :: FDL => NULL()  ! Zonal derivative field
  CLASS (FIELD_3RB), POINTER :: FDM => NULL()  ! Meridional derivative field
  CLASS (FIELD_3RB), POINTER :: FDL9 => NULL() ! Zonal derivative field at t-dt
  CLASS (FIELD_3RB), POINTER :: FDM9 => NULL() ! Meridional derivative field at t-dt

CONTAINS
  PROCEDURE :: UPDATE_VIEW => VARIABLE_3RB_UPDATE_VIEW
  PROCEDURE :: CLONE => VARIABLE_3RB_CLONE
  PROCEDURE :: FINAL => VARIABLE_3RB_FINAL
  ! Timestepping utilities
  PROCEDURE :: PH9TOT0 => VARIABLE_3RB_PH9TOT0
  PROCEDURE :: PH9TOT9 => VARIABLE_3RB_PH9TOT9
END TYPE VARIABLE_3RB


TYPE :: VARIABLE_3RB_PTR
  TYPE (TYPE_GFL_COMP) :: YCOMP
  TYPE (VARIABLE_3RB), POINTER :: YV => NULL ()
END TYPE 

TYPE, EXTENDS(VARIABLE_BASE) :: VARIABLE_4RB
  ! TODO: Allocation-specific metadata, like shapes and dimensions
  ! Note that storing things like NLEV would break templating

  ! Array view pointers, to be set up from associated fields
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: P(:,:,:)  => NULL()  ! Basic field at t
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T0(:,:,:) => NULL()  ! Basic field at t (alias of P)
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T1(:,:,:) => NULL()  ! Basic field at t+dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: T9(:,:,:) => NULL()  ! Basic field at t-dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: PH9(:,:,:)=> NULL()  ! Basic field for physics
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DL(:,:,:) => NULL()  ! Zonal derivative field
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DM(:,:,:) => NULL()  ! Meridional derivative field
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DL9(:,:,:) => NULL()  ! Zonal derivative field at t-dt
  REAL(KIND=JPRB), POINTER, CONTIGUOUS :: DM9(:,:,:) => NULL()  ! Meridional derivative field at t-dt

  ! Pointers to associated FIELD objects
  CLASS (FIELD_4RB), POINTER :: FT0 => NULL()  ! Basic field at t
  CLASS (FIELD_4RB), POINTER :: FT1 => NULL()  ! Basic field at t+dt
  CLASS (FIELD_4RB), POINTER :: FT9 => NULL()  ! Basic field at t-dt
  CLASS (FIELD_4RB), POINTER :: FPH9 => NULL() ! Basic field for physics
  CLASS (FIELD_4RB), POINTER :: FDL => NULL()  ! Zonal derivative field
  CLASS (FIELD_4RB), POINTER :: FDM => NULL()  ! Meridional derivative field
  CLASS (FIELD_4RB), POINTER :: FDL9 => NULL() ! Zonal derivative field at t-dt
  CLASS (FIELD_4RB), POINTER :: FDM9 => NULL() ! Meridional derivative field at t-dt

CONTAINS
  PROCEDURE :: UPDATE_VIEW => VARIABLE_4RB_UPDATE_VIEW
  PROCEDURE :: CLONE => VARIABLE_4RB_CLONE
  PROCEDURE :: FINAL => VARIABLE_4RB_FINAL
  ! Timestepping utilities
  PROCEDURE :: PH9TOT0 => VARIABLE_4RB_PH9TOT0
  PROCEDURE :: PH9TOT9 => VARIABLE_4RB_PH9TOT9
END TYPE VARIABLE_4RB


TYPE :: VARIABLE_4RB_PTR
  TYPE (TYPE_GFL_COMP) :: YCOMP
  TYPE (VARIABLE_4RB), POINTER :: YV => NULL ()
END TYPE 

TYPE, EXTENDS(VARIABLE_BASE) :: VARIABLE_2RD
  ! TODO: Allocation-specific metadata, like shapes and dimensions
  ! Note that storing things like NLEV would break templating

  ! Array view pointers, to be set up from associated fields
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: P(:)  => NULL()  ! Basic field at t
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T0(:) => NULL()  ! Basic field at t (alias of P)
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T1(:) => NULL()  ! Basic field at t+dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T9(:) => NULL()  ! Basic field at t-dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: PH9(:)=> NULL()  ! Basic field for physics
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DL(:) => NULL()  ! Zonal derivative field
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DM(:) => NULL()  ! Meridional derivative field
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DL9(:) => NULL()  ! Zonal derivative field at t-dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DM9(:) => NULL()  ! Meridional derivative field at t-dt

  ! Pointers to associated FIELD objects
  CLASS (FIELD_2RD), POINTER :: FT0 => NULL()  ! Basic field at t
  CLASS (FIELD_2RD), POINTER :: FT1 => NULL()  ! Basic field at t+dt
  CLASS (FIELD_2RD), POINTER :: FT9 => NULL()  ! Basic field at t-dt
  CLASS (FIELD_2RD), POINTER :: FPH9 => NULL() ! Basic field for physics
  CLASS (FIELD_2RD), POINTER :: FDL => NULL()  ! Zonal derivative field
  CLASS (FIELD_2RD), POINTER :: FDM => NULL()  ! Meridional derivative field
  CLASS (FIELD_2RD), POINTER :: FDL9 => NULL() ! Zonal derivative field at t-dt
  CLASS (FIELD_2RD), POINTER :: FDM9 => NULL() ! Meridional derivative field at t-dt

CONTAINS
  PROCEDURE :: UPDATE_VIEW => VARIABLE_2RD_UPDATE_VIEW
  PROCEDURE :: CLONE => VARIABLE_2RD_CLONE
  PROCEDURE :: FINAL => VARIABLE_2RD_FINAL
  ! Timestepping utilities
  PROCEDURE :: PH9TOT0 => VARIABLE_2RD_PH9TOT0
  PROCEDURE :: PH9TOT9 => VARIABLE_2RD_PH9TOT9
END TYPE VARIABLE_2RD


TYPE :: VARIABLE_2RD_PTR
  TYPE (TYPE_GFL_COMP) :: YCOMP
  TYPE (VARIABLE_2RB), POINTER :: YV => NULL ()
END TYPE 

TYPE, EXTENDS(VARIABLE_BASE) :: VARIABLE_3RD
  ! TODO: Allocation-specific metadata, like shapes and dimensions
  ! Note that storing things like NLEV would break templating

  ! Array view pointers, to be set up from associated fields
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: P(:,:)  => NULL()  ! Basic field at t
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T0(:,:) => NULL()  ! Basic field at t (alias of P)
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T1(:,:) => NULL()  ! Basic field at t+dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T9(:,:) => NULL()  ! Basic field at t-dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: PH9(:,:)=> NULL()  ! Basic field for physics
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DL(:,:) => NULL()  ! Zonal derivative field
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DM(:,:) => NULL()  ! Meridional derivative field
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DL9(:,:) => NULL()  ! Zonal derivative field at t-dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DM9(:,:) => NULL()  ! Meridional derivative field at t-dt

  ! Pointers to associated FIELD objects
  CLASS (FIELD_3RD), POINTER :: FT0 => NULL()  ! Basic field at t
  CLASS (FIELD_3RD), POINTER :: FT1 => NULL()  ! Basic field at t+dt
  CLASS (FIELD_3RD), POINTER :: FT9 => NULL()  ! Basic field at t-dt
  CLASS (FIELD_3RD), POINTER :: FPH9 => NULL() ! Basic field for physics
  CLASS (FIELD_3RD), POINTER :: FDL => NULL()  ! Zonal derivative field
  CLASS (FIELD_3RD), POINTER :: FDM => NULL()  ! Meridional derivative field
  CLASS (FIELD_3RD), POINTER :: FDL9 => NULL() ! Zonal derivative field at t-dt
  CLASS (FIELD_3RD), POINTER :: FDM9 => NULL() ! Meridional derivative field at t-dt

CONTAINS
  PROCEDURE :: UPDATE_VIEW => VARIABLE_3RD_UPDATE_VIEW
  PROCEDURE :: CLONE => VARIABLE_3RD_CLONE
  PROCEDURE :: FINAL => VARIABLE_3RD_FINAL
  ! Timestepping utilities
  PROCEDURE :: PH9TOT0 => VARIABLE_3RD_PH9TOT0
  PROCEDURE :: PH9TOT9 => VARIABLE_3RD_PH9TOT9
END TYPE VARIABLE_3RD


TYPE :: VARIABLE_3RD_PTR
  TYPE (TYPE_GFL_COMP) :: YCOMP
  TYPE (VARIABLE_3RB), POINTER :: YV => NULL ()
END TYPE 

TYPE, EXTENDS(VARIABLE_BASE) :: VARIABLE_4RD
  ! TODO: Allocation-specific metadata, like shapes and dimensions
  ! Note that storing things like NLEV would break templating

  ! Array view pointers, to be set up from associated fields
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: P(:,:,:)  => NULL()  ! Basic field at t
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T0(:,:,:) => NULL()  ! Basic field at t (alias of P)
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T1(:,:,:) => NULL()  ! Basic field at t+dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: T9(:,:,:) => NULL()  ! Basic field at t-dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: PH9(:,:,:)=> NULL()  ! Basic field for physics
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DL(:,:,:) => NULL()  ! Zonal derivative field
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DM(:,:,:) => NULL()  ! Meridional derivative field
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DL9(:,:,:) => NULL()  ! Zonal derivative field at t-dt
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DM9(:,:,:) => NULL()  ! Meridional derivative field at t-dt

  ! Pointers to associated FIELD objects
  CLASS (FIELD_4RD), POINTER :: FT0 => NULL()  ! Basic field at t
  CLASS (FIELD_4RD), POINTER :: FT1 => NULL()  ! Basic field at t+dt
  CLASS (FIELD_4RD), POINTER :: FT9 => NULL()  ! Basic field at t-dt
  CLASS (FIELD_4RD), POINTER :: FPH9 => NULL() ! Basic field for physics
  CLASS (FIELD_4RD), POINTER :: FDL => NULL()  ! Zonal derivative field
  CLASS (FIELD_4RD), POINTER :: FDM => NULL()  ! Meridional derivative field
  CLASS (FIELD_4RD), POINTER :: FDL9 => NULL() ! Zonal derivative field at t-dt
  CLASS (FIELD_4RD), POINTER :: FDM9 => NULL() ! Meridional derivative field at t-dt

CONTAINS
  PROCEDURE :: UPDATE_VIEW => VARIABLE_4RD_UPDATE_VIEW
  PROCEDURE :: CLONE => VARIABLE_4RD_CLONE
  PROCEDURE :: FINAL => VARIABLE_4RD_FINAL
  ! Timestepping utilities
  PROCEDURE :: PH9TOT0 => VARIABLE_4RD_PH9TOT0
  PROCEDURE :: PH9TOT9 => VARIABLE_4RD_PH9TOT9
END TYPE VARIABLE_4RD


TYPE :: VARIABLE_4RD_PTR
  TYPE (TYPE_GFL_COMP) :: YCOMP
  TYPE (VARIABLE_4RB), POINTER :: YV => NULL ()
END TYPE 


INTERFACE VARIABLE_2RB
  MODULE PROCEDURE :: VARIABLE_2RB_INIT
  ! MODULE PROCEDURE :: VARIABLE_FROM_NAMELIST
END INTERFACE VARIABLE_2RB
INTERFACE VARIABLE_3RB
  MODULE PROCEDURE :: VARIABLE_3RB_INIT
  ! MODULE PROCEDURE :: VARIABLE_FROM_NAMELIST
END INTERFACE VARIABLE_3RB
INTERFACE VARIABLE_4RB
  MODULE PROCEDURE :: VARIABLE_4RB_INIT
  ! MODULE PROCEDURE :: VARIABLE_FROM_NAMELIST
END INTERFACE VARIABLE_4RB
INTERFACE VARIABLE_2RD
  MODULE PROCEDURE :: VARIABLE_2RD_INIT
  ! MODULE PROCEDURE :: VARIABLE_FROM_NAMELIST
END INTERFACE VARIABLE_2RD
INTERFACE VARIABLE_3RD
  MODULE PROCEDURE :: VARIABLE_3RD_INIT
  ! MODULE PROCEDURE :: VARIABLE_FROM_NAMELIST
END INTERFACE VARIABLE_3RD
INTERFACE VARIABLE_4RD
  MODULE PROCEDURE :: VARIABLE_4RD_INIT
  ! MODULE PROCEDURE :: VARIABLE_FROM_NAMELIST
END INTERFACE VARIABLE_4RD

INTERFACE ARGUMENT_VALUE
  ! Helper interface to resolve values of optional arguments with defaults
  MODULE PROCEDURE ARGUMENT_VALUE_REAL
  MODULE PROCEDURE ARGUMENT_VALUE_INTEGER
  MODULE PROCEDURE ARGUMENT_VALUE_STRING
  MODULE PROCEDURE ARGUMENT_VALUE_LOGICAL
END INTERFACE ARGUMENT_VALUE

CONTAINS

  FUNCTION ARGUMENT_VALUE_REAL(ARG, DEFAULT) RESULT(VAL)
    
    REAL(KIND=JPRB), OPTIONAL, INTENT(IN) :: ARG
    REAL(KIND=JPRB), INTENT(IN) :: DEFAULT
    REAL(KIND=JPRB) :: VAL

    
  END FUNCTION ARGUMENT_VALUE_REAL

  FUNCTION ARGUMENT_VALUE_INTEGER(ARG, DEFAULT) RESULT(VAL)
    
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN) :: ARG
    INTEGER(KIND=JPIM), INTENT(IN) :: DEFAULT
    INTEGER(KIND=JPIM) :: VAL

    
  END FUNCTION ARGUMENT_VALUE_INTEGER

  FUNCTION ARGUMENT_VALUE_STRING(ARG, DEFAULT) RESULT(VAL)
    
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: ARG
    CHARACTER(LEN=*), INTENT(IN) :: DEFAULT
    CHARACTER(:), ALLOCATABLE :: VAL

    
  END FUNCTION ARGUMENT_VALUE_STRING

  FUNCTION ARGUMENT_VALUE_LOGICAL(ARG, DEFAULT) RESULT(VAL)
    
    LOGICAL, OPTIONAL, INTENT(IN) :: ARG
    LOGICAL, INTENT(IN) :: DEFAULT
    LOGICAL :: VAL

    
  END FUNCTION ARGUMENT_VALUE_LOGICAL


  FUNCTION VARIABLE_2RB_INIT (NAME, CNAME, IGRBCODE, LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT,&
      & LCDERS, RCP, R) RESULT(SELF)
    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
    
    TYPE(VARIABLE_2RB) :: SELF
    CHARACTER(LEN=*), INTENT(IN) :: NAME
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: CNAME
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN) :: IGRBCODE
    LOGICAL, OPTIONAL, INTENT(IN) :: LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT, LCDERS
    REAL(KIND=JPRB), OPTIONAL, INTENT (IN) :: RCP, R

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END FUNCTION VARIABLE_2RB_INIT

  FUNCTION VARIABLE_3RB_INIT (NAME, CNAME, IGRBCODE, LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT,&
      & LCDERS, RCP, R) RESULT(SELF)
    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
    
    TYPE(VARIABLE_3RB) :: SELF
    CHARACTER(LEN=*), INTENT(IN) :: NAME
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: CNAME
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN) :: IGRBCODE
    LOGICAL, OPTIONAL, INTENT(IN) :: LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT, LCDERS
    REAL(KIND=JPRB), OPTIONAL, INTENT (IN) :: RCP, R

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END FUNCTION VARIABLE_3RB_INIT

  FUNCTION VARIABLE_4RB_INIT (NAME, CNAME, IGRBCODE, LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT,&
      & LCDERS, RCP, R) RESULT(SELF)
    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
    
    TYPE(VARIABLE_4RB) :: SELF
    CHARACTER(LEN=*), INTENT(IN) :: NAME
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: CNAME
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN) :: IGRBCODE
    LOGICAL, OPTIONAL, INTENT(IN) :: LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT, LCDERS
    REAL(KIND=JPRB), OPTIONAL, INTENT (IN) :: RCP, R

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END FUNCTION VARIABLE_4RB_INIT

  FUNCTION VARIABLE_2RD_INIT (NAME, CNAME, IGRBCODE, LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT,&
      & LCDERS, RCP, R) RESULT(SELF)
    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
    
    TYPE(VARIABLE_2RD) :: SELF
    CHARACTER(LEN=*), INTENT(IN) :: NAME
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: CNAME
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN) :: IGRBCODE
    LOGICAL, OPTIONAL, INTENT(IN) :: LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT, LCDERS
    REAL(KIND=JPRB), OPTIONAL, INTENT (IN) :: RCP, R

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END FUNCTION VARIABLE_2RD_INIT

  FUNCTION VARIABLE_3RD_INIT (NAME, CNAME, IGRBCODE, LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT,&
      & LCDERS, RCP, R) RESULT(SELF)
    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
    
    TYPE(VARIABLE_3RD) :: SELF
    CHARACTER(LEN=*), INTENT(IN) :: NAME
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: CNAME
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN) :: IGRBCODE
    LOGICAL, OPTIONAL, INTENT(IN) :: LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT, LCDERS
    REAL(KIND=JPRB), OPTIONAL, INTENT (IN) :: RCP, R

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END FUNCTION VARIABLE_3RD_INIT

  FUNCTION VARIABLE_4RD_INIT (NAME, CNAME, IGRBCODE, LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT,&
      & LCDERS, RCP, R) RESULT(SELF)
    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
    
    TYPE(VARIABLE_4RD) :: SELF
    CHARACTER(LEN=*), INTENT(IN) :: NAME
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: CNAME
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN) :: IGRBCODE
    LOGICAL, OPTIONAL, INTENT(IN) :: LACTIVE, LADV, LT1, LT9, LPH9, LDL, LDM, LDL9, LDM9, LGP, LWATER, LTHERMACT, LCDERS
    REAL(KIND=JPRB), OPTIONAL, INTENT (IN) :: RCP, R

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  END FUNCTION VARIABLE_4RD_INIT


  FUNCTION VARIABLE_2RB_CLONE(SELF) RESULT(NEWOBJ)
    
    
    
    
    CLASS(VARIABLE_2RB) :: SELF
    TYPE(VARIABLE_2RB) :: NEWOBJ

    
    
    








  END FUNCTION VARIABLE_2RB_CLONE

  FUNCTION VARIABLE_3RB_CLONE(SELF) RESULT(NEWOBJ)
    
    
    
    
    CLASS(VARIABLE_3RB) :: SELF
    TYPE(VARIABLE_3RB) :: NEWOBJ

    
    
    








  END FUNCTION VARIABLE_3RB_CLONE

  FUNCTION VARIABLE_4RB_CLONE(SELF) RESULT(NEWOBJ)
    
    
    
    
    CLASS(VARIABLE_4RB) :: SELF
    TYPE(VARIABLE_4RB) :: NEWOBJ

    
    
    








  END FUNCTION VARIABLE_4RB_CLONE

  FUNCTION VARIABLE_2RD_CLONE(SELF) RESULT(NEWOBJ)
    
    
    
    
    CLASS(VARIABLE_2RD) :: SELF
    TYPE(VARIABLE_2RD) :: NEWOBJ

    
    
    








  END FUNCTION VARIABLE_2RD_CLONE

  FUNCTION VARIABLE_3RD_CLONE(SELF) RESULT(NEWOBJ)
    
    
    
    
    CLASS(VARIABLE_3RD) :: SELF
    TYPE(VARIABLE_3RD) :: NEWOBJ

    
    
    








  END FUNCTION VARIABLE_3RD_CLONE

  FUNCTION VARIABLE_4RD_CLONE(SELF) RESULT(NEWOBJ)
    
    
    
    
    CLASS(VARIABLE_4RD) :: SELF
    TYPE(VARIABLE_4RD) :: NEWOBJ

    
    
    








  END FUNCTION VARIABLE_4RD_CLONE


  SUBROUTINE VARIABLE_2RB_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(VARIABLE_2RB) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    REAL(KIND=JPRB), TARGET, SAVE :: ZDUM(1)

    
    
    
    
    
    
    
    
    
      
  END SUBROUTINE VARIABLE_2RB_UPDATE_VIEW

  SUBROUTINE VARIABLE_3RB_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(VARIABLE_3RB) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    REAL(KIND=JPRB), TARGET, SAVE :: ZDUM(1, 1)

    
    
    
    
    
    
    
    
    
      
  END SUBROUTINE VARIABLE_3RB_UPDATE_VIEW

  SUBROUTINE VARIABLE_4RB_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(VARIABLE_4RB) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    REAL(KIND=JPRB), TARGET, SAVE :: ZDUM(1, 1, 1)

    
    
    
    
    
    
    
    
    
      
  END SUBROUTINE VARIABLE_4RB_UPDATE_VIEW

  SUBROUTINE VARIABLE_2RD_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(VARIABLE_2RD) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    REAL(KIND=JPRD), TARGET, SAVE :: ZDUM(1)

    
    
    
    
    
    
    
    
    
      
  END SUBROUTINE VARIABLE_2RD_UPDATE_VIEW

  SUBROUTINE VARIABLE_3RD_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(VARIABLE_3RD) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    REAL(KIND=JPRD), TARGET, SAVE :: ZDUM(1, 1)

    
    
    
    
    
    
    
    
    
      
  END SUBROUTINE VARIABLE_3RD_UPDATE_VIEW

  SUBROUTINE VARIABLE_4RD_UPDATE_VIEW(SELF, BLOCK_INDEX)
    
    CLASS(VARIABLE_4RD) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    REAL(KIND=JPRD), TARGET, SAVE :: ZDUM(1, 1, 1)

    
    
    
    
    
    
    
    
    
      
  END SUBROUTINE VARIABLE_4RD_UPDATE_VIEW


  SUBROUTINE VARIABLE_2RB_PH9TOT0(SELF)
    
    CLASS(VARIABLE_2RB), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_2RB_PH9TOT0

  SUBROUTINE VARIABLE_3RB_PH9TOT0(SELF)
    
    CLASS(VARIABLE_3RB), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_3RB_PH9TOT0

  SUBROUTINE VARIABLE_4RB_PH9TOT0(SELF)
    
    CLASS(VARIABLE_4RB), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_4RB_PH9TOT0

  SUBROUTINE VARIABLE_2RD_PH9TOT0(SELF)
    
    CLASS(VARIABLE_2RD), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_2RD_PH9TOT0

  SUBROUTINE VARIABLE_3RD_PH9TOT0(SELF)
    
    CLASS(VARIABLE_3RD), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_3RD_PH9TOT0

  SUBROUTINE VARIABLE_4RD_PH9TOT0(SELF)
    
    CLASS(VARIABLE_4RD), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_4RD_PH9TOT0


  SUBROUTINE VARIABLE_2RB_PH9TOT9(SELF)
    
    CLASS(VARIABLE_2RB), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_2RB_PH9TOT9

  SUBROUTINE VARIABLE_3RB_PH9TOT9(SELF)
    
    CLASS(VARIABLE_3RB), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_3RB_PH9TOT9

  SUBROUTINE VARIABLE_4RB_PH9TOT9(SELF)
    
    CLASS(VARIABLE_4RB), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_4RB_PH9TOT9

  SUBROUTINE VARIABLE_2RD_PH9TOT9(SELF)
    
    CLASS(VARIABLE_2RD), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_2RD_PH9TOT9

  SUBROUTINE VARIABLE_3RD_PH9TOT9(SELF)
    
    CLASS(VARIABLE_3RD), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_3RD_PH9TOT9

  SUBROUTINE VARIABLE_4RD_PH9TOT9(SELF)
    
    CLASS(VARIABLE_4RD), TARGET :: SELF

    
  END SUBROUTINE VARIABLE_4RD_PH9TOT9



  SUBROUTINE VARIABLE_2RB_FINAL(SELF)
    
    CLASS(VARIABLE_2RB) :: SELF

    
    
    
    
    
    
    
    
  END SUBROUTINE VARIABLE_2RB_FINAL

  SUBROUTINE VARIABLE_3RB_FINAL(SELF)
    
    CLASS(VARIABLE_3RB) :: SELF

    
    
    
    
    
    
    
    
  END SUBROUTINE VARIABLE_3RB_FINAL

  SUBROUTINE VARIABLE_4RB_FINAL(SELF)
    
    CLASS(VARIABLE_4RB) :: SELF

    
    
    
    
    
    
    
    
  END SUBROUTINE VARIABLE_4RB_FINAL

  SUBROUTINE VARIABLE_2RD_FINAL(SELF)
    
    CLASS(VARIABLE_2RD) :: SELF

    
    
    
    
    
    
    
    
  END SUBROUTINE VARIABLE_2RD_FINAL

  SUBROUTINE VARIABLE_3RD_FINAL(SELF)
    
    CLASS(VARIABLE_3RD) :: SELF

    
    
    
    
    
    
    
    
  END SUBROUTINE VARIABLE_3RD_FINAL

  SUBROUTINE VARIABLE_4RD_FINAL(SELF)
    
    CLASS(VARIABLE_4RD) :: SELF

    
    
    
    
    
    
    
    
  END SUBROUTINE VARIABLE_4RD_FINAL


END MODULE VARIABLE_MODULE
