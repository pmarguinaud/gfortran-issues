MODULE FIELD_2RD_MODULE


USE FIELD_BASIC_MODULE
USE PARKIND1, ONLY : JPRM, JPRB, JPRD, JPIM, JPLM

IMPLICIT NONE

PRIVATE

INTEGER (KIND=JPIM), PARAMETER :: NDEVFRESH = INT(B'00000001', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: NHSTFRESH = INT(B'00000010', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: UNALLOCATED = INT(B'00000100', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: NH2D = 1, ND2H = 2
INTEGER (KIND=JPIM), PARAMETER :: NRD = INT(B'00000001', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: NWR = INT(B'00000010', KIND=JPIM)

TYPE, ABSTRACT, EXTENDS (FIELD_BASIC) :: FIELD_2RD
  REAL(KIND=JPRD), POINTER :: PTR(:,:) => NULL()
  REAL(KIND=JPRD), POINTER, CONTIGUOUS :: DEVPTR(:,:) => NULL()
CONTAINS

  PROCEDURE :: FINAL => FIELD_2RD_FINAL
  PROCEDURE :: FIELD_2RD_FINAL
  PROCEDURE :: DELETE_DEVICE => FIELD_2RD_DELETE_DEVICE
  PROCEDURE :: GET_VIEW => FIELD_2RD_GET_VIEW
  PROCEDURE :: GET_DEVICE_DATA_RDONLY => FIELD_2RD_GET_DEVICE_DATA_RDONLY
  PROCEDURE :: GET_DEVICE_DATA_RDWR => FIELD_2RD_GET_DEVICE_DATA_RDWR
  PROCEDURE :: GET_HOST_DATA_RDONLY => FIELD_2RD_GET_HOST_DATA_RDONLY
  PROCEDURE :: GET_HOST_DATA_RDWR => FIELD_2RD_GET_HOST_DATA_RDWR
  PROCEDURE :: SYNC_HOST_RDWR => FIELD_2RD_SYNC_HOST_RDWR
  PROCEDURE :: SYNC_HOST_RDONLY => FIELD_2RD_SYNC_HOST_RDONLY
  PROCEDURE :: SYNC_DEVICE_RDWR => FIELD_2RD_SYNC_DEVICE_RDWR
  PROCEDURE :: SYNC_DEVICE_RDONLY => FIELD_2RD_SYNC_DEVICE_RDONLY
  PROCEDURE :: COPY_OBJECT => FIELD_2RD_COPY_OBJECT
  PROCEDURE :: WIPE_OBJECT => FIELD_2RD_WIPE_OBJECT

  PROCEDURE, PRIVATE :: GET_DEVICE_DATA => FIELD_2RD_GET_DEVICE_DATA
  PROCEDURE, PRIVATE :: GET_HOST_DATA => FIELD_2RD_GET_HOST_DATA
  PROCEDURE, PRIVATE :: FIELD_2RD_GET_HOST_DATA
  PROCEDURE, PRIVATE :: COPY_DATA =>  FIELD_2RD_COPY_DATA
  PROCEDURE, PRIVATE :: CREATE_DEVICE_DATA => FIELD_2RD_CREATE_DEVICE_DATA
END TYPE FIELD_2RD

PUBLIC :: FIELD_2RD

TYPE, EXTENDS(FIELD_2RD) :: FIELD_2RD_WRAPPER
CONTAINS
  PROCEDURE :: INIT => FIELD_2RD_WRAP
  PROCEDURE :: FINAL => FIELD_2RD_WRAPPER_FINAL
END TYPE FIELD_2RD_WRAPPER

PUBLIC :: FIELD_2RD_WRAPPER

TYPE, EXTENDS(FIELD_2RD) :: FIELD_2RD_OWNER
  INTEGER(KIND=JPIM) :: LBOUNDS(2), UBOUNDS(2)
CONTAINS
  PROCEDURE :: INIT => FIELD_2RD_OWNER_INIT
  PROCEDURE :: FINAL => FIELD_2RD_OWNER_FINAL
  PROCEDURE, PRIVATE :: ALLOCATE => FIELD_2RD_ALLOCATE
  PROCEDURE, PRIVATE :: GET_HOST_DATA => FIELD_2RD_OWNER_GET_HOST_DATA
END TYPE FIELD_2RD_OWNER

PUBLIC :: FIELD_2RD_OWNER

TYPE FIELD_2RD_PTR
  CLASS(FIELD_2RD), POINTER :: PTR => NULL()
END TYPE FIELD_2RD_PTR

PUBLIC :: FIELD_2RD_PTR

TYPE FIELD_2RD_VIEW
  REAL(KIND=JPRD), POINTER :: P(:) => NULL()
END TYPE FIELD_2RD_VIEW

PUBLIC :: FIELD_2RD_VIEW


CONTAINS

  SUBROUTINE FIELD_2RD_WRAP(SELF, DATA, PERSISTENT, LBOUNDS)
    
    CLASS(FIELD_2RD_WRAPPER), INTENT(INOUT) :: SELF
    REAL(KIND=JPRD), TARGET, INTENT(IN) :: DATA(:,:)
    LOGICAL, INTENT(IN), OPTIONAL :: PERSISTENT
    INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: LBOUNDS(2)
    LOGICAL :: LLPERSISTENT

    
    

    
    
    

    

  END SUBROUTINE FIELD_2RD_WRAP

  SUBROUTINE FIELD_2RD_OWNER_INIT (SELF, LBOUNDS, UBOUNDS, PERSISTENT, DELAYED)
    CLASS(FIELD_2RD_OWNER) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: LBOUNDS(2)
    INTEGER(KIND=JPIM), INTENT(IN) :: UBOUNDS(2)
    LOGICAL, OPTIONAL,  INTENT(IN) :: PERSISTENT
    LOGICAL, OPTIONAL,  INTENT(IN) :: DELAYED
    LOGICAL :: IS_DELAYED

    
    

    
    
    

    
    

    

    

    
    
  END SUBROUTINE FIELD_2RD_OWNER_INIT

  SUBROUTINE FIELD_2RD_ALLOCATE (SELF)
    
    CLASS(FIELD_2RD_OWNER) :: SELF

    
    
  END SUBROUTINE FIELD_2RD_ALLOCATE

  FUNCTION FIELD_2RD_GET_VIEW(SELF, BLOCK_INDEX, ZERO) RESULT(VIEW_PTR)
    CLASS(FIELD_2RD) :: SELF
    REAL(KIND=JPRD), POINTER :: VIEW_PTR(:)
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    LOGICAL, OPTIONAL,  INTENT(IN) :: ZERO
    INTEGER(KIND=JPIM) :: IDX
    INTEGER(KIND=JPIM) :: LBOUNDS(2)

    
    

    
    

    
  END FUNCTION FIELD_2RD_GET_VIEW

  SUBROUTINE FIELD_2RD_DELETE_DEVICE(SELF)
    
    CLASS(FIELD_2RD) :: SELF

    
  END SUBROUTINE FIELD_2RD_DELETE_DEVICE

  SUBROUTINE FIELD_2RD_FINAL(SELF)
    
    CLASS(FIELD_2RD) :: SELF
    
    
  END SUBROUTINE FIELD_2RD_FINAL

  SUBROUTINE FIELD_2RD_WRAPPER_FINAL(SELF)
    
    CLASS(FIELD_2RD_WRAPPER) :: SELF
    REAL(KIND=JPRD), POINTER :: PTR(:,:)
    
    
  END SUBROUTINE FIELD_2RD_WRAPPER_FINAL

  SUBROUTINE FIELD_2RD_OWNER_FINAL(SELF)
    
    CLASS(FIELD_2RD_OWNER) :: SELF
    
    
  END SUBROUTINE FIELD_2RD_OWNER_FINAL

  SUBROUTINE FIELD_2RD_COPY_OBJECT (SELF, LDCREATED)
  CLASS(FIELD_2RD) :: SELF
  LOGICAL, INTENT (IN), OPTIONAL :: LDCREATED
  LOGICAL :: LLCREATED

  
  

  

  

  END SUBROUTINE FIELD_2RD_COPY_OBJECT

  SUBROUTINE FIELD_2RD_WIPE_OBJECT (SELF, LDDELETED)
  CLASS(FIELD_2RD) :: SELF
  LOGICAL, INTENT (IN), OPTIONAL :: LDDELETED
  LOGICAL :: LLDELETED

  
  

  

  

  END SUBROUTINE FIELD_2RD_WIPE_OBJECT

  SUBROUTINE FIELD_2RD_COPY_DATA (SELF, KDIR, QUEUE)
  CLASS(FIELD_2RD) :: SELF
  INTEGER (KIND=JPIM),           INTENT(IN) :: KDIR
  INTEGER (KIND=JPIM), OPTIONAL, INTENT(IN) :: QUEUE
  REAL :: START, FINISH

  

  
  
  
  

  

    

    

    


  END SUBROUTINE FIELD_2RD_COPY_DATA


  SUBROUTINE FIELD_2RD_GET_HOST_DATA (SELF, MODE, PTR, QUEUE)
    CLASS(FIELD_2RD),                INTENT(INOUT) :: SELF
    INTEGER (KIND=JPIM),           INTENT(IN)    :: MODE

    REAL(KIND=JPRD), POINTER,          INTENT(INOUT) :: PTR(:,:)
    INTEGER (KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE

    INTEGER(KIND=JPIM) :: LBOUNDS(2)

    
    
    
    

  END SUBROUTINE FIELD_2RD_GET_HOST_DATA

  SUBROUTINE FIELD_2RD_OWNER_GET_HOST_DATA (SELF, MODE, PTR, QUEUE)
    CLASS(FIELD_2RD_OWNER),          INTENT(INOUT) :: SELF
    INTEGER (KIND=JPIM),           INTENT(IN)    :: MODE
    REAL(KIND=JPRD), POINTER,          INTENT(INOUT) :: PTR(:,:)
    INTEGER (KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE

    
    

  END SUBROUTINE FIELD_2RD_OWNER_GET_HOST_DATA

  SUBROUTINE FIELD_2RD_GET_HOST_DATA_RDONLY (SELF, PPTR, QUEUE)
    CLASS(FIELD_2RD),               INTENT(INOUT) :: SELF
    REAL(KIND=JPRD), POINTER,         INTENT(INOUT) :: PPTR(:,:)
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE

    

  END SUBROUTINE FIELD_2RD_GET_HOST_DATA_RDONLY

  SUBROUTINE FIELD_2RD_SYNC_HOST_RDONLY (SELF, QUEUE)
    CLASS(FIELD_2RD),               INTENT(INOUT) :: SELF
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE
    REAL(KIND=JPRD), POINTER                        :: ZPTR(:,:)

    

  END SUBROUTINE FIELD_2RD_SYNC_HOST_RDONLY

  SUBROUTINE FIELD_2RD_GET_HOST_DATA_RDWR (SELF, PPTR, QUEUE)
    CLASS(FIELD_2RD),   INTENT(INOUT) :: SELF
    REAL(KIND=JPRD), POINTER,         INTENT(INOUT) :: PPTR(:,:)
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE

    

  END SUBROUTINE FIELD_2RD_GET_HOST_DATA_RDWR

  SUBROUTINE FIELD_2RD_SYNC_HOST_RDWR (SELF, QUEUE)
    CLASS(FIELD_2RD),               INTENT(INOUT) :: SELF
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE
    REAL(KIND=JPRD), POINTER                        :: ZPTR(:,:)

    

  END SUBROUTINE FIELD_2RD_SYNC_HOST_RDWR

  SUBROUTINE FIELD_2RD_CREATE_DEVICE_DATA (SELF)
    CLASS(FIELD_2RD), INTENT (INOUT) :: SELF
    
  END SUBROUTINE

  SUBROUTINE FIELD_2RD_GET_DEVICE_DATA (SELF, MODE, PTR, QUEUE)
    CLASS(FIELD_2RD),                INTENT(INOUT) :: SELF
    INTEGER (KIND=JPIM),           INTENT(IN)    :: MODE
    REAL(KIND=JPRD), POINTER,          INTENT(INOUT) :: PTR(:,:)
    INTEGER (KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE
    INTEGER(KIND=JPIM)                           :: LBOUNDS(2)

    
    
    
    

  END SUBROUTINE FIELD_2RD_GET_DEVICE_DATA

  SUBROUTINE FIELD_2RD_GET_DEVICE_DATA_RDONLY (SELF, PPTR, QUEUE)
    CLASS(FIELD_2RD),               INTENT(INOUT) :: SELF
    REAL(KIND=JPRD), POINTER,         INTENT(INOUT) :: PPTR(:,:)
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE

    

  END SUBROUTINE FIELD_2RD_GET_DEVICE_DATA_RDONLY

  SUBROUTINE FIELD_2RD_SYNC_DEVICE_RDONLY (SELF, QUEUE)
    CLASS(FIELD_2RD),               INTENT(INOUT) :: SELF
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE
    REAL(KIND=JPRD), POINTER                        :: ZPTR(:,:)

    

  END SUBROUTINE FIELD_2RD_SYNC_DEVICE_RDONLY

  SUBROUTINE FIELD_2RD_GET_DEVICE_DATA_RDWR (SELF, PPTR, QUEUE)
    CLASS(FIELD_2RD),               INTENT(INOUT) :: SELF
    REAL(KIND=JPRD), POINTER,         INTENT(INOUT) :: PPTR(:,:)
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE

    

  END SUBROUTINE FIELD_2RD_GET_DEVICE_DATA_RDWR

  SUBROUTINE FIELD_2RD_SYNC_DEVICE_RDWR (SELF, QUEUE)
    CLASS(FIELD_2RD),               INTENT(INOUT) :: SELF
    INTEGER(KIND=JPIM), OPTIONAL, INTENT(IN)    :: QUEUE
    REAL(KIND=JPRD), POINTER                        :: ZPTR(:,:)

    

  END SUBROUTINE FIELD_2RD_SYNC_DEVICE_RDWR


  INTEGER (KIND=JPIM) FUNCTION FIELD_2RD_GET_LAST_CONTIGUOUS_DIMENSION (PTR) RESULT (JDIM)
  REAL(KIND=JPRD), POINTER :: PTR (:,:)
  INTEGER*8 :: ISTRIDE (2)
  INTEGER (KIND=JPIM) :: J, LB(2)

  
  
  

  
  

  

  

  

  END FUNCTION FIELD_2RD_GET_LAST_CONTIGUOUS_DIMENSION

END MODULE FIELD_2RD_MODULE
