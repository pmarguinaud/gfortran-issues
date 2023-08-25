MODULE REGLATLON_FIELD_MIX

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
IMPLICIT NONE
SAVE

! Define a regular lat-lon field. 
! Assume that:
!       latitude and longitude spacing is regular
!       That the grid spans the entire longitude circle
!
!     Hans Hersbach  9/12/2010
!     M. Fisher   7-March-2012 Use DEALLOCATE_IF_ASSOCIATED + remove semicolons


TYPE REGLATLON_FIELD
  INTEGER(KIND=JPIM)       :: NLAT               ! # of latitudes   ; >=1
  INTEGER(KIND=JPIM)       :: NLON               ! # of longitudes  ; >=1
  REAL(KIND=JPRB)          :: DLAT               ! Latitude  increment (degrees), can be positve/negative
  REAL(KIND=JPRB)          :: DLON               ! Longitude increment (degrees), is assumed to be positve
  REAL(KIND=JPRB) ,POINTER :: PFLD(:,:)=>NULL()  ! Field contents as (lat,lon)
  REAL(KIND=JPRB) ,POINTER :: PLAT(:)  =>NULL()  ! List of NLAT  Latitudes (degrees)
  REAL(KIND=JPRB) ,POINTER :: PSIN(:)  =>NULL()  ! List of NLAT  SIN(Latitudes)
  REAL(KIND=JPRB) ,POINTER :: PLON(:)  =>NULL()  ! List of NLON Longitudes (degrees)
END TYPE REGLATLON_FIELD

CONTAINS

! - -----------------------------------------------------------------------------------------------

SUBROUTINE CREATE_REGLATLON_FIELD(YDFLL,KNLAT,KNLON,PLAT1,PLATN,PLON1,LDEW,CDNAME)


  

  TYPE(REGLATLON_FIELD)      ,INTENT(OUT) :: YDFLL
  INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN ) :: KNLAT,KNLON
  REAL(KIND=JPRB)   ,OPTIONAL,INTENT(IN ) :: PLAT1,PLATN,PLON1
  LOGICAL           ,OPTIONAL,INTENT(IN ) :: LDEW
  CHARACTER(LEN=*)  ,OPTIONAL,INTENT(IN ) :: CDNAME


  INTEGER(KIND=JPIM) :: IKLAT,ILAT,IKLON,ILON,I
  REAL(KIND=JPRB)    :: ZLL
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  


  
  

  
  
   


  
  


  
  

  
  
  
  
  
  


  


  

  
  
  




   
     
   
     

  
       
  

  

  

END SUBROUTINE CREATE_REGLATLON_FIELD

! - -----------------------------------------------------------------------------------------------

SUBROUTINE INTPOL_REGLATLON(PINTVAL,YDFLL,PLAT,PLON,PSIN)

  

  REAL (KIND=JPRB)         ,INTENT(OUT) :: PINTVAL
  TYPE (REGLATLON_FIELD)   ,INTENT(IN ) :: YDFLL
  REAL (KIND=JPRB)         ,INTENT(IN ) :: PLON
  REAL (KIND=JPRB),OPTIONAL,INTENT(IN ) :: PSIN,PLAT
  

  INTEGER(KIND=JPIM) :: ILON(2), ILAT(2), I,J
  REAL   (KIND=JPRB) :: ZXD , ZLAT
  REAL   (KIND=JPRB) :: ZDLON(2), ZDLAT(2),ZDX1,ZDX12
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  

  


  
  

  
  
  
  

  

  
  


  

  

  
  
  
  
  
  


  
  

  

END SUBROUTINE INTPOL_REGLATLON

! - -----------------------------------------------------------------------------------------------

SUBROUTINE STATS_REGLATLON(CDTEXT,YDFLL)


  

  TYPE (REGLATLON_FIELD),INTENT(IN ) :: YDFLL
  CHARACTER(LEN=*)      ,INTENT(IN ) :: CDTEXT

  REAL (KIND=JPRB) :: ZAVE
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  

  
  
  

END SUBROUTINE STATS_REGLATLON

! - -----------------------------------------------------------------------------------------------

FUNCTION XCAP(PX)
  
  REAL (KIND=JPRB)            :: XCAP 
  REAL (KIND=JPRB),INTENT(IN) :: PX 
  
END FUNCTION XCAP

FUNCTION XMOD(PX)
  
  REAL (KIND=JPRB)            :: XMOD
  REAL (KIND=JPRB),INTENT(IN) :: PX 
  
END FUNCTION XMOD

END MODULE REGLATLON_FIELD_MIX
