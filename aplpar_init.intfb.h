INTERFACE
SUBROUTINE APLPAR_INIT&
 & (LDAROME, KIDIA , KFDIA , KLON , KLEV , KSGST , KCSS ,&
 & PVEG0 ,&
 & PFRMQ ,&
 & PCPS , PLHS ,&
 & PRS , PLH , PLSCPE , PQSAT ,&
 & PQW , PTW , PCD , PCDN , PCH ,&
 & PC1 , PC2 , PEMIS ,&
 & PFEVI ,&
 & PFTKE , PFTKEI , PFEFB1 , PFEFB2 , PFEFB3 ,&
 & PNEIJ , PVEG , PQSATS ,&
 & KCLPH&
 & ) 
USE PARKIND1 ,ONLY : JPIM ,JPRB
LOGICAL, INTENT(IN) :: LDAROME
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KSGST
INTEGER(KIND=JPIM),INTENT(IN) :: KCSS
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
REAL(KIND=JPRB) ,INTENT(IN) :: PVEG0(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PFRMQ(KLON,0:KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PFTKE(KLON,0:KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PFTKEI(KLON,0:KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PFEFB1(KLON,0:KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PFEFB2(KLON,0:KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PFEFB3(KLON,0:KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PCPS(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PLHS(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PRS(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PLH(KLON,KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PLSCPE(KLON,KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PQSAT(KLON,KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PQW(KLON,KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PTW(KLON,KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PCD(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PCDN(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PCH(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PC1(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PC2(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PEMIS(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PFEVI(KLON,KSGST+1)
REAL(KIND=JPRB) ,INTENT(OUT) :: PNEIJ(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PVEG(KLON)
REAL(KIND=JPRB) ,INTENT(OUT) :: PQSATS(KLON)
INTEGER(KIND=JPIM),INTENT(OUT) :: KCLPH(KLON)
END SUBROUTINE APLPAR_INIT
END INTERFACE
