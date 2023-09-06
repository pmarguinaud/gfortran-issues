INTERFACE
SUBROUTINE APL_ARPEGE_CLOUDINESS (YDMF_PHYS_BASE_STATE, YDCPG_BNDS, YDCPG_OPTS, YDCPG_MISC,&
 & YDMF_PHYS, YDVARS, YDMODEL, LDREDPR, PAIPCMT, PBLH, PDECRD, PFLU_QSAT, PMSC_QW, PNEBC0,&
 & PNEBCH, PNEBS, PNEBS0, PNEB_CVPP, PPFL_FPLCH, PQI, PQL, PQLIS, PQLIS0, PQLI_CVP, PQLI_CVPP, PQV,&
 & PUNEBH, YDSTA) 

IMPORT

TYPE (MF_PHYS_BASE_STATE_TYPE), INTENT(IN) :: YDMF_PHYS_BASE_STATE
TYPE(CPG_BNDS_TYPE), INTENT(IN) :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE), INTENT(IN) :: YDCPG_OPTS
TYPE(CPG_MISC_TYPE), INTENT(INOUT) :: YDCPG_MISC
TYPE(MF_PHYS_TYPE), INTENT(INOUT) :: YDMF_PHYS
TYPE(FIELD_VARIABLES), INTENT(INOUT) :: YDVARS
TYPE(MODEL), INTENT(IN) :: YDMODEL
LOGICAL, INTENT(IN) :: LDREDPR
REAL(KIND=JPRB), INTENT(IN) :: PAIPCMT(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(IN) :: PBLH(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(IN) :: PDECRD(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(IN) :: PFLU_QSAT (YDCPG_OPTS%KLON, 1:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PMSC_QW (YDCPG_OPTS%KLON, 1:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PNEBC0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PNEBCH(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBS0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PNEB_CVPP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PPFL_FPLCH (YDCPG_OPTS%KLON, 0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQI(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQL(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLIS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLIS0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQLI_CVP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQLI_CVPP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQV(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PUNEBH(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
TYPE(TSTA), INTENT(IN) :: YDSTA
END SUBROUTINE APL_ARPEGE_CLOUDINESS
END INTERFACE
