INTERFACE
SUBROUTINE APL_ARPEGE_SHALLOW_CONVECTION_AND_TURBULENCE (YDMF_PHYS_BASE_STATE, YDCPG_BNDS, YDCPG_OPTS,&
 & YDCPG_MISC, YDMF_PHYS, YDMODEL, YDDDH, KNLAB_CVPP, PCDROV, PCHROV, PCOEFN,&
 & PCONDCVPPI, PCONDCVPPL, PDIFCVPPQ, PDIFCVPPS, PEPS, PFLU_CD, PFLU_CH, PKQLROV, PKQROV, PKTROV,&
 & PKUROV, PMSC_LSCPE, PNBVNO, PNEBS, PNEBS0, PNEB_CVPP, PPFL_FPLCH, PPFL_FTKE, PPFL_FTKEI,&
 & PPRODTH_CVPP, PQI, PQIC, PQL, PQLC, PQLIS, PQLIS0, PQLI_CVPP, PQV, PTKE1, PXTROV, PXUROV) 

IMPORT

TYPE (MF_PHYS_BASE_STATE_TYPE), INTENT(IN) :: YDMF_PHYS_BASE_STATE
TYPE(CPG_BNDS_TYPE), INTENT(IN) :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE), INTENT(IN) :: YDCPG_OPTS
TYPE(CPG_MISC_TYPE), INTENT(INOUT) :: YDCPG_MISC
TYPE(MF_PHYS_TYPE), INTENT(INOUT) :: YDMF_PHYS
TYPE(MODEL), INTENT(IN) :: YDMODEL
TYPE(TYP_DDH), INTENT(INOUT) :: YDDDH
INTEGER(KIND=JPIM), INTENT(OUT) :: KNLAB_CVPP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PCDROV(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(IN) :: PCHROV(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PCOEFN(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PCONDCVPPI(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PCONDCVPPL(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PDIFCVPPQ(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PDIFCVPPS(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PEPS
REAL(KIND=JPRB), INTENT(IN) :: PFLU_CD (YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(IN) :: PFLU_CH (YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PKQLROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PKQROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PKTROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PKUROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PMSC_LSCPE (YDCPG_OPTS%KLON, 1:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNBVNO(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBS0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PNEB_CVPP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PPFL_FPLCH (YDCPG_OPTS%KLON, 0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PPFL_FTKE (YDCPG_OPTS%KLON, 0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PPFL_FTKEI (YDCPG_OPTS%KLON, 0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PPRODTH_CVPP(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQI(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQIC(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQL(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQLC(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLIS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLIS0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(INOUT) :: PQLI_CVPP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(IN) :: PQV(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PTKE1(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PXTROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PXUROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
END SUBROUTINE APL_ARPEGE_SHALLOW_CONVECTION_AND_TURBULENCE
END INTERFACE
