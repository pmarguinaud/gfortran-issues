INTERFACE
SUBROUTINE APL_ARPEGE_INIT (YDCST, YDMF_PHYS_BASE_STATE, YDCPG_BNDS, YDCPG_OPTS, YDCPG_MISC,&
 & YDMF_PHYS, YDMF_PHYS_SURF, YDVARS, YDMODEL, KMOC_CLPH, KNLAB_CVPP, PAER, PAERINDS, PAIPCMT, PALBD,&
 & PALBP, PALPHA1, PCEMTR, PCFATH, PCFAU, PCFBTH, PCFBU, PCFBV, PCOEFA, PCONDCVPPI, PCONDCVPPL, PCTRSO,&
 & PDECRD, PDIFCVPPQ, PDIFCVPPS, PDIFCVPPU, PDIFCVPPV, PDIFWQ, PDIFWS, PEDMFQ, PEDMFS, PEDMFU, PEDMFV,&
 & PEPS0, PEPSNEB, PFPCOR, PKQLROV, PKQROV, PKTROV, PKUROV, PLVT, PMF_UP, PMRIPP, PNEBC0, PNEBCH,&
 & PNEBDIFF, PNEBS, PNEBS0, PNEB_CVPP, PPFL_FPLCH, PPFL_FPLSH, PPOID, PPRODTH_CVPP, PQC_DET_PCMT, PQI,&
 & PQIC, PQICE, PQL, PQLC, PQLIS, PQLIS0, PQLI_CVP, PQLI_CVPP, PQO3, PQR, PQS, PQV, PSC_FCLL,&
 & PSC_FCLN, PSC_FEVI, PSC_FEVN, PSEDIQI, PSEDIQL, PSFSWDIF, PSFSWDIR, PSUDU, PTENHA, PTENQVA, PTENT,&
 & PTRSOD, PTRSODIF, PTRSODIR, PUNEBH, PXDROV, PXHROV, PXQRO, PXTRO, PXTROV, PXURO, PXUROV) 
USE PARKIND1, ONLY : JPIM, JPRB
USE MF_PHYS_BASE_STATE_TYPE_MOD&
 & , ONLY : MF_PHYS_BASE_STATE_TYPE 
USE CPG_OPTS_TYPE_MOD , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE CPG_TYPE_MOD , ONLY : CPG_MISC_TYPE, CPG_DYN_TYPE,&
 & CPG_SL2_TYPE, CPG_GPAR_TYPE 
USE MF_PHYS_TYPE_MOD , ONLY : MF_PHYS_TYPE
USE MF_PHYS_SURFACE_TYPE_MOD&
 & , ONLY : MF_PHYS_SURF_TYPE 
USE FIELD_VARIABLES_MOD, ONLY : FIELD_VARIABLES
USE TYPE_MODEL , ONLY : MODEL
USE YOMCST, ONLY : TCST
TYPE(TCST), INTENT(IN) :: YDCST
TYPE (MF_PHYS_BASE_STATE_TYPE), INTENT(IN) :: YDMF_PHYS_BASE_STATE
TYPE(CPG_BNDS_TYPE), INTENT(IN) :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE), INTENT(IN) :: YDCPG_OPTS
TYPE(CPG_MISC_TYPE), INTENT(INOUT) :: YDCPG_MISC
TYPE(MF_PHYS_TYPE), INTENT(INOUT) :: YDMF_PHYS
TYPE(MF_PHYS_SURF_TYPE), INTENT(INOUT) :: YDMF_PHYS_SURF
TYPE(FIELD_VARIABLES), INTENT(INOUT) :: YDVARS
TYPE(MODEL), INTENT(IN) :: YDMODEL
INTEGER(KIND=JPIM), INTENT(OUT) :: KMOC_CLPH (YDCPG_OPTS%KLON)
INTEGER(KIND=JPIM), INTENT(OUT) :: KNLAB_CVPP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PAER(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,6)
REAL(KIND=JPRB), INTENT(OUT) :: PAERINDS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PAIPCMT(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PALBD(YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB), INTENT(OUT) :: PALBP(YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB), INTENT(OUT) :: PALPHA1(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCEMTR(YDCPG_OPTS%KLON,0:1)
REAL(KIND=JPRB), INTENT(OUT) :: PCFATH(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCFAU(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCFBTH(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCFBU(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCFBV(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCOEFA (YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCONDCVPPI(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCONDCVPPL(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PCTRSO(YDCPG_OPTS%KLON,0:1)
REAL(KIND=JPRB), INTENT(OUT) :: PDECRD (YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PDIFCVPPQ(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PDIFCVPPS(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PDIFCVPPU(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PDIFCVPPV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PDIFWQ (YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PDIFWS (YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PEDMFQ(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PEDMFS(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PEDMFU(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PEDMFV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PEPS0
REAL(KIND=JPRB), INTENT(OUT) :: PEPSNEB
REAL(KIND=JPRB), INTENT(OUT) :: PFPCOR(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PKQLROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PKQROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PKTROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PKUROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PLVT (YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PMF_UP(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PMRIPP(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBC0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBCH(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBDIFF(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEBS0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PNEB_CVPP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PPFL_FPLCH (YDCPG_OPTS%KLON, 0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PPFL_FPLSH (YDCPG_OPTS%KLON, 0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PPOID(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PPRODTH_CVPP(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQC_DET_PCMT(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQI(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQIC(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQICE (YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQL(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLC(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLIS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLIS0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLI_CVP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQLI_CVPP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQO3(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQR(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PQV(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PSC_FCLL(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PSC_FCLN(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PSC_FEVI (YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PSC_FEVN(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PSEDIQI(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PSEDIQL(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PSFSWDIF (YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB), INTENT(OUT) :: PSFSWDIR (YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB), INTENT(OUT) :: PSUDU(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PTENHA(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PTENQVA(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PTENT(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PTRSOD(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PTRSODIF (YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB), INTENT(OUT) :: PTRSODIR (YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB), INTENT(OUT) :: PUNEBH(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PXDROV(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PXHROV(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PXQRO(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PXTRO(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PXTROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PXURO(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB), INTENT(OUT) :: PXUROV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
END SUBROUTINE APL_ARPEGE_INIT
END INTERFACE