INTERFACE
SUBROUTINE MF_PHYS_FPL_PART2 (YDCPG_BNDS, YDCPG_OPTS, PPFL_FPLCH, PPFL_FPLSH, PCPF_T1, PSPF_T1,&
 & YDMODEL) 

IMPORT

TYPE(CPG_BNDS_TYPE),INTENT(IN) :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE),INTENT(IN) :: YDCPG_OPTS
REAL (KIND=JPRB), INTENT(IN) :: PPFL_FPLCH (YDCPG_OPTS%KLON, 0:YDCPG_OPTS%KFLEVG)
REAL (KIND=JPRB), INTENT(IN) :: PPFL_FPLSH (YDCPG_OPTS%KLON, 0:YDCPG_OPTS%KFLEVG)
REAL (KIND=JPRB), INTENT(OUT) :: PCPF_T1 (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL (KIND=JPRB), INTENT(OUT) :: PSPF_T1 (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
TYPE(MODEL) ,INTENT(IN) :: YDMODEL
END SUBROUTINE MF_PHYS_FPL_PART2
END INTERFACE
