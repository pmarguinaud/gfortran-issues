INTERFACE
SUBROUTINE MF_PHYS_MOCON (YDCPG_BNDS, YDCPG_OPTS, PRDG_LCVQ, PMOC_CLPH, YDMF_PHYS, YDMF_PHYS_BASE_STATE)

IMPORT

TYPE(CPG_BNDS_TYPE), INTENT(IN) :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE), INTENT(IN) :: YDCPG_OPTS
REAL(KIND=JPRB), INTENT(IN) :: PRDG_LCVQ (YDCPG_OPTS%KLON, 1:YDCPG_OPTS%KFLEVG)
INTEGER (KIND=JPIM), INTENT(IN) :: PMOC_CLPH (YDCPG_OPTS%KLON)
TYPE(MF_PHYS_TYPE), INTENT(INOUT) :: YDMF_PHYS
TYPE(MF_PHYS_BASE_STATE_TYPE), INTENT(IN) :: YDMF_PHYS_BASE_STATE
END SUBROUTINE MF_PHYS_MOCON
END INTERFACE
