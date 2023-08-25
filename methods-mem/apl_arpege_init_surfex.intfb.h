INTERFACE
SUBROUTINE APL_ARPEGE_INIT_SURFEX (YDMF_PHYS_BASE_STATE, YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS,&
 & YDCPG_MISC, YDCPG_GPAR, YDMF_PHYS, YDVARS, YDMODEL, PALBD, PALBP, PFLU_EMIS, PSGROUPEL, PSRAIN,&
 & PSSNOW, PTSN) 
USE PARKIND1, ONLY : JPIM, JPRB
USE MF_PHYS_BASE_STATE_TYPE_MOD&
 & , ONLY : MF_PHYS_BASE_STATE_TYPE 
USE GEOMETRY_MOD , ONLY : GEOMETRY
USE CPG_OPTS_TYPE_MOD , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE CPG_TYPE_MOD , ONLY : CPG_MISC_TYPE, CPG_DYN_TYPE,&
 & CPG_SL2_TYPE, CPG_GPAR_TYPE 
USE CPG_TYPE_MOD , ONLY : CPG_MISC_TYPE, CPG_DYN_TYPE,&
 & CPG_SL2_TYPE, CPG_GPAR_TYPE 
USE MF_PHYS_TYPE_MOD , ONLY : MF_PHYS_TYPE
USE FIELD_VARIABLES_MOD, ONLY : FIELD_VARIABLES
USE TYPE_MODEL , ONLY : MODEL
TYPE (MF_PHYS_BASE_STATE_TYPE), INTENT(IN) :: YDMF_PHYS_BASE_STATE
TYPE(GEOMETRY), INTENT(IN) :: YDGEOMETRY
TYPE(CPG_BNDS_TYPE), INTENT(IN) :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE), INTENT(IN) :: YDCPG_OPTS
TYPE(CPG_MISC_TYPE), INTENT(INOUT) :: YDCPG_MISC
TYPE(CPG_GPAR_TYPE), INTENT(INOUT) :: YDCPG_GPAR
TYPE(MF_PHYS_TYPE), INTENT(INOUT) :: YDMF_PHYS
TYPE(FIELD_VARIABLES), INTENT(INOUT) :: YDVARS
TYPE(MODEL), INTENT(IN) :: YDMODEL
REAL(KIND=JPRB), INTENT(OUT) :: PALBD(YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB), INTENT(OUT) :: PALBP(YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB), INTENT(OUT) :: PFLU_EMIS (YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PSGROUPEL(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PSRAIN(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PSSNOW(YDCPG_OPTS%KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PTSN(YDCPG_OPTS%KLON)
END SUBROUTINE APL_ARPEGE_INIT_SURFEX
END INTERFACE