SUBROUTINE APL_ARPEGE(YDMF_PHYS_BASE_STATE, YDMF_PHYS_NEXT_STATE, YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, &
& YDCPG_MISC, YDCPG_GPAR, YDCPG_PHY0, YDMF_PHYS, YDCPG_DYN0, YDMF_PHYS_SURF, YDCPG_SL2, YDVARS,       &
& YDMODEL, YDDDH)

USE GEOMETRY_MOD       , ONLY : GEOMETRY
USE MF_PHYS_TYPE_MOD   , ONLY : MF_PHYS_TYPE
USE CPG_TYPE_MOD       , ONLY : CPG_MISC_TYPE, CPG_DYN_TYPE, &
                              & CPG_SL2_TYPE, CPG_GPAR_TYPE, &
                              & CPG_PHY_TYPE
USE CPG_OPTS_TYPE_MOD  , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE MF_PHYS_SURFACE_TYPE_MOD  &
                     & , ONLY : MF_PHYS_SURF_TYPE
USE FIELD_VARIABLES_MOD, ONLY : FIELD_VARIABLES
USE MF_PHYS_BASE_STATE_TYPE_MOD &
                     & , ONLY : MF_PHYS_BASE_STATE_TYPE
USE MF_PHYS_NEXT_STATE_TYPE_MOD &
                     & , ONLY : MF_PHYS_NEXT_STATE_TYPE
USE TYPE_MODEL         , ONLY : MODEL

USE PARKIND1           , ONLY : JPIM     ,JPRB
USE YOMHOOK            , ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE DDH_MIX            , ONLY : TYP_DDH

USE YOMSTA             , ONLY : TSTA
USE YOMCST             , ONLY : TCST

USE MODEL_PHYSICS_MF_MOD, ONLY : MODEL_PHYSICS_MF_TYPE
USE YOMCLI              , ONLY : TCLI
USE YOMDIMV             , ONLY : TDIMV
USE YOMDVISI            , ONLY : TDVISI
USE YOMPHY0             , ONLY : TPHY0
USE YOMPHY1             , ONLY : TPHY1
USE YOMPHY2             , ONLY : TPHY2
USE YOMPHY              , ONLY : TPHY
USE YOMRIP              , ONLY : TRIP
USE YOMTOPH             , ONLY : TTOPH
USE YOM_YGFL            , ONLY : TYPE_GFLD

IMPLICIT NONE

TYPE(MF_PHYS_BASE_STATE_TYPE),  INTENT(IN)    :: YDMF_PHYS_BASE_STATE
TYPE(MF_PHYS_NEXT_STATE_TYPE),  INTENT(INOUT) :: YDMF_PHYS_NEXT_STATE
TYPE(GEOMETRY),                 INTENT(IN)    :: YDGEOMETRY
TYPE(CPG_BNDS_TYPE),            INTENT(IN)    :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE),            INTENT(IN)    :: YDCPG_OPTS
TYPE(CPG_MISC_TYPE),            INTENT(INOUT) :: YDCPG_MISC
TYPE(CPG_GPAR_TYPE),            INTENT(INOUT) :: YDCPG_GPAR
TYPE(CPG_PHY_TYPE),             INTENT(IN)    :: YDCPG_PHY0
TYPE(MF_PHYS_TYPE),             INTENT(INOUT) :: YDMF_PHYS
TYPE(CPG_DYN_TYPE),             INTENT(IN)    :: YDCPG_DYN0
TYPE(MF_PHYS_SURF_TYPE),        INTENT(INOUT) :: YDMF_PHYS_SURF
TYPE(CPG_SL2_TYPE),             INTENT(INOUT) :: YDCPG_SL2
TYPE(FIELD_VARIABLES),          INTENT(INOUT) :: YDVARS
TYPE(MODEL),                    INTENT(IN)    :: YDMODEL
TYPE(TYP_DDH),                  INTENT(INOUT) :: YDDDH

#include "accldia.intfb.h"
#include "acclph.intfb.h"
#include "acdnshf.intfb.h"
#include "acdrag.intfb.h"
#include "acdrme.intfb.h"
#include "acevadcape.intfb.h"
#include "achmt.intfb.h"
#include "achmtls.intfb.h"
#include "acpluis.intfb.h"
#include "acsol.intfb.h"
#include "actqsat.intfb.h"
#include "acvisih.intfb.h"
#include "aplpar_init.intfb.h"
#include "checkmv.intfb.h"
#include "cpphinp.intfb.h"
#include "cpqsol.intfb.h"
#include "mf_phys_bayrad.intfb.h"
#include "mf_phys_corwat.intfb.h"
#include "mf_phys_cvv.intfb.h"
#include "mf_phys_fpl_part1.intfb.h"
#include "mf_phys_fpl_part2.intfb.h"
#include "mf_phys_mocon.intfb.h"
#include "mf_phys_precips.intfb.h"
#include "mf_phys_save_phsurf_part1.intfb.h"
#include "mf_phys_save_phsurf_part2.intfb.h"
#include "mf_phys_transfer.intfb.h"
#include "ppwetpoint.intfb.h"
#include "qngcor.intfb.h"
#include "aplpar_flexdia.intfb.h"
#include "apl_arpege_oceanic_fluxes.intfb.h"
#include "apl_wind_gust.intfb.h"
#include "apl_arpege_shallow_convection_and_turbulence.intfb.h"
#include "apl_arpege_albedo_computation.intfb.h"
#include "apl_arpege_aerosols_for_radiation.intfb.h"
#include "apl_arpege_cloudiness.intfb.h"
#include "apl_arpege_radiation.intfb.h"
#include "apl_arpege_soil_hydro.intfb.h"
#include "apl_arpege_surface.intfb.h"
#include "apl_arpege_deep_convection.intfb.h"
#include "apl_arpege_precipitation.intfb.h"
#include "apl_arpege_hydro_budget.intfb.h"
#include "apl_arpege_surface_update.intfb.h"
#include "apl_arpege_atmosphere_update.intfb.h"
#include "apl_arpege_init.intfb.h"
#include "apl_arpege_init_surfex.intfb.h"
#include "apl_arpege_dprecips.intfb.h"

END SUBROUTINE APL_ARPEGE

