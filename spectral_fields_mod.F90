MODULE SPECTRAL_FIELDS_MOD

!   Author.
!   -------
!     Yannick Tremolet
!
!   Modifications.
!   --------------
!     Original    02-03-05
!     Y.Tremolet  02-07-25 Added operators.
!     C.Fischer   03-03-03 Aladin spectral treatment added.
!      N.B.1: the mean wind spec%sp1d is treated in assign_sp_sp and in
!             assign_scalar_sp but not in assign_ar_sp and assign_sp_ar.
!             therefore, whenever a fortran array is copied to a spectral_field
!             type in Aladin, one should take care of the mean wind externally.
!             see for example get_cv_init and set_cv_init in control_vectors.F90.
!     Y.Tremolet  11-Mar-2004 Type definition moved to SPECTRAL_FIELDS_DATA.
!     Y.Tremolet  11-Mar-2004 SPECTRAL_FIELDS_PARA_MOD for distributed operators.
!     Y.Tremolet  22-Jul-2004 SPECTRAL_FIELDS_OPER_MOD for basic operators.
!     M.Fisher    23-Aug-2004 Added RANDOM_SPECTRAL_FIELD
!     C. Fischer  20-May-2005 RANDOM_SPECTRAL_FIELD updated for Aladin
!     M. Fisher   7-March-2012 Use DEALLOCATE_IF_ASSOCIATED
!     R. El Khatib 08-Jul-2014 Optional argument kpsurf to create_spec, mandatory to copy_spec
!     M. Chrust   3-Jan-2020 Add public SELF_AXPY
! ------------------------------------------------------------------

USE PARKIND1, ONLY: JPIM, JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
USE GEOMETRY_MOD, ONLY: GEOMETRY
USE SPECTRAL_VARIABLES_MOD, ONLY: SPECTRAL_VARIABLES
USE SPECTRAL_FIELDS_DATA, ONLY: SPECTRAL_FIELD, &
                              & NPRTRV, NPRTRW, MYSETV, MYSETW, LELAM, NULOUT, &
                              & NGRBVO,NGRBD,NGRBT,NGRBQ,NGRBO3,NGRBLNSP, NGRBZ, &
                              & NGRBCLWC, NGRBCIWC, NGRB118, NGRB119, NGRBNHX

IMPLICIT NONE

REAL(KIND=JPRB), TARGET :: SPECTRAL_DUMMY2D (1), SPECTRAL_DUMMY3D (1,1), SPECTRAL_DUMMY4D (1,1,1)
SAVE

! ------------------------------------------------------------------

INTERFACE ALLOCATE_SPEC
MODULE PROCEDURE CREATE_SPEC, CREATE_COPY, ALLOC_SPEC
END INTERFACE

! ------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------

SUBROUTINE SETUP_SPEC(KPRTRV,KPRTRW,KSETV,KSETW,LDLAM,KULOUT, &
                    & KGRBVO,KGRBD,KGRBT,KGRBQ,KGRBO3,KGRBLNSP,KGRBZ, &
                    & KGRBCLWC,KGRBCIWC,KGRB118,KGRB119,KGRBNHX)




INTEGER(KIND=JPIM), INTENT(IN) :: KPRTRV,KPRTRW,KSETV,KSETW, &
              & KULOUT,KGRBVO,KGRBD,KGRBT,KGRBQ,KGRBO3,KGRBLNSP,KGRBZ, &
              & KGRBCLWC,KGRBCIWC,KGRB118,KGRB119,KGRBNHX
LOGICAL, INTENT(IN) :: LDLAM

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE






















END SUBROUTINE SETUP_SPEC

! ------------------------------------------------------------------

SUBROUTINE ALLOC_SPEC(YDSP, YDGEOM, YDVAR)

TYPE(SPECTRAL_FIELD), TARGET, INTENT(INOUT) :: YDSP
TYPE(GEOMETRY), INTENT(IN) :: YDGEOM
TYPE(SPECTRAL_VARIABLES), INTENT(IN) :: YDVAR
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE






END SUBROUTINE ALLOC_SPEC

! ------------------------------------------------------------------

SUBROUTINE CREATE_SPEC(YDSP, KFLEVL, KFLEVG, KNUMP, KMYMS, KSMAX, KMSMAX, &
 & KALLMS, KPTRMS, KNUMLL, KPTRLL, KPSURF, KS3D, KS2D, KGRIB)


TYPE (SPECTRAL_FIELD), TARGET, INTENT(INOUT) :: YDSP
INTEGER(KIND=JPIM), INTENT(IN) :: KFLEVL, KFLEVG, KNUMP, KMYMS(KNUMP), KSMAX, KMSMAX
INTEGER(KIND=JPIM), INTENT(IN) :: KALLMS(:)
INTEGER(KIND=JPIM), INTENT(IN) :: KPTRMS(NPRTRW)
INTEGER(KIND=JPIM), INTENT(IN) :: KNUMLL(NPRTRV), KPTRLL(NPRTRV), KPSURF(NPRTRV)
INTEGER(KIND=JPIM), INTENT(IN) :: KS3D, KS2D, KGRIB(KS3D+KS2D)

INTEGER(KIND=JPIM) :: IDUM1,IDUM2(0:KSMAX),IDUM3,IDUM4, &
           & IDUM5(NPRTRW+1),IDUM6(0:KSMAX),JP,JM,IMAXMS, &
           & IMDIM,IL,IK,IND,IPOS,IKM,ISC3
INTEGER(KIND=JPIM) :: IPT, ITHER, IGFL, JJ, JPT, JL
INTEGER(KIND=JPIM) :: IKNTMP(0:KMSMAX),IKMTMP(0:KSMAX),ICPL4M(0:KMSMAX)
LOGICAL :: LLMISSING
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

















































































































































  
END SUBROUTINE CREATE_SPEC

! ------------------------------------------------------------------

SUBROUTINE CREATE_COPY(YDSELF, YDOTHER)

TYPE(SPECTRAL_FIELD), INTENT(INOUT) :: YDSELF
TYPE(SPECTRAL_FIELD), INTENT(IN)    :: YDOTHER
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE







END SUBROUTINE CREATE_COPY

! ------------------------------------------------------------------

LOGICAL FUNCTION ALLOCATED_SPEC (YDSP)
  TYPE (SPECTRAL_FIELD), INTENT(IN) :: YDSP
  
END FUNCTION ALLOCATED_SPEC

! ------------------------------------------------------------------

SUBROUTINE DEALLOCATE_SPEC(YDSP)

TYPE (SPECTRAL_FIELD), INTENT(INOUT) :: YDSP
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE












































END SUBROUTINE DEALLOCATE_SPEC

! ------------------------------------------------------------------

SUBROUTINE INQ_SPEC(YDSP, KFLEVL, KFLEVG, KNUMP, KMYMS, KSMAX, KMSMAX, &
 & KALLMS, KPTRMS, KNUMLL, KPTRLL, KPSURF, KS3D, KS2D)

USE PARKIND1, ONLY: JPIM, JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
USE SPECTRAL_FIELDS_DATA, ONLY: SPECTRAL_FIELD, NPRTRV, NPRTRW, LELAM, MYSETV, MYSETW




TYPE (SPECTRAL_FIELD), INTENT(OUT) :: YDSP
INTEGER(KIND=JPIM), INTENT(IN) :: KFLEVL, KFLEVG, KNUMP, KMYMS(KNUMP), KSMAX, KMSMAX
INTEGER(KIND=JPIM), INTENT(IN) :: KALLMS(:)
INTEGER(KIND=JPIM), INTENT(IN) :: KPTRMS(NPRTRW)
INTEGER(KIND=JPIM), INTENT(IN) :: KNUMLL(NPRTRV), KPTRLL(NPRTRV), KPSURF(NPRTRV)
INTEGER(KIND=JPIM), INTENT(IN) :: KS3D, KS2D

INTEGER(KIND=JPIM) :: IDUM1,IDUM2(0:KSMAX),IDUM3,IDUM4,&
           & IDUM5(NPRTRW+1),IDUM6(0:KSMAX),JP,JM,IMAXMS,&
           & IMDIM,IL,IK,IND,IPOS,IKM,JL
INTEGER(KIND=JPIM) :: IKNTMP(0:KMSMAX),IKMTMP(0:KSMAX),ICPL4M(0:KMSMAX)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE























































END SUBROUTINE INQ_SPEC

! ------------------------------------------------------------------

SUBROUTINE RANDOM_SPECTRAL_FIELD (SPEC,KSEED)









USE RANDOM_NUMBERS_MIX

TYPE (SPECTRAL_FIELD), INTENT(INOUT) :: SPEC
INTEGER(KIND=JPIM), INTENT(INOUT) :: KSEED

INTEGER(KIND=JPIM) :: II,IM,IOFF,IL,JF,JM,JS,JN,JIR,JL,LEN,ISEED
REAL(KIND=JPRB), ALLOCATABLE :: ZTMP(:)
TYPE(RANDOMNUMBERSTREAM), ALLOCATABLE :: YL_RANDOM_STREAM(:)


REAL(KIND=JPHOOK) :: ZHOOK_HANDLE















END SUBROUTINE RANDOM_SPECTRAL_FIELD

! ------------------------------------------------------------------

END MODULE SPECTRAL_FIELDS_MOD

