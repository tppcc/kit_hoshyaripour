! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! The ODE Function of Chemical Model File
! 
! Generated by KPP-2.2.1_rs5 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : dynho_Function.f90
! Time                 : Fri Feb 10 12:04:04 2023
! Working directory    : /pfs/data5/home/kit/imk-tro/ii5664/kpp/simple_dynH
! Equation file        : dynho.kpp
! Output root filename : dynho
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE dynho_Function

  USE dynho_Parameters
  IMPLICIT NONE

! A - Rate for each equation
  REAL(kind=dp) :: A(NREACT)

CONTAINS


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Fun - time derivatives of variables - Agregate form
!   Arguments :
!      V         - Concentrations of variable species (local)
!      F         - Concentrations of fixed species (local)
!      RCT       - Rate constants (local)
!      Vdot      - Time derivative of variable species concentrations
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Fun ( V, F, RCT, Vdot )

! V - Concentrations of variable species (local)
  REAL(kind=dp) :: V(NVAR)
! F - Concentrations of fixed species (local)
  REAL(kind=dp) :: F(NFIX)
! RCT - Rate constants (local)
  REAL(kind=dp) :: RCT(NREACT)
! Vdot - Time derivative of variable species concentrations
  REAL(kind=dp) :: Vdot(NVAR)


! Computation of equation rates
  A(1) = RCT(1)*V(3)
  A(2) = 1.8e-12*V(1)*V(3)
  A(3) = 1.5e-12*V(2)*V(2)
  A(4) = 1.1e-10*V(1)*V(2)

! Aggregate function
  Vdot(1) = 2*A(1)-A(2)-A(4)
  Vdot(2) = A(2)-2*A(3)-A(4)
  Vdot(3) = -A(1)-A(2)+A(3)
      
END SUBROUTINE Fun

! End of Fun function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE dynho_Function
