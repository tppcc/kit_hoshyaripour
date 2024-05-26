! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Sparse Hessian Data Structures File
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
! File                 : dynho_HessianSP.f90
! Time                 : Fri Feb 10 12:04:04 2023
! Working directory    : /pfs/data5/home/kit/imk-tro/ii5664/kpp/simple_dynH
! Equation file        : dynho.kpp
! Output root filename : dynho
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE dynho_HessianSP

  PUBLIC
  SAVE


! Hessian Sparse Data
! 

  INTEGER, PARAMETER, DIMENSION(7) :: IHESS_I = (/ &
       1,  1,  2,  2,  2,  3,  3 /)

  INTEGER, PARAMETER, DIMENSION(7) :: IHESS_J = (/ &
       1,  1,  1,  1,  2,  1,  2 /)

  INTEGER, PARAMETER, DIMENSION(7) :: IHESS_K = (/ &
       2,  3,  2,  3,  2,  3,  2 /)


END MODULE dynho_HessianSP

