! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Utility Data Module File
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
! File                 : dynho_Monitor.f90
! Time                 : Fri Feb 10 12:04:04 2023
! Working directory    : /pfs/data5/home/kit/imk-tro/ii5664/kpp/simple_dynH
! Equation file        : dynho.kpp
! Output root filename : dynho
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE dynho_Monitor


  CHARACTER(LEN=32), PARAMETER, DIMENSION(5) :: SPC_NAMES = (/ &
     'OH                              ','HO2                             ','H2O2                            ', &
     'H2O                             ','O2                              ' /)

  INTEGER, PARAMETER, DIMENSION(3) :: LOOKAT = (/ &
       1,  2,  3 /)

  INTEGER, PARAMETER, DIMENSION(3) :: MONITOR = (/ &
       1,  2,  3 /)

  CHARACTER(LEN=32), DIMENSION(1) :: SMASS
  CHARACTER(LEN=100), PARAMETER, DIMENSION(4) :: EQN_NAMES = (/ &
     '     H2O2 --> 2 OH                                                                                  ', &
     'OH + H2O2 --> HO2 + H2O                                                                             ', &
     '    2 HO2 --> H2O2 + O2                                                                             ', &
     ' OH + HO2 --> H2O + O2                                                                              ' /)

! INLINED global variables

! End INLINED global variables


END MODULE dynho_Monitor
