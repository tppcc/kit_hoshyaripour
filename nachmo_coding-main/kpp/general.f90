PROGRAM KPP_ROOT_Driver
  
  USE KPP_ROOT_Model
  USE KPP_ROOT_Initialize, ONLY: Initialize

      KPP_REAL :: T, DVAL(NSPEC)
      KPP_REAL :: RSTATE(20)
      INTEGER :: i
      INTEGER :: ICNTRL(20)
!~~~> Initialization 

      STEPMIN = 0.0d0
      STEPMAX = 0.0d0

      DO i=1,NVAR
        RTOL(i) = 1.0d-4
        ATOL(i) = 1.0d-3
      END DO
     
      CALL Initialize()
      CALL InitSaveData()

!~~~> Time loop
      T = TSTART
kron: DO WHILE (T < TEND)

        TIME = T
        CALL GetMass( C, DVAL )
        WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,       &
                   ( TRIM(SPC_NAMES(MONITOR(i))),           &
                     C(MONITOR(i))/CFACTOR, i=1,NMONITOR ), &
                   ( TRIM(SMASS(i)), DVAL(i)/CFACTOR, i=1,NMASS )
        CALL SaveData()
        CALL Update_SUN() 
        CALL Update_RCONST()
        ICNTRL = 0
        ICNTRL(3) = 2
        CALL INTEGRATE( TIN = T, TOUT = T+DT, RSTATUS_U = RSTATE, &
        ICNTRL_U = ICNTRL )
        T = RSTATE(1)

      END DO kron
!~~~> End Time loop
      
      CALL GetMass( C, DVAL )
      WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,     &
               ( TRIM(SPC_NAMES(MONITOR(i))),           &
                 C(MONITOR(i))/CFACTOR, i=1,NMONITOR ), &
               ( TRIM(SMASS(i)), DVAL(i)/CFACTOR, i=1,NMASS )
      TIME = T
      CALL SaveData()
      CALL CloseSaveData()

991   FORMAT(F6.1,'%. T=',E9.3,2X,200(A,'=',E11.4,'; '))
      
END PROGRAM KPP_ROOT_Driver

