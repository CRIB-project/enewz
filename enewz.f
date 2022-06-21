C======================================================================C
      PROGRAM ENEWZ
C======================================================================C
C     CALCULATE ENERGY LOSS IN MATTER
C       USING ZIEGLER'S SUBROUTINE
C======================================================================C

      IMPLICIT NONE
      







      INTEGER MAX_NKE
      PARAMETER(MAX_NKE=10)
      INTEGER MAX_NM
      PARAMETER(MAX_NM=100)

      INTEGER Z1
      REAL M1, E

      INTEGER NM
      CHARACTER*32 MATTER(MAX_NM)
      INTEGER NKE(MAX_NM), Z2(MAX_NKE, MAX_NM)
      REAL RE(MAX_NKE, MAX_NM), RHO(MAX_NM), MW(MAX_NM)
      REAL PRESSURE, TEMPERATURE, THICK(MAX_NM)
      INTEGER UNIT_PRESSURE, UNIT_TEMPERATURE, UNIT_THICK
      CHARACTER*20 C_UNIT

      REAL ENEW(0:MAX_NM)

      CHARACTER*14 COLUMN_MATTER(5)

      INTEGER Z2_TMP(MAX_NKE)
      REAL RE_TMP(MAX_NKE)
      REAL M2

      INTEGER J, K, L
      REAL DUMMY, PCOEF(8)

      REAL ENERGYNEWZ

      INCLUDE 'SNKE_MATTER.INC'
      
      integer f1





C----------------------------------------------------------------------C

      CALL SNKEMATTER('MATTER', MAX_NKE, NKE(1), Z2_TMP, RE_TMP,
     &     RHO(1), MW(1))

      write(*,*)
      write(*,'(1X, ''1) MeV/amu or 2) MeV? > '')')
      read(*,*) f1

      if(f1 .eq. 1) then
         WRITE(*,*)
         WRITE(*,'(1X, ''Input : Z, Mass(amu), Energy(MeV/amu) > '')')
         READ(*,*) Z1, M1, E
      elseif(f1 .eq. 2) then
         WRITE(*,*)
         WRITE(*,'(1X, ''Input : Z, Mass(amu), Energy(MeV) > '')')
         READ(*,*) Z1, M1, E
         E = E/M1
      else
         write(*,*) "Start over again"
         stop
      endif

      WRITE(*,*)
      WRITE(*,'(1X ,''Input : Number of layer > '')')
      READ(*,*) NM
      IF (NM.GT.MAX_NM) THEN
         WRITE(*,*) 
     &        'ERROR : TOO LARGE ''NM'' ; NM = ',NM,' > ',MAX_NM
         STOP
      END IF

      DO J=1, NM
         
         WRITE(*,*)

         WRITE(*,'(1X, ''Input : Matter of layer#'',I2)') J

         WRITE(*,*) '------------------------------',
     &        ' MATTER LIST ', 
     &        '------------------------------'
         DO K=1, NUMBER_OF_TABLE/5+1
            DO L=1,5
               IF ( ((K-1)*5+L) .LE. NUMBER_OF_TABLE ) THEN
                  COLUMN_MATTER(L) = T_MATTER(((K-1)*5+L))
               ELSE
                  COLUMN_MATTER(L) = ' '
               END IF
            END DO
            WRITE(*,'(5(A, '' ''))') COLUMN_MATTER
         END DO
         WRITE(*,*) '------------------------------',
     &        '-------------', 
     &        '------------------------------'

         WRITE(*,'(1X, ''Input : Matter > '')')
         READ(*,*) MATTER(J)

         CALL SNKEMATTER(MATTER(J), MAX_NKE, NKE(J), Z2_TMP, RE_TMP,
     &        RHO(J), MW(J))

         IF (NKE(J).EQ.0) THEN
            DO K=1, MAX_NKE
               WRITE(*,'(3X,A,A)')
     &              'Input : Z, Number of element in one molecule',
     &              ' (END = -1 -1.)'
               WRITE(*,'(5X, ''> '')') 
               READ(*,*) Z2(K,J), RE(K,J)
               IF ((Z2(K,J).EQ.-1).AND.(RE(K,J).EQ.-1.)) THEN
                  NKE(J) = K-1
                  GO TO 100
               END IF
            END DO

            WRITE(*,'(1X, ''Too many elements.'')')
            STOP

 100        CONTINUE
            
            WRITE(*,*) 'Input : Rho(g/cm**3) ( -1. = GAS ) > '
            READ(*,*) RHO(J)

            MW(J) = 0.
            DO K=1, NKE(J)
               CALL SCOEF(Z2(K,J), DUMMY, DUMMY, M2, DUMMY, DUMMY,
     &              DUMMY, DUMMY, PCOEF)
               MW(J) = MW(J) + M2*RE(K,J)
            END DO
         ELSE 
            DO K=1,NKE(J)
               Z2(K,J) = Z2_TMP(K)
               RE(K,J) = RE_TMP(K)
            END DO
         END IF

         WRITE(*,*)
         IF (RHO(J) .EQ. -1) THEN
            WRITE(*,*) 'Input : Unit ( [1] torr, [2] mbar [3] atm ) > '
            READ(*,*) UNIT_PRESSURE
            IF ( UNIT_PRESSURE .EQ. 1 ) THEN
               C_UNIT = 'torr'
            ELSE IF ( UNIT_PRESSURE .EQ. 2 ) THEN
               C_UNIT = 'mbar'
            ELSE IF ( UNIT_PRESSURE .EQ. 3 ) THEN
               C_UNIT = 'atm'
            END IF
               WRITE(*,'(1X,A,A4,A)')
     &              'Input : Pressure ( ', C_UNIT, ' ) of gas > '
            READ(*,*) PRESSURE
                        
            WRITE(*,*) 'Input : Temperature (K) > '
            READ(*,*) TEMPERATURE
            
            CALL SRHOGAS( PRESSURE, TEMPERATURE, 
     &           UNIT_PRESSURE, 1, MW(J), RHO(J) )
            
         END IF
         
         WRITE(*,*)
         WRITE(*,*) 'Input : Unit ( [1] mm, [2] mg/cm**2 ) > '
         READ(*,*) UNIT_THICK
         IF (UNIT_THICK.EQ.1) THEN
            C_UNIT = 'mm'
         ELSE
            C_UNIT = 'mg/cm**2'
         END IF
         WRITE(*,'(1X,A,A8,A,I2,A)')
     &        'Input : Thickness ( ', C_UNIT, ' ) of layer#',J,' > '
         READ(*,*) THICK(J)

         IF (UNIT_THICK.EQ.1) THEN
            THICK(J) = THICK(J) / 10. * RHO(J) * 1000.
         END IF
         
      END DO
      
      ENEW(0)=E
      
      DO J=1,NM
         DO K=1,NKE(J)
            Z2_TMP(K) = Z2(K,J)
            RE_TMP(K) = RE(K,J)
         END DO
         ENEW(J) = ENERGYNEWZ(Z1,M1,NKE(J),
     &        Z2_TMP,RE_TMP,ENEW(J-1),THICK(J))
      END DO
      
c      WRITE(*,*) 
c      WRITE(*,'(1X,F10.5'' MeV/amu ('',F10.5'' MeV)'')')
c     &     ENEW(0), ENEW(0)*M1


c      DO J=1,NM
c         WRITE(*,'(1X,''--- MATTER : '',A,'' '',F8.3,'' mg/cm^2 ---'')') 
c     &        MATTER(J), THICK(J)
c         WRITE(*,'(1X,F10.5,'' MeV/amu ('',F10.5,'' MeV)'')')
c     &        ENEW(J), ENEW(J)*M1
c      END DO

      WRITE(*,*) 
c      WRITE(*,'(1X,F12.7,'' MeV/amu ('',F12.7,'' MeV)'')')
      WRITE(*,'(1X,F10.5,'' MeV/amu ('',F10.5,'' MeV)'')')
     &     ENEW(0), ENEW(0)*M1

      DO J=1,NM
c         WRITE(*,'(1X,''--- MATTER : '',A,'' '',F9.6,'' mg/cm^2 ---'')') 
         WRITE(*,*) 
         WRITE(*,'(2X,"MATTER: ",A," ",F9.6," mg/cm^2")') 
     &        MATTER(J), THICK(J)
         WRITE(*,'(2X,"E LOSS: ",F10.5," MeV/amu (",F10.5," MeV)")')
     &        ENEW(J-1)-ENEW(J), (ENEW(J-1)-ENEW(J))*M1
         WRITE(*,'(" -----------------------------------------------")')
c         WRITE(*,'(1X,F12.7,'' MeV/amu ('',F12.7,'' MeV)'')')
         WRITE(*,'(1X,F10.5,'' MeV/amu ('',F10.5,'' MeV)'')')
     &        ENEW(J), ENEW(J)*M1
c         write(*,*)
      END DO

      STOP
      END
      
C======================================================================C

