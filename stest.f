C======================================================================C
      PROGRAM STEST
C
C     J. F. Ziegler, J. P. Biersack and U. Littmark
C     "The Stopping and Range of Ions in Solids"
C     Pergamon Press, New York (1985)
C
C======================================================================C

      IMPLICIT NONE

      INTEGER Z(6), Z1, Z2
      INTEGER UNITS

      REAL M(6), E(4), M1, EE

      REAL SE, SN

      INTEGER I,J,K

C     BELOW ARE THE ATOMIC NUMBERS OF BOTH THE IONS AND TARGETS.
      DATA Z/1, 2, 3, 6, 55, 92/

C     BELOW ARE THE MASSES OF THE IONS.
      DATA M/1.0078, 4.0026, 7.016, 12., 134., 238.04/

C     BELOW ARE THE ION ENERGIES IN KEV/AMU
      DATA E/.1, 1., 25., 100000./

C----------------------------------------------------------------------C

      WRITE(6,10)
 10   FORMAT(' TEST OF #### STOP FORTRAN #### FUNCTIONS')

      WRITE(6,12) Z
 12   FORMAT(' ION/TARGET COMBINATIONS ARE : ',6I5)

      WRITE(6,13) M
 13   FORMAT(' ION MASSES ARE : ',6F9.4)

      WRITE(6,14) E
 14   FORMAT(' ENERGIES (KEV/AMU) = ', 4F8.2)

      WRITE(6,11)
 11   FORMAT(' UNITS ?  1= EV-CM2, 2= MEV-CM2/MG, 3= EV/A, 4=LSS')
      READ(5,*) UNITS

      DO I=1,6
         DO J=1,6
            DO K=1,4
 
               Z1 = Z(I)
               M1 = M(I)
               Z2 = Z(J)
               EE = E(K)*M1
               
               SE = 0.0
               SN = 0.0
               
               WRITE(6,22) Z1, M1, Z2, E(K), EE
 22            FORMAT(' Z1=',I4, '; M1=',F9.4, '; Z2=',I4,
     &              '; E/M=',F10.2,' KEV/AMU; E=',F12.2)
               CALL STOP (Z1, M1, Z2, EE, SE, SN, UNITS)
               WRITE(6,24) SE, SN
 24            FORMAT(' STOPPING: ELECTRONIC = ',F10.5,
     &              ' ; NUCLEAR  = ',F10.5)
               WRITE(6,23)
 23            FORMAT(1X,
     &              '**********************************************')
            END DO
         END DO
      END DO

      STOP
      END

C======================================================================C
