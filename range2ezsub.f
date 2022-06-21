C======================================================================C
c	PROGRAM	RANGE2EZ
	subroutine range2ezsub(z1,m1,range,matter, E)
C======================================================================C

	IMPLICIT NONE

	INTEGER MAX_NKE
	PARAMETER(MAX_NKE=10)

	INTEGER Z1
	REAL M1,RANGE

	CHARACTER*32 MATTER
	INTEGER NKE, Z2(MAX_NKE)
	REAL RE(MAX_NKE), RHO, MW

	REAL E

	CHARACTER*14 COLUMN_MATTER(5)

	INTEGER K, L

	REAL R2ENERGYZ

	INCLUDE 'SNKE_MATTER.INC'

C----------------------------------------------------------------------C

	CALL SNKEMATTER('MATTER', MAX_NKE, NKE, Z2, RE, RHO, MW)

c$$$	WRITE(*,*)
c$$$	WRITE(*,'(1X, ''Input : Z, Mass(amu), Range(mg/cm^2) > '')')
c$$$	READ(*,*) Z1,M1,RANGE
c$$$
c$$$	WRITE(*,*)
c$$$
c$$$        WRITE(*,*) '------------------------------',
c$$$     &             ' MATTER LIST ', 
c$$$     &             '------------------------------'
        DO K=1, NUMBER_OF_TABLE/5+1
           DO L=1,5
              IF ( ((K-1)*5+L) .LE. NUMBER_OF_TABLE ) THEN
                 COLUMN_MATTER(L) = T_MATTER(((K-1)*5+L))
              ELSE
                 COLUMN_MATTER(L) = ' '
              END IF
           END DO
c$$$           WRITE(*,'(5(A, '' ''))') COLUMN_MATTER
        END DO
c$$$        WRITE(*,*) '------------------------------',
c$$$     &             '-------------', 
c$$$     &             '------------------------------'
c$$$
c$$$        WRITE(*,'(1X, ''Input : Matter > '')')
c$$$        READ(*,*) MATTER

        CALL SNKEMATTER(MATTER, MAX_NKE, NKE, Z2, RE, RHO, MW)

        IF (NKE.EQ.0) THEN
           DO K=1, MAX_NKE
              WRITE(*,'(3X,A,A)')
     &                'Input : Z, Number of element in one molecule',
     &                ' (END = -1 -1.)'
              WRITE(*,'(5X, ''> '')') 
              READ(*,*) Z2(K), RE(K)
              IF ((Z2(K).EQ.-1).AND.(RE(K).EQ.-1.)) THEN
                 NKE = K-1
                 GO TO 100
              END IF
           END DO

           WRITE(*,'(1X, ''Too many elements.'')')
           STOP
           
 100       CONTINUE

	END IF

	E = R2ENERGYZ(Z1,M1,NKE,Z2,RE,RANGE)

c$$$	WRITE(*,*) 
c$$$	WRITE(*,'(1X, F10.5,'' MeV/amu'')') E

c	STOP

	return
	END

C======================================================================C
