C======================================================================C
c	PROGRAM	RANGEZ
	subroutine rangezsub(z1,m1,E,matter,range)
C======================================================================C

	IMPLICIT NONE

	INTEGER MAX_NKE
	PARAMETER(MAX_NKE=10)

	INTEGER Z1
	REAL M1, E

	CHARACTER*32 MATTER
	INTEGER NKE, Z2(MAX_NKE)
	REAL RE(MAX_NKE), RHO, MW

	REAL RANGE

	CHARACTER*14 COLUMN_MATTER(5)

	INTEGER K, L

	REAL E2RANGEZ

	INCLUDE 'SNKE_MATTER.INC'

C----------------------------------------------------------------------C

	CALL SNKEMATTER('MATTER', MAX_NKE, NKE, Z2, RE, RHO, MW)

c	WRITE(*,*)
c	WRITE(*,'(1X, ''Input : Z, Mass(amu), Energy(MeV/amu) > '')')
c	READ(*,*) Z1, M1, E
      
c	WRITE(*,*)

c	WRITE(*,*) '------------------------------',
c     &             ' MATTER LIST ', 
c     &             '------------------------------'
	DO K=1, NUMBER_OF_TABLE/5+1
	   DO L=1,5
	      IF ( ((K-1)*5+L) .LE. NUMBER_OF_TABLE ) THEN
		 COLUMN_MATTER(L) = T_MATTER(((K-1)*5+L))
	      ELSE
		 COLUMN_MATTER(L) = ' '
	      END IF
	   END DO
c	   WRITE(*,'(5(A, '' ''))') COLUMN_MATTER
	END DO
c	WRITE(*,*) '------------------------------',
c     &             '-------------', 
c     &             '------------------------------'

c	WRITE(*,'(1X, ''Input : Matter > '')')
c	READ(*,*) MATTER

	CALL SNKEMATTER(MATTER, MAX_NKE, NKE, Z2, RE, RHO, MW)

	IF (NKE.EQ.0) THEN
	   DO K=1, MAX_NKE
c	      WRITE(*,'(3X,A,A)')
c     &	              'Input : Z, Number of element in one molecule',
c     &	              ' (END = -1 -1.)'
c	      WRITE(*,'(5X, ''> '')') 
	      READ(*,*) Z2(K), RE(K)
	      IF ((Z2(K).EQ.-1).AND.(RE(K).EQ.-1.)) THEN
		 NKE = K-1
		 GO TO 100
	      END IF
	   END DO

c	   WRITE(*,'(1X, ''Too many elements.'')')
	   STOP
	   
 100	   CONTINUE
            
	END IF

	RANGE = E2RANGEZ(Z1,M1,NKE,Z2,RE,E)

c	WRITE(*,*) 
c	IF (RHO.NE.-1) THEN
c	   WRITE(*,'(1X,F12.5,'' mg/cm^2 ( '',F12.5,'' mm )'')')
c     &	        RANGE, RANGE/1000./RHO*10.
c	ELSE
c	   WRITE(*,'(1X,F12.5,'' mg/cm^2'')') RANGE
c	END IF
c	STOP
	return
	END

C======================================================================C








