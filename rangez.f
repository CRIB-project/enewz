C======================================================================C
	PROGRAM	RANGEZ
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

	integer f1
C----------------------------------------------------------------------C

	CALL SNKEMATTER('MATTER', MAX_NKE, NKE, Z2, RE, RHO, MW)

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

c$$$	WRITE(*,*)
c$$$	WRITE(*,'(1X, ''Input : Z, Mass(amu), Energy(MeV/amu) > '')')
c$$$	READ(*,*) Z1, M1, E
      
	WRITE(*,*)

	WRITE(*,*) '------------------------------',
     &             ' MATTER LIST ', 
     &             '------------------------------'
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
     &             '-------------', 
     &             '------------------------------'

	WRITE(*,'(1X, ''Input : Matter > '')')
	READ(*,*) MATTER

	CALL SNKEMATTER(MATTER, MAX_NKE, NKE, Z2, RE, RHO, MW)

	IF (NKE.EQ.0) THEN
	   DO K=1, MAX_NKE
	      WRITE(*,'(3X,A,A)')
     &	              'Input : Z, Number of element in one molecule',
     &	              ' (END = -1 -1.)'
	      WRITE(*,'(5X, ''> '')') 
	      READ(*,*) Z2(K), RE(K)
	      IF ((Z2(K).EQ.-1).AND.(RE(K).EQ.-1.)) THEN
		 NKE = K-1
		 GO TO 100
	      END IF
	   END DO

	   WRITE(*,'(1X, ''Too many elements.'')')
	   STOP
	   
 100	   CONTINUE
            
	END IF

	RANGE = E2RANGEZ(Z1,M1,NKE,Z2,RE,E)

	WRITE(*,*) 
	IF (RHO.NE.-1) THEN
	   WRITE(*,'(1X,F12.5,'' mg/cm^2 ( '',F12.5,'' mm )'')')
     &	        RANGE, RANGE/1000./RHO*10.
	ELSE
	   WRITE(*,'(1X,F12.5,'' mg/cm^2'')') RANGE
	END IF
	STOP
	END

C======================================================================C








