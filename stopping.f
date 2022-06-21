C======================================================================C
      PROGRAM STOPPING
C
C     J. F. Ziegler, J. P. Biersack and U. Littmark
C     "The Stopping and Range of Ions in Solids"
C     Pergamon Press, New York (1985)
C
C======================================================================C

      IMPLICIT NONE

      CHARACTER*30 FILENAME
      
      INTEGER Z1,Z2
      INTEGER NUMB, UNITS

      REAL M1, MM1, RHO, RHOA, LFCTR, VFERMI
      REAL EE(400), SE(400), SN(400)

      REAL E, SEE, SNN

      REAL X, XX(8)

      INTEGER I, A

C----------------------------------------------------------------------C

 100  WRITE(6,110)
 110  FORMAT(1X,
     &     '***************************',
     &     '***********************************')

      WRITE(6,120)
 120  FORMAT(1X,
     &     '**        ',
     &     'Z.B.L.  STOPPING  CROSS-SECTIONS  FOR  SOLIDS',
     &     '     **')

      WRITE(6,130)
 130  FORMAT(1X,
     &     '** FROM   ',
     &     '"THE STOPPING AND RANGE OF IONS IN MATTER"',
     &     '        **')

      WRITE(6,140)
 140  FORMAT(1X,
     &     '**        ',
     &     'J.F. ZIEGLER, J.P. BIERSACK AND U. LITTMARK',
     &     '       **')

      WRITE(6,110)

      WRITE(6,*) ' '
      WRITE(6,150)
 150  FORMAT(' INPUT: ION ATOMIC NUMBER AND  MASS (AMU)')
      WRITE(6,160)
 160  FORMAT('        (INPUT MASS=0 FOR NORMAL ION MASS)')
      WRITE(6,170)
 170  FORMAT('        EXAMPLE FOR SILICON IONS: 14,27.977  OR  14,0')
      READ(5,*) Z1, M1

      CALL SCOEF(Z1,X,MM1,X,X,X,X,LFCTR,XX)
      IF (M1.EQ.0.) M1 = MM1
      WRITE(6,180) M1
 180  FORMAT(' MASS OF ION IS = ',F8.4,'(AMU)')

      WRITE(6,*) ' '
      WRITE(6,190)
 190  FORMAT(' INPUT: TARGET ATOMIC NUMBER')
      WRITE(6,200)
 200  FORMAT('        ( EXAMPLE:  79 )')
      READ(5,*) Z2
      CALL SCOEF(Z2,X,X,X,RHO,RHOA,VFERMI,X,XX)
      RHOA = RHOA / 1.E22

      WRITE(6,*) ' '
      WRITE(6,210)
 210  FORMAT(' INPUT: LIST OF ION ENERGIES (KEV)')
      WRITE(6,220)
 220  FORMAT('       (ONE ENERGY PER LINE, END WITH A ZERO)')
      DO I=1, 400
         READ(5,*) E
         EE(I) = E
         IF (EE(I).EQ.0.) GOTO 240
      END DO

 240  NUMB = I-1

      WRITE(6,*) ' '
      WRITE(6,250)
 250  FORMAT(' STOPPING CROSS-SECTIONS CAN BE IN FOUR DIFFERENT UNITS:')
      WRITE(6,260)
 260  FORMAT(' 1= EV/(1E15 ATOMS/CM2), 2= MEV/(MG/CM2),',
     &     ' 3= EV/ANGSTROM, 4= L.S.S. UNITS')
      WRITE(6,270)
 270  FORMAT(' INPUT CROSS-SECTION UNITS:')
      READ(5,*) UNITS
      WRITE(6,110)

C----------------------------------------------------------------------C
C START CALCULATION 
C----------------------------------------------------------------------C

      WRITE(6,280) Z1, M1, Z2
 280  FORMAT(' Z1 =',I4, ';   M1 =',F9.4, ';   Z2 =',I4)
      
      IF (UNITS.EQ.1) WRITE(6,290)
      IF (UNITS.EQ.2) WRITE(6,300)
      IF (UNITS.EQ.3) THEN
         WRITE(6,310)
         WRITE(6,320) RHO, RHOA
      END IF
      IF (UNITS.EQ.4) WRITE(6,330)

 290  FORMAT(' STOPPING UNITS = EV/(1E15 ATOMS/CM2)')
 300  FORMAT(' STOPPING UNITS = MEV/(MG/CM2)')
 310  FORMAT(' STOPPING UNITS = EV/ANGSTROM')
 320  FORMAT(' (DENBSITY = ',F6.3,' G/CM3 = ',F5.2,'E22 ATOMS/CM3)')
 330  FORMAT(' STOPPING UNITS = L.S.S. REDUCED UNITS')

      WRITE(6,380)
 380  FORMAT(' ')
      WRITE(6,340)
 340  FORMAT('    ENERGY  ELECTRONIC     NUCLEAR')
      WRITE(6,350)
 350  FORMAT('    (KEV)   STOPPING      STOPPING')
      DO I=1, NUMB
         E = EE(I)
         CALL STOP(Z1,M1,Z2,E,SEE,SNN,UNITS)
         SE(I) = SEE
         SN(I) = SNN
         WRITE(6,370) E, SEE, SNN
 370     FORMAT(1X, F9.1, 2F12.5)
       END DO

       WRITE(6,380)
       WRITE(6,390) LFCTR, VFERMI
 390   FORMAT(' ION SCREENING FACTOR =',F4.2,
     &      ',  TARGET FERMI VELOCITY = ',F4.2,' V0')
       WRITE(6,110)

C----------------------------------------------------------------------C
C END CALCULATION
C WRITE ON DISK
C----------------------------------------------------------------------C

       WRITE(6,400)
 400   FORMAT(' WRITE DATA ON DISK ?   0=NO,  1=YES')
       READ(5,*) A

       IF(A.NE.1) GOTO 440
       WRITE(6,410)
 410   FORMAT(' INPUT FILE NAME WITHIN QUOTES ')
       WRITE(6,420)
 420   FORMAT(' ( EXAMPLE: "HE-SI"  OR  "HE-SI.DAT" )')
       READ(*,*) FILENAME
       OPEN(7,FILE=FILENAME,STATUS='NEW')

       WRITE(7,110)
       WRITE(7,120)
       WRITE(7,110)
       WRITE(7,280) Z1,M1,Z2
 
       IF (UNITS.EQ.1) WRITE(7,290)
       IF (UNITS.EQ.2) WRITE(7,300)
       IF (UNITS.EQ.3) THEN
          WRITE(7,310)
          WRITE(7,320) RHO, RHOA
       END IF
       IF (UNITS.EQ.4) WRITE(7,330)
       
       WRITE(7,380)
       WRITE(7,340)
       WRITE(7,350)

       DO I=1,NUMB
          E=EE(I)
          SEE=SE(I)
          SNN=SN(I)
          WRITE(7,370) E, SEE, SNN
       END DO

       WRITE(7,380)
       WRITE(7,390) LFCTR,VFERMI

C----------------------------------------------------------------------C
C LOOP TO BEGINNING FOR OTHER STOPPINGS
C----------------------------------------------------------------------C

 440   WRITE(6,110)
       WRITE(6,450)
 450   FORMAT(' CALCULATE ANOTHER STOPPING CROSS-SECTION ?',
     &      '  0=NO, 1=YES')
       READ(5,*) A
       IF (A.EQ.1) GOTO 100

      STOP
      END

C======================================================================C
