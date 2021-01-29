
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~  This routine calculate the state variables  ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE calstatevariable
C
	INCLUDE 'CMM.DM'
C
	double precision   TIME, TSTEP

	IF ( TIME.GT.0.0 ) GO TO 20
	DO I = 1, NGEN
	   DO J = 1, 17
	   SAVE(I,J) = PLUG(I,J)
	   END DO
	END DO
20      CONTINUE
	DO I = 1, NGEN
	   DO J = 1, 17
	   OUT(I,J)  = OUT(I,J) + PLUG(I,J) * TSTEP +
     +              ( PLUG(I,J) - SAVE(I,J) ) * 0.5 * TSTEP
	   SAVE(I,J) = PLUG(I,J)
	   END DO
	END DO
C
	RETURN
	END

C
C   routine printing variables to be ploted
C
	SUBROUTINE outputsimulation
C
	INCLUDE 'CMM.DM'
C
	double precision   TIME, TSTEP
C
	OPEN(ISIMUO,FILE='SIMU.OUT')
C
	IF(TIME.EQ.0.0) THEN
	  WRITE(ISIMUO,20)

20        FORMAT(8X,'time')        
	  WRITE(ISIMUO,30)

30        FORMAT(2X,' I ',1X,4X,' DEL',4X,4X,' OME',4X,3X,'EQ',3X,
     *           3X,'ED',3X,
     *           3X,'VT',3X,5X,'PE',5X,5X,'QE',5X,3X,'EF',3X,
     *           5X,'PM',5X,3X,'CS',3X,3X,'EA',3X,3X,'GA',3X,
     *           3X,'Eq"',2X,3X,'IFD',2X,3X,'VS')
	  WRITE(ISIMUO,*)  
	END IF
	  WRITE(ISIMUO,40)TIME
40        FORMAT(8X,F8.4)
	DO I=1,NGEN
	  WRITE(ISIMUO,50)I,(PVAR(I,J),J=1,15)

50        FORMAT(1X,I3,2E12.4,3F8.4,2E12.4,F8.4,
     *               E12.4,4F8.4, 2E12.5)
	ENDDO
	RETURN
	END
