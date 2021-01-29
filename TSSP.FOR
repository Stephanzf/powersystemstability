C
C     ****************************************************************
C     *      Transient Stability Simulation Package(TSSP)            *
C     *                     COORDINATOR                              *
C     ****************************************************************
C        
C       %Stephan Z. Ao, 3-06-93
C       Copyright(c) 1993-1996 by 
C       The Universal Machine, Inc.        
C       %Revision: 1.2%  %14-10-95
C
C       Questions should be directed to
C       Stephan Z. Ao
C       3A, 9011 - 112 St.
C       Edmonton, AB
C       CANADA  T6G 2C5
C       zefeng@nyquist.ee.ualberta.ca 
C  -------------------------------------------------------------------
C        
	INCLUDE 'CMM.DM'    
C
C   variables declaration block   
C       
	double precision     TIME , TSTEP, TPRINT, TOLD, TFIN, TEND

	INTEGER              NSTEP, NPRINT , IPSSTYPE

C
C   open input and output files    
C
	OPEN( ISIMUO, FILE = 'SIMU.OUT'  )

C
C   switch variable controlling the ENGINE
C
	SWII = 0
	
	CALL ENGINE(SWII)
C
C   input simulation time length in second from keyboard
C
	WRITE( * , '(A40)')  ' Please enter the simulation time(s) ?'
4       READ ( * , * , ERR = 5 )     TEND
	WRITE( * , '(T6, F8.4/ )' ) TEND
	GOTO   6
5       WRITE( * , '(A35)' ) ' Error, try again, please! '
	GOTO   4
C
C   input generator,avr,turbine and pss data from mach.inp file
C
C   If a problem is met in reading the file, please check the
C   output file mach.out the end of which indicates where the 
C   problem occurs in the data file mach.inp.
C        
6       continue

	CALL ENGINE(SWII)
	CALL ENGINE(SWII)
C     
C   new network structure introduced if any    
C

200     CONTINUE
	
	TOLD = TFIN

C
C   read new network admittance matrix
C
	DO I=1, NGEN
	READ ( IPQLFO, 950 ,END = 300 ) (Y(I,J), J=1,NGEN)
	WRITE( IMAINO, 950 )            (Y(I,J), J=1,NGEN)
	END DO
950     FORMAT(1X, 15(2F14.6, 2X) )

	READ ( IPQLFO, 960 ) TFIN
	IF  ( TFIN.EQ.50.0 ) TFIN = TEND
960     FORMAT( 1X, F14.6 )

	CALL internalymatrix
C
C   integration loop for each time step
C

100     CONTINUE

C
C   display simulation progress on screen        
C

	IF ( ( TIME.LE.(TEND*.30+TSTEP) ).AND. 
     *       ( TIME.GE.(TEND*.30-TSTEP) )     )
     *            WRITE(*,'(A30)' )  '  30 % completed !'
	IF ( ( TIME.LE.(TEND*.60+TSTEP) ).AND. 
     *       ( TIME.GE.(TEND*.60-TSTEP) )     )
     *            WRITE(*,'(A30)' )  '  60 % completed !'
	IF ( ( TIME.LE.(TEND*.90+TSTEP) ).AND. 
     *       ( TIME.GE.(TEND*.90-TSTEP) )     )
     *            WRITE(*,'(A30)' )  '  90 % completed !'
	IF ( TIME.GE.TEND ) GOTO 300
C
C   interface with network eqns to modify terminal conditions 
C        
	CALL solventwkarmature
	CALL ENGINE(SWII)
C
C   output of simulation results   
C
	IF ( NSTEP.EQ.0 )  CALL outputsimulation
	IF ( NPRINT * TSTEP.LT.(TPRINT-.0001) ) GOTO 125
	CALL outputsimulation

	NPRINT=0
125     CONTINUE

C
C   integration of all first-order differential eqns 
C
	CALL calstatevariable
	NSTEP  = NSTEP + 1
	TIME   = NSTEP * TSTEP
	NPRINT = NPRINT + 1
C     
C   check whether calculation is finished within 
C   the same network configuration
C
	IF ( TIME.LT.TFIN )  GOTO 100
C
C   back to new network condition 
C        
	GO TO 200
    
300     CONTINUE

	write(*, '(A40/)' ) ' Results are stored in file:simu.out.'
	write(*, '(A40/)' ) ' Plot utility WPLOT, SORT can be used '
	write(*, '(A40/)' ) ' to display and sort interested variables.'
	
	STOP
	END
