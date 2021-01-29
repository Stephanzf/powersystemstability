C        
C   This subroutine read in the data file mach.inp
C
	SUBROUTINE READIN
C
	INCLUDE 'CMM.DM'
C   
	double precision     TIME , TSTEP, TPRINT
	double precision     E1   , S1   , E2    , S2

	INTEGER              IPSSTYPE     ,  J1 , J2

	OPEN( IMACHI, FILE = 'MACH.INP' )
	OPEN( IMACHO, FILE = 'MACH.OUT' )
C   
	CALL STRING ( IMACHI, IMACHO )
	READ ( IMACHI, * , ERR =90, END =90  ) NGEN, TSTEP, TPRINT
90      WRITE( IMACHO, 100 ) NGEN, TSTEP, TPRINT
c       WRITE( *, 100 )      NGEN, TSTEP, TPRINT 
100     FORMAT(T4, I4, T23, F8.4, T34, F8.4 )
C
C    **********INPUT GENERATOR PARAMETERS***********
C        
	CALL STRING ( IMACHI, IMACHO )
	DO I=1,NGEN
	READ( IMACHI, * ) IG, GTYPE(IG,1)
	J1 = 0
C
C   GENERATOR MODEL E"q, E"d (MODEL 4):
	 IF ( GTYPE(IG,1).EQ.4 ) THEN
	  READ(IMACHI,*) PBASE(IG),H(IG),R(IG),XL(IG)
	  READ(IMACHI,*) XD(IG),XD1(IG),XD11(IG),XQ(IG),XQ11(IG)
	  READ(IMACHI,*) TD1(IG),TD11(IG),TQ11(IG)
	  READ(IMACHI,*) DAMP(IG), S1, S2, J1  
	  IF ( J1.NE. 0)  CALL  COMPUTEAGBG(  S1, S2 ) 
444       C1(IG) = S1
	  C2(IG) = S2
C        
	  WRITE( IMACHO, 490 ) IG, GTYPE( IG,1 )
	  WRITE(IMACHO,540) PBASE(IG),H(IG),R(IG),XL(IG)
	  WRITE(IMACHO,550) XD(IG),XD1(IG),XD11(IG),XQ(IG),XQ11(IG)
	  WRITE(IMACHO,530) TD1(IG),TD11(IG),TQ11(IG)
	  WRITE(IMACHO,530) DAMP(IG),C1(IG),C2(IG)
	  END IF
490       FORMAT(T3, 2I4)
C
C    GENERATOR MODEL E'q, E'd (MODEL 3):
	 IF (GTYPE(IG,1).EQ.3) THEN
	  READ(IMACHI,*) PBASE(IG),H(IG),R(IG),XL(IG)
	  READ(IMACHI,*) XD(IG),XD1(IG),XQ(IG),XQ1(IG)
	  READ(IMACHI,*) TD1(IG),TQ1(IG)
	  READ(IMACHI,*) DAMP(IG), S1, S2, J1  
	  IF ( J1.NE. 0)  CALL  COMPUTEAGBG(  S1, S2 ) 
333       C1(IG) = S1
	  C2(IG) = S2

C        
	  WRITE( IMACHO, 490 ) IG,GTYPE( IG,1 )
	  WRITE(IMACHO,540) PBASE(IG),H(IG),R(IG),XL(IG)
	  WRITE(IMACHO,540) XD(IG),XD1(IG),XQ(IG),XQ1(IG)
	  WRITE(IMACHO,520) TD1(IG),TQ1(IG)
	  WRITE(IMACHO,530) DAMP(IG),C1(IG),C2(IG)
	 END IF
C
C    GENERATOR MODEL E'q (MODEL 2):
	 IF (GTYPE(IG,1).EQ.2) THEN
	  READ(IMACHI,*) PBASE(IG),H(IG),R(IG),XL(IG)
	  READ(IMACHI,*) XD(IG),XD1(IG),XQ(IG)
	  READ(IMACHI,*) TD1(IG)
	  READ(IMACHI,*) DAMP(IG), S1, S2, J1  
	  IF ( J1.NE. 0)  CALL  COMPUTEAGBG(  S1, S2 ) 
222       C1(IG) = S1
	  C2(IG) = S2

	  XQ1(IG) = XQ(IG)
	  TQ1(IG) = 100000.0
	  TQ1(IG) = 100000.0
C        
	  WRITE( IMACHO, 490 ) IG, GTYPE( IG,1 )
	  WRITE(IMACHO,540) PBASE(IG),H(IG),R(IG),XL(IG)
	  WRITE(IMACHO,530) XD(IG),XD1(IG),XQ(IG)
	  WRITE(IMACHO,510) TD1(IG)
	  WRITE(IMACHO,530) DAMP(IG),C1(IG),C2(IG)
	 END IF
C
C   GENERATOR MODEL E'q=CONSTANT (MODEL 1):
	 IF (GTYPE(IG,1).EQ.1) THEN
	  READ(IMACHI,*) PBASE(IG),H(IG),R(IG),XL(IG)
	  READ(IMACHI,*) XD1(IG)
	  READ(IMACHI,*) DAMP(IG), S1, S2, J1  
	  IF ( J1.NE. 0)  CALL  COMPUTEAGBG(  S1, S2 ) 
111       C1(IG) = S1
	  C2(IG) = S2

	  XQ(IG)  = XD1(IG)
	  XD(IG)  = XD1(IG)
	  XQ1(IG) = XD1(IG)
	  TD1(IG) = 100000.0
	  TQ1(IG) = 100000.0
C        
	  WRITE( IMACHO, 490 ) IG, GTYPE( IG,1 )
	  WRITE(IMACHO,540) PBASE(IG),H(IG),R(IG),XL(IG)
	  WRITE(IMACHO,510) XD1(IG)
	  WRITE(IMACHO,530) DAMP(IG),C1(IG),C2(IG)
	 END IF
C
C   GENERATOR MODEL SIMULATING INFINITIVE BUSBAR 
C    E'q=Ef=Eq=Vt=infinite busbar voltage (MODEL 0):
	 IF (GTYPE(IG,1).EQ.0) THEN
	  READ(IMACHI,*) PBASE(IG)
	     H(IG)   = 100000.0
	     R(IG)   = 0.000000
	     XL(IG)  = 0.000000
	     XD(IG)  = 0.000001
	     XD1(IG) = 0.000001
	     XQ(IG)  = 0.000001
	     XQ1(IG) = 0.000001
	     TD1(IG) = 100000.0
	     TQ1(IG) = 100000.0
	     DAMP(IG)= 0.000000
	     C1(IG)  = 0.000000
	     C2(IG)  = 0.000000
C        
	  WRITE( IMACHO, 490 ) IG, GTYPE( IG,1 )
	  WRITE(IMACHO,510) PBASE(IG)
	END IF
	END DO
C
C   ***********INPUT AVR PARAMETERS****************
C        
	CALL STRING ( IMACHI, IMACHO )
	DO I=1,NGEN
	READ(IMACHI,*) IG, GTYPE(IG,2)
	J1 = 0
C     -------------dc excitation---------------
C     AVR IEEE DC1or DC2 or EXDC2B MODELS (MODEL 11 or 12 or 14):
	  IF ((GTYPE(IG,2).EQ.11).OR. 
     *        (GTYPE(IG,2).EQ.12).OR.
     *        (GTYPE(IG,2).EQ.14)     ) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,3)
	   READ(IMACHI,*) ( AVR(IG,J),J=4,8)
	   READ(IMACHI,*) ( AVR(IG,J),J=9,10)
	   READ(IMACHI,*) E1, S1, E2, S2, J1 
	   IF ( J1.NE. 0 ) CALL COMPUTEAB( E1, S1, E2, S2 ) 
1111       AVR(IG,11) = E1
	   AVR(IG,12) = S1
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,530) ( AVR(IG,J),J=1,3)
	   WRITE(IMACHO,550) ( AVR(IG,J),J=4,8)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=9,10)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=11,12)
	  ENDIF
1490       FORMAT(T3, 2I4)
C
C  AVR IEEE DC3 MODEL (MODEL 13):
	  IF (GTYPE(IG,2).EQ.13) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,2)
	   READ(IMACHI,*) ( AVR(IG,J),J=3,4)
	   READ(IMACHI,*) ( AVR(IG,J),J=5,6)
	   READ(IMACHI,*) E1, S1, E2, S2, J1 
	   IF ( J1.NE. 0 ) CALL COMPUTEAB( E1, S1, E2, S2 ) 
1313       AVR(IG,7) = E1
	   AVR(IG,8) = S1
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=1,2)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=3,4)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=5,6)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=7,8)
	  ENDIF
C     -----------ac excitation---------------------
C     AVR IEEE AC1 MODEL (MODEL 21):
	  IF (GTYPE(IG,2).EQ.21) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,5)
	   READ(IMACHI,*) ( AVR(IG,J),J=6,10)
	   READ(IMACHI,*) ( AVR(IG,J),J=11,12)
	   READ(IMACHI,*) E1, S1, E2, S2, J1 
	   IF ( J1.NE. 0 ) CALL COMPUTEAB( E1, S1, E2, S2 ) 
2121       AVR(IG,13) = E1
	   AVR(IG,14) = S1
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,550) ( AVR(IG,J),J=1,5)
	   WRITE(IMACHO,560) ( AVR(IG,J),J=6,10)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=11,12)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=13,14)
	  ENDIF
C
C  AVR IEEE AC2 MODEL (MODEL 22):
	  IF (GTYPE(IG,2).EQ.22) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,8)
	   READ(IMACHI,*) ( AVR(IG,J),J=9,13)
	   READ(IMACHI,*) ( AVR(IG,J),J=14,17)
	   READ(IMACHI,*)  AVR(IG,18),  E1, S1, E2, S2, J1 
	   IF ( J1.NE. 0 ) CALL COMPUTEAB( E1, S1, E2, S2 ) 
2222       AVR(IG,11) = E1
	   AVR(IG,12) = S1
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,580) ( AVR(IG,J),J=1,8)
	   WRITE(IMACHO,560) ( AVR(IG,J),J=9,13)
	   WRITE(IMACHO,540) ( AVR(IG,J),J=14,17)
	   WRITE(IMACHO,530) ( AVR(IG,J),J=18,20)
	  ENDIF
C
C  AVR IEEE AC3 MODEL (MODEL 23):
	  IF (GTYPE(IG,2).EQ.23) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,8)
	   READ(IMACHI,*) ( AVR(IG,J),J=9,13)
	   READ(IMACHI,*) ( AVR(IG,J),J=14,15)
	   READ(IMACHI,*)  AVR(IG,16), E1, S1, E2, S2, J1  
	   IF ( J1.NE. 0 ) CALL COMPUTEAB( E1, S1, E2, S2 ) 
2323       AVR(IG,11) = E1
	   AVR(IG,12) = S1
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,580) ( AVR(IG,J),J=1,8)
	   WRITE(IMACHO,560) ( AVR(IG,J),J=9,13)
	   WRITE(IMACHO,540) ( AVR(IG,J),J=14,15)
	   WRITE(IMACHO,530) ( AVR(IG,J),J=16,18)
	  ENDIF
C
C  AVR IEEE AC4 MODEL (MODEL 24):
	  IF (GTYPE(IG,2).EQ.24) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,2)
	   READ(IMACHI,*) ( AVR(IG,J),J=3,5)
	   READ(IMACHI,*) ( AVR(IG,J),J=6,9)
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=1,2)
	   WRITE(IMACHO,530) ( AVR(IG,J),J=3,5)
	   WRITE(IMACHO,540) ( AVR(IG,J),J=6,9)
	  ENDIF
C    --------static excitation------------
C    AVR IEEE ST1 MODEL (MODEL 31):
	  IF (GTYPE(IG,2).EQ.31) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,3)
	   READ(IMACHI,*) ( AVR(IG,J),J=4,7)
	   READ(IMACHI,*) ( AVR(IG,J),J=8,11)
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,530) ( AVR(IG,J),J=1,3)
	   WRITE(IMACHO,540) ( AVR(IG,J),J=4,7)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=8,11)
	  ENDIF
C
C  AVR IEEE ST2 MODEL (MODEL 32):
	  IF (GTYPE(IG,2).EQ.32) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,6)
	   READ(IMACHI,*) ( AVR(IG,J),J=7,9)
	   READ(IMACHI,*) ( AVR(IG,J),J=10,12)
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,560) ( AVR(IG,J),J=1,6)
	   WRITE(IMACHO,530) ( AVR(IG,J),J=7,9)
	   WRITE(IMACHO,530) ( AVR(IG,J),J=10,12)
	  ENDIF
C
C  AVR IEEE ST3 MODEL (MODEL 33):
	  IF (GTYPE(IG,2).EQ.33) THEN
	   READ(IMACHI,*) ( AVR(IG,J),J=1,6)
	   READ(IMACHI,*) ( AVR(IG,J),J=7,9)
	   READ(IMACHI,*) ( AVR(IG,J),J=10,15)
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,560) ( AVR(IG,J),J=1,6)
	   WRITE(IMACHO,530) ( AVR(IG,J),J=7,9)
	   WRITE(IMACHO,560) ( AVR(IG,J),J=10,15)
	  ENDIF
C
C  AVR IEEE SCRX MODEL (MODEL 41):
	  IF (GTYPE(IG,2).EQ.41) THEN
	   READ(IMACHI,*)  AVR(IG,1), J1, J2
	   READ(IMACHI,*) ( AVR(IG,J),J=4,7)
	   READ(IMACHI,*) ( AVR(IG,J),J=8,9)
	   AVR(IG,2) = J1
	   AVR(IG,3) = J2
C        
	   WRITE(IMACHO,1490) IG, GTYPE(IG,2)
	   WRITE(IMACHO,531)   AVR(IG,1), J1, J2
	   WRITE(IMACHO,520) ( AVR(IG,J),J=4,7)
	   WRITE(IMACHO,520) ( AVR(IG,J),J=8,9)
	  ENDIF
531       FORMAT(T1, F10.4, T13, I2,6X, T23, I2 )
	
	ENDDO
C
C   *********INPUT TUNIBE GOVERNOR PARAMETER*********
C        
	CALL STRING ( IMACHI, IMACHO )
	DO I=1,NGEN
	READ(IMACHI,*) IG, GTYPE(IG,3)

C    steam speed governor and turbine system (1):
c     nonreheat(211):        
	IF (GTYPE(IG,3).EQ.211) THEN        
	TUR(IG,1)=PBASE(IG)
	READ(IMACHI,*) (TUR(IG,J),J=1,1)
C
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	WRITE(IMACHO, 510) (TUR(IG,J),J=1,1)
	END IF   
C     tandem-compound,single reheat (212):        
	IF (GTYPE(IG,3).EQ.212) THEN        
	TUR(IG,1)=PBASE(IG)
	READ(IMACHI,*) (TUR(IG,J),J=2,4)
	READ(IMACHI,*) (TUR(IG,J),J=5,8)
	READ(IMACHI,*) (TUR(IG,J),J=9,12)
	READ(IMACHI,*) (TUR(IG,J),J=13,15)
C
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	WRITE(IMACHO, 540) (TUR(IG,J),J=1,4)
	WRITE(IMACHO, 540) (TUR(IG,J),J=5,8)
	WRITE(IMACHO, 540) (TUR(IG,J),J=9,12)
	WRITE(IMACHO, 530) (TUR(IG,J),J=13,15)
       END IF
2490    FORMAT(T3, 2I4)
C        
C     tandem-compound,double reheat (213):        
	IF (GTYPE(IG,3).EQ.213) THEN        
	TUR(IG,1)=PBASE(IG)
	READ(IMACHI,*) (TUR(IG,J),J=2,5)
	READ(IMACHI,*) (TUR(IG,J),J=6,10)
	READ(IMACHI,*) (TUR(IG,J),J=11,14)
	READ(IMACHI,*) (TUR(IG,J),J=15,17)
	
C
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	WRITE(IMACHO, 550) (TUR(IG,J),J=1,5)
	WRITE(IMACHO, 550) (TUR(IG,J),J=6,10)
	WRITE(IMACHO, 540) (TUR(IG,J),J=11,14)
	WRITE(IMACHO, 530) (TUR(IG,J),J=15,17)
	WRITE(IMACHO,*)  
	END IF   
C     cross-compound,single reheat1 (214):       
	IF (GTYPE(IG,3).EQ.214) THEN        
	TUR(IG,1)=PBASE(IG)
	READ(IMACHI,*) (TUR(IG,J),J=2,4)
	READ(IMACHI,*) (TUR(IG,J),J=5,9)
	READ(IMACHI,*) (TUR(IG,J),J=10,13)
	READ(IMACHI,*) (TUR(IG,J),J=14,16)
	
C
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	WRITE(IMACHO, 540) (TUR(IG,J),J=1,4)
	WRITE(IMACHO, 550) (TUR(IG,J),J=5,9)
	WRITE(IMACHO, 540) (TUR(IG,J),J=10,13)
	WRITE(IMACHO, 530) (TUR(IG,J),J=14,16)
       END IF
C     cross-compound,single reheat2 (215):       
	IF (GTYPE(IG,3).EQ.215) THEN        
	TUR(IG,1)=PBASE(IG)
	READ(IMACHI,*) (TUR(IG,J),J=2,4)
	READ(IMACHI,*) (TUR(IG,J),J=5,8)
	READ(IMACHI,*) (TUR(IG,J),J=9,12)
	READ(IMACHI,*) (TUR(IG,J),J=13,15)
	
C
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	WRITE(IMACHO, 540) (TUR(IG,J),J=1,4)
	WRITE(IMACHO, 540) (TUR(IG,J),J=5,8)
	WRITE(IMACHO, 540) (TUR(IG,J),J=9,12)
	WRITE(IMACHO, 530) (TUR(IG,J),J=13,15)
	END IF
C     cross-compound,double reheat (216):       
	IF (GTYPE(IG,3).EQ.216) THEN        
	TUR(IG,1)=PBASE(IG)
	READ(IMACHI,*) (TUR(IG,J),J=2,5)
	READ(IMACHI,*) (TUR(IG,J),J=6,12)
	READ(IMACHI,*) (TUR(IG,J),J=13,16)
	READ(IMACHI,*) (TUR(IG,J),J=17,19)
C
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	WRITE(IMACHO, 550) (TUR(IG,J),J=1,5)
	WRITE(IMACHO, 570) (TUR(IG,J),J=6,12)
	WRITE(IMACHO, 540) (TUR(IG,J),J=13,16)
	WRITE(IMACHO, 530) (TUR(IG,J),J=17,19)
	END IF
C
C   hydro speed governor and turbine system (21):
	IF (GTYPE(IG,3).EQ.221) THEN        
	TUR(IG,1)=PBASE(IG)
	READ(IMACHI,*) (TUR(IG,J),J=2,5)
	READ(IMACHI,*) (TUR(IG,J),J=6,7)
	READ(IMACHI,*) (TUR(IG,J),J=8,11)
C
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	WRITE(IMACHO, 550) (TUR(IG,J),J=1,5)
	WRITE(IMACHO, 520) (TUR(IG,J),J=6,7)
	WRITE(IMACHO, 540) (TUR(IG,J),J=8,11)
	END IF
C
C   simplified hydro speed governor and turbine system (22):
	IF (GTYPE(IG,3).EQ.222) THEN        
	TUR(IG,1)=PBASE(IG)
	READ(IMACHI,*) (TUR(IG,J),J=2,5)
	READ(IMACHI,*) (TUR(IG,J),J=6,6)
	READ(IMACHI,*) (TUR(IG,J),J=7,8)
C
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	WRITE(IMACHO, 550) (TUR(IG,J),J=1,5)
	WRITE(IMACHO, 510) (TUR(IG,J),J=6,6)
	WRITE(IMACHO, 520) (TUR(IG,J),J=7,8)
	END IF
C                 
C   no governor for this generator:
	IF (GTYPE(IG,3).EQ.888) THEN        
	WRITE(IMACHO,2490) IG, GTYPE(IG, 3)
	END IF
      END DO
C        
C
C   ************PSS PARAMETER INPUT***************
C
	CALL STRING ( IMACHI, IMACHO )
	DO I = 1, NGEN
	READ(IMACHI,*) IG, GTYPE(IG,4)

	IF (GTYPE(IG,4).EQ.311) THEN
C    IEEE standard model ( SaskPower IEEESN )(1)/PSS
	  READ(IMACHI,*) IPSSTYPE(IG)
	  READ(IMACHI,*)  PSS(IG,1)
	  READ(IMACHI,*) (PSS(IG,J),J=2,7)
	  READ(IMACHI,*) (PSS(IG,J),J=8,10)
C
	  WRITE(IMACHO,3490) IG , GTYPE( IG, 4 )
	  WRITE(IMACHO,3490) IPSSTYPE(IG)
	  WRITE(IMACHO,510)  PSS(IG,1)
	  WRITE(IMACHO,560) (PSS(IG,J),J=2,7)
	  WRITE(IMACHO,530) (PSS(IG,J),J=8,10)
	  END IF

	IF (GTYPE(IG,4).EQ.312) THEN
C    IEEE standard model ( SaskPower IEEEST ) (2)/PSS  
	  READ(IMACHI,*) IPSSTYPE(IG)
	  READ(IMACHI,*)  PSS(IG,1)
	  READ(IMACHI,*)(PSS(IG,J),J=2,7)
	  READ(IMACHI,*)(PSS(IG,J),J=8,10)
C
	  WRITE(IMACHO,3490) IG , GTYPE( IG, 4 )
	  WRITE(IMACHO,3490) IPSSTYPE(IG)
	  WRITE(IMACHO,510)  PSS(IG,1)
	  WRITE(IMACHO,560) (PSS(IG,J),J=2,7)
	  WRITE(IMACHO,560) (PSS(IG,J),J=8,10)
	  END IF
	  END DO
3490      FORMAT(T3,2I4)
C      
510       FORMAT(T1, F10.4 )
520       FORMAT(T1, F10.4, T13, F8.4 )
530       FORMAT(T1, F10.4, T13, F8.4,T23, F8.4 )
540       FORMAT(T1, F10.4, T13, F8.4,T23, F8.4, T33, F8.4 )
550       FORMAT(T1, F10.4, T13, F8.4,T23, F8.4, T33, F8.4,
     *          T43, F8.4   )
560       FORMAT(T1, F10.4, T13, F8.4,T23, F8.4, T33, F8.4,
     *          T43, F8.4 , T53, F8.4  )
570       FORMAT(T1, F10.4, T13, F8.4,T23, F8.4, T33, F8.4,
     *          T43, F8.4 , T53, F8.4,T63, F8.4   )
580       FORMAT(T1, F10.4, T13, F8.4,T23, F8.4, T33, F8.4,
     *          T43, F8.4 , T53, F8.4,T63, F8.4, T73, F8.4   )
1100    CONTINUE  

	do i=1, 10
	write( imacho, 1199) i,h(i),damp(i),r(i),xd1(i),xq1(i),
     +           xd(i),xq(i),td1(i),tq1(i),xl(i),pbase(i)
1199    format(1x, i3, 13f8.4)
	end do

	CALL STRING ( IMACHI, IMACHO )
	
	CLOSE ( IMACHI )
	CLOSE ( IMACHO )

	RETURN
	END
C
C   This subroutine is for reading the character constants

	SUBROUTINE STRING( IMACHI, IMACHO )
	CHARACTER*1      STR
	CHARACTER*75     STRR

10      READ ( IMACHI, 20, END = 30 )  STR
	IF ( STR.EQ. '!' )   THEN
	   BACKSPACE IMACHI
	   READ ( IMACHI, 22 )  STRR
	   WRITE( IMACHO, 25 )  STRR
	   GOTO  10
	END IF 
20      FORMAT( A1  )
22      FORMAT( A75 )
25      FORMAT( 2X, A75 )
	BACKSPACE IMACHI
30      RETURN
	END 
C        
C   This routine COMPUTE the saturated constants
C
	SUBROUTINE COMPUTEAB( E1, S1, E2, S2 )
	double precision      E1, S1, E2, S2
	S1 = ( DLOG( S1 / S2 ) ) / ( E1 - E2 )
	E1 = S2 / DEXP( S1 * E2 )
	RETURN
	END

	SUBROUTINE COMPUTEAGBG( S1, S2 )
	double precision        S1, S2 
	S2 = 5.0 * DLOG( S2 / S1 )
	S1 = S1 / DEXP( S2 )
	RETURN
	END
