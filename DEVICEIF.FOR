	SUBROUTINE APPARATUSONE
	
	INCLUDE 'CMM.DM'
C
C   initialize device variables    
C     
C
C   ----------------------generators--------------------------        
C
	DO I = 1 , NGEN
	  IF ( GTYPE(I,1).EQ.4 )  CALL initializegenmdl4(I)
	  IF ( GTYPE(I,1).LE.3 )  CALL initializegenmdl3(I)
	END DO
	
C
C   ----------------------exciters----------------------------        
C
	DO I = 1 , NGEN
c
c   Type DC1, DC2, DC3 exciters           
c
	  IF ( GTYPE(I,2).EQ.11 )  CALL  initializedc1(I)
	  IF ( GTYPE(I,2).EQ.12 )  CALL  initializedc2(I) 
	  IF ( GTYPE(I,2).EQ.13 )  CALL  initializedc3(I) 
c         IF ( GTYPE(I,2).EQ.14 )  CALL  initializedc4(I) 

c
c   Type AC1, AC2, AC3, AC4 exciters           
c
c         IF ( GTYPE(I,2).EQ.21 )  CALL  initializeac1(I)
c         IF ( GTYPE(I,2).EQ.22 )  CALL  initializeac2(I) 
c         IF ( GTYPE(I,2).EQ.23 )  CALL  initializeac3(I) 
	  IF ( GTYPE(I,2).EQ.24 )  CALL  initializeac4(I)
c
c   Type ST1, ST2, ST3 , SRX exciters           
c
	  IF ( GTYPE(I,2).EQ.31 )  CALL  initializest1(I) 
c         IF ( GTYPE(I,2).EQ.32 )  CALL  initializest2(I) 
c         IF ( GTYPE(I,2).EQ.33 )  CALL  initializest3(I)
	  IF ( GTYPE(I,2).EQ.41 )  CALL  initializescrx(I) 
       
       END DO

C
C   ------------------governor and turbines------------------        
C

       DO I=1,NGEN
C         
C   governor and steam turbines
C
	 IF ( GTYPE(I,3).EQ.211 ) CALL initializetur11(I)
	 IF ( GTYPE(I,3).EQ.212 ) CALL initializetur12(I)  
	 IF ( GTYPE(I,3).EQ.213 ) CALL initializetur13(I) 
	 IF ( GTYPE(I,3).EQ.214 ) CALL initializetur14(I)
	 IF ( GTYPE(I,3).EQ.215 ) CALL initializetur15(I)  
	 IF ( GTYPE(I,3).EQ.216 ) CALL initializetur16(I) 
C         
C   governor and hydro turbines
C
	 IF ( GTYPE(I,3).EQ.221 ) CALL initializetur21(I)
	 IF ( GTYPE(I,3).EQ.222 ) CALL initializetur22(I)
       
       END DO
C 
C   ------------------PSS------------------------------------
C
       DO I = 1 , NGEN
C         
C   ieeest model 
C
	   IF ( GTYPE(I,4).EQ.311 )  CALL initializepss1(I)
C         
C   ieeesn model
C
	   IF ( GTYPE(I,4).EQ.312 )  CALL initializepss2(I)
C         
C   ieee standard model
C
c          IF ( GTYPE(I,4).EQ.313 )  CALL initializepss3(I)
       END DO

       RETURN
       END

       
       
       SUBROUTINE APPARATUSTWO

       INCLUDE 'CMM.DM'

C  
C   calculate device variables at TIME and their derivatives  
C    
C
C   ----------------------generators--------------------------        
C
	DO I = 1 , NGEN
	  IF ( GTYPE(I,1).EQ.4 )  CALL generatormodel4(I)
	  IF ( GTYPE(I,1).LE.3 )  CALL generatormodel3(I)
	END DO
	
C
C   ----------------------exciters----------------------------        
C
	DO I = 1 , NGEN
c
c   Type DC1, DC2, DC3 exciters           
c
	  IF ( GTYPE(I,2).EQ.11 )  CALL  exciterdc1(I)
	  IF ( GTYPE(I,2).EQ.12 )  CALL  exciterdc2(I) 
	  IF ( GTYPE(I,2).EQ.13 )  CALL  exciterdc3(I) 
c         IF ( GTYPE(I,2).EQ.14 )  CALL  exciterdc4(I) 

c
c   Type AC1, AC2, AC3, AC4 exciters           
c
c         IF ( GTYPE(I,2).EQ.21 )  CALL  exciterac1(I)
c         IF ( GTYPE(I,2).EQ.22 )  CALL  exciterac2(I) 
c         IF ( GTYPE(I,2).EQ.23 )  CALL  exciterac3(I) 
	  IF ( GTYPE(I,2).EQ.24 )  CALL  exciterac4(I)
c
c   Type ST1, ST2, ST3 , SRX exciters           
c
	  IF ( GTYPE(I,2).EQ.31 )  CALL  exciterst1(I) 
c         IF ( GTYPE(I,2).EQ.32 )  CALL  exciterst2(I) 
c         IF ( GTYPE(I,2).EQ.33 )  CALL  exciterst3(I)
	  IF ( GTYPE(I,2).EQ.41 )  CALL  exciterscrx(I) 
       
       END DO

C
C   ------------------governor and turbines------------------        
C

       DO I=1,NGEN
C         
C   governor and steam turbines
C
	 IF ( GTYPE(I,3).EQ.211 ) CALL turbinegovernor11(I)
	 IF ( GTYPE(I,3).EQ.212 ) CALL turbinegovernor12(I)  
	 IF ( GTYPE(I,3).EQ.213 ) CALL turbinegovernor13(I) 
	 IF ( GTYPE(I,3).EQ.214 ) CALL turbinegovernor14(I)
	 IF ( GTYPE(I,3).EQ.215 ) CALL turbinegovernor15(I)  
	 IF ( GTYPE(I,3).EQ.216 ) CALL turbinegovernor16(I) 
C         
C   governor and hydro turbines
C
	 IF ( GTYPE(I,3).EQ.221 ) CALL turbinegovernor21(I)
	 IF ( GTYPE(I,3).EQ.222 ) CALL turbinegovernor22(I)
       
       END DO
C 
C   ------------------PSS------------------------------------
C
       DO I = 1 , NGEN
C         
C   ieeest model 
C
	   IF ( GTYPE(I,4).EQ.311 )  CALL powerstablizer1(I)
C         
C   ieeesn model
C
	   IF ( GTYPE(I,4).EQ.312 )  CALL powerstablizer2(I)
C         
C   ieee standard model
C
c          IF ( GTYPE(I,4).EQ.313 )  CALL powerstablizer3(I)
       END DO

       RETURN
       END


       SUBROUTINE APPARATUSTHREE

       INCLUDE 'CMM.DM'

       double precision     CONST

       DOUBLE PRECISION     PT   , QT     , VMAG  , VARG
	
       DOUBLE COMPLEX       DCONJG, DCMPLX

       OPEN( IPQLFO, FILE = 'PQLF.OUT', STATUS = 'OLD' )

C
C   clear integrators     
C        
	DO I = 1, NGEN
	    DO J = 1, 16
	    PLUG(I,J) = 0.0
	    OUT(I,J)  = 0.0
	    SAVE(I,J) = 0.0
	    END DO
	END DO
	DO I = 1,NGEN
	VS(I) = 0.0
	    DO J = 1, 20
	    PVAR(I,J) = 0.0
	    END DO
	END DO
C
C   data conversion to 100MVA base
C       
	DO  I   = 1 , NGEN
	CONST   = 100.0/PBASE(I)
	H(I)    = H(I)/CONST
	R(I)    = R(I)*CONST
	XL(I)   = XL(I)*CONST
	XD(I)   = XD(I)*CONST
	XD1(I)  = XD1(I)*CONST
	XD11(I) = XD11(I)*CONST
	XQ(I)   = XQ(I)*CONST
	XQ1(I)  = XQ1(I)*CONST
	XQ11(I) = XQ11(I)*CONST
	DAMP(I) = DAMP(I)/CONST
	END DO
	
	DO I = 1, NGEN
C
C   read format must agree with that of PQLF  program     
C
	READ ( IPQLFO,  *  ) IG, PT, QT, VMAG, VARG
	PT    = PT/100.
	QT    = QT/100.
	VARG  = VARG*3.1416/180.0
	VT(I) = VMAG*DCMPLX(DCOS(VARG),DSIN(VARG))
	CT(I) = DCONJG(DCMPLX(PT,QT)/VT(I))
	END DO

	RETURN
	END
