C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~           IEEE standard  stablizer           ~
C   ~      SaskPower IEEESN ( model # : 311 )      ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE powerstablizer1(I)
C
	INCLUDE 'CMM.DM'
C
	DOUBLE PRECISION  X0, X10, X1, X2 ,X3
	INTEGER           IPSSTYPE
C
C   define stablizer variables
C
C       X0    = OUT(I,1)
C  -------- pss type selection ------------------------       
	IF (IPSSTYPE(I).EQ.1) X0 = OUT(I,1)
	IF (IPSSTYPE(I).EQ.2) X0 = DABS( VREF(I) ) - CDABS( VT(I) )
	IF (IPSSTYPE(I).EQ.3) X0 = PVAR(I,6)/100.0
	IF (IPSSTYPE(I).EQ.4) X0 = PM(I) - PVAR(I,6)/100.0
C        
	X10   = OUT(I,16)
	IF ( PSS(I,1).LE.0.00001 ) X10 = X0
	X1    = OUT(I,17) + X10 * PSS(I,2) / PSS(I,3)
	X2    = X1  * PSS(I,4) / PSS(I,5) - OUT(I,14) 
	X3    = X2 * PSS(I,8) *PSS(I,6) / PSS(I,7)
	VS(I) = X3 - OUT(I,15)
C
	IF ( VS(I).GT.PSS(I,10) ) VS(I) = PSS(I,10)
	IF ( VS(I).LT.PSS(I, 9) ) VS(I) = PSS(I, 9)
c        IF ( DABS(X0).LT.1.0E-5 ) VS(I) = 0.0

C
C   define stablizer inputs
C
	PLUG(I,17) = ( X10 - X1 ) / PSS(I,3)
	PLUG(I,14) =  X2 / PSS(I,5)
	PLUG(I,15) =  VS(I) / PSS(I,7)
	PLUG(I,16) = ( X0 - X10 )/ PSS(I,1)
	PVAR(I,15) =  VS(I)
	RETURN
C
C   initilize the stablizer
C
	ENTRY initializepss1(I)
	IF ( PSS(I,1).EQ.0.0 ) PSS(I,1) = 0.000001
	IF( (PSS(I,2).EQ.0.0).AND.(PSS(I,3).EQ.0.0) ) THEN   
		 PSS(I,2) = 1.E+6
		 PSS(I,3) = 1.E+6
	END IF
	IF( (PSS(I,4).EQ.0.0).AND.(PSS(I,5).EQ.0.0) ) THEN   
		 PSS(I,4) = 1.E+6
		 PSS(I,5) = 1.E+6
	END IF
	OUT(I,17) = 0.0
	OUT(I,14) = 0.0
	OUT(I,15) = 0.0
	OUT(I,16) = 0.0
	RETURN
	END




C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~           IEEE standard  stablizer           ~
C   ~      SaskPower IEEEST ( model # : 312 )      ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE powerstablizer2(I)
C
	INCLUDE 'CMM.DM'
C
	DOUBLE PRECISION  X0, X10, X1, X2 ,X3
	INTEGER           IPSSTYPE
C
C   define stablizer variables
C
C       X0    = OUT(I,1)
C  -------- pss type selection ------------------------       
	IF (IPSSTYPE(I).EQ.1) X0 = OUT(I,1)
	IF (IPSSTYPE(I).EQ.2) X0 = DABS( VREF(I) ) - CDABS( VT(I) )
	IF (IPSSTYPE(I).EQ.3) X0 = PVAR(I,6)/100.0
	IF (IPSSTYPE(I).EQ.4) X0 = PM(I) - PVAR(I,6)/100.0
C        
	X10   = OUT(I,16)
	IF ( PSS(I,1).LE.0.00001 ) X10 = X0
	X1    = OUT(I,17) + X10 * PSS(I,2) / PSS(I,3)
	X2    = OUT(I,14) + X1  * PSS(I,4) / PSS(I,5)
	X3    = X2 * PSS(I,8) *PSS(I,6) / PSS(I,7)
	VS(I) = X3 - OUT(I,15)
C
	IF ( VS(I).GT.PSS(I,10) ) VS(I) = PSS(I,10)
	IF ( VS(I).LT.PSS(I, 9) ) VS(I) = PSS(I, 9)
	IF ( DABS(X0).LT.1.0E-5 ) VS(I) = 0.0
C
C   define stablizer inputs
C
	PLUG(I,17) = ( X10 - X1 ) / PSS(I,3)
	PLUG(I,14) = ( X1  - X2 ) / PSS(I,5)
	PLUG(I,15) = VS(I) / PSS(I,7)
	PLUG(I,16) = ( X0 - X10 ) / PSS(I,1)
	PVAR(I,15) = VS(I)
	RETURN
C
C   initilize the stablizer
C
	ENTRY initializepss2(I)
	IF ( PSS(I,1).EQ.0.0 ) PSS(I,1) = 0.000001
	IF( ( PSS(I,2).EQ.0.0 ).AND. 
     +      ( PSS(I,3).EQ.0.0 )      ) THEN
		 PSS(I,2) = 1.E+6
		 PSS(I,3) = 1.E+6
	END IF
	IF( ( PSS(I,4).EQ.0.0 ).AND. 
     +      ( PSS(I,5).EQ.0.0 )      ) THEN
		 PSS(I,4) = 1.E+6
		 PSS(I,5) = 1.E+6
	END IF
	OUT(I,17) = 0.0
	OUT(I,14) = 0.0
	OUT(I,15) = 0.0
	OUT(I,16) = 0.0
	RETURN
	END


C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~  IEEE PSS MODEL WITHOUT HIH FREQUENCY FILTER  ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE powerstablizer3(I)
C
	INCLUDE 'CMM.DM'
C
	DOUBLE PRECISION  X0, X1, X2
	INTEGER           IPSSTYPE
C
C   define stablizer variables
C
C       X0    = OUT(I,1)
C  -------- pss type selection ------------------------       
	IF (IPSSTYPE(I).EQ.1) X0 = OUT(I,1)
	IF (IPSSTYPE(I).EQ.2) X0 = DABS( VREF(I) ) - CDABS( VT(I) )
	IF (IPSSTYPE(I).EQ.3) X0 = PVAR(I,6)/100.0
	IF (IPSSTYPE(I).EQ.4) X0 = PM(I) - PVAR(I,6)/100.0

C        OME   = OUT(I,1)
C        X0    = OME
	
	X1    = OUT(I,17)+X0*PSS(I,2)/PSS(I,3)
	X2    = OUT(I,14)+X1*PSS(I,4)/PSS(I,5)
	VS(I) = X2*PSS(I,7)-OUT(I,15)
C
	IF(VS(I).GT.PSS(I,9)) VS(I)=PSS(I,9)
	IF(VS(I).LT.PSS(I,8)) VS(I)=PSS(I,8)
C
C   define stablizer inputs
C
	PLUG(I,17) = (X0-X1)/PSS(I,3)
	PLUG(I,14) = (X1-X2)/PSS(I,5)
	PLUG(I,15) = VS(I)/PSS(I,6)
	PVAR(I,15) = VS(I)
	RETURN
C
C   initilize the stablizer
C
	ENTRY initializepss3(I)
	IF( (PSS(I,4).EQ.0.0).AND.(PSS(I,5).EQ.0.0) ) THEN   
		 PSS(I,4) = 1.E+6
		 PSS(I,5) = 1.E+6
	END IF
	OUT(I,17) = 0.0
	OUT(I,14) = 0.0
	OUT(I,15) = 0.0
	RETURN
	END
