C
C    This is the governor subroutine library
C    which includes six steam turbines and
C    one hydro turbine and  governors modellings.
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~    steam turbine and governor systems      ~
C   ~             IEEE model                     ~
C   ~         ( non - reheat model )             ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE turbinegovernor11(I)
C
	INCLUDE 'CMM.DM'
C
	double precision  G , OME
	DOUBLE PRECISION  X1, X2, X3 ,X4, X5, X6, X7
C
C   define integrator output  
C
	OME = OUT(I,1)
	X4  = OUT(I,9)
	X5  = OUT(I,10)
	X6  = OUT(I,11)
	X7  = OUT(I,12)
C     CALCULATE INTERMIDIEATE VARIABLES:
	G=X4
	IF (X4.GT.TUR(I,11)) G=TUR(I,11)
	IF (X4.LT.TUR(I,12)) G=TUR(I,12)
	X1=(WREF(I)-OME)*TUR(I,8)
	X2=(X1-G)/TUR(I,13)
	X3=X2
	IF (X2.GT.TUR(I,10)) X3=TUR(I,10)
	IF (X2.LT.TUR(I,9 )) X3=TUR(I,9 )
	PM(I)=(TUR(I,5)*X5+TUR(I,6)*X6+TUR(I,7)*X7)*TUR(I,1)/100.0
C     CALCULATE  INTERMEDIATE INPUTS:
	PLUG(I,9) =X3
	PLUG(I,10)=( G-X5)/TUR(I,2)
	PLUG(I,11)=(X5-X6)/TUR(I,3)
	PLUG(I,12)=(X6-X7)/TUR(I,4)
	PVAR(I,12)=G
	RETURN
C
C
	ENTRY initializetur11(I)
	OME=OUT(I,1)            
	G=PM(I)*100.0/TUR(I,1)/(TUR(I,5)+TUR(I,6)+TUR(I,7))
	OUT(I,9 )=G
	OUT(I,10)=G
	OUT(I,11)=G
	OUT(I,12)=G
C     CALCULATE SET POINTS:
	WREF(I)=OME+OUT(I,9)/TUR(I,8)
C     CHECK IF INITIAL SETTING ARE WITHIN LIMITS:
	IF (OUT(I,9).GT.TUR(I,12)) WRITE( * , 10 ) I
	IF (OUT(I,9).LT.TUR(I,11)) WRITE( * , 10 ) I

10      FORMAT( ' gate limits exceeded at ', i4/ )

	RETURN
	END
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~    steam turbine and governor systems      ~
C   ~             IEEE model                     ~
C   ~  ( tandem compound, single reheat model )  ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE turbinegovernor12(I)
C
	INCLUDE 'CMM.DM'
C
	double precision  G , OME
	DOUBLE PRECISION  X1, X2, X3 ,X4, X5, X6, X7
C
C   define integrator output  
C
	OME = OUT(I,1)
	X4  = OUT(I,9)
	X5  = OUT(I,10)
	X6  = OUT(I,11)
	X7  = OUT(I,12)
C
	G=X4
	IF ( X4.GT.TUR(I,12) ) G = TUR(I,12)
	IF ( X4.LT.TUR(I,11) ) G = TUR(I,11)
	X1 = (WREF(I)-OME)*TUR(I,8)
	X2 = (X1-G)/TUR(I,13)
	X3 = X2
	IF ( X2.GT.TUR(I,10) ) X3 = TUR(I,10)
	IF ( X2.LT.TUR(I,9 ) ) X3 = TUR(I,9 )
	PM(I) = (TUR(I,5)*X5+TUR(I,6)*X6+TUR(I,7)*X7)*TUR(I,1)/100.0
C
	PLUG(I,9 ) = X3
	PLUG(I,10) = (  G - X5 )/TUR(I,2)
	PLUG(I,11) = ( X5 - X6 )/TUR(I,3)
	PLUG(I,12) = ( X6 - X7 )/TUR(I,4)
	PVAR(I,12) = G
	RETURN
C
C   initialize the system variables
C
	ENTRY initializetur12(I)

	OME = OUT(I,1)
	G   = PM(I)*100.0/TUR(I,1)/(TUR(I,5)+TUR(I,6)+TUR(I,7))
	OUT(I,9 ) = G
	OUT(I,10) = G
	OUT(I,11) = G
	OUT(I,12) = G
	WREF(I)   = OME+OUT(I,9)/TUR(I,8)
C
	IF ( OUT(I,9).GT.TUR(I,12) )  WRITE( *, 10) I
	IF ( OUT(I,9).LT.TUR(I,11) )  WRITE( *, 10) I

10      FORMAT( ' gate limits exceeded at ', i4/ )

	RETURN
	END
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~    steam turbine and governor systems      ~
C   ~             IEEE model                     ~
C   ~  ( tandem compound, double reheat model )  ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	SUBROUTINE turbinegovernor13(I)
C
	INCLUDE 'CMM.DM'
C
	double precision  G , OME
	DOUBLE PRECISION  X1, X2, X3 ,X4, X5, X6, X7
C
C   define integrator output  
C
	OME = OUT(I,1)
	X4  = OUT(I,9)
	X5  = OUT(I,10)
	X6  = OUT(I,11)
	X7  = OUT(I,12)
C     CALCULATE INTERMIDIEATE VARIABLES:
	G=X4
	IF (X4.GT.TUR(I,11)) G=TUR(I,11)
	IF (X4.LT.TUR(I,12)) G=TUR(I,12)
	X1=(WREF(I)-OME)*TUR(I,8)
	X2=(X1-G)/TUR(I,13)
	X3=X2
	IF (X2.GT.TUR(I,10)) X3=TUR(I,10)
	IF (X2.LT.TUR(I,9 )) X3=TUR(I,9 )
	PM(I)=(TUR(I,5)*X5+TUR(I,6)*X6+TUR(I,7)*X7)*TUR(I,1)/100.0
C     CALCULATE  INTERMEDIATE INPUTS:
	PLUG(I,9) =X3
	PLUG(I,10)=( G-X5)/TUR(I,2)
	PLUG(I,11)=(X5-X6)/TUR(I,3)
	PLUG(I,12)=(X6-X7)/TUR(I,4)
	PVAR(I,12)=G
	RETURN
C
C
	ENTRY initializetur13(I)

	OME=OUT(I,1)            
	G=PM(I)*100.0/TUR(I,1)/(TUR(I,5)+TUR(I,6)+TUR(I,7))
	OUT(I,9 )=G
	OUT(I,10)=G
	OUT(I,11)=G
	OUT(I,12)=G
C     CALCULATE SET POINTS:
	WREF(I)=OME+OUT(I,9)/TUR(I,8)
C     CHECK IF INITIAL SETTING ARE WITHIN LIMITS:
	IF (OUT(I,9).GT.TUR(I,12)) WRITE( * , 10 ) I
	IF (OUT(I,9).LT.TUR(I,11)) WRITE( * , 10 ) I

10      FORMAT( ' gate limits exceeded at ', i4/ )

	RETURN
	END
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~    steam turbine and governor systems      ~
C   ~             IEEE model                     ~
C   ~ ( cross compound, single reheat model 1 )  ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE turbinegovernor14(I)
C
	INCLUDE 'CMM.DM'
C
	double precision  G , OME
	DOUBLE PRECISION  X1, X2, X3 ,X4, X5, X6, X7
C
C   define integrator output  
C
	OME = OUT(I,1)
	X4  = OUT(I,9)
	X5  = OUT(I,10)
	X6  = OUT(I,11)
	X7  = OUT(I,12)
C     CALCULATE INTERMIDIEATE VARIABLES:
	G=X4
	IF (X4.GT.TUR(I,11)) G=TUR(I,11)
	IF (X4.LT.TUR(I,12)) G=TUR(I,12)
	X1=(WREF(I)-OME)*TUR(I,8)
	X2=(X1-G)/TUR(I,13)
	X3=X2
	IF (X2.GT.TUR(I,10)) X3=TUR(I,10)
	IF (X2.LT.TUR(I,9 )) X3=TUR(I,9 )
	PM(I)=(TUR(I,5)*X5+TUR(I,6)*X6+TUR(I,7)*X7)*TUR(I,1)/100.0
C     CALCULATE  INTERMEDIATE INPUTS:
	PLUG(I,9) =X3
	PLUG(I,10)=( G-X5)/TUR(I,2)
	PLUG(I,11)=(X5-X6)/TUR(I,3)
	PLUG(I,12)=(X6-X7)/TUR(I,4)
	PVAR(I,12)=G
	RETURN
C
C
	ENTRY initializetur14(I)
	OME=OUT(I,1)            
	G=PM(I)*100.0/TUR(I,1)/(TUR(I,5)+TUR(I,6)+TUR(I,7))
	OUT(I,9 )=G
	OUT(I,10)=G
	OUT(I,11)=G
	OUT(I,12)=G
C     CALCULATE SET POINTS:
	WREF(I)=OME+OUT(I,9)/TUR(I,8)
C     CHECK IF INITIAL SETTING ARE WITHIN LIMITS:
	IF (OUT(I,9).GT.TUR(I,12)) WRITE( * , 10 ) I
	IF (OUT(I,9).LT.TUR(I,11)) WRITE( * , 10 ) I

10      FORMAT( ' gate limits exceeded at ', i4/ )

	RETURN
	END
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~    steam turbine and governor systems      ~
C   ~             IEEE model                     ~
C   ~ ( cross compound, single reheat model 2 )  ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE turbinegovernor15(I)
C
	INCLUDE 'CMM.DM'
C
	double precision  G , OME
	DOUBLE PRECISION  X1, X2, X3 ,X4, X5, X6, X7
C
C   define integrator output  
C
	OME = OUT(I,1)
	X4  = OUT(I,9)
	X5  = OUT(I,10)
	X6  = OUT(I,11)
	X7  = OUT(I,12)
C     CALCULATE INTERMIDIEATE VARIABLES:
	G=X4
	IF (X4.GT.TUR(I,11)) G=TUR(I,11)
	IF (X4.LT.TUR(I,12)) G=TUR(I,12)
	X1=(WREF(I)-OME)*TUR(I,8)
	X2=(X1-G)/TUR(I,13)
	X3=X2
	IF (X2.GT.TUR(I,10)) X3=TUR(I,10)
	IF (X2.LT.TUR(I,9 )) X3=TUR(I,9 )
	PM(I)=(TUR(I,5)*X5+TUR(I,6)*X6+TUR(I,7)*X7)*TUR(I,1)/100.0
C     CALCULATE  INTERMEDIATE INPUTS:
	PLUG(I,9) =X3
	PLUG(I,10)=( G-X5)/TUR(I,2)
	PLUG(I,11)=(X5-X6)/TUR(I,3)
	PLUG(I,12)=(X6-X7)/TUR(I,4)
	PVAR(I,12)=G
	RETURN
C
C
	ENTRY initializetur15(I)
	OME=OUT(I,1)            
	G=PM(I)*100.0/TUR(I,1)/(TUR(I,5)+TUR(I,6)+TUR(I,7))
	OUT(I,9 )=G
	OUT(I,10)=G
	OUT(I,11)=G
	OUT(I,12)=G
C     CALCULATE SET POINTS:
	WREF(I)=OME+OUT(I,9)/TUR(I,8)
C     CHECK IF INITIAL SETTING ARE WITHIN LIMITS:
	IF (OUT(I,9).GT.TUR(I,12)) WRITE( * , 10 ) I
	IF (OUT(I,9).LT.TUR(I,11)) WRITE( * , 10 ) I

10      FORMAT( ' gate limits exceeded at ', i4/ )

	RETURN
	END
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~    steam turbine and governor systems      ~
C   ~             IEEE model                     ~
C   ~  ( cross compound, double reheat model )   ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE turbinegovernor16(I)
C
	INCLUDE 'CMM.DM'
C
	double precision  G , OME
	DOUBLE PRECISION  X1, X2, X3 ,X4, X5, X6, X7
C
C   define integrator output  
C
	OME = OUT(I,1)
	X4  = OUT(I,9)
	X5  = OUT(I,10)
	X6  = OUT(I,11)
	X7  = OUT(I,12)
C     CALCULATE INTERMIDIEATE VARIABLES:
	G=X4
	IF (X4.GT.TUR(I,11)) G=TUR(I,11)
	IF (X4.LT.TUR(I,12)) G=TUR(I,12)
	X1=(WREF(I)-OME)*TUR(I,8)
	X2=(X1-G)/TUR(I,13)
	X3=X2
	IF (X2.GT.TUR(I,10)) X3=TUR(I,10)
	IF (X2.LT.TUR(I,9 )) X3=TUR(I,9 )
	PM(I)=(TUR(I,5)*X5+TUR(I,6)*X6+TUR(I,7)*X7)*TUR(I,1)/100.0
C     CALCULATE  INTERMEDIATE INPUTS:
	PLUG(I,9) =X3
	PLUG(I,10)=( G-X5)/TUR(I,2)
	PLUG(I,11)=(X5-X6)/TUR(I,3)
	PLUG(I,12)=(X6-X7)/TUR(I,4)
	PVAR(I,12)=G
	RETURN
C
C
	ENTRY initializetur16(I)
	OME=OUT(I,1)            
	G=PM(I)*100.0/TUR(I,1)/(TUR(I,5)+TUR(I,6)+TUR(I,7))
	OUT(I,9 )=G
	OUT(I,10)=G
	OUT(I,11)=G
	OUT(I,12)=G
C     CALCULATE SET POINTS:
	WREF(I)=OME+OUT(I,9)/TUR(I,8)
C     CHECK IF INITIAL SETTING ARE WITHIN LIMITS:
	IF (OUT(I,9).GT.TUR(I,12)) WRITE( * , 10 ) I
	IF (OUT(I,9).LT.TUR(I,11)) WRITE( * , 10 ) I

10      FORMAT( ' gate limits exceeded at ', i4/ )

	RETURN
	END
