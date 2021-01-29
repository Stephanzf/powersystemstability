C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~	 hydro turbine and governor systems	 ~
C   ~             IEEE model                     ~
C   ~						 ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       SUBROUTINE turbinegovernor21(I)
C
	INCLUDE 'CMM.DM'
C
	double precision  G , OME
	DOUBLE PRECISION  X1, X2, X3 , X4 , X5 , X6,
     +                    X7, X8, X0 , X20, X21
C
C   define integrator output  
C
	OME = OUT(I,1)
	X4  = OUT(I,9)
	X5  = OUT(I,10)
	X7  = OUT(I,11)
	X21 = OUT(I,12)
C
C   calculate intermediate variables        
C
	G   = X4
	IF (X4.GT.TUR(I,11)) G = TUR(I,11)
	IF (X4.LT.TUR(I,10)) G = TUR(I,10)
	X6  = G*TUR(I,7)-X5
	X1  = WREF(I) - OME
	X20 = (X1-X6-G*TUR(I,6))
	IF (TUR(I,3).LE.0.00001)   X21 = X20  
	X2  = X21/TUR(I,4)
	X3  = X2
	IF (X2.GT.TUR(I,9)) X3 = TUR(I,9)
	IF (X2.LT.TUR(I,8)) X3 = TUR(I,8)
	X8  = 2.0*(X7-G)
	PM(I) = X8*TUR(I,1)/100.0
C
C   define inputs
C
	PLUG(I,9)  = X3
	PLUG(I,10) = X6/TUR(I,5)
	PLUG(I,11) = (G-X8)/TUR(I,2)
	PLUG(I,12) = (X20-X21)/( TUR(I,3)+0.00001)
	PVAR(I,12) = G
	RETURN
C
C   initialize the system variables
C
	ENTRY initializetur21(I)
	X0        = PM(I)*100.0/TUR(I,1)
	OME       = OUT(I,1)
	OUT(I,9 ) = X0
	OUT(I,10) = X0*TUR(I,7)
	OUT(I,11) = 1.5*X0
	OUT(I,12) = 0.0
	WREF(I)   = OME+OUT(I,9)*TUR(I,6)
C
	IF ( OUT(I,9).GT.TUR(I,11) )  WRITE( *, 10) I
	IF ( OUT(I,9).LT.TUR(I,10) )  WRITE( *, 10) I

10      FORMAT( ' gate limits exceeded at ', i4/ )

	RETURN
	END

C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~    hydro turbine and governor systems      ~
C   ~             IEEE model                     ~
C   ~  ( simplified speed - governor model )     ~   
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       SUBROUTINE turbinegovernor22(I)
C
	INCLUDE    'CMM.DM'
C
        double precision  OME
	DOUBLE PRECISION  X0, X1  , X2  , X3 , X4 , X5 , X6, X7, X8
C
C   define integrator output  
C
	OME = OUT(I,1)
	X2  = OUT(I,9)
	X4  = OUT(I,10)
	X6  = OUT(I,11)
C
C   calculate intermediate variables        
C
	X1  = TUR(I,6) * ( WREF(I) - OME )
	X3 = X2 + X1 * TUR(I,4) / TUR(I,3)
	IF ( TUR(I,5).LT.0.0001 )  X4 = X3
	X5 = GREF(I) - X4
	IF ( X5.GT.TUR(I,8) )   X5 = TUR(I,8)
	IF ( X5.LT.TUR(I,7) )   X5 = TUR(I,7)
	X8  = 1.5 * X5 - X6
	X7  = X5 - X8 
	PM(I) = X7 * TUR(I,1) / 100.0
C
C   define integer inputs
C
	PLUG(I,9)  = ( X1 - X3 ) / TUR(I,3)
	PLUG(I,10) = ( X3 - X4 ) / TUR(I,5) 
	PLUG(I,11) = X8 / TUR(I,2)
	PVAR(I,12) = X5
	RETURN
C
C   initialize the system variables
C
	ENTRY initializetur22(I)
C
C   define default time constants
C
	IF ( TUR(I,3).EQ.0.0 )    TUR(I,3) = 1.0E+6
	IF ( TUR(I,4).EQ.0.0 )    TUR(I,4) = 1.0E+6
	IF ( TUR(I,5).LT.0.0001 ) TUR(I,5) = 1.0E-6

	X0        = PM(I) * 100.0 / TUR(I,1)
	GREF(I)   = X0
	OUT(I,9 ) = 0.0
	OUT(I,10) = 0.0
	OUT(I,11) = 1.5 * X0
	WREF(I)   = OUT(I,1) 
C
	IF ( X0.GT.TUR(I,8) )  WRITE( *, 10) I
	IF ( X0.LT.TUR(I,7) )  WRITE( *, 10) I

10      FORMAT( ' Gate limits exceeded at ', i4/ )

	RETURN
	END
