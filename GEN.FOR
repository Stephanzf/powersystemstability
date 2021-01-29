C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~    synchronous machine Eq''-Ed'' model      ~
C   ~                                             ~
C   ~  1. field and damping windings on d-axis    ~
c   ~  2.      damping winding  on q-axis         ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE generatormodel4(I)
C
	INCLUDE 'CMM.DM'
C
	double precision     TIME

	DOUBLE COMPLEX       EQD, CURR, DCMPLX, DCONJG, VOLT
	DOUBLE PRECISION     ID , IQ  , OME  , DEL  , EQ   ,
     +                       ED , EQPP, PE   , QE   , PL   ,
     +                       EAT, XDS , XQS  , TD11S, TQ11S, TD1S,
     +                       EAD , EAQ
C
C   define integrator output
C
	OME  = OUT(I,1)
	DEL  = OUT(I,2)
	EQ   = OUT(I,3)
	ED   = OUT(I,4)
	EQPP = OUT(I,13) + EQ
C
	CURR = CT(I)*DCMPLX( DSIN(DEL), DCOS(DEL) )
	ID   = DREAL( CURR )
	IQ   = DIMAG( CURR )
C
	PE   = DREAL( VT(I) * DCONJG( CT(I) )  )
	QE   = DIMAG( VT(I) * DCONJG( CT(I) )  )
	PL   = CDABS( CURR ) ** 2 * R(I)
C
	XDS      = CSAT(I) * XD(I) + ( 1.0 - CSAT(I) ) * XL(I)
	XQS      = CSAT(I) * XQ(I) + ( 1.0 - CSAT(I) ) * XL(I)
	TD1S     =  TD1(I) * (  CSAT(I) + ( 1.0 - CSAT(I) ) *
     +                ( XD1(I) - XL(I) ) / ( XD(I) - XL(I) )  )
	TD11S    = TD11(I) * (  CSAT(I) + ( 1.0 - CSAT(I) ) *
     +                ( XD11(I) - XL(I) ) / ( XD(I) - XL(I) )  )
	TQ11S    = TQ11(I) * (  CSAT(I) + ( 1.0 - CSAT(I) ) *
     +                ( XQ11(I) - XL(I) ) / ( XD(I) - XL(I) )  )
	
	EAQ      = EQPP - ( XD11(I) - XL(I) ) * ID
	EAD      = ED   + ( XQ11(I) - XL(I) ) * IQ
	EAT      = DSQRT( EAQ ** 2 + EAD ** 2 )
	CSAT(I)  = 1.0 / ( 1.0 + C1(I) * DEXP( C2(I) * EAT ) )
C
	PLUG(I,1)  = ( PM(I) - PE - PL - DAMP(I) * OME ) / ( 2.0*H(I) )
	PLUG(I,2)  = 377.0 * OME
	PLUG(I,3)  = ( CSAT(I) * EF(I) -EQ-( XDS-XD1(I) ) *ID ) / TD1S
	PLUG(I,4)  = ( -ED + ( XQS - XQ11(I) ) * IQ ) / TQ11S
	PLUG(I,13) = ( -EQPP + EQ-( XD1(I) - XD11(I) ) * ID ) / TD11S
C
C     plug(i,13) - Eq" integrator input.
C
	PVAR(I,1)   = DEL * 180.0 / 3.1416
	PVAR(I,2)   = OME
	PVAR(I,3)   = EQ
C       PVAR(I,4)   = ED
	PVAR(I,4)   = CDABS( CT(I) )
	PVAR(I,5)   = CDABS( VT(I) )
	PVAR(I,6)   = PE * 100.0
	PVAR(I,7)   = QE * 100.0
	PVAR(I,8)   = EF(I)
	PVAR(I,9)   = PM(I) * 100.0
	PVAR(I,10)  = CSAT(I)
	PVAR(I,11)  = EAT
	PVAR(I,13)  = EQPP
	RETURN
C
	ENTRY initializegenmdl4(I)

	CSAT(I) = 1.0
	DO J    = 1, 3
	XDS     = CSAT(I)*XD(I)+(1.0-CSAT(I))*XL(I)
	XQS     = CSAT(I)*XQ(I)+(1.0-CSAT(I))*XL(I)
	EQD     = VT(I) + DCMPLX( R(I), XQS ) * CT(I)
	DEL     = DATAN2( DIMAG(EQD), DREAL(EQD) )
	CURR    = CT(I) * DCMPLX( DSIN(DEL), DCOS(DEL) )
	ID      = DREAL( CURR )
	IQ      = DIMAG( CURR )
	VOLT    = VT(I) * DCMPLX( DSIN(DEL), DCOS(DEL) )
	VD      = DREAL( VOLT )
	VQ      = DIMAG( VOLT )
	EQ      = VQ + XD1(I) * ID
	EF(I)   = ( EQ + (XDS-XD1(I)) * ID) / CSAT(I)
	EQPP    = VQ + XD11(I) * ID
	ED      = VD - XQ11(I) * IQ
	EAQ     = EQPP - ( XD11(I) - XL(I) ) * ID
	EAD     = ED   + ( XQ11(I) - XL(I) ) * IQ
	EAT     = DSQRT( EAQ ** 2 + EAD ** 2  )
	CSAT(I) = 1.0 / ( 1.0 + C1(I) * DEXP( C2(I) * EAT ) )
	END DO
	PE      = DREAL( VT(I) * DCONJG( CT(I) ) )
     +                     + CDABS( CURR )**2*R(I)
	PM(I)     = PE
	OUT(I,1)  = 0.0
	OUT(I,2)  = DEL
	OUT(I,3)  = EQ
	OUT(I,4)  = ED
	OUT(I,13) = EQPP - EQ
	RETURN
	END

C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~       synchronous machine models:        ~
C   ~              Eq' - Ed'                   ~
C   ~              Eq' -                       ~
C   ~              Eq' = const                 ~
C   ~              infinitive bus              ~
C   ~                                          ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SUBROUTINE generatormodel3(I)
C
	INCLUDE 'CMM.DM'
C
	double precision     TIME

	DOUBLE COMPLEX       EQD, CURR, DCMPLX, DCONJG
	DOUBLE PRECISION     ID , IQ  , OME   , DEL   , EQ  ,
     +                       ED , PE  , QE    , PL    , EAT ,
     +                       XDS, XQS , TD1S  , TQ1S  , EAD , EAQ
C
C   define integrator output
C
	OME = OUT(I,1)
	DEL = OUT(I,2)
	EQ  = OUT(I,3)
	ED  = OUT(I,4)
C
C   transform current to m/c reference
C
	CURR = CT(I)*DCMPLX( DSIN(DEL), DCOS(DEL) )
	ID   = DREAL( CURR )
	IQ   = DIMAG( CURR )
C
C   compute power and loss
C
	PE  = DREAL( VT(I)*DCONJG(CT(I)) )
	QE  = DIMAG( VT(I)*DCONJG(CT(I)) )
	PL  = CDABS( CURR )**2*R(I)
C
C   adjust reactance and time constant
C
	XDS  = CSAT(I)*XD(I)+(1.0-CSAT(I))*XL(I)
	XQS  = CSAT(I)*XQ(I)+(1.0-CSAT(I))*XL(I)

	IF( XQ1(I).EQ.XQ(I) )  XQS = XQ(I)

	TD1S = TD1(I)*(1.0-(1.0-CSAT(I))*(XD(I)-XD1(I))/(XD(I)-XL(I)))
	TQ1S = TQ1(I)*(1.0-(1.0-CSAT(I))*(XQ(I)-XQ1(I))/(XQ(I)-XL(I)))
	EAQ  = EQ-( XD1(I)-XL(I) )*ID
	EAD  = ED+( XQ1(I)-XL(I) )*IQ

	IF( XQ1(I).EQ.XQ(I) )  EAD = 0.0

	EAT     = DSQRT( EAQ**2+EAD**2 )
	CSAT(I) = 1.0/(1.0+C1(I)*DEXP(C2(I)*EAT))
C
C   define integrator input and print out
C     
	PLUG(I,1)  = (PM(I)-PE-PL-DAMP(I)*OME)/(2.0*H(I))
	PLUG(I,2)  =  377.0*OME
	PLUG(I,3)  = (CSAT(I)*EF(I)-EQ-(XDS-XD1(I))*ID)/TD1S
	PLUG(I,4)  = (-ED+(XQS-XQ1(I))*IQ)/TQ1S
	PVAR(I,1)  = DEL*180.0/3.1416
	PVAR(I,2)  = OME
	PVAR(I,3)  = EQ
C       PVAR(I,4)  = ED
	PVAR(I,4)  = CDABS( CT(I) )
	PVAR(I,5)  = CDABS( VT(I) )
	PVAR(I,6)  = PE*100.0   
	PVAR(I,7)  = QE*100.0   
	PVAR(I,8)  = EF(I)
	PVAR(I,9)  = PM(I)*100.0   
	PVAR(I,10) = CSAT(I)
	PVAR(I,11) = EAT
	RETURN
C
	ENTRY initializegenmdl3(I)

	CSAT(I) = 1.0
	DO J = 1 , 3
	  XDS = CSAT(I)*XD(I)+(1.0-CSAT(I))*XL(I)
	  XQS = CSAT(I)*XQ(I)+(1.0-CSAT(I))*XL(I)
	  IF(  XQ1(I).EQ.XQ(I)  )  XQS = XQ(I)
C
C   compute the angle of generator's q - axis
C
	  EQD = VT(I)+DCMPLX( R(I), XQS )*CT(I)
	  DEL = DATAN2( DIMAG(EQD), DREAL(EQD) )
C
C   transform current to synchronous reference
C
	  CURR = CT(I)*DCMPLX(DSIN(DEL),DCOS(DEL))
	  ID   = DREAL(CURR)
	  IQ   = DIMAG(CURR)
	  EQ   = CDABS(EQD)-(XQS-XD1(I))*ID
	  EF(I)= (EQ+(XDS-XD1(I))*ID)/CSAT(I)
	  ED   = (XQS-XQ1(I))*IQ
	  EAQ  = EQ-(XD1(I)-XL(I))*ID
	  EAD  = ED+(XQ1(I)-XL(I))*IQ

	  IF( XQ1(I).EQ.XQ(I) ) EAD = 0.0

	  EAT  = DSQRT( EAQ**2+EAD**2 )
	  CSAT(I) = 1.0 / ( 1.0+C1(I)*DEXP(C2(I)*EAT) )
	END DO
	PE       = DREAL( VT(I)*DCONJG(CT(I)) )+CDABS(CURR)**2*R(I)
	PM(I)    = PE
	OUT(I,1) = 0.0
	OUT(I,2) = DEL
	OUT(I,3) = EQ
	OUT(I,4) = ED
	RETURN
	END
