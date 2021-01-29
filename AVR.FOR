C ----------------------------------------------------------       
C
C       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C       ~  IEEE excitation system    ~
C       ~  model DC1 (model no 11)   ~
C       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
C ----------------------------------------------------------       
        SUBROUTINE exciterdc1(I)
C
	INCLUDE 'CMM.DM'
C
	double precision X1 , X2 , X3, X5, X6, X7, X8
	double precision X41, X42
C
C   define integrator output  
C
	EF(I) = OUT(I,5)
	X5    = OUT(I,6)
	X3    = OUT(I,7)
C
C   calculating intermediate variables
C         
	X1  = VREF(I)-CDABS(VT(I))
	X2  = AVR(I,3)/AVR(I,8)*EF(I)-X3
	X41 = X1-X2+VS(I)
	X42 = OUT(I,8)+AVR(I,4)/AVR(I,5)*X41
	X6  = X5
	X1  = AVR(I,1)/AVR(I,6)*X42-X5/AVR(I,6)
C        
C   non-windup limiter 
C
	IF( (X6.LE.AVR(I,9 )).AND.(X1.LT.0.0) ) THEN
	   X1 = 0.0
	   X6 = AVR(I,9)
	ENDIF
	IF( (X6.GE.AVR(I,10)).AND.(X1.GT.0.0) ) THEN 
	   X1 = 0.0
	   X6 = AVR(I,10)
	ENDIF

	X7  = SE(I)*EF(I)
	X8  = X6-X7
C
C   define integrator inputs
C
	PLUG(I,5) = X8/AVR(I,7)-AVR(I,2)/AVR(I,7)*EF(I)
	PLUG(I,6) = X1
	PLUG(I,7) = X2/AVR(I,8)
	PLUG(I,8) = X41/AVR(I,5)-X42/AVR(I,5)
	RETURN
C
C   init ialize the exciter
C
	ENTRY initializedc1(I)

	IF ( AVR(I,4).EQ.0.0 ) AVR(I,4) = 1.0E+6
	IF ( AVR(I,5).EQ.0.0 ) AVR(I,5) = 1.0E+6
	SE(I)    = AVR(I,11) * DEXP( AVR(I,12) * EF(I)  )
	VREF(I)  = CDABS(VT(I))+(AVR(I,2)+SE(I))*EF(I)/AVR(I,1)
	OUT(I,5) = EF(I)
	OUT(I,6) = EF(I)*(AVR(I,2)+SE(I))
	OUT(I,7) = EF(I)*AVR(I,3)/AVR(I,8)
	OUT(I,8) = EF(I)*(AVR(I,2)+SE(I))/AVR(I,1)*(1.0-AVR(I,4)
     +                                   /AVR(I,5))
	IF(OUT(I,6) .LT.AVR(I,9))  WRITE(*,10) I
	IF(OUT(I,6) .GT.AVR(I,10)) WRITE(*,10) I

10      FORMAT(' AVR limits exceeded at machine',i3/)

	RETURN
	END     

C ----------------------------------------------------------       
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~  IEEE excitation systems   ~
C   ~  model DC2 (model no 12)   ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
C ----------------------------------------------------------       
C        
	SUBROUTINE exciterdc2(I)
C
	INCLUDE 'CMM.DM'
C
	double precision X1 , X2 , X3, X5, X6, X7, X8
	double precision X41, X42
C
C   define integrator output  
C
	X5    = OUT(I,6)
	EF(I) = OUT(I,5)
	X3    = OUT(I,7)
C
C   calculating intermediate variables
C
	VMAG  = CDABS(VT(I))
	X1    = VREF(I)-CDABS(VT(I))
	X2    = AVR(I,3)/AVR(I,8)*EF(I)-X3
	X41   = X1-X2+VS(I)
	X42   = OUT(I,8)+AVR(I,4)/AVR(I,5)*X41
	X6    = X5
	X1    = AVR(I,1)/AVR(I,6)*X42-X5/AVR(I,6)
C        
C   non-windup limiter 
C
	IF( (X6.LE.AVR(I,9)*VMAG).AND.(X1.LT.0.0) ) THEN
	   X1 = 0.0
	   X6 = AVR(I,9)*VMAG
	ENDIF
	IF( (X6.GE.AVR(I,10)*VMAG).AND.(X1.GT.0.0) ) THEN 
	   X1 = 0.0
	   X6 = AVR(I,10)*VMAG
	ENDIF

	X7   = SE(I)*EF(I)
	X8   = X6-X7
C
C   define integrator inputs
C
	PLUG(I,5) = X8/AVR(I,7)-AVR(I,2)/AVR(I,7)*EF(I)
	PLUG(I,6) = X1
	PLUG(I,7) = X2/AVR(I,8)
	PLUG(I,8) = X41/AVR(I,5)-X42/AVR(I,5)
	RETURN
C
C   initialize the exciter
C
	ENTRY initializedc2(I)
C
C   set default time constants     
C
	IF (AVR(I,4).EQ.0.0) AVR(I,4)=1.0E+6
	IF (AVR(I,5).EQ.0.0) AVR(I,5)=1.0E+6

	VMAG     = CDABS(VT(I))
	SE(I)    = AVR(I,11)*EXP(AVR(I,12)*EF(I))
	VREF(I)  = CDABS(VT(I))+(AVR(I,2)+SE(I))*EF(I)/AVR(I,1)
	OUT(I,5) = EF(I)
	OUT(I,6) = EF(I)*(AVR(I,2)+SE(I))
	OUT(I,7) = EF(I)*AVR(I,3)/AVR(I,8)
	OUT(I,8) = EF(I)*(AVR(I,2)+SE(I))/AVR(I,1)*(1.0-AVR(I,4)
     +                                   /AVR(I,5))
	IF(OUT(I,6) .LT.AVR(I,9 )*VMAG)  WRITE(*,10) I
	IF(OUT(I,6) .GT.AVR(I,10)*VMAG)  WRITE(*,10) I

10      FORMAT(' AVR limits exceeded at machine',i3/)
	RETURN
	END

C ----------------------------------------------------------       
C
C       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C       ~  IEEE excitation systems   ~
C       ~  model DC3 (model no 13)   ~
C       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
C ----------------------------------------------------------       
C        
	SUBROUTINE exciterdc3(I)
C
	INCLUDE 'CMM.DM'
C
	double precision X1 , X2 , X3, X5, X6, VR
C
C   define integrator output  
C
	X5    = OUT(I,5)
	X6    = OUT(I,6)
C
C   calculating intermediate variables
C
	X1    = VREF(I)-CDABS(VT(I))+VS(I)
C
C   X1 (Verr) must be in between Kv and (-Kv)
C
	IF (X1.GE.AVR(I,2))   X2 = AVR(I,2)
	IF (X1.LE.-AVR(I,2))  X2 = -AVR(I,2)
	
	X3 = X2*( AVR(I,6)-AVR(I,5) )/(AVR(I,2)*AVR(I,3))
C        
C   non-windup limiter 
C
	IF( (X6.LE.AVR(I,5)).AND.(X3.LT.0.0) ) THEN
	   X3 = 0.0
	   X6 = AVR(I,5)
	ENDIF
	IF( (X6.GE.AVR(I,6)).AND.(X3.GT.0.0) ) THEN 
	   X3 = 0.0
	   X6 = AVR(I,6)
	ENDIF
C
C   logic circuit
C
	VR = X6
	IF (X1.GE.AVR(I,2))   VR = AVR(I,2)
	IF (X1.LE.-AVR(I,2))  VR = -AVR(I,2)
C
C   define integrator inputs
C
	X4 = VR - X5*(AVR(I,1) + SE(I))
	PLUG(I,5) = X4/AVR(I,4)
	PLUG(I,6) = X3
	RETURN
C
C   initialize the exciter
C
	ENTRY initializedc3(I)

	SE(I)    = AVR(I,7)*EXP(AVR(I,8)*EF(I))
	VREF(I)  = CDABS(VT(I))
	OUT(I,5) = EF(I)
	OUT(I,6) = 0.0
	
	RETURN
	END

C ----------------------------------------------------------       
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~  IEEE excitation systems   ~
C   ~  model AC1 (model no 21)   ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C        
C ----------------------------------------------------------       
C        
C        SUBROUTINE exciterac1(I)
C
C        INCLUDE 'CMM.DM'
C
C        double precision X1 , X2 , X3, X5, X6, X7, X8
C
C   define integrator output  
C

C
C   calculating intermediate variables
C

C        
C   non-windup limiter 
C

C
C   define integrator inputs
C
	
C        RETURN

C
C   initialize the exciter
C
C        ENTRY initializeac1(I)
	
C        RETURN
C        END

C ----------------------------------------------------------       
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~  IEEE excitation systems   ~
C   ~  model AC2 (model no 22)   ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C        
C ----------------------------------------------------------       
C        
C        SUBROUTINE exciterac2(I)
C
C        INCLUDE 'CMM.DM'
C
C        double precision X1 , X2 , X3, X5, X6, X7, X8
C
C   define integrator output  
C

C
C   calculating intermediate variables
C

C        
C   non-windup limiter 
C

C
C   define integrator inputs
C
	
C        RETURN

C
C   initialize the exciter
C
C        ENTRY initializeac2(I)
	
C        RETURN
C        END

C ----------------------------------------------------------       
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~  IEEE excitation systems   ~
C   ~  model AC3 (model no 23)   ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C        
C ----------------------------------------------------------       
C        
C        SUBROUTINE exciterac3(I)
C
C        INCLUDE 'CMM.DM'
C
C        double precision X1 , X2 , X3, X5, X6, X7, X8
C
C   define integrator output  
C

C
C   calculating intermediate variables
C

C        
C   non-windup limiter 
C

C
C   define integrator inputs
C
	
C        RETURN

C
C   initialize the exciter
C
C        ENTRY initializeac3(I)
	
C        RETURN
C        END

C ----------------------------------------------------------       
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~  IEEE excitation systems   ~
C   ~  model AC4 (model no 24)   ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
C ----------------------------------------------------------       
C        
	SUBROUTINE exciterac4(I)
C
	INCLUDE 'CMM.DM'
C
        double precision X1 , X2 , X3 , X4 ,  X5
	double precision X5MAX, X5MIN
C
C   define integrator output  
C
	X5 = OUT(I,5)
	X3 = OUT(I,6)
C
C   calculate intermediate variables
C
	VMAG=CDABS(VT(I))
	X5MAX=AVR(I,9)*VMAG-AVR(I,2)*IFD(I)
	X5MIN=AVR(I,8)*VMAG-AVR(I,2)*IFD(I)
	
	X6 = X5
	IF (X5.GE.X5MAX) X6 = X5MAX
	IF (X5.LE.X5MIN) X6 = X5MIN
	
	X1 = VREF(I) - VMAG + VS(I) 
	X2 = X1
	IF (X1.GE.AVR(I,7)) X2 = AVR(I,7)
	IF (X1.LE.AVR(I,6)) X2 = AVR(I,6)

	X4 = X3 + AVR(I,3)/AVR(I,4)*X2
C
C   define integrator inputs
C
	PLUG(I,5) = AVR(I,1)/AVR(I,5)*X4 - X5/AVR(I,5)
	PLUG(I,6) = (X2 - X4)/AVR(I,4) 
	RETURN
C
C   initialize the exciter
C
        ENTRY initializeac4(I)

C
C   set default time constants     
C
	IF (AVR(I,3).EQ.0.0) AVR(I,3)=1.0E+6
	IF (AVR(I,4).EQ.0.0) AVR(I,4)=1.0E+6
	
	VMAG = CDABS(VT(I)) 
	VREF(I)=VMAG+EF(I)/AVR(I,1)*AVR(I,4)/AVR(I,3)
	
	OUT(I,5)=EF(I)
	OUT(I,6)=EF(I)/AVR(I,1)*(1.0-AVR(I,3)/AVR(I,4))

        X5MAX=AVR(I,9 )-AVR(I,2)*IFD(I)
        X5MIN=AVR(I,8 )-AVR(I,2)*IFD(I)
	
	IF(OUT(I,5).LT.X5MIN) WRITE(*,10) I
	IF(OUT(I,5).GT.X5MAX) WRITE(*,10) I

10      FORMAT(' AVR limits exceeded at machine',i3/)
	
	RETURN
	END

C ----------------------------------------------------------       
C
C       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C       ~  IEEE excitation systems   ~
C       ~  model ST1 (model no 31)   ~
C       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
C ----------------------------------------------------------       
C        
	SUBROUTINE exciterst1(I)
C
	INCLUDE 'CMM.DM'
C
	double precision X1 , X2 , X3 , X4 ,  X5 , X7 , X8
	double precision X5MAX, X5MIN
C
C   define integrator output  
C
	X5 = OUT(I,5)
	X3 = OUT(I,6)
	X7 = OUT(I,7)
C
C   calculate intermediate variables
C
	VMAG=CDABS(VT(I))
	X5MAX=AVR(I,11)*VMAG-AVR(I,2)*IFD(I)
	X5MIN=AVR(I,10)*VMAG-AVR(I,2)*IFD(I)
	
	X6 = X5
	IF (X5.GE.X5MAX) X6 = X5MAX
	IF (X5.LE.X5MIN) X6 = X5MIN
	
	X8 = X5*AVR(I,3)/AVR(I,7) - X7
	X1 = VREF(I) - VMAG + VS(I) - X8
	X2 = X1
	IF (X1.GE.AVR(I,9)) X2 = AVR(I,9)
	IF (X1.LE.AVR(I,8)) X2 = AVR(I,8)

	X4 = X3 + AVR(I,4)/AVR(I,5)*X2
C
C   define integrator inputs
C
	PLUG(I,5) = AVR(I,1)/AVR(I,6)*X4 - X5/AVR(I,6)
	PLUG(I,6) = (X2 - X4)/AVR(I,5) 
	PLUG(I,7) = X8/AVR(I,7)
	RETURN
C
C   initialize the exciter
C
	ENTRY initializest1(I)

C
C   set default time constants     
C
	IF (AVR(I,4).EQ.0.0) AVR(I,4)=1.0E+6
	IF (AVR(I,5).EQ.0.0) AVR(I,5)=1.0E+6
	
	VMAG = CDABS(VT(I)) 
	VREF(I)=VMAG+EF(I)/AVR(I,1)*AVR(I,5)/AVR(I,4)
	
	OUT(I,5)=EF(I)
	OUT(I,7)=EF(I)*AVR(I,3)/AVR(I,7)
	OUT(I,6)=EF(I)/AVR(I,1)*(1.0-AVR(I,4)/AVR(I,5))

	X5MAX=AVR(I,11)*VMAG-AVR(I,2)*IFD(I)
	X5MIN=AVR(I,10)*VMAG-AVR(I,2)*IFD(I)
	
	IF(OUT(I,5).LT.X5MIN) WRITE(*,10) I
	IF(OUT(I,5).GT.X5MAX) WRITE(*,10) I

10      FORMAT(' AVR limits exceeded at machine',i3/)
	
	RETURN
	END

C ----------------------------------------------------------       
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C   ~  IEEE excitation systems   ~
C   ~  model SCRX (model no 41) ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
C ----------------------------------------------------------       
C        
	SUBROUTINE exciterscrx(I)
C
	INCLUDE 'CMM.DM'
C
	double precision X1  , X2 , X3   , X6   , X7
	double precision VMAG

	INTEGER          busfed   , neglogic
C
C   define integrator output  
C
	X3 = OUT(I,5)
	X6 = OUT(I,6)
	X7 = OUT(I,7)
C
C   calculate intermediate variables
C
	IF ( AVR(I,4).LE. 0.00001 )  X6 = CDABS( VT(I) )
	X1 = VREF(I) -  X6 + VS(I)
	X2 = X7 + X1*AVR(I,5)/AVR(I,6)
	X4 = X3
	IF    ( X4.GT.AVR(I,9) )     X4 = AVR(I,9)
	IF    ( X4.LT.AVR(I,8) )     X4 = AVR(I,8)
C
C   busfed = 1 :     fed from terminal bus
C   busfed = 0 : fed from independend power source
C
	neglogic = AVR(I,2)
	busfed   = AVR(I,3)
	IF ( busfed.EQ.1  )   X4 = X4 * X6
	EF (I) = X4

	IF ( IFD(I).GT.0.0 )  GOTO  10
	IF ( neglogic.EQ.0 )  EF(I) = - X3
10      CONTINUE
C
C   define integrator inputs
C
	PLUG(I,5) = ( AVR(I,1)*X2  - X3 ) / ( AVR(I,7) + 0.00001 )
	PLUG(I,6) = ( CDABS(VT(I)) - X6 ) / ( AVR(I,4) + 0.00001 )
	PLUG(I,7) = ( X1 - X2 ) / ( AVR(I,6) + 0.00001 )
	RETURN
C
C   initialize the exciter
C
	ENTRY initializescrx(I)
C
C   set default time constants     
C
	IF( ( AVR(I,5).EQ.0.0 ).AND.( AVR(I,6).EQ.0.0 ) ) THEN
	    AVR(I,5) = 1.0E+6
	    AVR(I,6) = 1.0E+6
	END IF
	IF ( AVR(I,5).EQ.0.0 ) AVR(I,5) = 0.000001
	VMAG = CDABS( VT(I) )
	X3   = EF(I)
	IF ( AVR(I, 3).EQ. 1.0 ) X3 = X3 / VMAG
	VREF(I)  = VMAG + X3 / ( AVR(I,1) ) * AVR(I,6) / AVR(I,5)
	IF ( AVR(I,5).LE.0.00001 )  VREF(I)  = VMAG + X3 / AVR(I,1)  
	OUT(I,5) = X3
	OUT(I,6) = VMAG
	OUT(I,7) = X3 / AVR(I,1)*( 1.0 - AVR(I,5) / AVR(I,6) )
	IF ( OUT(I,5).LT.AVR(I,8) )  WRITE(*,20) I
	IF ( OUT(I,5).GT.AVR(I,9) )  WRITE(*,20) I

20      FORMAT(' avr linterits exceeded at machine',i3/)
	
	RETURN
	END
