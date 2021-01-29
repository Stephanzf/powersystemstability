C
C   This subroutine is for network reduction according to
C         fault location and type concerned
C
	SUBROUTINE NWRD ( NBUS, NGEN , IPQLFO )
	PARAMETER ( NBU=65, NG=50,  IZGNDO=18 )
	COMMON/S15/Y(NBU,NBU),BUSTYPE(NBU),   PG(NBU),
     *                QG(NBU), PL(NBU),QL(NBU),VSPEC(NBU),
     *                QMIN(NBU),QMAX(NBU) ,VMAG(NBU), VANG(NBU),
     *                DF(NBU)  ,DX(NBU)   ,A(NBU,NBU)
	COMMON/S17/GENBUS( NG ),V( NBU )
	COMMON/S18/Y0( NBU, NBU )
	COMMON/S19/BUSTYPE0(NBU)
	INTEGER        BUSTYPE , NBUS ,  NGEN , GENBUS  ,  FLTBUS
	COMPLEX        CMPLX   , CONJG,  Y    , Y0      ,  V 
	COMPLEX        CURR    , Zeq
	REAL           VMAG    , VANG ,  PL   , QL , PG ,  QG
	INTEGER        BUSTYPE0 
C
C   open file created by the STCC program        
C
	OPEN (IZGNDO, FILE = ' ZGND.OUT ' , STATUS = ' OLD ' )

	DO I=1,NBUS
	VANG(I) = VANG( I ) * 3.141592/180.0
	V(I)    = VMAG( I ) * CMPLX( COS(VANG(I)), SIN(VANG(I)) )    
	Y(I,I)  = Y(I,I) + CMPLX( PL(I),-QL(I) )*0.01/VMAG(I)**2
	END DO
	
	DO 10 I = 1, NBUS
	BUSTYPE0(I) = BUSTYPE(I)
	DO 10 J = 1, NBUS
10      Y0( I, J ) = Y( I, J )  
C
C   read fault information output from STCC program
C
	LL = 0
5       READ (IZGNDO, 280, END = 7 ) XI, XJ, XL, ST, CT, RT, Zeq
	LL = LL +  1
	GOTO    5
280     FORMAT( 8F10.4 )
7       NFAULT = LL 
	FLTBUS = INT( XI )

	IF ( FLTBUS.EQ.0 ) THEN
	   CALL   KRON ( NBUS , 0  )   
	   CALL PYTIME ( NGEN , 50.0, IPQLFO )
C
C   check if reduction is correct
840        WRITE( *, 850 )
850        FORMAT( 2X, 'COMPUTED GENERATION'/)
	   DO I = 1 , NGEN
	   CURR = ( 0.0 , 0.0 )
	     DO J = 1 , NGEN
	     CURR = CURR + Y( GENBUS(I),GENBUS(J) )*V( GENBUS(J) )
	     ENDDO
	   CURR = V( GENBUS(I) ) * CONJG(CURR)
	   WRITE( * , 900 ) GENBUS(I) , CURR
	   END DO
900        FORMAT(2X,I5,2P2F14.4)
	   RETURN 
	END IF
	IF ( ST.NE.0.0 ) THEN
	   CALL  KRON  ( NBUS , 0 )   
	   CALL PYTIME ( NGEN , ST, IPQLFO )
	END IF
C        
C   start the fault and modify the healthy Y ( Y0 ) matrix
C
	   DO 20  I = 1, NBUS
	   BUSTYPE(I) = BUSTYPE0(I)
	   DO 20  J = 1, NBUS
20         Y( I,J ) = Y0( I, J )  
	IF ( Zeq.EQ.(0.0,0.0) )  THEN
	   DO I=1,NBUS
	   Y( I , FLTBUS ) = ( 0.0, 0.0 )
	   Y( FLTBUS, I  ) = ( 0.0, 0.0 )
	   ENDDO
	ELSE
	   write(*,*) fltbus, Zeq

	   Y( FLTBUS, FLTBUS ) = Y( FLTBUS, FLTBUS ) + 1.0 / Zeq
	END IF
	CALL  KRON  ( NBUS , FLTBUS ) 
	CALL PYTIME ( NGEN , CT, IPQLFO )
C        
C   restore to the healthy Y0 matrix used upto end of simulation
C
	DO 30  I = 1, NBUS
	BUSTYPE(I) = BUSTYPE0(I)
	DO 30  J = 1, NBUS
30      Y( I,J ) = Y0( I, J )  
	CALL  KRON  ( NBUS , 0  ) 
	CALL PYTIME ( NGEN , 50.0, IPQLFO )
C
	CLOSE(IZGNDO)
	RETURN
	END
C
C   This subroutine is a Kron elimilation process  
C
	SUBROUTINE KRON ( NBUS, FLTBUS )
	PARAMETER ( NBU=65 )
	COMMON/S15/Y(NBU,NBU),BUSTYPE(NBU),   PG(NBU),
     *                QG(NBU), PL(NBU),QL(NBU),VSPEC(NBU),
     *                QMIN(NBU),QMAX(NBU) ,VMAG(NBU), VANG(NBU),
     *                DF(NBU)  ,DX(NBU)   ,A(NBU,NBU)

	INTEGER       BUSTYPE  , FLTBUS
	COMPLEX       Y

	DO K = 1, NBUS
	IF( (     K.EQ.FLTBUS      ).OR.
     *      (   BUSTYPE(K).EQ.1  ).OR.
     *      ( CABS(Y(K,K)).EQ.0.0)    )  GOTO   200
	BUSTYPE(K) = -1
	DO I = 1 , NBUS
	 IF (BUSTYPE(I).NE.-1)  THEN
	   DO J = 1 , NBUS
	   IF(BUSTYPE(J).NE.-1)   THEN
	   Y( I,J ) = Y( I,J ) - Y( I,K ) * Y( K,J ) / Y( K,K )
	   ENDIF
	   END DO
	 END IF
	END DO
200     END DO
	RETURN
	END
C           
C   This subroutine prints various Ys and TIME to a file
C
	SUBROUTINE PYTIME ( NGEN , XT, IPQLFO )
	PARAMETER ( NBU=65 , NG=50 )
	COMMON/S15/Y(NBU,NBU),BUSTYPE(NBU),   PG(NBU),
     *                QG(NBU), PL(NBU),QL(NBU),VSPEC(NBU),
     *                QMIN(NBU),QMAX(NBU) ,VMAG(NBU), VANG(NBU),
     *                DF(NBU)  ,DX(NBU)   ,A(NBU,NBU)
	COMMON/S17/GENBUS( NG ),V( NBU )

	INTEGER       BUSTYPE  , GENBUS
	COMPLEX       Y        , V
	REAL          XT   

	DO  I=1,NGEN
	WRITE(IPQLFO,950)( Y( GENBUS(I),GENBUS(J) ),J=1,NGEN)
	ENDDO
	WRITE(IPQLFO,960) XT
950     FORMAT(1X, 15(2F14.6, 2X) )
960     FORMAT(1X, F14.6 ) 
	RETURN
	END
