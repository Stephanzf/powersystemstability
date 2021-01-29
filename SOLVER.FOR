
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
C   ~   This subroutine interfaces network   ~
C   ~        and armature equations          ~
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE solventwkarmature
C
       INCLUDE 'CMM.DM'
C
	double precision     TIME , TSTEP, TPRINT

	DOUBLE PRECISION     DI   ,  QI  , VD    , VQ  , VD1 , VQ1,
     +                       XDD  ,  XQQ , ED    , EQ

C
	DO I=1,NGEN
	  DELTA(I) = OUT(I,2)
	  EQ       = OUT(I,3)
	  ED       = OUT(I,4)
	  IF (GTYPE(I,1).EQ.4) EQ = OUT(I,13) + EQ
C                                             
	  THETA(I)  = DELTA(I)-3.1416/2.0
	  E(I)   = DCMPLX(ED,EQ)*DCMPLX(DCOS(THETA(I)),DSIN(THETA(I)))
	  ROTAT1(I) = DCMPLX(DCOS(2.0*THETA(I)),DSIN(2.0*THETA(I))) 
	  ROTAT2(I) = DCMPLX(DCOS(THETA(I)),-DSIN(THETA(I))) 
	END DO
	ITER  = 0
C
C   iteration loop
C
88      CONTINUE
	ITER  = ITER+1
	DO  I = 1, NGEN
	EFICT(I) = E(I)+SCALE(I)*DCONJG(E(I)-VT(I))*ROTAT1(I)
	END DO
	DO I = 1, NGEN
	CT(I) = (0.0,0.0)
	      DO J = 1, NGEN
	      CT(I) = CT(I)+Y(I,J)*EFICT(J)
	      END DO
	END DO
	DO I = 1, NGEN
	VT(I) = EFICT(I)-CT(I)/YFICT(I)
	END DO
C
C   convergence check
C
	NFLAG = 0
	DO I  = 1, NGEN
	  EQ  = OUT(I,3)
	  ED  = OUT(I,4)
	  IF ( GTYPE(I,1).EQ.4 ) EQ = OUT(I,13) + EQ
C
C   transform terminal voltage and current to m/c reference
C
	  DI =  DREAL(CT(I)*ROTAT2(I))
	  QI =  DIMAG(CT(I)*ROTAT2(I))
	  VD =  DREAL(VT(I)*ROTAT2(I))
	  VQ =  DIMAG(VT(I)*ROTAT2(I))
	  JJ =  GTYPE(I,1)
	     IF ( JJ.EQ.4 ) THEN
	     XDD = XD11(I)
	     XQQ = XQ11(I)
	     END IF
	     IF ( JJ.LE.3 ) THEN
	     XDD = XD1(I)
	     XQQ = XQ1(I)
	     END IF
	  VD1 = ED-R(I)*DI+XQQ*QI
	  VQ1 = EQ-R(I)*QI-XDD*DI
	     IF( ( ABS(VD1-VD)+ABS(VQ1-VQ) ).GT.0.0001 )  THEN
	     NFLAG = 1
	     II = I
	     END IF
	END DO
C
C   if not converged, go back for another iteration
C
	IF ( ITER.GE.50 ) GO TO 8
	IF ( NFLAG.EQ.1 ) GO TO 88
	RETURN

8       WRITE( * , 10 ) II
10      FORMAT(' generator no ',I3, ' saliency iteration diverged!'/ )
	WRITE( * , 20 ) "  please check xd, xq, xd', xq ' ... "
20      FORMAT( 2X, A40 )
	STOP
	END
