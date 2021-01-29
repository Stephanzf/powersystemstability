
C  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C  ~ This routine calculate the equivelant admittance  ~
C  ~      of generator internal buses                  ~
C  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE internalymatrix
C
	INCLUDE 'CMM.DM'
C
	double precision     TIME , TSTEP, TPRINT

	DOUBLE COMPLEX       DCMPLX
C
C   augment  Y with generator buses and eliminate terminal bus
C
      IF (TIME.EQ.0.0) THEN
	DO I = 1, NGEN
	  JJ = GTYPE(I,1)
	  IF (JJ.EQ.4) THEN
	    XDD = XD11(I)
	    XQQ = XQ11(I)
	  END IF
	  IF (JJ.LE.3) THEN
	    XDD = XD1(I)
	    XQQ = XQ1(I)
	  END IF
	  TEMP = 0.0 
	SCALE(I) = DCMPLX(TEMP,(XQQ-XDD)*0.5)/
     +             DCMPLX(R(I),-(XQQ+XDD)*0.5 )
	YFICT(I) = DCMPLX(R(I),-(XDD+XQQ)/2.0)/(R(I)*R(I)+XDD*XQQ) 
	END DO
      END IF
C
	N1 = NGEN + 1
	DO 88 I = 1, NGEN
	  Y(N1,N1) = Y(I,I)
	  Y(I,I)   = (0.0,0.0)
	  DO J = 1, NGEN
	      Y(N1,J)  = Y(I,J)
	      Y(I,J)   = (0.0,0.0)
	      Y(J,N1)  = Y(J,I)
	      Y(J,I)   = (0.0,0.0)
	  END DO
C
C   add in generator bus
C
	  Y(I,I)  = YFICT(I)
	  IF ( CDABS(Y(N1,N1)).EQ.0.0 )  GOTO 88
	  Y(N1,N1) =  Y(N1,N1)+YFICT(I)
	  Y(I,N1)  = -YFICT(I)
	  Y(N1,I)  = -YFICT(I)
C
C   eliminate terminal buses
C    
	  DO M = 1, NGEN
	     DO N = M, NGEN
	     Y(M,N) = Y(M,N)-Y(M,N1)*Y(N1,N)/Y(N1,N1)
	     Y(N,M) = Y(M,N)
	     END DO
	  END DO
88      CONTINUE
	RETURN
	END
