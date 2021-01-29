
C
C   ZFPAC ww_ploting program -- main
C

      INCLUDE   'FGRAPH.FI'
      INCLUDE   'FGRAPH.FD'

      LOGICAL    monitortype
      EXTERNAL   monitortype
      INTEGER    genenum,      varnum ,  NGEN ,  icon

      COMMON /t2 / genenum   , varnum ,  NGEN ,  icon
      
      IF( monitortype() ) THEN
         CALL setwinenv()
      ELSE
         WRITE (*,*) ' This program requires a CGA, EGA, or',
     +               ' VGA graphics card.'
      END IF
      END

C     
C  This function is used to enter graphics mode for psdswplot.
C
      LOGICAL FUNCTION monitortype()
      INCLUDE  'FGRAPH.FD'
      INTEGER*2            dummy
      RECORD /videoconfig/ screen
      COMMON /t0         / screen
C
C     Set to maximum number of available colors.
C
      CALL getvideoconfig( screen )
      SELECT CASE( screen.adapter )
         CASE( $CGA, $OCGA )
            dummy = setvideomode( $MRES4COLOR )
         CASE( $EGA, $OEGA )
            dummy = setvideomode( $ERESCOLOR )
         CASE( $VGA, $OVGA )
            dummy = setvideomode( $VRES16COLOR )
         CASE DEFAULT
            dummy = 0
      END SELECT
      CALL getvideoconfig( screen )
      monitortype = .TRUE.
      IF( dummy .EQ. 0 ) monitortype = .FALSE.
      END
C
C  This subroutine sets the window for psds curves.  
C
      SUBROUTINE setwinenv()
      INCLUDE  'FGRAPH.FD'
      INTEGER*2            dummy 
      INTEGER*2            xwidth, yheight, cols, rows, halfx, halfy 
      RECORD /videoconfig/ screen
      COMMON /t0         / screen
      COMMON /t5         / xwidth, yheight, cols, rows, halfx, halfy

      CALL clearscreen( $GCLEARSCREEN )
      xwidth  = screen.numxpixels
      yheight = screen.numypixels
      cols    = screen.numtextcols
      rows    = screen.numtextrows
      halfx   = xwidth / 2
      halfy   = (yheight / rows) * (rows / 2)
C
C  set the window enviroment.
C
      CALL setviewport( 120, 0, xwidth - 1, yheight - 1-95 )
      CALL settextwindow( 1, 1, rows , cols  )
      CALL pickout()
      CALL clearscreen( $GCLEARSCREEN )
      CALL simudraw()

      READ (*,*)         ! Wait for ENTER key to be pressed
      dummy = setvideomode( $DEFAULTMODE )
      CALL clearscreen( $GCLEARSCREEN )

      END
C
C  This subroutine sorts out data from file 'simu.out'
C
      SUBROUTINE pickout()
      INCLUDE  'FGRAPH.FD'
      PARAMETER (isimuo=12, iploto=28)
      
      CHARACTER*150 dumcharacter
      CHARACTER*6   statevar
      CHARACTER*4   time , string
      CHARACTER*60  strr
      
      INTEGER*2            status , cursorattr

      INTEGER*2            dummy , i1     , it
      INTEGER*2            xwidth, yheight, cols, rows, halfx, halfy 
      INTEGER              genenum   , varnum  ,  NGEN ,  icon           
      REAL                 temp(20)
      
      DOUBLE PRECISION     var    
      RECORD /rccoord/     curpos
 
      COMMON /t2     /     genenum   , varnum  ,  NGEN ,  icon
      COMMON /t21    /     var(3000,7)
      COMMON /T3     /     statevar(5)
      COMMON /t5     /     xwidth    , yheight , 
     +                     cols      , rows    ,  halfx, halfy
      
      OPEN(isimuo,file='simu.out')
      OPEN(iploto,file='pick.out')

      dummy = settextcolor( 5 )          
      dummy = setwindow( .TRUE., 0.0,-1.0 ,
     +                  1.0, 1.0 )
      dummy = settextcolor( 10 )    
C  ------------------------------
            cursorattr = #0707
      status = settextcursor(  cursorattr  )
cc      status = displaycursor( $GCURSORON )

      CALL settextposition( 5, 30, curpos )
      dumcharacter = '*********************************'
      WRITE(strr, ' (A36)' ) dumcharacter
      CALL outtext (' '//strr )
      CALL settextposition( 6, 30, curpos )
      dumcharacter = '*   Welcome to ZFPAC software!  *'
      WRITE(strr, ' (A36)' ) dumcharacter
      CALL outtext (' '//strr )
      CALL settextposition( 7, 30, curpos )
      dumcharacter = '*********************************'
      WRITE(strr, ' (A36)' ) dumcharacter
      CALL outtext (' '//strr )
      
      dummy = settextcolor( 12 )    
      dummy = rectangle_w( $GBORDER, 0.0 , -1.0 , 1.0, 1.0  )  
C  -----------------------------
      CALL settextposition( 10, 17, curpos )
      status = settextcursor( #0007 )

      dumcharacter= 'Enter the total number of machines:'
      WRITE( strr, '(A36)' ) dumcharacter
      CALL outtext( ' '//strr )
      CALL settextposition( 10, 55, curpos )
      READ (*,*)  NGEN
C  ------------------------------      
      CALL settextposition( 11, 17, curpos )
      dumcharacter= 'Enter the specific machine number:'
      WRITE( strr, '(A36)' ) dumcharacter
      CALL outtext( ' '//strr )
      CALL settextposition( 11, 55, curpos )
      READ (*,*)   genenum
C  ------------------------------
      CALL settextposition( 12, 17, curpos )
      dumcharacter= 'Enter the number of variables:'
      WRITE( strr, '(A31)') dumcharacter
      CALL outtext( ' '//strr )
      CALL settextposition( 12, 55, curpos )
      READ (*,*)  varnum
C  ------------------------------      
      
      CALL settextposition( 14, 17, curpos )
      dumcharacter = 'Input the variable strings:'
      WRITE(strr,'(A30)') dumcharacter
      CALL outtext(' '//strr )
      dummy = settextcolor( 13 )    
      
      CALL settextposition( 15, 17, curpos )
      dumcharacter = "  ang spd Eq' Eq'' Ed' Ed'' Eair"
      WRITE(strr,'(A40)') dumcharacter
      CALL outtext(' '//strr )
      
      CALL settextposition( 16, 17, curpos )
      dumcharacter = '  Vt Efd Pe Qe Pm  Gate Ifd Vpss'
      WRITE(strr,'(A40)') dumcharacter
      CALL outtext(' '//strr )
C  -----------------------------
      dummy = settextcolor( 12 )    

      DO i = 1, varnum  
      IF (i.EQ.1) THEN
      CALL settextposition( 18, 17, curpos )
      WRITE(strr,'(A32)') 'Enter the 1st variable string:'
      CALL outtext(' '//strr)
      END IF

      IF (i.EQ.2) THEN
      CALL settextposition( 19, 17, curpos )
      WRITE(strr,'(A32)') 'Enter the 2nd variable string:'
      CALL outtext(' '//strr )
      END IF

      IF (i.EQ.3) THEN
      CALL settextposition( 20, 17, curpos )
      WRITE(strr,'(A32)') 'Enter the 3rd variable string:'
      CALL outtext(' '//strr )
      END IF
      
      IF (i.EQ.4) THEN
      CALL settextposition( 21, 17, curpos )
      WRITE(strr,'(A32)') 'Enter the 4th variable string:'
      CALL outtext(' '//strr )
      END IF
      
      IF (i.EQ.5) THEN
      CALL settextposition( 22, 17, curpos )
      WRITE(strr, '(A32)' )  'Enter the 5th variable string:'
      CALL outtext(' '//strr )
      END IF
     
      CALL settextposition( 17+i, 55, curpos )
      READ (*,5) statevar(i)
5     FORMAT(A4)
      END DO
      status = displaycursor( $GCURSOROFF )

C  -----------------------------

      string='Time'
      WRITE(iploto,4) string,(statevar(i),i=1,varnum)
4     FORMAT(2x,6x,A4,2x,6(4x,A4,4x) )
      READ (isimuo,2) time
2     FORMAT(8x,A4)
      
      CALL settextposition( 17+varnum+1, 17, curpos )
      dumcharacter=   'Sorting data, please wait ...'   
      WRITE( strr, '(A30)' ) dumcharacter
      CALL outtext( ' '//strr )

      READ (isimuo,10) dumcharacter
10    FORMAT(A136/)

      DO it = 1, 3000
      READ (isimuo, *, END=500, ERR=600) var(it,1)
20    FORMAT(8x,f8.4)

      DO IG=1, NGEN

      READ (isimuo,50) i1, (temp(k), k=1,15)
50        FORMAT(1X,I3,2E12.4,3F8.4,2E12.4,F8.4,
     *               E12.4,4F8.4, 2E12.5)
        IF (IG.EQ.genenum) THEN
        DOj=1, varnum
        IF (statevar(j).EQ.'ang'  )   var(it,j+1)=temp(1)
        IF (statevar(j).EQ.'spd'  ) 
     +                  var(it,j+1) = 376.99112*( 1.0 + temp(2) )
        IF (statevar(j).EQ."Eq'" )    var(it,j+1)=temp(3)
        if((statevar(j).EQ."Ed''" )
     +   .OR.(statevar(j).EQ."Ed'"))  var(it,j+1)=temp(4)
        IF (statevar(j).EQ.'Vt'  )    var(it,j+1)=temp(5)
        IF (statevar(j).EQ.'Pe'  )    var(it,j+1)=temp(6)
        IF (statevar(j).EQ.'Qe'  )    var(it,j+1)=temp(7)
        IF (statevar(j).EQ.'Efd' )    var(it,j+1)=temp(8)
        IF (statevar(j).EQ.'Pm'  )    var(it,j+1)=temp(9)
        IF (statevar(j).EQ.'Eair')    var(it,j+1)=temp(11)
        IF (statevar(j).EQ.'Gate')    var(it,j+1)=temp(12)
        IF (statevar(j).EQ."Eq''")    var(it,j+1)=temp(13)
        IF (statevar(j).EQ.'Ifd' )    var(it,j+1)=temp(14)
        IF (statevar(j).EQ.'Vpss')    var(it,j+1)=temp(15)
        END DO                        

        WRITE(iploto, 70) (var(it,j), j=1,varnum+1) 
70      FORMAT(2x,F12.4,6E12.4)       
        END IF                     
      END DO                    
      END DO                  

500   CONTINUE
      GOTO 700
600   dumcharacter= 'Error in reading simu.out file!' 
      WRITE( strr, '(A60)' ) dumcharacter
      CALL outtext( ' '//strr )

700   CONTINUE
      CLOSE(iploto)
      CLOSE(isimuo)
      RETURN
      END

CC  zfpac - This subroutine calculates and plots .
      SUBROUTINE simudraw()
      INCLUDE  'FGRAPH.FD'
      parameter ( iploto=28 )

      INTEGER*2            dummy , ii
      INTEGER*2            xwidth, yheight, cols, rows, halfx, halfy 
      INTEGER              genenum   , varnum  ,  NGEN ,  icon           
      CHARACTER*6          statevar  , str
      CHARACTER*4          string
      DOUBLE PRECISION     var      ,  xaxis  ,  yaxis , ymaxerr
      DOUBLE PRECISION     ymin     ,  ymax   ,  yrange 
      RECORD /videoconfig/ screen
      RECORD /wxycoord/    wxy
      COMMON /t0     /     screen
c     COMMON /t1     /     iploto
      COMMON /t2     /     genenum   , varnum  ,  NGEN ,  icon
      COMMON /t21    /     var(3000,7)
      COMMON /T3     /     statevar(5)
      COMMON /t4     /     yaxis(5,5), xaxis(5)    
      COMMON /t5     /     xwidth    , yheight , 
     +                     cols      , rows    ,  halfx, halfy
      

      OPEN(iploto,file='pick.out')
      READ(iploto,4) string,(statevar(i),i=1,varnum)
4     format(2x,6x,A4,2x,6(4x,A4,4x) )
C
C  find max/min var.
C
      DO j = 1, varnum
      yaxis(5,j) = 10000.0
      yaxis(1,j) =-10000.0
      END DO

      icon=0
      DO i = 1, 3000
        icon = icon + 1
        READ (iploto, 70, END=77) (var(i,j), j=1,varnum+1)
C       WRITE(*, 70)              (var(i,j), j=1,varnum+1)
70      FORMAT(2x,F12.4,6E12.4)
        DO j = 1, varnum
        IF (var(i,j+1).LT.yaxis(5,j)) yaxis(5,j)=var(i,j+1)
        IF (var(i,j+1).GT.yaxis(1,j)) yaxis(1,j)=var(i,j+1)
        END DO
      END DO
77    CONTINUE

      DO j = 1, varnum
        DO i = 2, 4
        ymax = yaxis(1,j)
        ymin = yaxis(5,j)

        IF (ymax.GE.0.0) THEN
          yrange = ymax - ymin
        ELSE
          yrange = DABS(ymin) + ymax
        END IF
          yaxis(i,j) = yaxis(i-1,j) -yrange/5.0
        END DO

      END DO

      xaxis(1) = 0.0
      xaxis(5) = 0.1*INT2( var(icon-1,1)*10.0 )
      DO i = 2, 4
        xaxis(i) = xaxis(i-1) + xaxis(5)/5.0
      END DO
      
C
C  draw the curves in different colors.       
C       

       ymaxerr = 0.0001
      DO j = 1,varnum
       dummy = setcolor( 6 )
       ymin  = yaxis(5,j)
       ymax  = yaxis(1,j)
       IF ( DABS(yaxis(1,j) - yaxis(5,j) ) .LE. ymaxerr ) THEN
           ymin = ymin -1.0
           ymax = ymax +1.0
       END IF
                   
       ymin = ymin - ymaxerr
       ymax = ymax + ymaxerr

       dummy = setwindow( .TRUE., 0.0, ymin ,
     +                       var(icon-1,1), ymax )
       dummy = rectangle_w( $GBORDER, 0.0 , ymin , 
     +                       var(icon-1,1), ymax )
cc     dummy = setbkcolor( 10 )

       CALL    moveto_w( var(1 , 1), var(1 , j+1) , wxy )   
       dummy =  setcolor(j+1)
          DO i=1,icon-2
          dummy = lineto_w( var(i+1,1), var(i+1,j+1) )
          END DO
      END DO

      DO j = varnum, 1, -1      
            ii =  24 - varnum + j
            CALL settextposition( ii , 45, curpos )
            dummy = settextcolor( j+1 )
            WRITE (str, '( A6 )' )  statevar(j)
            CALL outtext( '---- '// str )
       END DO

      CALL xyaxislabel()  
      END

C
C  This subroutine labels both the x_ and y_axis.
C
      SUBROUTINE xyaxislabel()
      INCLUDE  'FGRAPH.FD'
      INTEGER*2            dummy
      INTEGER*2            xwidth, yheight, cols, rows, halfx, halfy 

      character*11         str1
      CHARACTER*6          str ,  statevar
      CHARACTER*30         strr,  string
      INTEGER              genenum   , varnum  ,  NGEN ,  icon      
      DOUBLE PRECISION     var ,  xaxis    , yaxis
      RECORD /videoconfig/ screen
      RECORD /rccoord/     curpos
      COMMON /t0     /     screen
      COMMON /t2     /     genenum   , varnum  ,  NGEN ,  icon
      COMMON /t21    /     var(3000,7)
      COMMON /T3     /     statevar(5)
      COMMON /t4     /     yaxis(5,5), xaxis(5)    
      COMMON /t5     /     xwidth    , yheight , 
     +                     cols      , rows    ,  halfx, halfy
C
C  print colored labels on both axises.
C  y_axis label    
C       
       DO j = 1 , varnum
          k = 1
         DO i = 1 , 25, 5
            CALL settextposition( i+j-1, 2, curpos )
            dummy = settextcolor( j+1 )
            WRITE (str1, '(F11.4)')  yaxis( k,j)
            CALL outtext( ' ' // str1 )
            k = k + 1         
         END DO
       END DO
C
C  x_ais label       
C
       DO i =1 , 5
         CALL settextposition( 27, i*14.5, curpos )
         dummy = settextcolor( 11 )
         WRITE (str, '(F6.2)') xaxis(i)
         CALL outtext( ' ' // str )
       END DO

         CALL settextposition( 29, 41, curpos )
         dummy = settextcolor( 4 )
         string  = ' time in second '
         WRITE (strr, '(A30)') string
         CALL outtext( ' ' // strr )

C
C     Draw a bordered rectangle around curves.
C
      dummy = setcolor( 7 )
c      dummy = rectangle_w( $GBORDER, -1.00, -1.00, 1.00, 1.00 )
c      dummy = rectangle_w( $GBORDER, -1.02, -1.02, 1.02, 1.02 )

      END
