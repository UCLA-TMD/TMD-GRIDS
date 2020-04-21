      subroutine PFFCKT(x,Q,u,ub,d,db,s,sb)
      implicit none
      real*8 x,Q
      real*8 u,d,s,sb,db,ub
      integer NX, NQ
      integer IIREAD
      integer NARG,IX,IQ,N,M
      parameter (NX=100, NQ=100, NARG=2)
      integer NA(NARG)
      integer PFFINI,PPFFINI
      COMMON / PFFRAGINI / PFFINI
      double precision ARRF(NX+NQ)
      data NA/NQ,NX/
      double precision XT(NARG), XB(NX), QQ(NQ)
      double precision DBFINT
      external DBFINT
      double precision FPARTON(6,NQ,NX-1)
      real*8 uparton(NX ,NQ), ubparton(NX,NQ)
      real*8 dparton(NX ,NQ), dbparton(NX,NQ)
      real*8 sparton(NX ,NQ), sbparton(NX,NQ)
      save UPARTON
      save DPARTON
      save SPARTON
      save UBPARTON
      save DBPARTON
      save SBPARTON
      save ARRF

      DATA XB/
     $  0.2	  , 0.2040404 , 0.20808081, 0.21212121, 0.21616162,
     $  0.22020202, 0.22424242, 0.22828283, 0.23232323, 0.23636364,
     $  0.24040404, 0.24444444, 0.24848485, 0.25252525, 0.25656566,
     $  0.26060606, 0.26464646, 0.26868687, 0.27272727, 0.27676768,
     $  0.28080808, 0.28484848, 0.28888889, 0.29292929, 0.2969697 ,
     $  0.3010101 , 0.30505051, 0.30909091, 0.31313131, 0.31717172,
     $  0.32121212, 0.32525253, 0.32929293, 0.33333333, 0.33737374,
     $  0.34141414, 0.34545455, 0.34949495, 0.35353535, 0.35757576,
     $  0.36161616, 0.36565657, 0.36969697, 0.37373737, 0.37777778,
     $  0.38181818, 0.38585859, 0.38989899, 0.39393939, 0.3979798 ,
     $  0.4020202 , 0.40606061, 0.41010101, 0.41414141, 0.41818182,
     $  0.42222222, 0.42626263, 0.43030303, 0.43434343, 0.43838384,
     $  0.44242424, 0.44646465, 0.45050505, 0.45454545, 0.45858586,
     $  0.46262626, 0.46666667, 0.47070707, 0.47474747, 0.47878788,
     $  0.48282828, 0.48686869, 0.49090909, 0.49494949, 0.4989899 ,
     $  0.5030303 , 0.50707071, 0.51111111, 0.51515152, 0.51919192,
     $  0.52323232, 0.52727273, 0.53131313, 0.53535354, 0.53939394,
     $  0.54343434, 0.54747475, 0.55151515, 0.55555556, 0.55959596,
     $  0.56363636, 0.56767677, 0.57171717, 0.57575758, 0.57979798,
     $  0.58383838, 0.58787879, 0.59191919, 0.5959596 , 0.6
     $ /

      DATA QQ /
     $    5.        ,   5.23807876,   5.48749383,   5.74878498,
     $    6.0225177 ,   6.30928442,   6.60970574,   6.92443186,
     $    7.25414389,   7.59955541,   7.96141397,   8.34050269,
     $    8.737642  ,   9.1536914 ,   9.58955131,  10.04616501,
     $   10.52452072,  11.0256537 ,  11.5506485 ,  12.10064132,
     $   12.67682247,  13.28043891,  13.91279701,  14.57526531,
     $   15.26927754,  15.99633569,  16.75801325,  17.55595867,
     $   18.39189886,  19.26764297,  20.18508629,  21.14621437,
     $   22.15310729,  23.20794417,  24.3130079 ,  25.47069007,
     $   26.68349616,  27.95405091,  29.28510409,  30.67953637,
     $   32.14036559,  33.67075329,  35.27401155,  36.95361017,
     $   38.71318413,  40.55654154,  42.4876718 ,  44.51075427,
     $   46.63016734,  48.85049786,  51.17655109,  53.6133611 ,
     $   56.16620165,  58.84059762,  61.64233697,  64.57748325,
     $   67.65238873,  70.87370815,  74.24841311,  77.7838072 ,
     $   81.48754173,  85.36763237,  89.43247645,  93.69087114,
     $   98.1520325 , 102.82561542, 107.7217345 , 112.85098598,
     $  118.22447063, 123.8538178 , 129.75121057, 135.92941214,
     $  142.40179342, 149.18236201, 156.28579248, 163.72745814,
     $  171.52346432, 179.69068319, 188.24679034, 197.21030297,
     $  206.60062001, 216.43806405, 226.74392541, 237.54050811,
     $  248.85117822, 260.7004144 , 273.11386088, 286.11838297,
     $  299.74212516, 314.01457209, 328.96661233, 344.63060522,
     $  361.04045092, 378.23166378, 396.24144918, 415.10878407,
     $  434.87450131, 455.58137806, 477.27422833, 500.
     $ /

*...CHECK OF X AND Q2 VALUES :
       IF ( (X.LT.0.2) .OR. (X.GT.0.6) ) THEN
           WRITE(6,91)
  91	   FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
C          STOP
       ENDIF
       IF ((Q.LT.dsqrt(5d0)).or.(Q.GT.dsqrt(500d0))) THEN
           WRITE(6,92)
  92	   FORMAT (2X,'PARTON INTERPOLATION: Q OUT OF RANGE')
C          STOP
       ENDIF

*.....PFFINI is 1 if grids has been previously initialized
      PPFFINI = PFFINI

*.....Check if grid has been initialized, if not then initialize
      IF (PPFFINI.NE.0) GOTO 16
      print *, 'Initializing PFF grid'
      IIREAD=11       
      OPEN(IIREAD,FILE='PFF.grid')

       DO 15 M = 1, NX-1 
       DO 15 N = 1, NQ
       READ(IIREAD,90) FPARTON(1,N,M), FPARTON(2,N,M), FPARTON(3,N,M), 
     1                 FPARTON(4,N,M), FPARTON(5,N,M), FPARTON(6,N,M)
  90  FORMAT (6(E14.5))
  15   CONTINUE
       CLOSE(IIREAD)

      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        UPARTON(IX,IQ)  = FPARTON(1,IQ,IX)
        DPARTON(IX,IQ)  = FPARTON(2,IQ,IX)
        SPARTON(IX,IQ)  = FPARTON(3,IQ,IX)
        UBPARTON(IX,IQ) = FPARTON(6,IQ,IX)
        DBPARTON(IX,IQ) = FPARTON(5,IQ,IX)
        SBPARTON(IX,IQ) = FPARTON(4,IQ,IX)
  20  CONTINUE
        UPARTON(NX ,IQ) = 0d0
        DPARTON(NX ,IQ) = 0d0              
        SPARTON(NX ,IQ) = 0d0             
        UBPARTON(NX,IQ) = 0d0             
        DBPARTON(NX,IQ) = 0d0            
        SBPARTON(NX,IQ) = 0d0
  10  CONTINUE

*.....Tell main program that grid has been initialized
      PFFINI = 1

      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = XB(IX)
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = dsqrt(QQ(IQ))
  40  CONTINUE

  16  CONTINUE

*...INTERPOLATION :
      XT(1) = X
      XT(2) = Q

      u = DBFINT(NARG,XT,NA,ARRF,UPARTON)
      d = DBFINT(NARG,XT,NA,ARRF,DPARTON)
      s = DBFINT(NARG,XT,NA,ARRF,SPARTON)
      ub= DBFINT(NARG,XT,NA,ARRF,UBPARTON)
      db= DBFINT(NARG,XT,NA,ARRF,DBPARTON)
      sb= DBFINT(NARG,XT,NA,ARRF,SBPARTON)

      return
      end

c---------------------------------------------------------------

      DOUBLE PRECISION FUNCTION DBFINT(NARG,ARG,NA,ENT,TABLE)
      implicit real*8 (a-h,o-z)
      INTEGER NA(NARG), INDEX(32)
      double precision
     +       ARG(NARG),ENT(32),TABLE(10),WEIGHT(32)
      DATA ZEROD/0.D0/ONED/1.D0/
C

           DBFINT =  ZEROD
           IF(NARG .LT. 1  .OR.  NARG .GT. 5)  RETURN
C
           LMAX      =  0
           ISTEP     =  1
           KNOTS     =  1
           INDEX(1)  =  1
           WEIGHT(1) =  ONED
           DO 100    N  =  1, NARG
              X     =  ARG(N)
              NDIM  =  NA(N)
              LOCA  =  LMAX
              LMIN  =  LMAX + 1
              LMAX  =  LMAX + NDIM
              IF(NDIM .GT. 2)  GOTO 10
              IF(NDIM .EQ. 1)  GOTO 100
              H  =  X - ENT(LMIN)
              IF(H .EQ. ZEROD)  GOTO 90
              ISHIFT  =  ISTEP
              IF(X-ENT(LMIN+1) .EQ. ZEROD)  GOTO 21
              ISHIFT  =  0
              ETA     =  H / (ENT(LMIN+1) - ENT(LMIN))
              GOTO 30
   10         LOCB  =  LMAX + 1
   11         LOCC  =  (LOCA+LOCB) / 2
              IF(X-ENT(LOCC))  12, 20, 13
   12         LOCB  =  LOCC
              GOTO 14
   13         LOCA  =  LOCC
   14         IF(LOCB-LOCA .GT. 1)  GOTO 11
              LOCA    =  MIN ( MAX (LOCA,LMIN), LMAX-1 )
              ISHIFT  =  (LOCA - LMIN) * ISTEP
              ETA     =  (X - ENT(LOCA)) / (ENT(LOCA+1) - ENT(LOCA))
              GOTO 30
   20         ISHIFT  =  (LOCC - LMIN) * ISTEP
   21         DO 22  K  =  1, KNOTS
                 INDEX(K)  =  INDEX(K) + ISHIFT
   22            CONTINUE
              GOTO 90
   30         DO 31  K  =  1, KNOTS
                 INDEX(K)         =  INDEX(K) + ISHIFT
                 INDEX(K+KNOTS)   =  INDEX(K) + ISTEP
                 WEIGHT(K+KNOTS)  =  WEIGHT(K) * ETA
                 WEIGHT(K)        =  WEIGHT(K) - WEIGHT(K+KNOTS)
   31            CONTINUE
              KNOTS  =  2*KNOTS
   90         ISTEP  =  ISTEP * NDIM
  100         CONTINUE
           DO 200    K  =  1, KNOTS
              I  =  INDEX(K)
              DBFINT =  DBFINT + WEIGHT(K) * TABLE(I)
  200         CONTINUE
           RETURN
           END
