********************************************************************
*                                                                  *
*    CKT Polarizing Fragmentation Functions for Lambda Baryons     *
*           D.Callos, Z.Kang, J.Terry   (arXiv 2003.04828)         *
*                                                                  *
*              CALL PFFCKT(x,Q,u,ub,d,db,s,sb)                     *
*                                                                  *
*  INPUT:                                                          *
*   x  = hadron momentum fraction (between  0.2    and  0.6 )      *
*   Q  = scale in GeV             (between  2.24   and  22.4)      *
*                                                                  *
*  OUTPUT:                                                         *
*                    U, UB, D, DB, S, SB                           *
*      The fragmentation functions to Lambda baryons               *
*                                                                  *
*   LAMBDABAR:                                                     *
*      For anti-Lambda production use                              *
*              CALL PFFCKT(x,Q,ub,u,db,d,sb,s)                     *
*                                                                  *
*                                                                  *
*   COMMON:  The main program or the calling routine has to have   *
*      a common block  COMMON / PFFRAGINI / PFFINI, and  PFFINI    *
*      has always to be zero when CKT is called for the            *
*      first time or when the SET has been changed.                *
*                                                                  *
********************************************************************

      subroutine PFFCKT(x,Q,u,ub,d,db,s,sb)
      implicit none
      real*8  x,Q
      real*8 u ,d ,s ,sb,db,ub
      integer NX, NQ
      integer  IIREAD
      integer  NARG,IX,IQ,N,M
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
     $  0.2       , 0.20414141, 0.20828283, 0.21242424, 0.21656566,
     $  0.22070707, 0.22484848, 0.2289899 , 0.23313131, 0.23727273,
     $  0.24141414, 0.24555556, 0.24969697, 0.25383838, 0.2579798 ,
     $  0.26212121, 0.26626263, 0.27040404, 0.27454545, 0.27868687,
     $  0.28282828, 0.2869697 , 0.29111111, 0.29525253, 0.29939394,
     $  0.30353535, 0.30767677, 0.31181818, 0.3159596 , 0.32010101,
     $  0.32424242, 0.32838384, 0.33252525, 0.33666667, 0.34080808,
     $  0.34494949, 0.34909091, 0.35323232, 0.35737374, 0.36151515,
     $  0.36565657, 0.36979798, 0.37393939, 0.37808081, 0.38222222,
     $  0.38636364, 0.39050505, 0.39464646, 0.39878788, 0.40292929,
     $  0.40707071, 0.41121212, 0.41535354, 0.41949495, 0.42363636,
     $  0.42777778, 0.43191919, 0.43606061, 0.44020202, 0.44434343,
     $  0.44848485, 0.45262626, 0.45676768, 0.46090909, 0.46505051,
     $  0.46919192, 0.47333333, 0.47747475, 0.48161616, 0.48575758,
     $  0.48989899, 0.4940404 , 0.49818182, 0.50232323, 0.50646465,
     $  0.51060606, 0.51474747, 0.51888889, 0.5230303 , 0.52717172,
     $  0.53131313, 0.53545455, 0.53959596, 0.54373737, 0.54787879,
     $  0.5520202 , 0.55616162, 0.56030303, 0.56444444, 0.56858586,
     $  0.57272727, 0.57686869, 0.5810101 , 0.58515152, 0.58929293,
     $  0.59343434, 0.59757576, 0.60171717, 0.60585859, 0.61
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

********************************************************************
*              CALL TMDPFF(x,Q,kT,u,ub,d,db,s,sb)                  *
*                                                                  *
*  INPUT:                                                          *
*   x  = hadron momentum fraction (between  0.2    and  0.6 )      *
*   Q  = scale in GeV             (between  2.24   and  22.4)      *
*   pT = Transverse momentum of hadron relative to quark           *
*                                                                  *
*  OUTPUT:                                                         *
*                    U, UB, D, DB, S, SB                           *
*      The fragmentation functions to Lambda baryons               *
*                                                                  *
********************************************************************

      subroutine TMDPFF(x,Q,pT,u,ub,d,db,s,sb)
      implicit none
      real*8 x,Q,pT
      real*8 u ,ub ,d ,db ,s ,sb 
      real*8 uc,ubc,dc,dbc,sc,sbc
      real*8 MD2,pTdep,pi

      pi = 4d0*atan(1d0)
      MD2 = 0.118d0

      call PFFCKT(x,Q,uc,ubc,dc,dbc,sc,sbc)

      pTdep = dexp(-pT*pT/MD2)/pi/MD2

      u = uc *pTdep
      d = dc *pTdep
      s = sc *pTdep
      ub= ubc*pTdep
      db= dbc*pTdep
      sb= sbc*pTdep

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
