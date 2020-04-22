      program master
      implicit none 
      real*8 Q,z
      real*8 u ,ub ,d ,db ,s ,sb ,g
      real*8 pref
      real*8 ML, MD2
      integer i

      MD2  =  0.118d0
      ML = 1.115683d0

      open(1, FILE ='PFF.dat')
      write(1,*) 'z ', 'u ', 'd ',
     $           's ', 'sea '


      Q = 10.58d0
      do i = 1,50
      z = 0.20+(i-1)*0.008
      pref = MD2/2d0/z/z/ML/ML
      call PFFCKT(z,Q,u,ub,d,db,s,sb)
      write(1,*) z,z*pref*u,z*pref*d,z*pref*s,
     $           z*pref*(ub+db+sb)/3d0
      enddo
      close(1)

      return
      end
