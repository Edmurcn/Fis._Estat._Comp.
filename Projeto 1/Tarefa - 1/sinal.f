      implicit Real*8 (a-h,o-z)
      parameter (pi = acos(-1.d0))

      open(1,file = "data.in")

      !Impostando parâmetros do sinal

      read(*,*) N, a1, a2, w1, w2, dt
      write(*,*) "N, a1, a2, w1, w2, dt"
      
      w1 = w1*pi
      w2 = w2*pi
        
      t = 0.d0

      !Construção do sinal

      do i= 0, (N-1)
         t = dt*i
         y1 = a1*dcos(w1*t) + a2*dsin(w2*t)
         write(1,*) t, y1
      enddo

      close(1)

      end Program