      implicit Real*8 (a-h,o-z)
      dimension y_sinal(0:1000)
      parameter (pi = acos(-1.d0))
      complex*16 i, y_t, y_i, y_transf(0:1000)

      call cpu_time(t1)

      open(1, file = "data.in")
      open(2, file = "data.out")

      i = (0.d0,1.d0)

      dt = 0.04d0

      !Dados do Sinal

      do l = 0,1000 
          read(1,*,end = 1) a, y_sinal(l)
      enddo	
      1	continue

      N = l

      do m = 0, 250
      do k = 0, ((N/2)-1)
          y_t = (0,0)
          do j = 0,N-1
              y_t = y_t + y_sinal(j)*exp(2.d0*pi*i*k*j/N)
          enddo
          y_transf(k) = y_t*2.d0/N
      enddo

      do k = 0,N-1
          y_i = (0,0)
          do j = 0,((N/2)-1)
              y_i = y_i + y_transf(j)*exp(-2.d0*pi*i*k*j/N)
          enddo
          write(2,*) (k*dt), real(y_i)
      enddo
      enddo

      call cpu_time(t2)

      write(*,2) "N igual a", N
      2	format(A12, I4)

      write(*,3) "Tempo igual a", t2-t1, "s"
      3	format(A14, E9.2, A1)

      end Program