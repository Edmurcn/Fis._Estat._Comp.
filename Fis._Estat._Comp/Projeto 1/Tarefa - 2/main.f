      implicit Real*8 (a-h,o-z)
	  dimension y_sinal(0:1000), f(0:1000)
	  parameter (pi = acos(-1.d0))
	  complex*16 i, y_t, y_i, y_transf(0:1000)

      !Importando dados
      
	  open(2, file = "data.out")
      open(1, file = "data.in")
	  open(3, file = "Real-a")
	  open(4, file = "Imag-a")
	
	  i = (0.d0,1.d0)

      write(*,*) "dt"
	  read(*,*) dt
	
	  !Dados do Sinal
	
	  do l = 0,1000 
		   read(1,*,end = 1) a, y_sinal(l)
	  enddo

1     continue

	  N = l
	
	  !Transformada de Fourier
	
	  do k = 0, ((N/2)-1)
		f(k) = k/(N*dt)
		y_t = (0,0)
		do j = 0,N-1
			y_t = y_t + y_sinal(j)*exp(2.d0*pi*i*k*j/N)
		enddo
		y_transf(k) = y_t*2.d0/N
		
		!Espectros da transformada
		
		write(3,*) f(k), real(y_t*2.d0/N)
		write(4,*) f(k), aimag(y_t*2.d0/N)
	  enddo
	
	  !Transformada reversa
	
	  do k = 0,N-1
		y_i = (0,0)
		do j = 0,((N/2)-1)
			y_i = y_i + y_transf(j)*exp(-2.d0*pi*i*k*j/N)
		enddo
		write(2,*) (k*dt), real(y_i)
	  enddo
	
	  end Program
