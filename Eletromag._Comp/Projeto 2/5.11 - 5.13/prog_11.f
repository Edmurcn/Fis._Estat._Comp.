	implicit real (a-h, o-z)
	parameter (L = 10)
	parameter (dz = 0.1e0)
	
	open(1, file="integral.dat")
	open(2, file="soma.dat")
	
	N = 2*L/dz ! Total de iterações
	z_i = -1e0*real(L) ! z inicial
	
	! Alterando a distância do fio
	
	do j = 1, 100
	
	campo_int = 0.e0    ! Variável auxiliar
	campo_sum = 0.e0	  ! Variável auxiliar
	
	x = 0.01e0*real(j)
	
	! Cálculo da integral
	
	do i = 0, N, 2
	
	  z = z_i + i*dz
	  campo_int = campo_int + (1.e0/3.e0)*
     $(B(z+dz,x) + 4.e0*B(z,x) + B(z-dz,x))

	enddo

	! Calculo da soma
	
	do i = 0, N
	  z = z_i + i*dz
	  campo_sum = campo_sum + B(z,x)
	enddo
	
	write(1,1)  x, campo_int
1	format(F5.2, F8.3)
	write(2,2)  x, campo_sum
2	format(F5.2, F8.3)
	
	enddo	
	
	end
	
	! Função do Campo
	
	real function B(z,x)
	parameter (pi = acos(-1.e0))
	
	dz = 0.1e0
	b = z**2.e0
	a = x**2.e0
	soma = (a + b)**1.5e0
	B = x*dz/((4.e0*pi)*soma)
	
	end function
	
		
	
	
	
	
	
