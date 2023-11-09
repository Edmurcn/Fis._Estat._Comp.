	implicit real (a-h, o-z)
	parameter (pi = acos(-1.e0))
	
	open(1, file="erro-x.dat")
	
	write(1,*) "  Passos Tam. Fio   Erro"
	
	L = 10
	z_i = -1.e0*real(L) ! z inicial
	
	do j = 1, 20
	
	
	x = 0.05e0*real(j)			 ! Diatância do fio
	B_amp = 1.e0/(2.e0*pi*x) ! Campo lei de ampere
	ipassos = 2	  ! Número de passos da iteração
	erro = 1.e0

	
	! Iterando até a precisão proposta
	
	do while(erro.gt.0.05e0)
	
	campo_int = 0.e0    ! Variável auxiliar
	campo_sum = 0.e0	  ! Variável auxiliar

	dz = 2.e0*real(L)/real(ipassos) !Definindo o tamanho do infinitesimal

	! Calculo da soma
	
	do i = 0, ipassos
	  
	  z = z_i + i*dz
	  campo_sum = campo_sum + B(z,dz,x)
	  
	enddo
	
	erro = abs(B_amp - campo_sum)/B_amp

	ipassos = ipassos + 2
	 
	enddo
	
	write(1,1) ipassos, x, erro
1	format(I7, F8.2, F12.5)

	write(2,*) x, ipassos

	enddo
	
	end
	
	! Função do Campo
	
	real function B(z,dz,x)
	parameter (pi = acos(-1.e0))
	
	B = x*dz/((4.e0*pi)*(z**2.e0 + x**2.e0)**(1.5e0))
	
	end function
	
