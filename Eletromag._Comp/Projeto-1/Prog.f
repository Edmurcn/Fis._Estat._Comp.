	implicit real*4 (a-h, o-z)
	parameter (L = 5)
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	parameter (erro = 10e-6)

	v = 0.e0
	v_f = 0.e0
	soma = 0.e0
	count = 0.e0
	dif = 1.e0

	do i = 0, 2
		do j = 0, 2
			v(i,j) = 1.e0
		enddo
	enddo

	do while(dif.gt.erro)

		do i = 0,L
			do j = 0,L
				write(1,*) v(i,j)
			enddo
		enddo

		do j = 2, L-1
			do i = 2, L-1

				v_f(i,j) = (v(i-1, j) + v(i+1, j) + 
     &v(i, j-1) + v(i, j+1))/4.e0
				
				soma = soma + v_p(i,j) - v(i,j)
				count = count + 1.e0
			enddo
		enddo

		dif = soma/count	
		
		write(1,*) ""
		write(1,*) ""

	enddo
			
	end 