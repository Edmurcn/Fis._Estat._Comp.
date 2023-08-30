	implicit real*4 (a-h, o-z)
	parameter (L = 100, L2 = 20)
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	parameter (erro = 10e-8)
	
	open(2, file = "dados.dat")

	v = 0.e0
	count_ = 0.e0
	dif = 1.e0

	do i = 0, L2
		do j = 0, L2   
			v(i,j) = 1.e0
		enddo
	enddo
	
	v_f = v
	
	do while(dif.gt.erro*count_)

		do i = 0,L
			do j = 0,L
				write(1,2) i, j, v(i,j)
2				format(I3, I4, F6.3)		
			enddo
		enddo
		
		soma = 0.e0
		
		do j = 0, L-1
			do i = 0, L-1
				
				if (i.le.L2.and.j.le.L2) then
					v_f(i,j) = 1.e0
					exit
				else
					v_f(i,j) = (v(i-1, j) + v(i+1, j) + 
     &v(i, j-1) + v(i, j+1))/4.e0
				
					soma = soma + abs(v(i,j) - v_f(i,j))
					count_ = count_ + 1.e0
				endif

			enddo
		enddo

		dif = soma
		
		write(1,*) ""
		write(1,*) ""

		v = v_f

	enddo
	
	do i=0,L
		do j=0,L
			write(2,1) i, j, v(i,j)
1			format(I3, I4, F6.3)	
		enddo
	enddo
			
	end 
