	implicit real*4 (a-h, o-z)
	parameter (L = 20, L2 = 5)
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	dimension v_total(-L:L, -L:L)
	dimension iplus(0:L), iminus(0:L)
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
	
	do i = 1, L-1
		iplus(i) = i + 1
		iminus(i) = i - 1
	enddo
	
	iplus(0) = 0
	iplus(L) = L
	iminus(0) = 0
	iminus(L) = L
	
	v_f = v
	
	do while(dif.gt.erro*count_)
		
		do i = -L,L
			do j = -L,L
				write(1,2) i, j, v_total(i,j)
2				format(I3, I4, F6.3)		
			enddo
		enddo
		
		soma = 0.e0
		count_ = 0.e0
		
		do i = 0, L
			do j = 0, L
				
				if (i.le.L2 .and. j.le.L2) then
					v_f(i,j) = 1.e0

				else
					v_f(i,j) = (v(iminus(i), j) + 
     &v(iplus(i), j) + v(i, iminus(j)) + v(i, iplus(j)))/4.e0
				
					soma = soma + abs(v(i,j) - v_f(i,j))
					count_ = count_ + 1.e0
				endif

			enddo
		enddo

		dif = soma
		
		write(1,*) ""

		v = v_f
		
		do i = -1,-L, -1
			do j = 0, L
				v_total(i,j) = v(-i, j)
			enddo
		
			do j = -1, -L, -1
				v_total(i,j) = v(-i, -j)
			enddo
		enddo
	
	
		do i = 0,L
			do j = 0, L
				v_total(i,j) = v(i, j)
			enddo
		
			do j = -1, -L, -1
				v_total(i,j) = v(i,-j)
			enddo
		enddo
		
	enddo

	
	
	do i=-L,L
		do j=-L,L
			write(2,1) i, j, v_total(i,j)
1			format(I3, I4, F6.3)	
		enddo
	enddo
			
	end 
