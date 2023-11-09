	implicit real*4 (a-h, o-z)
	parameter (L = 50, L2 = 10)
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	dimension v_total(-L:L, -L:L)
	dimension iplus(0:L), iminus(0:L)
	
	open(1, file = "out_jacobi")
	
	write(1,*) "      Erro   Iterações"
	
	do m = 3, 9
	
	icount_2 = 0.e0
	erro = 1.e0/(10.e0**m)
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
		v = v_f
		icount_2 = icount_2 + 1
		
	enddo
	
	write(1,1) erro, icount_2 
1	format(E7.1, I8) 
	
	enddo

	end 
