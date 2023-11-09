	implicit real*4 (a-h, o-z)
	parameter (L = 50, L2 = 10)
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	dimension v_f2(-L:L, -L:L)
	dimension v_total(-L:L, -L:L)
	dimension iplus(-L:L), iminus(-L:L)
	parameter (erro = 10e-6)
	
	call cpu_time(start)
	
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
				
					soma = soma + abs(v_f(i,j) - v(i,j))
					count_ = count_ + 1.e0
				endif

			enddo
		enddo

		dif = soma
		v = v_f
		do i = 0, L
			do j = 0,L
				v_total(i,j) = v(i,j)
				v_total(i,-j) = v(i,j)
				v_total(-i,j) = v(i,j)
				v_total(-i,-j) = v(i,j) 
			enddo
		enddo
		
	enddo

	call cpu_time(finish)
	
	write(*,2) "Tempo de programação (1º Quadrante):", 
     $finish-start,"s"
2	format(A40, F6.3, A1)
     
	call cpu_time(start2)
	
	v_total = 0.e0
	count_ = 0.e0
	dif = 1.e0

	do i = -L2, L2
		do j = -L2, L2   
			v_total(i,j) = 1.e0
		enddo
	enddo
	
	do i = -L+1, L-1
		iplus(i) = i + 1
		iminus(i) = i - 1
	enddo
	
	iplus(-L) = -L
	iplus(L) = L
	iminus(-L) = -L
	iminus(L) = L
	
	v_f2 = v_total
	
	do while(dif.gt.erro*count_)
		
		soma = 0.e0
		count_ = 0.e0
		
		do i = -L, L
			do j = -L, L
				
				if (i.le.L2 .and. j.le.L2 .and. 
     &i.ge.(-L2) .and. j.ge.(-L2)) then
					v_f2(i,j) = 1.e0

				else
					v_f2(i,j) = (v_total(iminus(i), j) + 
     &v_total(iplus(i), j) + v_total(i, iminus(j)) + 
     &v_total(i, iplus(j)))/4.e0
				
					soma = soma + abs(v_total(i,j) - v_f2(i,j))
					count_ = count_ + 1.e0
				endif

			enddo
		enddo

		dif = soma

		v_total = v_f2
		
	enddo
	
	call cpu_time(finish2)
	
	write(*,3) "Tempo de programação (Total):", finish2-start2,"s"
3	format(A32, F6.3, A1)

	end 
