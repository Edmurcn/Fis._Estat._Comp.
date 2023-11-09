	implicit real*4 (a-h, o-z)
	parameter (L = 50, L2 = 10)
	dimension v_f(-L:L, -L:L)
	dimension v_total(-L:L, -L:L)
	dimension iplus(-L:L), iminus(-L:L)
	parameter (erro = 10e-6)
	parameter (pi = acos(-1.e0))
	
	v_total = 0.e0
	count_ = 0.e0
	dif = 1.e0
	
	alpha = 2.e0/(1.e0 + (pi/real(L)))

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
	

	do while(dif.gt.erro*count_)
		
		do i = -L,L
			do j = -L,L
				write(1,2) i, j, v_total(i,j)
2				format(I3, I4, F6.3)		
			enddo
		enddo
		
		soma = 0.e0
		count_ = 0.e0
		
		do i = -L, L
			do j = -L, L
				
				if (i.le.L2 .and. j.le.L2 .and. 
     &i.ge.(-L2) .and. j.ge.(-L2)) then
					v_f(i,j) = 1.e0

				else
					aux = v_total(i,j)
					v_total(i,j) = (v_total(iminus(i), j) + 
     &v_total(iplus(i), j) + v_total(i, iminus(j)) + 
     &v_total(i, iplus(j)))/4.e0
     					
     					v_total(i,j)=alpha*(v_total(i,j)-aux)+aux
				
					soma = soma + abs(v_total(i,j) - aux)
					count_ = count_ + 1.e0
				endif

			enddo
		enddo

		dif = soma
		
		write(1,*) ""

	enddo

	end 
