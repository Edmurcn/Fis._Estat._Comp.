	implicit real*4 (a-h, o-z)
	dimension v(0:200, 0:200)
	dimension v_total(-200:200, -200:200)
	dimension iplus(0:200), iminus(0:200)
	parameter (erro = 10e-6)
	parameter (pi = dcos(-1.d0))
	
	open(3, file = "SOR_out")

	do L = 10, 80, 10
	
	alpha = 2.e0/(1.e0 + (pi/real(L)))
	
	v = 0.e0
	count_ = 0.e0
	dif = 1.e0
	icount = 0
	
	L2 = 5
	
	do j = 0, 10   
		v(L2,j) = -1.e0
	enddo
	
	do i = 1, L-1
		iplus(i) = i + 1
		iminus(i) = i - 1
	enddo
	
	iplus(0) = 0
	iplus(L) = L
	iminus(0) = 0
	iminus(L) = L
	
	do while(dif.gt.erro*count_)
		icount = icount + 1
		
		soma = 0.e0
		count_ = 0.e0
		
		do i = 0, L
			do j = 0, L
			 
				if (i.eq.L2 .and. j.le.10) then
					v(i,j) = -1.e0

				else
					aux = v(i,j) 
					v(i,j) = (v(iminus(i), j) + 
     &v(iplus(i), j) + v(i, iminus(j)) + v(i, iplus(j)))/4.e0
					
					v(i,j) = alpha*(v(i,j) - aux) + aux
					soma = soma + abs(aux - v(i,j))
				
					count_ = count_ + 1.e0
				endif

			enddo
		enddo

		dif = soma
		
		do i = 0, L
			do j = 0, L
				v_total(-i,j) = -v(i,j)
				v_total(-i,-j) = -v(i,j)
				v_total(i,j) = v(i,j)
				v_total(i,-j) = v(i,j)
			enddo
		enddo	
			

	enddo
	
	write(3,*) L, icount
	
	enddo 

	end 
