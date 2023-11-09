	implicit real*4 (a-h, o-z)
	parameter (L = 50, L3 = 20 )
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	dimension v_total(-L:L, -L:L)
	dimension iplus(0:L), iminus(0:L)
	dimension iplus2(-L:L), iminus2(-L:L)
	parameter (erro = 10e-8)
	
	open(2, file = "campo.dat")


	do L2 = 1, 40, 1
	
		v = 0.e0
		count_ = 0.e0
		dif = 1.e0

		do j = 0, L3   
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
	
		do i = -L+1, L-1
			iplus2(i) = i + 1
			iminus2(i) = i - 1
		enddo
	
		iplus2(-L) = -L + 1
		iplus2(L) = L
			iminus2(-L) = -L
		iminus2(L) = L - 1
	
		v_f = v
	
		do while(dif.gt.erro*count_)
		
			soma = 0.e0
			count_ = 0.e0
		
			do i = 0, L
				do j = 0, L
			 
					if (i.eq.L2 .and. j.le.L3) then
						v_f(i,j) = -1.e0

					else
						v_f(i,j) = (v(iminus(i), j) + 
     &v(iplus(i), j) + v(i, iminus(j)) + v(i, iplus(j)))/4.e0
				
						soma = soma + abs(v(i,j) -v_f(i,j))
						count_ = count_ + 1.e0
					endif

				enddo
			enddo

			dif = soma

			v = v_f
		
			do i = 0, L
				do j = 0, L
					v_total(-i,j) = -v(i,j)
					v_total(-i,-j) = -v(i,j)
					v_total(i,j) = v(i,j)
					v_total(i,-j) = v(i,j)
				enddo
			enddo	
					
		enddo
	

		do i=-L,L
			do j=-L,L
				E_x = (v_total(iminus2(i),j) - v_total(iplus2(i),j))/2.e0
				E_y = (v_total(i,iminus2(j)) - v_total(i,iplus2(j)))/2.e0
			
				write(3,1) i, j, v_total(i,j), E_x, E_y
1				format(I4, I5, 3F9.3)
			enddo
		enddo
	
		write(3,*) ""
		write(3,*) ""
	
	enddo
	
	end 
