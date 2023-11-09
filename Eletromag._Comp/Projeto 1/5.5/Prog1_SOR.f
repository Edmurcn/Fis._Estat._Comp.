	implicit real*4 (a-h, o-z)
	parameter (L = 50, L2 = 10)
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	dimension iplus(0:L), iminus(0:L)
	
	open(1, file = "precisao2")
	
	vm = 0.e0
	Em = 0.e0
	
	do m = 2,13
	
	!Variáveis de Contagem
	
	n_ite = 0.e0
	E = 0.e0
	count_2 = 0.e0
	
	!Variáveis do potencial
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
					v(i,j) = 1.e0

				else
					aux = v(i,j)
					v(i,j) = (v(iminus(i), j) + 
     &v(iplus(i), j) + v(i, iminus(j)) + v(i, iplus(j)))/4.e0
				
					soma = soma + abs(aux - v(i,j))
					count_ = count_ + 1.e0
				endif
				
				n_ite = n_ite + 1.e0
				count_2 = v(i,j) + count_2

			enddo
		enddo

		do i=0,L
		  do j=0,L
		    E_x = (v(iminus(i),j) - v(iplus(i),j))/2.e0
		    E_y = (v(i,iminus(j)) - v(i,iplus(j)))/2.e0
		
		    E = E + sqrt(E_x**2.e0 + E_y**2.e0)
		
		  enddo
		enddo

		dif = soma
		
	enddo
	
	!Cálculo da Média
	
	erro_Em = abs(Em - (E/real(n_ite)))
	Em = E/(real(n_ite))
	erro_vm = abs(vm - count_2/real(n_ite))
	vm = count_2/real(n_ite)
	
	write(1,1) erro, vm, erro_vm, Em, erro_Em  
1	format(E9.1, F13.9, E9.1, F13.9, E9.1) 
	
	enddo

	end 
