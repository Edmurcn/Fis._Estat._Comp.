	implicit real*4 (a-h, o-z)
	parameter (L = 50, L2 = 10 )
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	dimension v_total(-L:L, -L:L)
	dimension iplus(0:L), iminus(0:L)
	parameter (erro = 10e-6)
	parameter ( pi = acos(-1.d0))
	
	open(2, file = "dados.dat")
	open(4, file = "equip.dat")
	
	
	v = 0.e0
	count_ = 0.e0
	dif = 1.e0

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
	
	v_f = v
	
	do while(dif.gt.erro*count_)
		
		soma = 0.e0
		count_ = 0.e0
		
		do i = 0, L
			do j = 0, L
			 
				if (i.eq.L2 .and. j.le.10) then
					v_f(i,j) = -1.e0

				else	
					v(i,j) = (v(iminus(i), j) + 
     &v(iplus(i), j) + v(i, iminus(j)) + v(i, iplus(j)))/4.e0
					
					soma = soma + abs(v(i,j) - v_f(i,j))
					count_ = count_ + 1.e0
				endif

			enddo
		enddo

		dif = soma
		
		write(1,*) ""
		
		do i = 0, L
			do j = 0, L
				v_total(-i,j) = -v(i,j)
				v_total(-i,-j) = -v(i,j)
				v_total(i,j) = v(i,j)
				v_total(i,-j) = v(i,j)
			enddo
		enddo	
		
		do i = -L,L
			do j = -L,L
				write(1,2) i, j, v_total(i,j)
2				format(I3, I4, F7.3)		
			enddo
		enddo
		
	enddo
	
	do i=-L,L
		do j=-L,L
			write(2,1) i, j, v_total(i,j)
1			format(I3, I4, F7.3)	
		enddo
	enddo
	
	do i=-L,L
		do j=-L,L
		  if (v_total(i,j).lt.0.96.and.v_total(i,j).gt.0.94) then
		    write(4,*) i, j 
		  endif
		  if (v_total(i,j).lt.0.56.and.v_total(i,j).gt.0.54)
     $then
		    write(4,*) i, j
		  endif
		  if (v_total(i,j).lt.0.36.and.v_total(i,j).gt.0.34)
     $then
                write(4,*) i, j
              endif
		  if (v_total(i,j).lt.0.16.and.v_total(i,j).gt.0.14)
     $then
                write(4,*) i, j
              endif
              if (v_total(i,j).lt.(-0.14).and.v_total(i,j).gt.(-0.16))
     $then	
                write(4,*) i, j
              endif
              if (v_total(i,j).lt.(-0.34).and.v_total(i,j).gt.(-0.36))
     $then
                write(4,*) i, j
              endif
              if (v_total(i,j).lt.(-0.54).and.v_total(i,j).gt.(-0.56))
     $then
		    write(4,*) i, j
		  endif
		   if (v_total(i,j).lt.(-0.94).and.v_total(i,j).gt.(-0.96))
     $then
		    write(4,*) i, j
		  endif
		enddo
	enddo

	
	end 
