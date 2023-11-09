	implicit real*4 (a-h, o-z)
	parameter (L = 81, L2 = 50)
	dimension v(0:L, 0:L), v_f(0:L, 0:L)
	dimension iplus(0:L), iminus(0:L)
	parameter (erro = 10e-6)
	
	open(2, file = "dados.dat")
	open(3, file = "campo.dat")
	
	v = -1.e0
	count_ = 0.e0
	dif = 1.e0

	do i = 0, L   
		v(i,0) = 1.e0
	enddo
	
	do j = 0, L2
		v((L/2),j) = 1.e0
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
				if (j.eq.0 .or. i.eq.L/2 .and. j.le.L2) then
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
			
	enddo
	
	do i=0,L
		do j=0,L
			write(2,11) i, j, v(i,j)
11			format(I3, I4, F7.3)	
		enddo
	enddo
	
	do i=0,L
	  do j=0,L
		E_x = (v(iminus(i),j) - v(iplus(i),j))/2.e0
		E_y = (v(i,iminus(j)) - v(i,iplus(j)))/2.e0
			
		write(3,1) i, j, v(i,j), E_x, E_y
1		format(I4, I5, 3F9.3)
	  enddo
	enddo
	
	do i=-L,L
		do j=-L,L
		  a = 0.96
		  b = 0.94
		  if (v(i,j).lt.a.and.v(i,j).gt.b) then
		    write(4,*) i, j 
		  endif
		  if (v(i,j).lt.(a-0.2).and.v(i,j).gt.(b-0.2))
     $then
		    write(4,*) i, j
		  endif
		  if (v(i,j).lt.(a-0.4).and.v(i,j).gt.(b-0.4))
     $then
		    write(4,*) i, j
		  endif
		  if (v(i,j).lt.(a-0.8).and.v(i,j).gt.(b-0.8))
     $then
                write(4,*) i, j
              endif
              if (v(i,j).lt.(a-1).and.v(i,j).gt.(b-1))
     $then
		    write(4,*) i, j
		  endif
		enddo
	enddo

	end 
