	implicit real*4 (a-h, o-z)
	parameter (L = 51)
	dimension v_f(L, L, L)
	dimension v_total(L, L, L)
	dimension ro(L, L, L)
	dimension iplus(L), iminus(L)
	parameter (erro = (1e-4)*real(L)**3.e0)
	parameter (pi = acos(-1.e0)) 
	
	open(2, file = "dados.dat")
	open(3, file = "campo.dat")
	
	!Parâmetros Iniciais
	
	alpha = 1.3
	
	ro = 0.e0
	ro(26, 26, 2) = 1.e0
	
	v_total = 0.e0
	count_ = 0.e0
	dif = 1.e0

	do i = 2, L-1
		iplus(i) = i + 1
		iminus(i) = i - 1
	enddo
	
	iplus(1) = 2
	iplus(L) = L
	iminus(1) = 1
	iminus(L) = L-1
	
	do while(count_.lt.1000)
		
		soma = 0.e0
		
		do i = 1, L
		  do j = 1, L
		     do k = 1, L
		     
		    	 aux = v_total(i,j,k)
			 v_total(i,j,k) = (v_total(iminus(i), j, k) + 
     &v_total(iplus(i), j, k) + v_total(i, iminus(j), k) + 
     &v_total(i, iplus(j), k) + v_total(i, j, iminus(k)) +
     & v_total(i, j, iplus(k)) + ro(i,j,k))/6.e0
			 
		     enddo
		  enddo
		enddo
		
		count_ = count_ + 1.e0
		dif = soma
		
	enddo

	
	do i=1,L
		do j=1,L
		  write(2,1) i, j, v_total(i,j, 10)
1		  format(I3, I4, F10.3)	
		enddo
	enddo
	
	do i=1,L
	  do j=1,L
	    if (i.ne.26.and.j.ne.26) then 
	    E_x = (v_total(iminus(i),j,26) - v_total(iplus(i),j,26))/2.e0
	    E_y = (v_total(i,iminus(j),26) - v_total(i,iplus(j),26))/2.e0
	    endif	
		write(3,2) i, j, v_total(i,j,26), E_x, E_y
2		format(I4, I5, 3F9.3)
	  enddo
	enddo
	
	do i=1,L
		do j=1,L
		  a = 0.3
		  b = 0.15
		  if (v_total(i,j,2).lt.a.and.v_total(i,j,2).gt.b) then
		    write(4,*) i, j 
		  endif
		  if (v_total(i,j,2).lt.0.05.and.
     $v_total(i,j,2).gt.0.03) then
		    write(4,*) i, j 
		  endif
		  if (v_total(i,j,2).lt.0.02.and.
     $v_total(i,j,2).gt.0.015) then
		    write(4,*) i, j 
		  endif
		  if (v_total(i,j,2).lt.0.01.and.
     $v_total(i,j,2).gt.0.008) then
		    write(4,*) i, j 
		  endif
		  if (v_total(i,j,2).lt.0.007.and.
     $v_total(i,j,2).gt.0.006) then
		    write(4,*) i, j 
		  endif
		  if (v_total(i,j,2).lt.0.005.and.
     $v_total(i,j,2).gt.0.003) then
		    write(4,*) i, j 
		  endif
		  
		enddo
	enddo
		
	
	end 
