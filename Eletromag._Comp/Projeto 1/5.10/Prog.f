	implicit real*4 (a-h, o-z)
	parameter (L = 100)
	dimension v_f(L)
	dimension v_total(L)
	dimension ro(L)
	dimension iplus(L), iminus(L)
	parameter (erro = (1e-4)*real(L)**3.e0)
	parameter (pi = acos(-1.e0)) 
	parameter (rmax = 5.e0)
	open(2, file = "dados.dat")
	open(3, file = "campo.dat")
	
	!Parâmetros Iniciais
	
	alpha = 1.8e0
	dr = rmax/real(L)
	v_total = 0.e0
	ro = 0.e0
	
	do i = 1,5
		ro(i) = 1.e0
	enddo
	
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
		     	 r = dr*real(i)
		     	 
		    	 aux = v_total(i)
			 v_total(i) = (v_total(iplus(i))*((dr/r) + 1.e0) +
     $v_total(iminus(i))*(1.e0 - (dr/r)) + ro(i)*(dr)**2.e0)/2.e0
     			 v_total = alpha*(v_total - aux) + aux
			 
		enddo
		
		count_ = count_ + 1.e0
		dif = soma
		
	enddo
	
	dphi = 2.e0*pi/100.e0
	theta = pi/2.e0

	do i=1,L
	  do j = 1,100
	  
		!printando o potencial do plano do centro da esfera	  
	   
	    	phi = dphi*real(j)
		r = dr*real(i)
		
		x = r*sin(theta)*cos(phi)
		y = r*sin(theta)*sin(phi)
		
		write(2,*) x, y, v_total(i)
1		format(F6.3, F6.3, F10.3)	

	  enddo
	enddo
	
	
	
	end 
