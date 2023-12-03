	implicit Real (a-h,o-z)
	dimension y(0:200,-1:601), iplus(0:200), iminus(0:200)
	
	!Corda de comprimento 1m
	data L/1.e0/, dx1/0.01e0/, r/1e0/, c1/300.e0/
	data dx2/0.005e0/, c2/150.e0/
	
	open(1,file="data_out")
	
	!Definição da condição de contorno
	
	do i = 1,199
		iplus(i) = i+1
		iminus(i) = i-1
	enddo
	
	iplus(0) = 1
	iminus(0) = 0
	iplus(200) = 200
	iminus(200) = 199
	
	!Pacote inicial
	
	x0 = 0.3d0
	s = L/30.d0
	
	! (y_1 - y_0) != 0
	
	do i = 0,200
		y(i,-1) = exp(-(i*dx1-x0)**2.d0/s**2.d0)  
		y(i,0) = exp(-(i*dx1-x0)**2.d0/s**2.d0)
	enddo
	
	!Propagação do Pulso
	
	do n = 0,600
		
		y(0,n) = 0.e0
		
		write(1,*) 0, y(0,n)
		
		do i = 1,50	
			
		y(i,n+1) = 2.d0*(1-r**2.d0)*y(i,n)+r**2.d0*
     $(y(iplus(i),n)+y(iminus(i),n))-y(i,n-1)
   
   		write(1,*) i*dx1, y(i,n)
   		
   		if (n.le.200.and.mod(n,50).eq.0) then
			write(2,*) i*dx1, y(i,n)-n/20.e0
   		endif
   		
		enddo
		
		y(200, n) = 0.e0
		
		do i = 101,199	
		
		j = i - 50
			
		y(j,n+1) = 2.d0*(1-r**2.d0)*y(j,n)+r**2.d0*
     $(y(iplus(j),n)+y(iminus(j),n))-y(j,n-1)
   
   		write(1,*) i*dx2, y(j,n)
   		
   		if (n.le.200.and.mod(n,50).eq.0) then
			write(2,*) i*dx2, y(j,n)-n/20.e0
   		endif
   		
		enddo
		
		write(1,*) 1, y(200,n)
		
		write(1,*) ""
		write(1,*) ""
		
	enddo
	
	end Program

