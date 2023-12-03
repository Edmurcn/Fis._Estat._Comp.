	implicit Real (a-h,o-z)
	dimension y(0:100,-1:201), iplus(0:100), iminus(0:100)
	
	!Corda de comprimento 1m
	data L/1.e0/, dx/0.01e0/, r/1e0/, c/300.e0/
	
	open(1,file="data_out")
	
	!Definição da condição de contorno
	
	do i = 1,99
		iplus(i) = i+1
		iminus(i) = i-1
	enddo
	
	iplus(0) = 1
	iminus(0) = 0
	iplus(100) = 100
	iminus(100) = 99
	
	!Pacote inicial
	
	x0 = L/3.d0
	s = L/30.d0
	
	! (y_1 - y_0) != 0
	
	do i = 0,100
		y(i,-1) = exp(-(i*dx-x0)**2.d0/s**2.d0)
		y(i,0) = exp(-(i*dx-(x0+dx))**2.d0/s**2.d0)	
	enddo
	
	!Propagação do Pulso
	
	do n = 0,200
		
		y(0,n) = 0.e0
		y(100, n) = 0.e0
		
		write(1,*) 0, y(0,n)
		
		do i = 1,99	
			
		y(i,n+1) = 2.d0*(1-r**2.d0)*y(i,n)+r**2.d0*
     $(y(iplus(i),n)+y(iminus(i),n))-y(i,n-1)
   
   		write(1,*) i*dx, y(i,n)
   		
   		if (n.le.100.and.mod(n,10).eq.0) then
			write(2,*) i*dx, y(i,n)-n/5.e0
   		endif
   		
		enddo
		
		write(1,*) 1, y(100,n)
		
		write(1,*) ""
		write(1,*) ""
		
	enddo
	
	end Program

