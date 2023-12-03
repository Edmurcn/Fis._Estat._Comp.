	implicit Real (a-h,o-z)
	dimension y(0:100,-1:401), iplus(0:100), iminus(0:100)
	parameter (pi = acos(-1.e0))
	
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
	
	!Pulso inicial
	
	w = pi*0.01e0*5.e0/2.e0
	A = 0.5e0
	
	!Propagação do Pulso

	do n = 0,400
		
		y(0,n) = A*sin(w*n)
		y(100, n) = 0.e0
		
		write(1,*) 0, y(0,n)
		
		do i = 1,99	
			
		y(i,n+1) = 2.d0*(1-r**2.d0)*y(i,n)+r**2.d0*
     $(y(iplus(i),n)+y(iminus(i),n))-y(i,n-1)
   
   		write(1,*) i*dx, y(i,n)
   		
   		if (mod(n,100).eq.0) then
			write(2,*) i*dx, y(i,n)
			
   		endif
   		
		enddo
		
		write(1,*) 1, y(100,n)
		
		write(1,*) ""
		write(1,*) ""
		
	enddo
	
	end Program

