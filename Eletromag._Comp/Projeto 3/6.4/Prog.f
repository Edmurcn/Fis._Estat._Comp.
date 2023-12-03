	implicit Real (a-h,o-z)
	dimension y(0:100,-1:201)
	data L/1.e0/, dx/0.01e0/, r/1.e0/, c/300.e0/
	
	open(1,file="data_out")
	
	!Construção do pulso inicial
	
	!Reta y = x
	
	y = 0
	
	do i = 25,50
		y(i,-1) = 2.e0*i*dx - 0.5e0
		y(i,0) = 2.e0*i*dx - 0.5e0
	enddo
	
	!Reta y = -x
	
	do i = 51,75
		y(i,-1) = -2.e0*i*dx + 1.5e0
		y(i,0) = -2.e0*i*dx + 1.5e0
	enddo
	
	!Propagação da Onda
	
	do n = 0,200
	
		y(0,n) = 0.e0
			
		do i = 1,99
				
		y(i,n+1) = 2.e0*(1-r**2.e0)*y(i,n)+r**2.e0*
     $(y(i+1,n)+y(i-1,n))-y(i,n-1)
     
     		write(1,*)  i*dx, y(i,n)
     	
		enddo
		
		write(1,*) 1, y(100,n)
		
		write(1,*) ""
		write(1,*) ""
	enddo
	
	end Program
