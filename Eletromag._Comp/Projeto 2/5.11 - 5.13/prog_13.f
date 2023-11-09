	implicit real*8 (a-h, o-z)
	parameter (pi = acos(-1.d0))
	
	open(1, File="saida_pi.dat")

	do j = 3, 8
		
	N = 10**(j)
	h = 1.d0/real(N)
	xi = 0.d0
	y_s = 0.d0
	y_b = 0.d0

	do i = 0,N-2,2
		x = xi + i*h
		y_s = y_s + (h/3.d0)*(f(x+h) + 4.d0*f(x) + f(x-h))
	enddo
	
	do i = 0, N-2, 4
		x = xi + i*h
		y_b = y_b + (2.d0*h/45.d0)*(7.d0*f(x-2.d0*h) + 32.d0*f(x-h)
     $+ 12.d0*f(x) + 32.d0*f(x+h) + 7.d0*f(x+2.d0*h))
	enddo
		
	pi_est_s = 4.d0*y_s
	pi_est_b = 4.d0*y_b
	erro = abs(pi_est_s - pi)
	
	write(1,1) N, pi_est_s, pi_est_b, pi
1	format(I12, 3F12.7)

	enddo
	
	end
	
	real*8 function f(x)
	real*8 x
	
	f = (1.d0 - x**2.d0)**0.5
	
	end function
		
		
		
	
	
	
