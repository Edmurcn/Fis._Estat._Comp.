	implicit real (a-h, o-z)
	parameter (pi = acos(-1.d0))
	dimension Bx(-10:10, -10:10, -10:10), By(-10:10, -10:10, -10:10)
      dimension Bz(-10:10, -10:10, -10:10)
	
	open(1, File="esp_out.dat")
	
	r = 5.e0
	dtheta = (2.e0*pi/8.e0)
	Bx = 0
	By = 0
	Bz = 0
	
	do i = -10, 10
	  do j = -10, 10
	    do k = -10, 10
	    	do m = 1,8
	    	
	    		theta1 = m*dtheta
	    		
	    		Bx(i,j,k) = Bx(i,j,k) + 
     $dbx1(real(i),real(j),real(k),theta1)  
			By(i,j,k) = By(i,j,k) + 
     $dby1(real(i),real(j),real(k),theta1) 
			Bz(i,j,k) = Bz(i,j,k) + 
     $dbz1(real(i),real(j),real(k),theta1)
     
		enddo
		do n = 1,8

	    		theta2 = n*dtheta
	    		
	    		Bx(i,j,k) = Bx(i,j,k) + 
     $dbx2(real(i),real(j),real(k),theta2)
			By(i,j,k) = By(i,j,k) + 
     $dby2(real(i),real(j),real(k),theta2)
			Bz(i,j,k) = Bz(i,j,k) + 
     $dbz2(real(i),real(j),real(k),theta2)
     	
            enddo	
	    enddo
	  enddo
	enddo
	 
	
	do i = -8,8, 2
	  do j = -8, 8, 2
	    do k = -8,8, 2 
	     write(1,1) i, j, k, Bx(i,j,k), By(i,j,k),  
     $Bz(i,j,k)
1	     format(3I4, 3F9.5)	
	    enddo
	  enddo
	enddo
			
	end
	
	!!!!!!!!!!!! Espira em z = r/2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	real function dbx1(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r,ze
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		ze1 = r/2.e0
		
		dbx = (1.e0/4.e0*pi)*(r*cos(theta)*(z-ze1)*dtheta)/
     $(sqrt((x-(r*cos(theta)))**2.d0 + (y-(r*sin(theta)))**2.d0 +
     $ (z-ze1)**2.d0))**3.d0
     
      end function
      
      real function dby1(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r,ze1
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		ze1 = r/2.e0
		
		dby = (1.e0/4.e0*pi)*(r*sin(theta)*(z-ze1)*dtheta)/
     $(sqrt((x-(r*cos(theta)))**2.d0 + (y-(r*sin(theta)))**2.d0 +
     $ (z-ze1)**2.d0))**3.d0
     
      end function
      
      real function dbz1(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r,ze1
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		ze1 = r/2.e0
		
		dbz =(-1.e0/4.e0*pi)*dtheta*(r*sin(theta)*(y-r*sin(theta))+ 
     $ r*cos(theta)*(x-r*cos(theta)))/(sqrt((x-(r*cos(theta)))**2.d0 + 
     $(y-(r*sin(theta)))**2.d0 + (z-ze1)**2.d0))**3.d0
     
      end function
      
      !!!!!!!!!!! Espira em z = -r/2 !!!!!!!!!!!!!!!!!!!!!! 
      
      real function dbx2(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r,ze
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		ze1 = -1.e0*r/2.e0
		
		dbx = (1.e0/4.e0*pi)*(r*cos(theta)*(z-ze1)*dtheta)/
     $(sqrt((x-(r*cos(theta)))**2.d0 + (y-(r*sin(theta)))**2.d0 +
     $ (z-ze1)**2.d0))**3.d0
     
      end function
      
      real function dby2(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r,ze1
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		ze1 = -1.e0*r/2.e0
		
		dby = (1.e0/4.e0*pi)*(r*sin(theta)*(z-ze1)*dtheta)/
     $(sqrt((x-(r*cos(theta)))**2.d0 + (y-(r*sin(theta)))**2.d0 +
     $ (z-ze1)**2.d0))**3.d0
     
      end function
      
      real function dbz2(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r,ze1
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		ze1 = -1.e0*r/2.e0
		
		dbz =(-1.e0/4.e0*pi)*dtheta*(r*sin(theta)*(y-r*sin(theta))+ 
     $ r*cos(theta)*(x-r*cos(theta)))/(sqrt((x-(r*cos(theta)))**2.d0 + 
     $(y-(r*sin(theta)))**2.d0 + (z-ze1)**2.d0))**3.d0
     
      end function
      
