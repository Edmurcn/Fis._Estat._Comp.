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
	    	
	    		theta = m*dtheta
	    		
	    		Bx(i,j,k) = Bx(i,j,k) + 
     $dbx1(real(i),real(j),real(k),theta)
			By(i,j,k) = By(i,j,k) + 
     $dby1(real(i),real(j),real(k),theta)
			Bz(i,j,k) = Bz(i,j,k) + 
     $dbz1(real(i),real(j),real(k),theta)
     
	    	enddo
	    		
	    enddo
	  enddo
	enddo
	 
	
	!do i = -8,8, 2
	  do j = -6, 6, 2
	    do k = -8,8, 2
	     write(1,1) 0, j, k, Bx(0,j,k), By(0,j,k),  
     $Bz(0,j,k)
1	     format(3I4, 3F9.5)	
	    enddo
	  enddo
	!enddo
			
	end
	
	real function dbx1(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		
		dbx = (-1.e0/4.e0*pi)*(r*cos(theta)*y*dtheta)/
     $(sqrt((x-(r*cos(theta)))**2.d0 + y**2.d0 +
     $ (z-(r*sin(theta)))**2.d0))**3.d0
     
      end function
      
      real function dby1(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		
		dby = (1.e0/4.e0*pi)*dtheta*(r*cos(theta)*(x-r*cos(theta)) 
     $+ r*sin(theta)*(z-r*sin(theta))) / (sqrt((x-(r*cos(theta)))**2.d0
     $ + y**2.d0 + (z-(r*sin(theta)))**2.d0))**3.d0
     
      end function
      
      real function dbz1(x,y,z,theta)
	parameter (pi = acos(-1.d0))
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 5.e0
		
		dbz =(-1.e0/4.e0*pi)*dtheta*(r*sin(theta)*y)
     $/(sqrt((x-(r*cos(theta)))**2.d0 + y**2.d0 
     $ + (z-(r*sin(theta)))**2.d0))**3.d0
     
      end function
      
     
     
