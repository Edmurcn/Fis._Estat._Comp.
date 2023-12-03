	implicit real (a-h, o-z)
	parameter (pi = acos(-1.d0))
	dimension Bx(-10:10, -10:10, -10:10), By(-10:10, -10:10, -10:10)
      dimension Bz(-10:10, -10:10, -10:10)
	
	open(1, File="esp_out.dat")
	
	dtheta = (2.e0*pi/8.e0)
	Bx = 0
	By = 0
	Bz = 0
	
	do i = -10, 10
	  do j = -10, 10
	    do k = -10, 10
	     do l = -1,1,2
	    	do m = 1,8
	    	
	    		theta = m*dtheta
	    		
	    		Bx(i,j,k) = Bx(i,j,k) + 
     $dbx1(real(i),real(j),real(k),theta,l)
			By(i,j,k) = By(i,j,k) + 
     $dby1(real(i),real(j),real(k),theta,l)
			Bz(i,j,k) = Bz(i,j,k) + 
     $dbz1(real(i),real(j),real(k),theta,l)
     
	    	enddo
	    	do n = 1,8
	    	
	    		theta = n*dtheta
	    		
	    		Bx(i,j,k) = Bx(i,j,k) + 
     $dbx2(real(i),real(j),real(k),theta,l)
			By(i,j,k) = By(i,j,k) + 
     $dby2(real(i),real(j),real(k),theta,l)
			Bz(i,j,k) = Bz(i,j,k) + 
     $dbz2(real(i),real(j),real(k),theta,l)
     
	    	enddo
	     enddo	
	    enddo
	  enddo
	enddo
	 
	
	do i = -10,10, 2
	  do j = -10, 10, 2
	    do k = -10,10, 2
	     write(1,1) i, j, k, Bx(i,j,k), By(i,j,k),  
     $Bz(i,j,k)
1	     format(3I4, 3F9.5)	
	    enddo
	  enddo
	enddo
	

			
	end
	
	real function dbx1(x,y,z,theta,l)
	parameter (pi = acos(-1.d0))
	integer l
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 2.5e0
		
		if (l.gt.0) then
		
		dbx1 = (-1.e0/4.e0*pi)*(r*cos(theta)*y*dtheta)/
     $(sqrt((x-(r+2.5e0+r*cos(theta)))**2.d0 + y**2.d0 +
     $ (z-(r*sin(theta)))**2.d0))**3.d0
     		
     		else
     		
     		dbx1 = (-1.e0/4.e0*pi)*(r*cos(theta)*y*dtheta)/
     $(sqrt((x-(-r-2.5e0-r*cos(theta)))**2.d0 + y**2.d0 +
     $ (z-(r*sin(theta)))**2.d0))**3.d0
     
     		endif
     		
      end function
      
      real function dby1(x,y,z,theta,l)
	parameter (pi = acos(-1.d0))
	integer l
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 2.5e0
		
		if (l.gt.0) then
		
		dby1 = (1.e0/4.e0*pi)*dtheta*(r*cos(theta)*
     $(x-(r+2.5e0+r*cos(theta))) + r*sin(theta)*(z-r*sin(theta)))
     $ / (sqrt((x-(r+2.5e0+r*cos(theta)))**2.d0
     $ + y**2.d0 + (z-(r*sin(theta)))**2.d0))**3.d0
     
     		else
     		
     		dby1 = (1.e0/4.e0*pi)*dtheta*(r*cos(theta)*
     $(x-(-r-2.5e0-r*cos(theta))) - r*sin(theta)*(z-r*sin(theta)))
     $ / (sqrt((x-(-r-2.5e0-r*cos(theta)))**2.d0
     $ + y**2.d0 + (z-(r*sin(theta)))**2.d0))**3.d0
     	
     		endif
     
      end function
      
      real function dbz1(x,y,z,theta,l)
	parameter (pi = acos(-1.d0))
	integer l
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 2.5e0
		
		if (l.gt.0) then
		
		dbz1 =(-1.e0/4.e0*pi)*dtheta*(r*sin(theta)*y)
     $/(sqrt((x-(r+2.5e0+r*cos(theta)))**2.d0 + y**2.d0 
     $ + (z-(r*sin(theta)))**2.d0))**3.d0
     
     		else
     		
     		dbz1 =(1.e0/4.e0*pi)*dtheta*(r*sin(theta)*y)
     $/(sqrt((x-(-r-2.5e0-r*cos(theta)))**2.d0 + y**2.d0 
     $ + (z-(r*sin(theta)))**2.d0))**3.d0
     
     		endif
      end function
      
      real function dbx2(x,y,z,theta,l)
	parameter (pi = acos(-1.d0))
	integer l
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 2.5e0
		
		if (l.gt.0) then
		
		dbx2 = (1.e0/4.e0*pi)*dtheta*(-r*sin(theta)*
     $(z-r*sin(theta))-r*cos(theta)*(y-(2.5+r+r*cos(theta))))/
     $(sqrt(x**2.d0 + (y-(2.5+r+r*cos(theta)))**2.d0 +
     $ (z-(r*sin(theta)))**2.d0))**3.d0
     		
     		else
     		
     		dbx2 = (1.e0/4.e0*pi)*dtheta*(r*sin(theta)*
     $(z-r*sin(theta))-r*cos(theta)*(y-(-2.5-r-r*cos(theta))))/
     $(sqrt(x**2.d0 + (y-(-2.5-r-r*cos(theta)))**2.d0 +
     $ (z-(r*sin(theta)))**2.d0))**3.d0
     
     		endif
     		
      end function
      
      real function dby2(x,y,z,theta,l)
	parameter (pi = acos(-1.d0))
	integer l
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 2.5e0
		
		if (l.gt.0) then
		
		dby2 = (1.e0/4.e0*pi)*dtheta*(r*cos(theta)*
     $x)/ (sqrt(x**2.d0 + (y-(r+2.5e0+r*cos(theta)))**2.d0
     $ + (z-(r*sin(theta)))**2.d0))**3.d0
     
     		else
     		
     		dby2 = (1.e0/4.e0*pi)*dtheta*(r*cos(theta)*
     $x)/ (sqrt(x**2.d0 + (y-(-r-2.5e0-r*cos(theta)))**2.d0
     $ + (z-(r*sin(theta)))**2.d0))**3.d0
     	
     		endif
     
      end function
      
      real function dbz2(x,y,z,theta,l)
	parameter (pi = acos(-1.d0))
	integer l
	real x,y,z,theta,dtheta,r
	
		dtheta = (2.e0*pi/8.e0)
		r = 2.5e0
		
		if (l.gt.0) then
		
		dbz2 =(-1.e0/4.e0*pi)*dtheta*(r*sin(theta)*x)
     $/(sqrt(x**2.d0 + (y-(r+2.5e0+r*cos(theta)))**2.d0 
     $ + (z-(r*sin(theta)))**2.d0))**3.d0
     
     		else
     		
     		dbz2 =(1.e0/4.e0*pi)*dtheta*(r*sin(theta)*x)
     $/(sqrt(x**2.d0 + (y-(-r-2.5e0-r*cos(theta)))**2.d0 
     $ + (z-(r*sin(theta)))**2.d0))**3.d0
     
     		endif
      end function
      
     
     
