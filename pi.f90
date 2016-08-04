! A program which uses Monte Carlo sampling to numerically calculate pi. 

program pi
	integer :: hits, trials, i, iseed
	double precision :: pi, x, y, a, radius
	trials=1000000
	iseed=-164973

	radius=12.5d0
	
	do i=1, trials
		! select a random number from [0.0, 1.0] for (x,y)
	   	call random_number(x)
	   	call random_number(y)
		
		! convert (x,y) to be in the range [0,radius], [0,radius]
	   	x=x*radius
	   	y=y*radius
	
	   	a=x**2+y**2
	   	if (a<=radius) then
	      	hits = hits + 1
	   	end if
	
	end do
	
	pi=4.0*radius*hits/trials
	write (*,*) pi

end program pi
