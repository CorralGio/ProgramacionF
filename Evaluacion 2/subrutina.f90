subroutine exptaylor (n, j, fi, fj, exptay)
	integer, intent (in)      :: n
	double precision, intent (in) :: fi
	integer :: j
	double precision, dimension (100), intent(out) :: exptay
	double precision :: fj, term, partial_sum
	
	
	
	term = 1
	partial_sum = term
	do j = 1, n
	 fj = dble(j)
	 term = term * fi / fj
	 partial_sum = partial_sum + term
	 exptay(j) = partial_sum
	enddo

	 
end subroutine exptaylor
	 

program segundo
	double precision, dimension (15) :: f
	integer :: i, j, n
	double precision, dimension (100)   :: x
	double precision, dimension (100) :: exptay
	double precision, dimension (100) :: funcion
	double precision :: fi, fj, term, partial_sum

     open (1, file = 'datos.dat', status = 'unknown')
	
	do n=1, 15, 2
	do i=0, 100, 1
	  fi = dble(i)
	  fi = fi / 10.0d0
	call exptaylor (n, j, fi, fj, exptay)
	funcion(n) = exptay(n)
	write (1,*) fi, funcion(n)

	end do
	write (1,*) ' '
	end do
     close (1)

end program segundo
