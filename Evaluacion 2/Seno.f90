subroutine seno (n, j, fi, fj, sen, signo, potencia, factorial)
	integer, intent (in)      :: n
	double precision, intent (in) :: fi
	integer :: j
	double precision, dimension (10000), intent(out) :: sen
	double precision :: fj, term, partial_sum, signo, potencia, factorial

	
	signo = 1.0d0
	term = fi
	partial_sum = term
	potencia = fi
	factorial = 1
	do j = 1, n
	 fj = dble(j)
	 potencia = fi**(j + 2)
	 factorial = factorial * (j + 1) * (j + 2)
	 signo = signo * (-1.0d0)
	 term = potencia / factorial
	 term = term * signo
	 partial_sum = partial_sum + term
	 sen(j) = partial_sum
	 
	enddo

	 
end subroutine seno
	 

program calculoseno
	double precision, dimension (10000) :: f
	integer :: i, j, n
	double precision, dimension (10000)   :: x
	double precision, dimension (10000) :: sen
	double precision, dimension (10000) :: funcion
	double precision :: fi, fj, term, partial_sum, signo, potencia, factorial
	

     open (1, file = 'senos.dat', status = 'unknown')
	fi = -3.1d0
	do i=1, 60
	write (1,*) fi, fi
	fi = fi + 0.1d0
	
	end do
	write (1,*) ' '
	do n=1, 15, 2
	  fi = -3.1d0
	do i=1, 60
	fi = fi + 0.1d0
	call seno (n, j, fi, fj, sen, signo, potencia, factorial)
	funcion(n) = sen(n)
	write (1,*) fi, funcion(n)

	end do
	write (1,*) ' '
	end do
     close (1)

end program calculoseno
