program serie
 implicit none
integer :: i
real :: n, suma, iteracion, pi

  pi = 1
  iteracion = 1
 write(*,*) 'El valor de pi/4 segun las repeticiones:'
     do i=1, 50
     iteracion = iteracion * (-1)
     n = 2 * i + 1
     n = 1 / n
     n = n * (iteracion)
     pi = pi + n
	if (i.EQ.10) then
	write(*,*) ' '
	write(*,*) '10:', pi
        end if

	if (i.EQ.20) then
	write(*,*) ' '
	write(*,*) '20:', pi
        end if

	if (i.EQ.30) then
	write(*,*) ' '
	write(*,*) '30:', pi
        end if

	if (i.EQ.40) then
	write(*,*) ' '
	write(*,*) '40:', pi
        end if

	if (i.EQ.50) then
	write(*,*) ' '
	write(*,*) '50:', pi
        end if

end do

end program serie






