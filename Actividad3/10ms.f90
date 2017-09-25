program ms
  implicit none
  integer :: i, a
  integer, parameter :: ntimes = 100
  integer, parameter :: maxang = 90
  real, dimension (200) :: x,y
  real :: radian, time, fa, fi
  real, parameter :: deltat = 0.1
  real, parameter :: g = 5  !como ocupamos 1/2 g para los calculos.
  real, parameter :: pi = 3.1415927
  real, parameter :: vo = 10


 open (1, file = 'losdatos.dat', status='unknown')
  do a=15, 90, 15
	fa = float(a)
	radian = fa * pi / 180
	do i=1, ntimes
	fi = float(i)
	time = fi * deltat
	x(i) = vo * time * cos(radian)
	y(i) = vo * time * sin(radian) - 0.5 * g * time * time
	if(y(i).LT.0) exit	
   	write (1,*) x(i), y(i)


  end do
write(1,*)' '
end do
close(1)


end program ms
   


!!Este programa calcula la posicion de una particula la ser lanzada
!!con una velocidad de 10 m/s y a distintos angulos predeterminados.
