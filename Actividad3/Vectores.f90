program Vector
  implicit none
  real, parameter :: g = 4.9  !como ocupamos 1/2 g para los calculos.
  real, parameter :: pi = 3.1415927
  integer :: i

  real :: a, vo
  real, dimension (20) :: x,y

  write(*,*) 'De por favor el angulo de tiro y la velocidad inicial'
  read (*,*) a, vo

  a = a * pi / 180

  open(1, file='data1.dat', status='unknown')
  do i =1, 20
  x(i) = vo * i * cos(a)
  y(i) = vo * i * sin(a) - (g * i**2)
  write(1,*) x(i), y(i)
  
end do
close(1)
end program Vector

!! Este es un programa donde se calcula la posicion de una particula,
!! pidiendole los datos al usuario.
