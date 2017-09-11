program tiempo_vuelo
  implicit none
  !definimos constantes que se utilizaran
  real, parameter :: g = 9.8
  real, parameter :: pi=3.1415927

  !se define las variables
  real :: a, v
  real :: t

  !Leer valores para el angulo y velocidad inicial.
  write(*,*) 'Diga angulo, y la velocdad inicial'
  read(*,*) a, v

  !Convertir los grados a radianes.
  a= a * pi / 180.0

  !la ecuacion para encontrar el tiempo.
  t= (2 * v) * sin(a)
  t= t / g

  !El resultado del tiempo.
  write(*,*) 't: ',t

 end program tiempo_vuelo
