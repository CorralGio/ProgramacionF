program x_max
  implicit none

  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415972

  real :: v, a
  real :: square, two, d !cuadrado de la velocidad, dos veces el
                         !angulo, distancia final. 

  write(*,*) 'Indique la velocidad inicial y el angulo de tiro: '
  read(*,*) v, a


  !sacar radianes
  a = a * pi /180
  
  !sacar la velocidad al cuadrado y esta entre gravedad.
  v = v * v
  v = v / g

  !el doble angulo.
  two = 2 * a

  !el calculo de la distancia.
  d = v * sin(two)

  write (*,*) 'La distancia maxima de tiro es: ', d

end program x_max
  
  
