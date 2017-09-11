program altura_max
  implicit none
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927
  real :: v, a
  real :: h, square, two !altura, cuadrado del seno, dos veces la
                         !gravedad.
  

  !leer valores para la velocidad inicial y el angulo de la
  !trayectoria.
  write (*,*) 'De la velocidad inicial y el angulo de tiro'
  read (*,*) v, a

  !convertir el angulo a radianes.
  a = a * pi / 180


  !sacar la altura maxima.
  v = v * v !velocidad al cuadrado
  square = sin(a) * sin(a) !seno al cuadrado
  two = 2 * g
  h = v * square
  h = h / two

  !el resultado
  write(*,*) 'altura maxima: ',h


end program altura_max
