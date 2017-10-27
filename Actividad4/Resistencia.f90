program resistencia
  implicit none
  integer :: i, v
  real :: vt
  integer, parameter :: ntimes = 10000
  real, dimension (10000) :: vx,vy
  real, dimension (10000) :: x,y
  real :: time, fa, fi, fv, area, constant
  real, parameter :: deltat = 0.01
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927
  real, parameter :: m = 0.142, d = 0.07, cd = 0.47 !Son, respectivamente, la masa (kg), el diametro (m) y el coeficiente de arrastre de la esfera. 
  real, parameter :: p = 1.225 !densidad del aire en kg/m³.
  real, parameter :: radian = pi / 4 !trabajaremos con 45 grados

  area = d * d * pi
  area = area / 4
  vt = 2 * m * g / (p * area * cd)
  vt = SQRT(vt)
  constant =  m * g / vt
  write (*,*) 'Constante: ',constant ! Estas operaciones son necesarias para encontrar la constante que se utilizara en el calculo de la posición.

  
  open (1, file = 'datos.dat', status = 'unknown')
    do v=10, 100, 10
        fv = float(v)
 
     do  i=1, ntimes  
        fi = float(i)
        time = fi * deltat  !los primeros dos deltat se encontraran despreciando la fricción.     
        if (i.LT.3) then
         vx(i) = fv * cos (radian)
         vy(i) = fv * sin (radian) - g * time
         x(i) = fv * time * cos(radian)
	 y(i) = fv * time * sin(radian) - 0.5 * g * time * time
        end if
         
      
       if (i.GT.2) then
       vx(i) = -vx(i-1) * deltat / m * constant + vx(i-1)
       vy(i) = -vy(i-1) * deltat / m * constant + vy(i-1) - deltat * g
       x(i) = x(i-1) + vx(i-2) * deltat - vx(i-2) * deltat * deltat * constant / m
       y(i) = y(i-1) - deltat * deltat * g + deltat * vy(i-2) - deltat * deltat * vy(i-2) * constant / m
       end if
       

       if(y(i).LT.0) exit	
   	write (1,*) x(i), y(i)

  end do
       write(1,*)' '
    end do
  close(1)

end program resistencia
