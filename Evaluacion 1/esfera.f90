program esfera


  implicit none  

  integer :: ierr
  character(1) :: yn
  real :: radius, area, volumen
  real, parameter :: pi = 3.141592653589793

  interactive_loop: do


    write (*,*) 'Declare el radio de la esfera'
    read (*,*,iostat=ierr) radius

    if (ierr /= 0) then
      write(*,*) 'Error, entrada invalida'
      cycle interactive_loop
    end if


    area = 4 * pi * radius * radius
    volumen = 4 * pi * radius**3 / 3


    write (*,'(1x,a7,f14.2,5x,a7,f14.2,5x,a9,f14.2)') &
      'radius=',radius,'area=',area, 'volumen=',volumen

    yn = ' '
    yn_loop: do
      write(*,*) 'Perform another calculation? s[n]'
      read(*,'(a1)') yn
      if (yn=='s' .or. yn=='S') exit yn_loop
      if (yn=='n' .or. yn=='N' .or. yn==' ') exit interactive_loop
    end do yn_loop

  end do interactive_loop












end program esfera
