program summation
implicit none
integer :: suma, a, conta
real :: aritme, armoni, sumarmo, fa, fc, fs

print*, "Este programa realiza las medias de una sumatoria, cuando quiera aplaste 0 para terminar"
open(unit=10, file="SumData.DAT", status='unknown')

suma = 0
conta = 0
sumarmo = 0

do
 print*, "De numero:"
 read*, a
 if (a == 0) then
  exit
 else
suma = suma + a
conta = conta + 1
fa = float(a)
fa = 1/fa
sumarmo = sumarmo + fa

 end if
 write(10,*) a
end do
fs = float(suma)
fc = float(conta)
aritme = fs / fc
armoni = fc / sumarmo


print*, "Sumatoria =", suma
write(10,*) "Sumatoria =", suma
write(10,*)' '
print*, "Media aritmetica =", aritme
write(10,*) "Media aritmetica =", aritme
write(10,*) ' '
print*, "Media armonica =", armoni
write(10,*) "Media armonica =", armoni
write(10,*) ' '


close(10)

end
