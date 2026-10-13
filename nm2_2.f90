real*8 function exponential(x)
real*8 :: x,sum, t
integer :: i
sum = 1
t = 1
do i = 1,1000
    t = t * (abs(x)/i)
    sum = sum + t
    if (abs(t/sum) <= 1E-16) exit
end do
if (x < 0) sum = 1/sum
exponential = sum
return 
end function exponential
            
program exponential_out
    real*8 :: x, exponential

    print *, "enter x"
    read *, x

    print *, "exp = ", exponential(x), exp(x)
end program exponential_out

