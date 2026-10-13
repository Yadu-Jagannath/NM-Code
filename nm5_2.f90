program q2
    implicit none
    real*8 :: f,x,f1_3,f2_3,f1_5,f2_5,h
    integer :: i

    f(x) = exp(x)
    !f(x) = tan(x)

    x = 0.0d0
    h = 1.1d0
    do i = 1,25
        f1_3 = (f(x+h)-f(x-h))/(2*h)
        f2_3 = (f(x+h)-2*f(x)+f(x-h))/(h**2)
        f1_5 = (f(x-2*h)-8*f(x-h)+8*f(x+h)-f(x+2*h))/(12*h)
        f2_5 = (-f(x-2*h)+16*f(x-h)-30*f(x)+16*f(x+h)-f(x+2*h))/(12*(h**2))
        h = h/2
        print *, f1_3, f2_3, f1_5, f2_5, h
    end do
end program 
