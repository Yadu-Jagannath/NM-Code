program q8
    implicit none
    real*8 :: a,b
    real*8 :: f, h, sum, sum1, sum2, sim1, sim2
    integer :: n, i, j

    a = 0.1d0
    b = 0.2d0
    n = 1
    sum = (f(a)+f(b))/2
    sum1 = 0
    sim1 = 0

    do j = 1,25
        sum2 = sum1
        sim2 = sim1
        n = n*2
        h = (b-a)/n
        do i = 1,n-1,2
            sum = sum + f(a + i*h)
        end do
        sum1 = sum * h
        
        sim1 = sum1 + (sum1 - sum2)/3.0d0
        print *,j, n, sum1, sim1 ! , (pi**4)/15  !, pi*besjn(1,t)

        if (abs((sim1-sim2)/sim1) < 1d-12) exit

    end do
end program q8

function f(x)
    implicit none
    real*8 :: f, x, pi
    integer :: i
    pi = 4.0d0*atan(1.0d0)
    f = 0
    do i = 1,10
        f = f + 2.0d0**(-i)*cos(x*pi*7.0d0**i)
    end do
    f = f*(1.0d0/pi)
end function 

