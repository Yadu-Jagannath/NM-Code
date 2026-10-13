program q1
    implicit none
    real*8 :: a,b, t, pi
    real*8 :: f, h, x, sum, sum1, sum2, sim1, sim2
    integer :: n, i, j


    !f(x) = x**x !q1
    !f(x) = exp(-(x+1/x))/x**20 !q2 f(a) = 0
    !f(x) = log(1/x)*sin(x) !q3 f(a) = 0
    !f(x) = cos(t*sin(x)-x) !q4
    !f(x) = 2.0d0/sqrt(pi)*(exp(-x**2)) !q5
    f(x) = (x**3)/(exp(x)-1.0d0) !q7 f(a) = f(b) = 0


    pi = 4.0d0*atan(1.0d0)
    t = 8

    a = 0
    !print *, 'enter b = '
    !read *, b
    b = 1000
    n = 1
    !sum = (f(a)+f(b))/2
    !sum = 0 !for q7
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

end program