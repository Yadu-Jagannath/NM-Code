program q5
    implicit none
    real*8 :: a,b, t, pi, vals(5)
    real*8 :: f, h, x, sum, sum1, sum2, sim1, sim2,ans1(4), ans2(4), v 
    integer :: n, i, j, k
    

    !f(x) = x**x !q1
    !f(x) = exp(-(x+1/x))/x**20 !q2 f(a) = 0
    !f(x) = log(1/x)*sin(x) !q3 f(a) = 0
    !f(x) = cos(t*sin(x)-x) !q4
    !f(x) = 2.0d0/sqrt(pi)*(exp(-x**2))

    f(x) = exp(1/x)

    pi = 4.0d0*atan(1.0d0)
    t = 8

    vals = [0.002d0,0.01d0,0.1d0,0.2d0,1.0d0]
    v = 0
    do k = 4,1,-1
    a = vals(k)
        
    b = vals(k+1)
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
        !print *,j, n, sum1, sim1

        if (abs((sim1-sim2)/sim1) < 1d-12) exit
        ans1(k) = sum1
        ans2(k) = sim1

    end do
    v = v + sim1
    print *, a,v
end do

!print *, '0.2 to 1 = ', ans1(4), ans2(4)
!print *, '0.1 to 1 = ', ans1(3)+ans1(4), ans2(3)+ans2(4)
!print *, '0.01 to 1 = ', ans1(2)+ans1(3)+ans1(4), ans2(2)+ans2(3)+ans2(4)
!print *, '0.002 to 1 = ', ans1(1)+ans1(2)+ans1(3)+ans1(4), ans2(1)+ans2(2)+ans2(3)+ans2(4)
    
end program