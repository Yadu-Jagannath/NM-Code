program question_1
    implicit none
    real*16:: x, f, x2, f2, x3, f3,m,f1, x1, S1
    integer :: i, m1, j

    !part 1
    print *, "part 1"

    x = 1.1

    do i = 1,2000
        f = (log(x))/(x**(0.01))
        
        x = x*2
    end do

    !print *, "lim x -> inf: ln(x)/x^0.01 = ", f1, "at i = 2000"

    !part 2
     print *, "part 2"

    x1 = 1.1
    do i = 1,50
        f1 = (exp(x1)-1)/(x1)
        x1 = x1/2
        !print *, i, x1, f1
    end do

    !part 3
    print *, "part 3"
    x2 = 1.1
    do i = 1,25
        f2 = (cos(x2) + cosh(x2) - 2)/(x2**4)
        x2 = x2/2
        !print *, i, x2, f2
    end do
    
    !part 4
    print *, "part 4"
    m = 10.0d0
    x3 = 2.0d0
    do i = 1,1000000
        f3 = (((1+ (2.0d0/m))**m)-exp(2.0d0))
        m = m+10.0d0
        print *, i, m, f3
    end do

    !part 5
    print *, "part 5"
    m1 = 1
    do  j = 1,20
        S1 = 0
        do i = 1,m1
            S1 = S1 + 1.0/i 
        end do  
        S1 = S1 - log(real(i))
        m1 = m1*2
        !print *, j, m1, S1
    end do

    !part 6
    print *, "part 6"
    x1 = 1.1
    do i = 1, 17000

        f1 = x1**(0.01)*log(x1)
        x1 = x1/2
        !print *, i, x1, f1
    end do

    
end program question_1