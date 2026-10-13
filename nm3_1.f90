program q1
    implicit none
    real :: a(4,4), b(4), x(4) = 0, sum
    real :: f(0:55), F_m(2,2)=0, x2(2) = 0, b2(2)=0, Det, Inv(2,2) = 0
    integer :: i, j, n
    data a/ 0.000007143, 0.6262, 0.4923, 0.8017, 0, 0.000007355, 0.2123, 0.6123, 0, 0, 0.000002534, 0.7165, 0, 0, 0, 0.000004133/
    data b/ 0.000009245, 0.3763, 0.6087, 0.4308/

    do i = 1, 4 
        x(i) = b(i)
        do j = 1, i-1
            x(i) = x(i) - a(i,j)*x(j)
        end do  
        x(i) = x(i)/a(i,i)
    end do

    !print *, "value of x = "
    !print *, '[ ', x, ' ]'
    
    
    do i = 1,4
        b(i) = 0
        do j = 1, i
            b(i) = b(i) + a(i,j)
        end do
    end do

    do i = 1, 4 
        x(i) = b(i)
        do j = 1, i-1
            x(i) = x(i) - a(i,j)*x(j)
        end do  
        x(i) = x(i)/a(i,i)
    end do

    !print *, "value of new x = "
    !print *, '[ ', x, ' ]'
    

    !fibonacci
    f(0) = 0
    f(1) = 1
    do i = 2,50
        f(i) = f(i-1) + f(i-2)
    end do

    print *, "enter value of n: "
    read *, n

    F_m(1,1) = f(n)
    F_m(1,2) = f(n+1)
    F_m(2,1) = f(n+1)
    F_m(2,2) = f(n+2)

    Det = (f(n)*f(n+2)-(f(n+1))*(f(n+1)))
    

    b2(1) = f(n+2)
    b2(2) = f(n+3)

    Inv(1,1) = F_m(2,2)
    Inv(1,2) = - F_m(1,2)
    Inv(2,1) = - F_m(2,1)
    Inv(2,2) = F_m(1,1)

    Inv = Inv/Det
    
    X2 = matmul(b2,Inv)

    print *, "matrix = ", F_m
    print *, "determinant = ", Det
    print *, "X1 and X2 are"
    print *, x2
    


end program q1