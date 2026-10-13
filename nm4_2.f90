program q2
    real*8 :: x(10), D(10,10), xvals(5)
    real*8 :: f, g, xx
    integer :: i, j, n

    f(xx) = sin(xx)
    n = 10
    print *, "forward interpolation"
    do i = 1,10
        x(i) = 1.560d0 + 0.001d0*i
        D(i,1) = f(x(i))
    end do
    do i = 2,n
        do j = 2,i
            D(i,j) = (D(i,j-1) - D(i-1,j-1))/(x(i)-(x(i-j+1)))
        end do
    end do

    xvals = [1.5716d0, 1.5736d0, 1.5635d0, 1.5605d0, 1.5585d0]

    do i = 1,5
        g = D(n,n)
        do j = 9,1,-1
            g = g*(xvals(i)-x(j)) + D(j,j)
        end do
        print *, " value at ", xvals(i), ' = ', g, g-sin(xvals(i))
    end do

    print *, "inverse interpolation"

    do i = 1,10
        D(i,1) = 1.560d0 + 0.001d0*i
        x(i) = f(D(i,1))
    end do
    print *, x(1)
    print *, x(n)
    do i = 2,n
        do j = 2,i
            D(i,j) = (D(i,j-1) - D(i-1,j-1))/(x(i)-(x(i-j+1)))
        end do
    end do

    xvals = [0.999995d0, 0.99995d0, 0.9999d0, 0.9995d0, 0.0d0]

    do i = 1,4
        g = D(n,n)
        do j = 9,1,-1
            g = g*(xvals(i)-x(j)) + D(j,j)
        end do
        print *, " value at ", xvals(i), ' = ', g, g-asin(xvals(i))
    end do

end program q2