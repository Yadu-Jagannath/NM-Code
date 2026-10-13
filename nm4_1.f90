program q1
    implicit none    
    character(len=*), parameter:: output_file = 'interpolation.txt'
    real*8, parameter :: pi = 3.14159265359
    integer :: n, i, j, fu
    real*8 :: a(20), D(20,20), error, maximum
    real*8 :: f,g, x
    !f(x) = sin(x)
    f(x) = 1.0d0/(1+25*(x**2))
    open(action = 'write', file = output_file, newunit = fu, status = 'replace')
    print *, "enter n: "
    read *, n



    do i = 1,n
        !equal spacing
        a(i) = -1.0d0 + (i-1)*(2.0d0/(n-1))
        !chebyshev
        !a(i) = cos(((2.0d0*i-1.0d0)*pi)/(2.0d0*n))
        D(i,1) = f(a(i))
    end do
    do i = 2,n
        do j = 2,i
            D(i,j) = (D(i,j-1) - D(i-1,j-1))/(a(i)-(a(i-j+1)))
        end do
    end do

    maximum = 0
    do i = 0,200
        x = -1 + i*(0.01d0)
        g = D(n,n)
        do j = n-1,1, -1
            g = g*(x-a(j)) + D(j,j)
        end do
        error = g - f(x)
        maximum = max(maximum, abs(error))
        write(fu,*) x, error
    end do

    print *, "max error = ", maximum
    close(fu)
    
end program q1






        




