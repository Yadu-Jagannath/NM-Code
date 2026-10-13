program q3
    implicit none
    real :: dist(8), time(8), D(8,8), x(4), actual(4), g, error, g1, error2
    integer :: i, j, n, i1, array(4)

    n = 8
    time = [9.58, 19.19, 43.03, 100.91, 206.00, 755.36, 1571.00, 5207.4]
    dist = [100, 200, 400, 800, 1500, 5000, 10000, 30000]
    x = [1000, 2000, 25000, 42195]
    actual = [131.83, 283.13, 72.25, 119.30]
    array = [5,6,8,8]

    do i = 1,8
        D(i,1) = time(i)
    end do

    do i = 2,8
        do j = 2,i
            D(i,j) = (D(i,j-1) - D(i-1,j-1))/(dist(i)-(dist(i-j+1)))
        end do
    end do

    print *, "dist and time: "
    do i = 1,4
        g = D(n,n)
        do j = 7,1,-1
            g = g*(x(i) - time(i)) + D(j,j)
        end do
        error = g - actual(i)
        print *, "for dist = ", x(i), " est. time = ", g, " error = ", error
    end do



    print *, "log(dist) vs log(time) :"
    do i = 1,8
        D(i,1) = log(time(i))
    end do

    do i = 2,8
        do j = 2,i
            D(i,j) = (D(i,j-1) - D(i-1,j-1))/(log(dist(i))-(log(dist(i-j+1))))
        end do
        print *, D(i,1:3)
    end do

    print *, "dist and time: "
    do i = 1,4
        g = D(n,n)
        do j = 7,1,-1
            g = g*(log(x(i)) - log(dist(j))) + D(j,j)
        end do
        i1 = array(i)
        g1 = D(i1,3)
        do j = i1-1, i1-2, -1
            g1 = g1*(log(x(i)) - log(dist(j))) + D(j,j+3-i1)
        end do
        error = exp(g) - (actual(i))
        error2 = exp(g1) - (actual(i))

        print *, "for dist = ", (x(i)), " est. time = ", exp(g), exp(g1), " error = ", error, error2
    end do

    stop
    print *, "inverse estimation: "
    do i = 1,8
        D(i,1) = time(i)
    end do

    do i = 2,8
        do j = 2,i
            D(i,j) = (D(i,j-1) - D(i-1,j-1))/(dist(i)-(dist(i-j+1)))
        end do
    end do

    print *, "dist and time: "
    do i = 1,4
        g = D(n,n)
        do j = 7,1,-1
            g = g*(x(i) - time(i)) + D(j,j)
        end do
        error = g - actual(i)
        print *, "for dist = ", x(i), " est. time = ", g, " error = ", error
    end do



    print *, "log(dist) vs log(time) :"
    do i = 1,8
        D(i,1) = log(time(i))
    end do

    do i = 2,8
        do j = 2,i
            D(i,j) = (D(i,j-1) - D(i-1,j-1))/(log(dist(i))-(log(dist(i-j+1))))
        end do
    end do

    print *, "dist and time: "
    do i = 1,4
        g = D(n,n)
        do j = 7,1,-1
            g = g*(log(x(i)) - log(time(i))) + D(j,j)
        end do
        error = g - log(actual(i))
        print *, "for dist = ", (x(i)), " est. time = ", g, " error = ", error
    end do
end program q3



