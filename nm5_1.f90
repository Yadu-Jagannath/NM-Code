program q1
    implicit none
    real*8 :: M(5,5), xval(5),yval(5), g, y(5), x0, y0,interpolate
    integer :: n, i, j

    n = 5
    data M/ 0.9401d0,0.9835d0,1.0277d0,1.0725d0,1.1180d0,0.9647d0,1.0118d0,1.0602d0,1.1097d0,1.1605d0,0.9876d0,1.0387d0,1.0915d0,1.1462d0,1.2030d0,1.0044d0,1.0587d0,1.1152d0,1.1743d0,1.2362d0,1.0107d0,1.0662d0,1.1242d0,1.1851d0,1.2492d0/
    xval = [50,60,70,80,90]
    yval = [50,52,54,56,58]

    x0 = 65
    y0 = 53
    !print matrix 
    do i = 1,n
        print *, (M(i,j),j=1,n) 
    end do

    print *, 'part 1'
    do i = 1,n
        y(i) = interpolate(n,xval,M(i,:),x0)
        print *, y(i)
    end do
    g = interpolate(n,yval,y,y0)
    print *, 'f(65,53 = )', g

    print *, 'part 2'
    do i = 1,n
        y(i) = interpolate(n,yval,M(:,i),y0)
        print *, y(i)
    end do
    g = interpolate(n,xval,y,x0)
    print *, 'f(65,53) = ', g

    print *, 'part 3'
    do i = 1,n

        y(i) = M(i,i)
    end do
    g = interpolate(n,xval,y,x0)
    print *, g
    do i = n-1,1,-1
            y(i) = M(i,n-i)
    end do
    g = interpolate(4,yval,y,y0)
    print *, g
end program q1

function interpolate(n,x,f,x0)
        implicit none
        real*8 :: x(n),f(n),x0,interpolate, D(10,10)
        integer :: i,j,n
        
        D(1:n,1) = f
        
    
        do i = 2,n
            do j = 2,i
                D(i,j) = (D(i,j-1) - D(i-1,j-1))/(x(i)-(x(i-j+1)))
            end do
        end do
        interpolate = D(n,n)
        do j = n-1,1,-1
            interpolate = interpolate*(x0 - x(j)) + D(j,j)
        end do
end function



