!inverse of matrix via gaussian elimination after joining right hand side
program q2
    real*8 :: A(20,20), B(20,20), temp(20), det, pivot, factor
    integer :: i,j,n,k, k1
    real*8, parameter :: pi = 4.0d0*atan(1.0d0)

    print *, "enter n"
    read *, n
    B = 0

    do i = 1,n
        B(i,i) = 1
        do j = 1,n
            A(i,j) = 1.0d0/(i+j-1)
            !A(i,j) = sqrt(2.0/(n+1))*sin(i*j*pi/(n+1))
        end do
    end do

    do i = 1,n
        B(i, n+1 ) = sum(A(i,1:n))
    end do

    do i = 1,n
        print *, A(i,1:n)
    end do

    det = 1
    do k = 1,n-1
        k1 = maxloc(abs(A(k:n,k)),dim=1) + k -1
        if (k1 /= k) then
            det = - det
            temp(k:n)=A(k,k:n)
            A(k,k:n) = A(k1,k:n)
            A(k1,k:n) = temp(k:n)

            temp(1:n+1)=B(k,1:n+1)
            B(k,1:n+1) = B(k1, 1:n+1)
            B(k1,1:n+1) = temp(1:n+1)
        endif

        pivot = A(k,k)
        A(k+1:n,k) = A(k+1:n,k)/pivot



        do i = k+1,n
            A(k+1:n,i) = A(k+1:n,i) - A(k+1:n,k)*A(k,i)
        end do        
        do i = 1,n+1
            B(k+1:n,i) = B(k+1:n,i) - B(k,i)*A(k+1:n,k)
        end do


    end do

    do i = 1,n
        det = det*A(i,i)
    end do
    print *, 'det = ', det

    print *, 'hello'

    do k = 1,n+1
        do j = n,1,-1
            do i = j+1,n
            B(j,k) = B(j,k) -A(j,i)*B(i,k)
            end do
         B(j,k) = B(j,k)/A(j,j)

        end do
    end do


    
    do i = 1,n
            print *, B(i,1:n+1)
    end do
 

end program q2

    