program infinite_series
    implicit none
    integer::i,n
    character*20::ch
    real::S,a,S1,S_back,S_back_10
    S = 0
    do i = 1,1000000 
        S1 = S
        a = 1./i
        write(ch,'(E13.3)')a
        read(ch,*)a
        S = S + a
        write(ch,'(E13.3)')S
        read(ch,*)S     
        
        if (S1 == S) exit
    end do
    n = i -1 
    print *, n
    print *, "Sum foward ="
    print '(E13.3)' , S

    S_back = 0
    do i = n, 1, -1
        a = 1./i
        write(ch,'(E13.3)') a
        read(ch,*)a
        S_back = S_back + a

        write(ch,'(E13.3)') S_back
        read(ch,*) S_back
    end do
    
    print *, n
    print *, "Sum backwards ="
    print '(E13.3)',  S_back

    S_back_10 = 0
    do i = 10*n, 1, -1
        a = 1./i
        write(ch,'(E13.3)') a
        read(ch,*)a
        S_back_10 = S_back_10 + a

        write(ch,'(E13.3)') S_back_10
        read(ch,*) S_back_10
    end do
    print *, 10*n
    print *, "sum backwards 10N ="
    print '(E13.3)', S_back_10



    S = 0
    do i = 1,1000000 
        S1 = S
        a = 1./i
        write(ch,'(E13.5)')a
        read(ch,*)a
        S = S + a
        write(ch,'(E13.5)')S
        read(ch,*)S     
        
        if (S1 == S) exit
    end do
    n = i -1 
    print *, n
    print *, "Sum foward ="
    print '(E13.5)' , S

    S_back = 0
    do i = n, 1, -1
        a = 1./i
        write(ch,'(E13.5)') a
        read(ch,*)a
        S_back = S_back + a

        write(ch,'(E13.5)') S_back
        read(ch,*) S_back
    end do
    
    print *, n
    print *, "Sum backwards ="
    print '(E13.5)',  S_back

    S_back_10 = 0
    do i = 10*n, 1, -1
        a = 1./i
        write(ch,'(E13.5)') a
        read(ch,*)a
        S_back_10 = S_back_10 + a...
        write(ch,'(E13.5)') S_back_10
        read(ch,*) S_back_10
    end do
    print *, 10*n
    print *, "sum backwards 10N ="
    print '(E13.5)', S_back_10
end program infinite_series
