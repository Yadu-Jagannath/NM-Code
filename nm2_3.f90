real*8 function besselJ(x)
real*8 :: x, j(0:50)
integer :: i
j(0) = 0.765197686557966d0
j(1) = 0.440050585744933d0
do i = 1,30
    j(i+1) = (2*i/x)*j(i) - j(i-1)
    print *, i, j(i+1), besjn(i+1, x)
end do
return 
end function besselJ

real*8 function besselJ_back(x)
real*8 :: x, j(0:50), S
integer :: i
m = 30
j(m+1 ) = 0
j(m) = 1
do i = m, 1, -1
    j(i-1) = (2*i/x)*j(i) - j(i+1)
end do
S = j(0)
do i = 2,m,2
    S = S + 2*j(i)
end do
j = j/S 
do i = 0,m
    print *, j(i), besjn(i, x)
end do
return
end function besselJ_back

program besselJ_out
    real*8 :: x, besselJ, besselJ_back, final, final2
    x = 1.0
    final = besselJ(x) 
    final2 = besselJ_back(x)
    print *, final, final2
end program besselJ_out 
