program quadratic_roots
    implicit none 
    real :: a, b, c
    real :: root1, root2, real_part, im_part
    print *, "Enter a, b and c for ax^2 + bx + c:"
    read *, a ,b , c
    if (b**2 - 4*a*c == 0) then
        print *, "duplicate roots"
        root1 = (-b/(2*a))
        print *, "root is = ", root1
    elseif (b**2 - 4*a*c < 0) then
        print *, "roots are complex"
        real_part = -b/(2*a)
        im_part = sqrt(-(b**2 - 4*a*c))/(2*a)
        print *, "root 1 = ", real_part, " + ", im_part, "i"
        print *, "root 1 = ", real_part, " - ", im_part, "i"
    else
        if (b>0) then
            root1 = (-b - sqrt(b**2 - 4*a*c))/(2*a)
            root2 = c/(a*(root1))
        else
            root1 = (-b + sqrt(b**2 - 4*a*c))/(2*a)
            root2 = c/(a*(root1))
        endif
        print *, "root 1 = ", root1
        print *, "root 2 = ", root2
    endif
    
end program quadratic_roots
