!Write subroutines to add and subtract two fixed-point numbers
!of n digits using an integral base b > 1. Also write subroutines
!to divide and multiply such numbers by a single-digit number in
!the same base. Using appropriate value of b (e.g., 10^5 ), calculate
!the value of π correct to 1000 decimal places by employing theseries expansion for
!π = 16 tan−1 (15) − 4 tan−1 (1239),
!where tan−1 x can be calculated by the Maclaurin series

program pi_1000_decimal_places
    implicit none
    integer, parameter :: base  = 100000          
    integer, parameter :: ndec  = 1000            
    integer, parameter :: guard = 20              
    integer, parameter :: n     = (ndec + guard) / 5   

    integer :: arctan5(n), arctan239(n), x(n), term(n), zeros(n)
    integer :: frac16(n), frac4(n), fracpi(n)
    integer :: k, sgn, rem, overflow16, overflow4, intpart

    zeros = 0

    !arctan(1/5) = sum (-1)^i * ((1/5)^(2i+1)/(2i+1))
    call div_digit(zeros, n, base, 5, 1, x, rem)
    arctan5 = x
    k = 1
    sgn = -1
    do
        call div_digit(x,n,base,25,0,x,rem)
        if (all_zero(x,n)) exit
        k = k+2
        call div_digit(x,n,base,k,0,term,rem)
        if (sgn < 0) then
            call sub_n(arctan5, term, n, base, arctan5)
        else
            call add_n(arctan5, term, n, base, arctan5)
        endif
        sgn = -sgn
    end do

    !arctan(1/239)
    call div_digit(zeros, n, base, 239, 1, x, rem)
    arctan239 = x
    k = 1
    sgn = -1
    do
        call div_digit(x, n, base, 57121, 0, x, rem) 
        if (all_zero(x, n)) exit
        k = k + 2
        call div_digit(x, n, base, k, 0, term, rem)
        if (sgn < 0) then
            call sub_n(arctan239, term, n, base, arctan239)
        else
            call add_n(arctan239, term, n, base, arctan239)
        end if
        sgn = -sgn
    end do

    ! pi = 16*arctan(1/5) - 4*arctan(1/239)
    call mul_digit(arctan5,   n, base, 16, frac16, overflow16)
    call mul_digit(arctan239, n, base, 4,  frac4,  overflow4)
    intpart = overflow16 - overflow4          ! the "carry beyond MSD" IS the integer part
    call sub_n(frac16, frac4, n, base, fracpi)

    call print_pi(intpart, fracpi, n, ndec)




contains

    !addition in base b 
    subroutine add_n(a, b, n, base, c)
        integer :: a(n), b(n), n, c(n), base
        integer :: i, carry, temp
        carry = 0
        do i = n, 1, -1
            temp = a(i) + b(i) + carry
            if (temp>= base) then
                c(i) = temp - base
                carry = 1
            else 
                c(i) = temp
                carry = 0
            endif
        end do
    end subroutine add_n

    !subtraction
    subroutine sub_n(a,b,n,base,c)
        integer :: a(n), b(n), n, c(n), base
        integer :: i, borrow, temp
        borrow = 0
        do i = n, 1, -1
            temp = a(i) - b(i) - borrow
            if (temp < 0) then
                c(i) = temp + base
                borrow = 1
            else
                c(i) = temp
                borrow = 0
            endif
        end do
    end subroutine sub_n

    !multipl
    subroutine mul_digit(a, n, base, d, c, overflow)
        integer :: a(n), n, base, d, c(n), overflow,t, carry
        integer :: i
        carry = 0
        do i = n, 1, -1
            t = a(i) * d  + carry
            c(i) = int(mod(t, base))
            carry = t / base
        end do
        overflow = int(carry)
    end subroutine mul_digit

    !division
    subroutine div_digit(a,n,base,d,r0,q,r)
        integer :: a(n), n, base, d, r0
        integer :: q(n), r, t, i
        r = r0
        do i = 1,n
            t = r * base + a(i)
            q(i) = t/d 
            r = mod(t, d)
        end do
    end subroutine div_digit

    logical function all_zero(a, n)
        integer, intent(in) :: a(n), n
        all_zero = all(a == 0)
    end function all_zero


    subroutine print_pi(intpart, fracpi, n, ndec)
        integer :: intpart, fracpi(n), n, ndec
        integer :: i, ngroups, unit
        character(len=20) :: fmt
        ngroups = ndec/5
        open(newunit = unit, file = 'pi1000.txt', status = 'replace')
        write(unit, '(I0,A)', advance='no') intpart, '.'
        do i = 1, ngroups
            write(unit, '(I5.5)', advance = 'no') fracpi(i)
        end do
        write(unit, *)
        close(unit)
        print '(A,I0,A)', 'wrote', ndec, 'decimal difgits of pi to pi1000.txt'

        end subroutine print_pi

end program pi_1000_decimal_places
    