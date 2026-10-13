!Write a program to perform oating point addition of two 4 digit
!numbers in decimal system. The numbers are represented in the
!form 0d1 d2 d3 d4 × 10e , where 0 ≤ di ≤ 9 are the decimal digits,
!with d1 > 0 and e is the exponent. The requirement d1 > 0
!ensures uniqueness of representation. The nal result should
!also be expressed in the same form. Assuming both numbers to
!be positive the program must nd the sum correctly rounded to
!nearest value with 4 decimal digits, for all possible input numbers
!satisfying the above restrictions. The program should use only
!integer variables and treat each digit separately.

!Write a program to perform oating point addition of two 4 digit
!numbers in decimal system. The numbers are represented in the
!form 0d1 d2 d3 d4 × 10e , where 0 ≤ di ≤ 9 are the decimal digits,
!with d1 > 0 and e is the exponent. The requirement d1 > 0
!ensures uniqueness of representation. The nal result should
!also be expressed in the same form. Assuming both numbers to
!be positive the program must nd the sum correctly rounded to
!nearest value with 4 decimal digits, for all possible input numbers
!satisfying the above restrictions. The program should use only
!integer variables and treat each digit separately.

program float_addition
implicit none

    integer :: d1_a, d2_a, d3_a, d4_a, e_a
    integer :: d1_b, d2_b, d3_b, d4_b, e_b
    integer :: diff, i
    integer :: carry 
    integer :: bigger(9) = 0, smaller(9) = 0
    integer :: shifted_smaller(9) = 0
    integer :: sum(9)
    integer :: result(4), exponent

    !input  first number
    print *, "enter first number in the form d1 d2 d3 d4 e. d1 must be between 1 and 9 and d2, d3, d4 must be between 0 and 9"
    read *, d1_a, d2_a, d3_a, d4_a, e_a
    !input second number
    print *, "enter second number in the form d1 d2 d3 d4 e. d1 must be between 1 and 9 and d2, d3, d4 must be between 0 and 9"
    read *, d1_b, d2_b, d3_b, d4_b, e_b

    !check which exponent bigger
    if (e_a >= e_b) then 
        bigger(1:4) = [d1_a, d2_a, d3_a, d4_a]
        smaller(1:4) = [d1_b, d2_b, d3_b, d4_b]
        diff = e_a - e_b
    else
        bigger(1:4) = [d1_b, d2_b, d3_b, d4_b]
        smaller(1:4) = [d1_a, d2_a, d3_a, d4_a]
        diff = e_b - e_a
    end if

    !shifting smaller nuber

    shifted_smaller = 0
    do i = 1, 4
        if (i + diff <= 9) then
            shifted_smaller(i + diff) = smaller(i)
        end if
    end do

   

    !addition with carry
    carry = 0
    do i = 9, 1, -1
        sum(i) = bigger(i) + shifted_smaller(i) + carry
        if (sum(i) >= 10) then
            carry = 1
            sum(i) = sum(i) - 10
        else
            carry = 0
        end if
    end do

    !rounding to 4 difits after decimal
    do i = 9, 5, -1
        if (sum(i) >= 5) then
            sum(i-1) = sum(i-1) + 1
            sum(i) = 0
        end if
    end do

    !normalize the result

    if (sum(1) > 9) then
        result(1) = sum(1) / 10
        result(2) = mod(sum(1), 10)
        result(3) = sum(2)
        result(4) = sum(3)
        exponent = max(e_a, e_b) + 1
    else
        result(1) = sum(1)
        result(2) = sum(2)
        result(3) = sum(3)
        result(4) = sum(4)
        exponent = max(e_a, e_b)
    end if        


    !print the result
    write (*,'(A,4I1,A,I0)') "The sum is: 0.", result(1), result(2), result(3), result(4), " x 10^", exponent

end program float_addition


    

    




