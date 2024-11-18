!Find a using a Taylor series expansion from e^0
!The estimates gets worse after 13th order polynominal
! because 13! is bigger than the maximum number that 
! can be stored in 4 byte integer 
program finde
    implicit none
    integer(kind=4) :: i, n
    real(kind=8) :: one
    real(kind=8), allocatable :: terms(:)
    
! A little bit change in strcture is being made,as i first typed the live session code but giving error 
    interface
        function factorial(n)
            integer(kind=4), intent(in) :: n  
            integer(kind=4) :: factorial
        end function factorial
    end interface

    one = dble(1.0)

    ! Here i add this print statement becuase initially it was not here 
    print *, 'Enter the polynomial order (n) for the Taylor series expansion:'
    read(*, *) n

    allocate(terms(n + 1))

    terms(1) = one
    do i = 1, n
        terms(i + 1) = one / real(factorial(i), kind=8)
    end do

    write(6,*) ' e is estimated as ',sum(terms),sum(terms)-dexp(one)
   
    deallocate(terms)
    
    stop
end program finde

! Here this factorial function that will be used 
integer(kind=4) function factorial(n)
    implicit none
    integer(kind=4), intent(in) :: n 
    integer(kind=4) :: i, x

    x = 1
    do i = 1, n
        x = x * i
    end do

    factorial = x
    return
end function factorial
