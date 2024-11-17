! Module with constant values

module consts
    integer (kind=4), parameter :: N = 12
    real (kind=4) :: pi
    real(kind=4) :: TanArr(N+1)
end module consts

module subroutines
    interface
        subroutine degtorad(deg, rad)
            real (kind=4), intent(in) :: deg
            real (kind=4), intent(out) :: rad
        end subroutine degtorad

        function traprule( ) result(area)
            use consts 
        end function traprule
    end interface
end module subroutines

program newtrap
    use consts
    use subroutines
    implicit none
    integer (kind=4) :: i
    real (kind=4) :: a = 0.0, b_deg = 60.0
    real (kind=4) :: area, mult_rad
    real (kind=4) ::  deg, radian

    ! Convert b=pi/3 to radians 
    pi = atan(1.0) * 4.0

    ! Calculate the values of TanArr(i) = f(xi) for i = 0, 1, ..., N
    do i = 1, N+1
        deg = (i - 1) * 5.0
        call degtorad(deg, radian)
        TanArr(i) = tan(radian)
        write(6,*) 'TanArr[', i, '] = ', TanArr(i)
    end do

    area = traprule()

    ! Approximated result
    write(6,*) 'Trapezoidal result is ', area

    ! Real result for comparison
    write(6,*) 'Real result is ', log(2.0)
end program newtrap

subroutine degtorad(deg, rad)
    real (kind=4), intent(in) :: deg
    real (kind=4), intent(out) :: rad
    real (kind=4) :: pi = 3.1415927
    rad = (pi * deg) / 180.0
end subroutine degtorad

function traprule( ) result(area)
    use consts
    real(kind=4) :: area,mult_rad
    integer (kind=4) :: i

    ! Sum tan(a)+tan(b) where a and b are in radians 
    area = TanArr(1) + TanArr(N+1)
    write(6,*) 'Initial area (sum at x(0) and x(12))', area

    ! Calculate the area at pts x1, x2, ..., x11
    do i = 2, N,1
        area = area + 2.0 * TanArr(i)
        write(6,*) 'New area at x(', i / 5, ') =', area
    end do

    ! Multiply area by the (pi/3)/2(12) after converting it to radians 
    call degtorad(((b_deg - a) / (2.0 * N)), mult_rad)
    !mult_rad=(pi*((b_deg-a)/(2*N)))/180.0;
    area = mult_rad * area;
end function traprule

