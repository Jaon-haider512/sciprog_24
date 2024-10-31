program Trap
implicit none

integer (kind=4) :: N=12, i
real (kind=4) :: a=0.0, b_deg=60.0
real (kind=4) :: pi, b_rad, area, mult_rad

! Convert b*pi/3 to radians
pi = atan(1.0) * 4.0     ! here define the value of pi
b_rad = (pi * b_deg) / 180.0

! Sum tan(a)+tan(b) where a and b are in radians
area = tan(a) + tan(b_rad)  ! area at the boundary points summation
write(6,*) 'Initial area (sum at x(0) and x(12)) = ', area

! Calculate the area at pts x1, x2, ..., x11 and add them up
! as in the formula using loops
! using do loop
!do i=5, 55, 5
!    area = area + 2 * tan((pi * i) / 180.0)
 !   write(6, *) 'New area of X(', i / 5, ') = ', area
!end do

! Using do while loop
!i = 5
!do while (i .lt. 60)
 !   area = area + 2 * tan((pi * i) / 180.0)
  !  write(6, *) 'New area of X(', i / 5, ') = ', area
   ! i = i + 5
!end do

! Use if statement in the loop
 i = 5
do while (i .lt. 60)
    area = area + 2 * tan((pi * i) / 180.0)
    if (mod(i, 2) .eq. 0) then
        write(6, *) 'New area of X(', i / 5, ') = ', area
    end if
    i = i + 5
end do

! Use select/case statement in the loop
! i = 5
! do while (i .lt. 60)
!     area = area + 2 * tan((pi * i) / 180.0)
!  select case (mod(i, 2))  !      case (0)
!           write(6, *) 'New area of X(', i / 5, ') = ', area
!    end select
!   i = i + 5
!end do

! Multiply area by (pi/3)/(2*12) after converting it to radians
mult_rad = (pi * (b_deg - a) / (2 * N)) / 180.0
area = mult_rad * area

! Approximated result after summation of areas
write(6,*) 'Trapezoidal result is ', area

! Real result
! Integral of tan is ln(2)
write(6,*) 'Real result is ', log(2.0)

stop
end program Trap
