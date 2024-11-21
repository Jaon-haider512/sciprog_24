Program printing

    implicit none
    integer (kind=4) :: i1=10, i2=200        ! define the integers 
    real (kind=4) :: fl1=1.11, fl2=2.2222    ! floats
    ! write statement is used as given in the practical3 exercise pdf file 
    write(6,'(a,i2,1x,i4,a,1x,f5.2,1x,f7.4)') 'Two ints ', i1, i2, &
         ' and two floats ', fl1, fl2

end program printing

