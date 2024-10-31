
program sum
! Declare variables
   integer (kind=4) :: i
   real (kind=4) :: sum1, sum2, diff
   

! First sum
   sum1 = 0.0
   do i=1,1000
      sum1=sum1+1.0/real(i)
   end do


! Second sum 
   sum2 = 0.0
   do i=1000,1,-1
      ! Insert the same line as above except use sum2
      sum2=sum2+1.0/real(i)
   end do

   write(6,*) ' Sum1=',sum1
   write(6,*) ' Sum2=',sum2

! Find the difference
   diff = sum1-sum2

   write(6,*) ' Difference between the two is ',diff

end program sum
