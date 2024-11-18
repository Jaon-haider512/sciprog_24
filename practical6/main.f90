module functions 
   interface
       function mutlimat(n,p,q,a,b) result(c)
          integer  (kind=4), intent(in) :: n, p , q
          real  (kind=8), intent(in) :: a(n,p), b(p,q)
          real  (kind=8)  :: c(n,q)
          integer(kind=4) :: i,j,k
      end function mutlimat  
end interface
end module functions


program matrixmult
  use functions
  implicit none
  integer, parameter :: n=5, p=3, q=4
  real(kind=8) :: a(n,p), b(p,q), c(n,q)
  integer :: i, j, k

  ! Initialize A,B,C matrices
  do i = 1, n
    do j = 1, p
      a(i,j) = i + j
    end do
  end do

  do i = 1, p
    do j = 1, q
      b(i,j) = i - j
    end do
  end do
  c = 0.0
! perform matrix multiplication 
  c=mutlimat(n,p,q,a,b)
  ! Print matrices
  write(*,*) 'This is matrix A:'
  do i = 1, n
    do j = 1, p
      write(*,'(f3.0)',advance='no') a(i,j)
    end do
    write(*,*)
  end do

  write(*,*) 'This is matrix B:'
  do i = 1, p
    do j = 1, q
      write(*,'(f3.0)',advance='no') b(i,j)
    end do
    write(*,*)
  end do

  write(*,*) 'This is matrix C:'
  do i = 1, n
    do j = 1, q
      write(*,'(f4.0)',advance='no') c(i,j)
    end do
    write(*,*)
  end do

end program matrixmult
