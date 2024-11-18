program matrixmult

  implicit none
  integer, parameter :: n=5, p=3, q=4
  real(kind=8) :: a(n,p), b(p,q), c(n,q)
  integer :: i, j, k

  ! Initialize A and B matrices
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

  ! Initialize C matrix to zero
  c = 0.0

  ! Perform matrix multiplication
  do i = 1, n
    do j = 1, q
      do k = 1, p
        c(i,j) = c(i,j) + a(i,k) * b(k,j)
      end do
    end do
  end do

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
