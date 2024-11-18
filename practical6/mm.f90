!Function for matrix multiplication c=a*b  
 function mutlimat(n,p,q,a,b) result(c)
          integer  (kind=4), intent(in) :: n, p , q
          real  (kind=8), intent(in) :: a(n,p), b(p,q)
          real  (kind=8)  :: c(n,q)
          integer(kind=4) :: i,j,k


 ! Perform matrix multiplication
         do i = 1, n
            do j = 1, q
               do k = 1, p
                   c(i,j) = c(i,j) + a(i,k) * b(k,j)
              end do
           end do
        end do
  return
end function mutlimat
