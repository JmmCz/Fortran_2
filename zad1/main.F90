program main

  use multiplication, only: mm

  implicit none
  
  real (kind=8), allocatable :: first(:,:), second(:,:)
  real (kind = 8),  allocatable::multiply(:,:)
  integer (kind=4) :: firstW, firstK, secondW, secondK,multiplyW, multiplyK, i, j, status

  write (*,*) "Please, type rows of first:"

  read (*,*) firstW

  write(*,*) "Please, type columns of first:"

  read (*,*) firstK

  write (*,*) "Please, type rows of second:"

  read (*,*) secondW

  write(*,*) "Please, type columns of second:"

  read (*,*) secondK

  multiplyW = firstW

  multiplyK = secondK

  allocate(first(firstW, firstK))
  allocate(second(secondW, secondK))
  allocate(multiply(multiplyW, multiplyK))

  print *, "INPUT FIRST ROWS"
  do i=1, firstW
    read *, (first(i,j), j=1, firstK)
  end do


  print *, "INPUT SECOND ROWS"
  do i=1, secondW
    read *, (second(i,j), j=1, secondK)
  end do

  call mm(first, second, multiply, status)

  deallocate(first)
  deallocate(second)
  deallocate(multiply)

end program 
