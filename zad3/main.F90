#define N 1020

program main

  use multiplication1, only: mm1
  use multiplication2, only: mm2

  implicit none
  
  real (kind=8), allocatable :: first(:,:), second(:,:)
  real (kind = 8),  allocatable::multiply(:,:)
  integer (kind=4) :: i, j, status
  real (kind = 8) ::start, finish

  allocate(first(N, N))
  allocate(second(N, N))
  allocate(multiply(N, N))

  do i=1, N
    do j=1, N
      first(i, j) = 1
      second(i, j) = 1
    end do
  end do

  do i=1, N
    do j=1, N
      multiply(i, j) = 0
      multiply(i, j) = 0
    end do
  end do

  call cpu_time(start)

  call mm1(first, second, multiply, status)

  call cpu_time(finish)

  write (*, *) "TIME OF MULT FROM EX1:", finish - start

  do i=1, N
    do j=1, N
      multiply(i, j) = 0
      multiply(i, j) = 0
    end do
  end do

  call cpu_time(start)

  call mm2(first, second, multiply, status)

  call cpu_time(finish)

  write (*, *) "TIME OF MULT FROM EX2:", finish - start

  do i=1, N
    do j=1, N
      multiply(i, j) = 0
      multiply(i, j) = 0
    end do
  end do

  call cpu_time(start)

  multiply = MATMUL(first, second)

  call cpu_time(finish)

  write (*, *) "TIME OF MULT FROM MATMUL:", finish - start

  deallocate(first)
  deallocate(second)
  deallocate(multiply)

end program 
