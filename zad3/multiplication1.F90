module multiplication1

  implicit none

  contains 

  subroutine mm1(first, second, multiply, status)
    
    real(kind = 8),intent(in):: first(:,:) ! pierwsza macierz
    real(kind = 8),intent(in):: second(: ,:) ! druga macierz
    real(kind = 8),intent(out):: multiply(:,:) ! macierz wynikowa
    integer(kind = 4),intent(out):: status ! kod błędu, 0 gdy OK
    integer(kind =4):: firstW, firstK, secondW, secondK, multiplyW, multiplyK, i, j, k

    firstW = SIZE(first(:, 1))
    firstK = SIZE(first(1, :))
    secondW = SIZE(second(:, 1))
    secondK = SIZE(second(1, :))
    multiplyW = SIZE(multiply(:, 1))
    multiplyK = SIZE(multiply(1, :))

    if (firstK .NE. secondW) then
    	!print *, "INCORRECT INPUT"
        status = 1
		!stop
    end if

    do i=1, multiplyW
        do j=1, multiplyK
            multiply(i,j) = 0
            do k=1, firstK
                multiply(i,j) = multiply(i, j) + (first(i,k) * second(k, j))
            end do
        end do
    end do

	!print *, "RESULT:"

	!do i=1, multiplyW
	!	print *, (multiply(i,j), j=1, multiplyK)
	!end do
	
    status = 0

  end subroutine

end module
