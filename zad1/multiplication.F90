module multiplication

	implicit none

  contains 

  subroutine mm(first, second, multiply, status)
    
	real(kind= 8),intent(in):: first(:,:) ! pierwsza macierz
	real(kind= 8),intent(in):: second(: ,:) ! druga macierz
	real(kind= 8),intent(out):: multiply(:,:) ! macierz wynikowa
	integer(kind= 4),intent(out):: status ! kod błędu, 0 gdy OK
	
  
  
  

  end subroutine

end module
