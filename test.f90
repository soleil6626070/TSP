Program test 
    Implicit None
    Integer :: N, a, b, a_prev, b_next
    N = 40
    REAL, DIMENSION(1:2,1:N) :: City
    a = 3
    b = 12
    CALL Reverse()
    Write(6,*) 


CONTAINS
Subroutine Reverse()
  Integer, Intent(In) :: a_prev, a, b, b_next, N
  Integer :: nodes_in_segment, half
  real :: tempcity(2) 

  ! number of nodes in the a -> b segment
  nodes_in_segment = MOD(b - a + N, N) + 1  !mod(7 - 3 + 40, 40) +1 = 5
  half = nodes_in_segment / 2
  Do iterable = 0, half - 1
    ! 2 pointers - same logic as palindrome check
    left = MOD(a - 1 + iterable, N) + 1
    right = MOD(b - 1 - iterable + N, N) + 1
    ! swap
    tempcity(:) = City(:,left)
    City(:,left) = City(:,right)
    City(:,right) = tempcity(:)
  End Do
End Subroutine Reversed 
End Program test 