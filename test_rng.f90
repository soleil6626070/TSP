program test_rng

integer :: i, k, counts(7), a, b, nodes_in_segment, N
real :: r
counts = 0

!do i = 1, 1000000
!  call random_number(r)
!  k = NINT( r*3 + 0.5 ) + 3
!  counts(k) = counts(k) + 1
!end do

!print *, counts 

N = 40

  ! Select segment of 3/4/5 cities
  CALL RANDOM_NUMBER(r)
  nodes_in_segment = NINT( r*3 + 0.5 ) + 2  ! returns 3, 4 or 5 evenly

  !CALL RANDOM_NUMBER(r)
  !a = NINT( r*N + 0.5 )
  a = 39

  b = MOD(a + nodes_in_segment, N)

  Write(6,*) a, b, nodes_in_segment 
End Program test_rng 