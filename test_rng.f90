program test_rng

integer :: i, k, counts(7), a, b, nodes_in_segment, N, nodes_not_in_segment
real :: r
counts = 0

nodes_not_in_segment = 6

do i = 1, 1000000
  call random_number(r)
  k = INT( r*(nodes_not_in_segment-1) ) + 1
  counts(k) = counts(k) + 1
end do

print *, counts 

!N = 40

  ! Select segment of 3/4/5 cities ( 2/3/4 edges )
!  CALL RANDOM_NUMBER(r)
!  nodes_in_segment = NINT( r*3 + 0.5 ) + 2  ! returns 3, 4 or 5 evenly

  !CALL RANDOM_NUMBER(r)
  !a = NINT( r*N + 0.5 )
!  a = 39

!  b = MOD(a + nodes_in_segment, N)

!  Write(6,*) a, b, nodes_in_segment 
End Program test_rng 