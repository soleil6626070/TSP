! Program to 'solve' the Travelling Salesman Problem using
! Simulated annealing, the Metropolis algorithm and 
! reverse/transport path swaps

Program V2_TSP
  Implicit None
  Integer :: sample_size
  Integer, Parameter :: N = 50  ! # of Cities
  Double Precision, Dimension(1:2,1:N) :: City, City_savedpath, Lmin_array, City_ini
  Double Precision :: T, annealsched
  Double Precision :: L, Lnew, dL, Lini, prev_L
  Double Precision :: r, r_or_t, accprob
  Integer :: i, j, z
  ! Cost Function
  Double Precision :: old_sum, new_sum
  Logical :: metropolis_accepted
  ! Segment selection
  Integer :: a, b, c, ins_point, ins_next
  Integer :: a_prev, a_next, b_prev, b_next
  Integer :: nodes_in_segment, nodes_not_in_segment
  ! Tracking
  Integer :: better_moves, attempted_moves, worse_moves_accepted, iter
  ! rng
  Double Precision :: custom_rand
  Integer :: iseed, first
  ! Logging data
  Logical :: write_data

! Log data to file?
write_data = .TRUE.

! RNG
first = 0
iseed = 971741

! Program repetions
sample_size = 10

! Randomly assign coordinates to N cities
DO i = 1,N
    City(1,i) = custom_rand(iseed,first)
    City(2,i) = custom_rand(iseed,first)
END DO
! save initial config
City_ini = City

! Calculate inital path length
Lini = 0.0
DO i = 1,(N-1)
    Lini = Lini + dist(i,i+1)
END DO
! add return trip home
Lini = Lini + dist(N,1)

Write(6,*) "Lini: ", Lini

! Outer do loop to repeat the program <sample size> times
Do z = 1, sample_size

    ! Update seed & reset RNG for reproducibility
    iseed = iseed + 1
    first = 0 

    ! Variable Initalisations
    T = 1
    annealsched = 0.9
    City = City_ini
    L = Lini
    prev_L = L + 10
    iter = 0

    ! Open file to log data for visualisation
    IF (write_data) THEN
      OPEN(20, file='v2_tsp_log.txt', status='replace')
      WRITE(20,'(A,I0)') 'Iteration #, Length, Temperature, Path (x1,y1,x2,y2,...,x1,y1), iseed: ', iseed
      Call log_data_to_file(iter, T, L, City, N)
    END IF

    ! Temperature do loop 
    Do While ( T > 0.0 )

        attempted_moves = 0
        better_moves = 0
        ! Path transformation do loop
        Do While (better_moves < 10*N .AND. attempted_moves < 10*N)
            ! Randomly select a segment of 3-5 cities
            CALL Select_Segment(N, a_prev, a, b, b_next, c, ins_point, ins_next, nodes_in_segment, nodes_not_in_segment)

            ! - 50/50 decision to reverse or transport segment
            CALL RANDOM_NUMBER(r)
            r_or_t = r

            ! cost eval - Length of edges before/after being transformed
            If (r_or_t >= 0.5) Then ! reverse cost evaluation
                old_sum = dist(a_prev, a) + dist(b, b_next)
                new_sum = dist(a_prev, b) + dist(a, b_next)
            Else                    ! transport cost evaluation
                old_sum = dist(a_prev, a) + dist(b, b_next) + dist(ins_point, ins_next)
                new_sum = dist(a_prev, b_next) + dist(ins_point, a) + dist(b, ins_next)
            End If
            ! cost eval - new path length
            Lnew = L + new_sum - old_sum
            dL = Lnew - L

            ! - Call metropolis acceptance evaluation
            CALL metropolis(dL, T, accprob, metropolis_accepted)

            ! - update values
            If (metropolis_accepted) Then
              If (r_or_t >= 0.5) Then ! reverse
                Call Reverse(a, b, a_next, b_prev, N, nodes_in_segment, City, old_sum, new_sum)
              Else                    ! transport
                Call Transport(a, b, a_prev, b_next, c, ins_point, ins_next, nodes_in_segment, &
                                nodes_not_in_segment, old_sum, new_sum, City)
              End If
              L = Lnew

              If (accprob < 1.0) Then
                better_moves = better_moves + 1
              Else 
                worse_moves_accepted = worse_moves_accepted + 1
              End If
            Else 
              ! Reject - Do Nothing
            End If
            attempted_moves = attempted_moves + 1
            iter = iter + 1
            City_savedpath = City
        End Do  ! Path transformation
        
        ! Output this temperature intevals results
        WRITE(*,'("L = ",F6.2,", Temp = ",F10.6,", att_Moves = ",I6,", bet_Moves = ",I6)') L, T, attempted_moves, better_moves
        ! Log Data to File
        If (write_data) Call log_data_to_file(iter, T, L, City, N)
        ! Decrease temperature
        T = T * annealsched
        ! If no improvements were made from previous temp, system is deemed frozen
        If (better_moves == 0) T = -1.0
        !If (prev_L <= L) T = -1.0
        !prev_L = L

    End Do  ! Temperature

    If (write_data) Close(20)

    Write(6,*)'The L #',z,'is', L 
    Write(6,*) " "

End Do  ! <sample_size> repititions


! Internal Functions/Subroutines
Contains

! Good old a^2 + b^2 = c^2
Double Precision Function dist(i, j)
  Implicit None
  INTEGER, INTENT(IN) :: i, j
  dist = SQRT( (City(1,i)-City(1,j))**2 + (City(2,i)-City(2,j))**2 )
END Function dist

Subroutine Select_2_Cities(a_prev, a, b, b_next)
  Double Precision :: r
  Integer, Intent(Out) :: a_prev, a, b, b_next

  ! Pick 2 cities to swap by calling 2 differing random numbers
  CALL RANDOM_NUMBER(r)
  a = NINT( r*N + 0.5 )
  CALL RANDOM_NUMBER(r) 
  b = NINT( r*N + 0.5 ) 
  ! Indicies of previous and next city of the two swapped cities
  a_prev = MOD(a-2 + N, N) + 1  ! MOD to handle wrapping
  b_next = MOD(b, N) + 1        ! ie. prev of 1st city is Nth city

  ! Handle ____ cases (cant think of the word)
  DO WHILE (b == a .OR. b == a_prev)
    CALL RANDOM_NUMBER(r)
    b = NINT( r*N + 0.5 )
    b_next = MOD(b, N) + 1 
  END DO
End Subroutine Select_2_Cities


Subroutine Select_Segment(N, a_prev, a, b, b_next, c, ins_point, ins_next, nodes_in_segment, nodes_not_in_segment)
  Double Precision :: r 
  Integer, Intent(In) :: N
  Integer, Intent(Out) :: a_prev, a, b, b_next, c, ins_point, ins_next
  Integer, Intent(Out) :: nodes_in_segment, nodes_not_in_segment

  ! Select segment of 3/4/5 cities ( 2/3/4 edges )
  CALL RANDOM_NUMBER(r)
  nodes_in_segment = NINT( r*3 + 0.5 ) + 2  ! returns 3, 4 or 5 evenly

  CALL RANDOM_NUMBER(r)
  a = NINT( r*N + 0.5 )

  ! a to b (inclusive) is nodes_in_segment long
  b = MOD(a + (nodes_in_segment - 1) - 1, N) + 1

  ! Indicies of previous and next city of the two swapped cities
  a_prev = MOD(a-2 + N, N) + 1
  b_next = MOD(b, N) + 1

  ! Insertion point selection O(1) for transport subroutine
  nodes_not_in_segment = N - nodes_in_segment
  ! random number between 1 and <nodes_not_in_segment - 1> (inclusive)
  CALL RANDOM_NUMBER(r) 
  c = INT( r*(nodes_not_in_segment-1) ) + 1   ! ins_point cannot overlap with a_prev
  ! put that random number after b
  ins_point = MOD(b + c - 1, N) + 1   ! ins_point can overlap with b_next
  ins_next = MOD(ins_point, N) + 1    ! ins_next can overlap with a_prev

End Subroutine Select_Segment


Subroutine Reverse(a, b, a_next, b_prev, N, nodes_in_segment, City, old_sum, new_sum)
  Integer, Intent(In) :: a, b, a_next, b_prev, N, nodes_in_segment
  Double Precision, Dimension(1:2,1:N), Intent(InOut) :: City
  Double Precision, Intent(Out) :: old_sum, new_sum
  Integer :: half, left, right, i
  Double Precision :: tempcity(2)

  ! half the number of nodes in the a -> b segment
  half = nodes_in_segment / 2

  Do i = 0, half - 1
    ! 2 pointers - same logic as palindrome check
    left = MOD(a - 1 + i, N) + 1
    right = MOD(b - 1 - i + N, N) + 1
    ! swap
    tempcity(:) = City(:,left)
    City(:,left) = City(:,right)
    City(:,right) = tempcity(:)
  End Do

  ! Logic:
  ! O((b-a)/2)
  ! 2 pointers pointing to the start and end of the array
  ! Swap them, and then advance the iteration by one to then
  ! point to start+1 and end-1, repeat.
End Subroutine Reverse 


Subroutine Transport(a, b, a_prev, b_next, c, ins_point, ins_next, nodes_in_segment, nodes_not_in_segment, old_sum, new_sum, City)
  Integer, Intent(In) :: a, b, a_prev, b_next, c, ins_point, ins_next
  Integer, Intent(In) :: nodes_in_segment, nodes_not_in_segment
  Double Precision, Intent(In) :: old_sum, new_sum
  Integer :: i, position, tour_idx

  Integer, Allocatable :: segment(:), not_segment(:), new_tour(:)
  Double Precision, Allocatable :: temp2City(:,:)
  Double Precision, Dimension(1:2,1:N), Intent(InOut) :: City

  ! Update values
  !
  Allocate(segment(nodes_in_segment))
  Do i = 1, nodes_in_segment
    position = MOD(a - 1 + (i-1), N) + 1
    segment(i) = position
  End Do

  Allocate(not_segment(nodes_not_in_segment))
  Do i = 1, nodes_not_in_segment
    position = MOD(b_next - 1 + (i-1), N) + 1
    not_segment(i) = position
  End Do

  Allocate(new_tour(N))
  tour_idx = 0    ! Will be carried over multiple loops
  Do i = 1, c
    tour_idx = tour_idx + 1
    new_tour(tour_idx) = not_segment(i)
  End Do 
  Do i = 1, nodes_in_segment
    tour_idx = tour_idx + 1
    new_tour(tour_idx) = segment(i)
  End Do 
  Do i = c+1, nodes_not_in_segment
    tour_idx = tour_idx + 1
    new_tour(tour_idx) = not_segment(i)
  End Do

  Allocate(temp2City(2, N))
  Do i = 1, N 
    temp2City(:, i) = City(:,new_tour(i))
  End Do 
  City(:, 1:N) = temp2City(:, 1:N)

  Deallocate(segment, not_segment, new_tour, temp2City)

! Logic: terrible terrible O(2N) swapping
! I need to initialise a linked list prev/next method

End Subroutine Transport


Subroutine metropolis(dL, T, accprob, metropolis_accepted)
  Implicit None
  Double Precision, INTENT(in) :: dL, T
  Double Precision, INTENT(out) :: accprob
  LOGICAL, INTENT(out) :: metropolis_accepted
  Double Precision :: r

  accprob = EXP(-dL/T)    ! analogous to P(dE) = exp^(-dE / kt)

  CALL RANDOM_NUMBER(r)   ! random number between 0 & 1

  metropolis_accepted = (accprob >= r)

  ! Logic:
  ! If dL is -ve (shorter path) then it will always be accepted, since accprob > 1.
  ! If dL is +ve (longer path) then it will be accepted only with 
  ! probability EXP(-dL/p), (ie is it greater than a randomly chosen
  ! number between 0 and 1)

END Subroutine metropolis


Subroutine log_data_to_file(iter, T, L, City, N)
Implicit None
Double Precision, Intent(In) :: T, L
Integer, Intent(In) :: iter, N
Double Precision, Dimension(1:2,1:N), Intent(In) :: City
Integer :: o 

WRITE(20,'(I8, 2X, F12.6, 2X, F12.6, 2X)', ADVANCE='NO') iter, L, T 
DO o = 1, N
  WRITE(20,'(F0.6,",",F0.6,",")', ADVANCE='NO') City(1,o), City(2,o)
END DO
WRITE(20,'(F0.6,",",F0.6,",")', ADVANCE='NO') City(1,1), City(2,1)  ! Close loop with 1st city
WRITE(20,*) ! Empty write to advance line

End Subroutine log_data_to_file

End Program V2_TSP


!----------External Function---------

Double Precision Function custom_rand(iseed,first)

!  This Function returns a pseudo-random number for each invocation.
!  It is an f90 adaptation of an
!  FORTRAN 77 adaptation 
!  by Dick Valent and Fred Clare
!  http://www.cisl.ucar.edu/zine/96/spring/articles/3.random-6.html
!  of the "Integer Version 2" minimal 
!  standard number generator whose Pascal code appears in the article:
!
!     Park, Steven K. and Miller, Keith W., "Random Number Generators: 
!     Good Ones are Hard to Find", Communications of the ACM, 
!     October, 1988.
!
  Implicit None
  integer, parameter :: MPLIER=16807
  integer, parameter :: MODLUS=2147483647
  integer, parameter :: MOBYMP=127773
  integer, parameter :: MOMDMP=2836
  integer :: hvlue,lvlue,testv,nextn,first,iseed
  save nextn
!
  if(first == 0) THEN
    nextn=iseed
    first=1
  endif
!
  hvlue=nextn/mobymp
  lvlue=mod(nextn,mobymp)
  testv=mplier*lvlue-momdmp*hvlue
  if(testv > 0)then
    nextn=testv
  else
    nextn=testv+modlus
  endif
  custom_rand = real(nextn)/real(modlus)
!
END Function custom_rand


