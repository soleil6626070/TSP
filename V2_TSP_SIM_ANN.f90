! Program to 'solve' the Travelling Salesman Problem using
! Simulated annealing, the Metropolis algorithm and 
! reverse/transport path swaps

Program V2_TSP

! RNG
first = 0
iseed = 971741

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

! Outer do loop to repeat the program <sample size> times
Do z = 1, sample_size

    ! Update seed & reset RNG for reproducibility
    iseed = iseed + 1
    first = 0 

    ! Variable Initalisations
    T = 100*N 
    annealsched = 0.9
    City = City_ini
    L = Lini

    ! Temperature do loop 
    Do While ( T > 0.0 )

        ! Inner do loop - 10*N better solutions or no more better sltns possible
        attempted_moves = 0
        better_moves = 0
        Do While (better_moves < 10*N .AND. attempted_moves < 100*N)
            ! Choose 2 cities at random
            CALL Select_2_Cities(a_prev, a, b, b_next)
            ! - 50/50 reverse or transport subroutine
            CALL RANDOM_NUMBER(r)
            If (r >= 0.5) Then
                CALL Reverse(a, b, N, City)
            Else
                CALL Transport()
            End If
            ! - Evaluate new path length - cost evaluation [ in the subroutine ]
            dL = Lnew - L
            ! - Call metropolis acceptance evaluation
            CALL metropolis(dL, T, accprob, metropolis_accepted)
            ! - update values
            If (metropolis_accepted) Then
                L = Lnew

                If (accprob < 1.0)
                    better_moves = better_moves + 1
                Else 
                    worse_moves_accepted = worse_moves_accepted + 1
                End If
            Else 
                City = City_savedpath  ! reset path to before the transformation
            End If
            attempted_moves = attempted_moves + 1
            City_savedpath = City
        ! End inner loop
        End Do 

        ! Decrease temperature
        T = T * annealsched
        ! If no improvements were made from previous temp, system is deemed frozen
        If (previous_T_L <= L) T = -1.0

    End Do
End Do 


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

Subroutine Reverse(a, b, N, City)
  Integer, Intent(In) :: a, b, N
  Integer, Intent(InOut) :: City
  Integer :: nodes_in_segment, half, left, right, i
  Double Precision :: tempcity(2) 

  ! number of nodes in the a -> b segment
  nodes_in_segment = MOD(b - a + N, N) + 1  !mod(7 - 3 + 40, 40) +1 = 5
  half = nodes_in_segment / 2

  Do i = 0, half - 1
    ! 2 pointers - same logic as palindrome check
    left = MOD(a - 1 + iterable, N) + 1
    right = MOD(b - 1 - iterable + N, N) + 1
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

  ! need to add back prev_next and do distance calculation
End Subroutine Reversed 

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

End Program V2_TSPTSP


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
