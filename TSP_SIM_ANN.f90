!----------------------------------------------------------!
!             The Travelling Salesman Problem              !
!                                                          !
!----------------------------------------------------------!


PROGRAM TSP
 IMPLICIT NONE
 DOUBLE PRECISION :: the_sum, Lmean, Lerror, SD_sum
 DOUBLE PRECISION :: dL_sum, dLmean, dLerror, dLSD_sum
 DOUBLE PRECISION :: L, Lstep, Lini, Lnew, dL, r, custom_rand, tempcity
 DOUBLE PRECISION :: p, annealsched, slow_cooling, fast_cooling, cooling_trigger
 DOUBLE PRECISION :: accprob, Lmin_scalar, dL_max, dL_min, absdLmax
 INTEGER :: iseed, first, i, k, a, b, z, y, progress, numswap, sucswap
 INTEGER :: accprobiter
 INTEGER, PARAMETER :: N = 50   !Number of Cities
 REAL, DIMENSION(1:2,1:N) :: City, City_savedpath, Lmin_array, City_ini
 INTEGER, PARAMETER :: sample_size = 100
 INTEGER, PARAMETER :: k16 = SELECTED_INT_KIND(R=16)
 INTEGER(KIND=k16) :: j
 REAL, DIMENSION(1:sample_size) :: Lvalues, dLvalues
 !swapping
 INTEGER :: a_prev, a_next, b_prev, b_next, worswap, totworse
 DOUBLE PRECISION :: old_sum, new_sum
 ! Early exit variables
 INTEGER :: moves_attempted, moves_accepted, check_interval
 INTEGER :: total_moves_attempted, total_moves_accepted
 DOUBLE PRECISION :: acceptance_rate
 LOGICAL :: system_frozen
 ! Gif generation variables
 INTEGER :: output_interval
 LOGICAL :: metropolis_accepted, write_data

 ! RNG
 first = 0
 iseed = 971741
 WRITE(6,'(a,i9)')'seed value that generates this sequence is ',iseed
 ! annealing schedule
 fast_cooling = 0.99
 slow_cooling = 0.99999
 cooling_trigger = 0.8
 ! stats
 Lmin_scalar = 9999999.0
 absdLmax = 0.0
 ! gif creation params
 write_data = .TRUE.
 output_interval = 1000


!-----This do-loop assigns N cities random x and y coordinates.--------

 DO i = 1,N
   City(1,i) = custom_rand(iseed,first)
   City(2,i) = custom_rand(iseed,first)
!  WRITE(6,*)City(1,i), City(2,i)
 END DO
 ! save initial config (vital)
 City_ini = City

! Do loop initialisations
the_sum = 0.0
dl_sum = 0.0
Lstep = 0.0
Lini = 0.0

! Initial path length calculation
Lini = 0.0
DO i = 1,(N-1)
  Lini = Lini + dist(i,i+1)
END DO
! add return trip home
Lini = Lini + dist(N,1)

! ---------- Outer Do Loop - Repeats the program z times ----------
DO z=1, sample_size

! Reset rng for reproducibility
iseed = iseed + 1
first = 0 

!---------- Open file for gif data ----------
IF (write_data) THEN
  OPEN(20, file='tsp_log.txt', status='replace')
  !WRITE(20,'(A)') 'Iteration #, Length, p, Path (x1,y1,x2,y2,...,x1,y1)' old
  WRITE(20,'(A,I0)') 'Iteration #, Length, p, Path (x1,y1,x2,y2,...,x1,y1), iseed: ', iseed
END IF

! rapid schedule
annealsched = fast_cooling
! reset city order 
City = City_ini
L = 0.0 
p  = 10*N  ! Initially very large with fast cooling schedule
numswap = 0
sucswap = 0
dL_max = 0.0
dL_min = 0.0

! itialise swaps & moves trackers
worswap = 0
totworse = 0
total_moves_attempted = 0
total_moves_accepted = 0

! initialise frozen system exit strategy tracking variables
moves_attempted = 0  
moves_accepted = 0    
check_interval = 10000 
system_frozen = .FALSE.


!--------------- Inner Do Loop - Attempts to solve the TSP -----------------------

DO j = 1,999999_k16        !smaller do loop, to find each L

! Calculate first path length & save initial frame
IF (j == 1) THEN
  L = 0.0
  DO i = 1,(N-1)
    L = L + dist(i,i+1)
  END DO
  L = L + dist(N,1) ! add distance from last city back to first
  
  ! gif data collection
  IF (write_data) THEN
    CALL log_data_to_file(j, L, p, City)
  END IF
END IF


! ----- Swap two cities at random & calculate the new path length -----
CALL random_city_swap(N, a, b, old_sum, new_sum)
Lnew = L + (new_sum - old_sum)
dL = Lnew - L
! Count attempted move
moves_attempted = moves_attempted + 1
total_moves_attempted = total_moves_attempted + 1 

! Statistical tracking of max/min values of DL (not important to acceptance)
IF ( dL > dL_max ) THEN
  dL_max = dL
ELSE IF ( dL < dL_min ) THEN
  dL_min = dL           
END IF

! ---------- Acceptance Criteria ----------

! Use the metropolis algorithm to determine whether we accept the new path
CALL metropolis(dL, p, accprob, metropolis_accepted)

IF (metropolis_accepted) THEN ! Update values
  ! swap x
  tempcity = City(1,a)
  City(1,a) = City(1,b)
  City(1,b) = tempcity
  ! swap y
  tempcity  = City(2,a)
  City(2,a) = City(2,b)
  City(2,b) = tempcity
  ! update
  L = Lnew
  City_savedpath = City
  sucswap = sucswap + 1
  IF (accprob < 1.0) worswap = worswap + 1
  moves_accepted = moves_accepted + 1
  total_moves_accepted = total_moves_accepted + 1
ELSE 
  ! reject- do nothing
END IF

! ---- write data to file for Gif ----
IF (write_data .AND. MOD(j, output_interval) == 0) THEN
  CALL log_data_to_file(j, L, p, City)
END IF

! ---- Annealing schedule update ----

! Count total worse swaps accepted
IF (accprob < 1.0 ) totworse = totworse + 1
! Adjust annealing schedule based on acceptance rate
! Swap to slow cooling once 80% of worse solutions are being accepted.
! minimum totworse solutions added to stabalise the trigger
IF (totworse> 100 .AND. REAL(worswap)/REAL(totworse) < cooling_trigger) THEN
  annealsched = slow_cooling
END IF

! Reduce temperature
p = p * annealsched

! Frozen system exit
! Every <check_interval> iterations:
IF (MOD(j, check_interval) == 0) THEN
  IF (moves_attempted > 0) THEN
    acceptance_rate = REAL(moves_accepted) / REAL(moves_attempted)

    IF (acceptance_rate < 0.0001) THEN
      ! system is frozen
      system_frozen = .TRUE.
      EXIT
    END if
  END IF
! reset for next interval
moves_attempted = 0
moves_accepted = 0
END IF

! Temperature based exit
IF (p < 0.001) EXIT

END DO

!-------------------^end of swapping cities do loop-----

! ----- Save final frame for gif -----
IF (write_data) THEN
  CALL log_data_to_file(j, L, p, City)
END IF

 WRITE(6,*)'The L #',z,'is'
 WRITE(6,*)L
! WRITE(6,*)' '
! WRITE(6,*)'biggest dl',dl_max
! WRITE(6,*)'smalest dL',dL_min
!-------------------------Absolute statistics----------------------------------

dL_sum = dL_sum + dL_max
the_sum = the_sum + L
Lvalues(z) = L 
dLvalues(z) = dL_max
IF (absdlmax < dl_max) THEN
    absdlmax = dl_max
END IF

!----------picks the shortest path out of the sample (10)-----------

 IF (L < Lmin_scalar ) THEN                       !mayb2
     Lmin_array = City_savedpath 
     Lmin_scalar = L
     OPEN(10, file='CitiesPlot.txt')

       DO k = 1,N
         WRITE(10,'(2f12.6)')Lmin_array(1,k), Lmin_array(2,k)
       END DO
       ! Add the first city for plotting
       WRITE(10,'(2f12.6)')Lmin_array(1,1), Lmin_array(2,1)

     CLOSE(10)

    IF (write_data) THEN
      FLUSH(20)
      ! Save best log file
      !CALL EXECUTE_COMMAND_LINE('cp tsp_log.txt tsp_log_best.txt')    ! Linux
      CALL EXECUTE_COMMAND_LINE('copy tsp_log.txt tsp_log_best.txt')  ! Windows
    END IF
  END IF

! ----------Exit reason----------
IF (system_frozen) THEN
  WRITE(6,*) 'Run ', z, 'exited due to frozen system.'
  WRITE(6,*) 'Total iterations: ', j
  WRITE(*,'(" Acceptance rate at freezing: ", F6.3," %")') acceptance_rate*100
ELSE
  WRITE(6,*) 'Run ', z, 'exited due to temperature.'
  WRITE(6,*) 'Total iterations: ', j
  WRITE(6,*) 'p: ', p
END IF

! Total acceptance rate
IF (total_moves_attempted > 0) THEN
  WRITE(6,'("Overall acceptance rate for run: ",F6.3," %")') &
      REAL(total_moves_accepted)/REAL(total_moves_attempted) * 100.0
  WRITE(6,*) ' '
END IF 


!-----------------------------------------------------------------

progress = (z*100)/sample_size
WRITE(6,*)progress,'%'

! Close file end of every z run
IF (write_data) THEN
  CLOSE(20)
END IF

!----------------end of z do loop -----------------------------

END DO



!---------------------mean and error-----------------------------------


 Lmean = the_sum/sample_size
 dLmean = dL_sum/sample_size
! WRITE(6,*)'Lmean is',Lmean
! WRITE(6,*)' '
! WRITE(6,*)'dLmean is',dLmean
! WRITE(6,*)' '
SD_sum = 0.0
dLSD_sum = 0.0

DO y=1, sample_size

  SD_sum = SD_sum + ( Lvalues(y) - Lmean )**2

  dLSD_sum = dLSD_sum + ( dLvalues(y) - dLmean )**2

END DO

  Lerror = SQRT(SD_sum/REAL(sample_size -1))
  dLerror = SQRT(dLSD_sum/REAL(sample_size -1))
 WRITE(6,*)'The initial length was'
 WRITE(6,*)Lini
 WRITE(6,*)' '


 WRITE(6,*)'The mean length is'
 WRITE(6,*)Lmean, "+-", Lerror
 WRITE(6,*)' '

 WRITE(6,*)'Lmin_scalar is'
 WRITE(6,*)Lmin_scalar

! WRITE(6,*)'The mean dL is'
! WRITE(6,*)dLmean,'+-',dLerror
! WRITE(6,*)' '
! WRITE(6,*)'absolute dL ',absdLmax


!----------Internal Functions/Subroutines---------
CONTAINS

! Good old a^2 = b^2 + c^2
DOUBLE PRECISION FUNCTION dist(i, j)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: i, j
  dist = SQRT( (City(1,i)-City(1,j))**2 + (City(2,i)-City(2,j))**2 )
END FUNCTION dist

SUBROUTINE random_city_swap(N, a, b, old_sum, new_sum)
  IMPLICIT NONE
  INTEGER, INTENT(in) :: N
  INTEGER, INTENT(out) :: a, b
  DOUBLE PRECISION, INTENT(out) :: old_sum, new_sum
  INTEGER :: a_prev, a_next, b_prev, b_next
  DOUBLE PRECISION :: r

  ! Pick 2 cities to swap by calling 2 differing random numbers
  CALL RANDOM_NUMBER(r)
  a = NINT( r*N + 0.5 )  
  CALL RANDOM_NUMBER(r) 
  b = NINT( r*N + 0.5 )
  DO WHILE (b == a)
    CALL RANDOM_NUMBER(r)
    b = NINT( r*N + 0.5 )
  END DO

  ! Indicies of previous and next city of the two swapped cities
  ! MOD to handle wrapping ie. prev of 1st city is Nth city
  a_prev = MOD(a-2 + N, N) + 1
  a_next = MOD(a, N) + 1
  b_prev = MOD(b-2 + N, N) + 1
  b_next = MOD(b, N) + 1

  ! Calculate new path length of proposed swap
  ! When a & b are not adjacent:
  IF (a_next /= b .AND. b_next /= a) THEN
    ! four edge swaps
    old_sum = dist(a_prev,a) + dist(a,a_next) + dist(b_prev,b) + dist(b,b_next)
    ! swap a and b
    new_sum = dist(a_prev,b) + dist(b,a_next) + dist(b_prev,a) + dist(a,b_next)
  ELSE
    ! a and b are adjacent, 3 edge swaps, order matters
    IF (a_next == b) THEN ! a before b
      old_sum = dist(a_prev,a) + dist(a,b) + dist(b,b_next)
      new_sum = dist(a_prev,b) + dist(b,a) + dist(a,b_next)

    ELSE IF (b_next == a) THEN ! b before a
      old_sum = dist(b_prev,b) + dist(b,a) + dist(a,a_next)
      new_sum = dist(b_prev,a) + dist(a,b) + dist(b,a_next)
    ELSE 
      WRITE(*,*) "logic flaw in new path length calculation"
    END IF
  END IF

  ! Logic:
  ! By calculating only the edges that have changed in the swap, 
  ! we achieve O(1) path length calculation
END SUBROUTINE random_city_swap

SUBROUTINE metropolis(dL, p, accprob, metropolis_accepted)
  IMPLICIT NONE
  DOUBLE PRECISION, INTENT(in) :: dL, p
  DOUBLE PRECISION, INTENT(out) :: accprob
  LOGICAL, INTENT(out) :: metropolis_accepted
  DOUBLE PRECISION :: r


  IF (p <= 10E-3 ) THEN     
      accprob = 0.0           ! Adhoc to get rid of the floating underflow.
  ELSE
      accprob = EXP(-dL/p)    ! analogous to P(dE) = exp^(-dE / kt)
  END IF

  CALL RANDOM_NUMBER(r)       ! random number between 0 & 1 to compare accprob to

  metropolis_accepted = (accprob >= r)

  ! Logic:
  ! If dL is -ve (shorter path) then it will always be accepted, since accprob > 1.
  ! If dL is +ve (longer path) then it will be accepted only with 
  ! probability EXP(-dL/p), (ie is it greater than a randomly chosen
  ! number between 0 and 1)

END SUBROUTINE metropolis


SUBROUTINE log_data_to_file(j, L, p, City)
  IMPLICIT NONE
  INTEGER(KIND=k16), INTENT(in) :: j
  DOUBLE PRECISION, INTENT(in) :: L, p 
  REAL, INTENT(in) :: City(2,N)
  INTEGER :: o ! local loop index

  WRITE(20,'(I8, 2X, F12.6, 2X, F12.6, 2X)', ADVANCE='NO') j, L, p
  DO o = 1, N
    WRITE(20,'(F0.6,",",F0.6,",")', ADVANCE='NO') City(1,o), City(2,o)
  END DO
  WRITE(20,'(F0.6,",",F0.6,",")', ADVANCE='NO') City(1,1), City(2,1)
  WRITE(20,*) ! Empty write to advance line
END SUBROUTINE log_data_to_file

END PROGRAM TSP


!----------External Function---------

DOUBLE PRECISION FUNCTION custom_rand(iseed,first)

!  This function returns a pseudo-random number for each invocation.
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
  IMPLICIT NONE
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
END FUNCTION custom_rand

! Gnuplot reminder, using gp script now though
!Terminal type set to 'qt'
!gnuplot> pl 'CitiesPlot.txt' u 2:3:1 w labels point offset character 0, character 1 tc rgb @blue@, ' ' u 2:3 w l



!---------- Further Considerations ----------

! - Neighbourhood trimming - instead of blind swaps, take two edges a-b & c-d then
!   reconnect them as a-c & b-d This is known as 2-opt heuristic, could try 3-opt too.
!   This would be really good because I've noticed thanks to the animation that it takes
!   a long time for the program to get rid of simple crossed paths.

! - Multiple runs in parallel

! - Writing to disk during annealing (even minor debugging write statements) 
!   impacts performance negatively

! - Subroutines/functions to despahgetti-fy the code 

! - <Ideal p, length of the greatest gap between two cities> - How could I 
!   impliment this? by keeping the p to that value for a long time perhaps?

! - get spherical coordinates
! - only change the angle, not radius

! - Traingle inequality?

! calculate brute force solution iteration requirements and compare

! - use prim's algorithm to find the 'minimum spanning tree' so that we have a lower bound 
!   we can compare our solution too

! - split into random swap - 3 opt 

! - reheating