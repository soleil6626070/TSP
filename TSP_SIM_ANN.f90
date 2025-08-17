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
 INTEGER, PARAMETER :: N = 40   !Number of Cities
 REAL, DIMENSION(1:2,1:N) :: City, City_savedpath, Lmin_array, City_ini
 INTEGER, PARAMETER :: sample_size = 10
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


 first = 0           !Ideal p, length of the greatest gap between
 iseed = 971740      !two cities.
 fast_cooling = 0.99
 slow_cooling = 0.99999
 cooling_trigger = 0.8
 Lmin_scalar = 9999999.0
 absdLmax = 0.0
 WRITE(6,'(a,i9)')'seed value that generates this sequence is ',iseed

!-----This do-loop assigns N cities random x and y coordinates.--------

 DO i = 1,N

   City(1,i) = custom_rand(iseed,first)
   City(2,i) = custom_rand(iseed,first)

!  WRITE(6,*)City(1,i), City(2,i)

 END DO

   City_ini = City


! Do loop initialisations
the_sum = 0.0
dl_sum = 0.0
Lstep = 0.0
Lini = 0.0

! DO i = 1,(N-1) 

!    Lstep = (City(1,i)-City(1,i+1))**2.0
!    Lstep = (City(2,i)-City(2,i+1))**2.0 + Lstep
!    Lini = Lini + SQRT(Lstep)

! END DO

! Initial path length calculation
Lini = 0.0
DO i = 1,(N-1)
  Lini = Lini + dist(i,i+1)
END DO
! add distance from last city back to first
Lini = Lini + dist(N,1)

! Outer do loop
DO z=1, sample_size

! rapid schedule
annealsched = fast_cooling
! reset city order
City = City_ini
L = 0.0 
p  = 1000*N  ! Initially very large with fast cooling schedule
numswap = 0
sucswap = 0
dL_max = 0.0
dL_min = 0.0
!accprobiter = 0

! itialise swaps & moves trackers
worswap = 0
totworse = 0
total_moves_attempted = 0
total_moves_accepted = 0

! initialise frozen system exit strategy tracking variables
moves_attempted = 0  
moves_accepted = 0    
check_interval = 10000  ! Check every 1000 iterations
system_frozen = .FALSE.


! IF (z == 2) THEN
!  WRITE(6,*)'1',L
! END IF
! L = 0.0
! L = Lini

!IF (z >= 2) THEN                                      !mayb 1
!    City = Lmin_array !City_savedpath !Lmin_array
!END IF

!----------Generates two random #s between 1 & N-----------------------

 DO j = 1,999999_k16        !smaller do loop, to find each L

   CALL RANDOM_NUMBER(r)
   a = NINT( r*N + 0.5 )  

   CALL RANDOM_NUMBER(r) 
   b = NINT( r*N + 0.5 )

   DO WHILE (b == a)
      CALL RANDOM_NUMBER(r)
      b = NINT( r*N + 0.5 )
   END DO

! new method, previously I was recalculating the entire path length;
! which costs O(n) per iteration. 
! But we only need to calculate the edges before and after the swapped 
! nodes.
! Then once we have the sum of the 4 distances prior to swapping, we 
! can caluculate the sum of the 4 new edges after the swap and find
! the difference.
! 
! This will cost O(1)

! indicies of previous and next city of the two swapped cities
! MOD to handle wrapping ie. prev of 1st city is Nth city
a_prev = MOD(a-2 + N, N) + 1
a_next = MOD(a, N) + 1
b_prev = MOD(b-2 + N, N) + 1
b_next = MOD(b, N) + 1

! calculate distance funcmtion dist
! a = sqrt( b^2 + c^2 )
!dist(i,j) = SQRT( (City(1,i)-City(1,j))**2 + (City(2,i)-City(2,j))**2 )

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

! Calculate first path length
IF (j == 1) THEN
  ! on first iteration
  L = 0.0
  DO i = 1,(N-1)
    L = L + dist(i,i+1)
  END DO
  ! add distance from last city back to first
  L = L + dist(N,1)
END IF

! Calculate new path length
Lnew = L + (new_sum - old_sum)

! difference in Length new path gives
dL = Lnew - L

! Statistical tracking of max/min values of DL (not important to acceptance)
IF ( dL > dL_max ) THEN
  dL_max = dL
ELSE IF ( dL < dL_min ) THEN
  dL_min = dL           
END IF

! Count attempted move
moves_attempted = moves_attempted + 1
total_moves_attempted = total_moves_attempted + 1 

! ---------- Acceptance Criteria ----------
! setting accprob to zero for p <= 10^-3 gets rid of the floating underflow.
! if p < 10E-3 then accprob is 10^-65
   IF (p <= 10E-3 ) THEN     
       accprob = 0.0         
   ELSE
       accprob = EXP(-dL/p)    ! analogous to P(dE) = exp^(-dE / kt)
   END IF

! Should we accept?

! if dL is -ve (shorter path) then it will always be accepted,
! since EXP(-dL/p) will always be > 1.
! if dL is +ve (longer path) then it will be accepted only with 
! probability EXP(-dL/p), (ie is it greater than a randomly chosen
! number between 0 and 1)
CALL RANDOM_NUMBER(r)
IF (accprob >= r) THEN ! accept
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

! ---- Annealing schedule update ----

! Count total worse swaps accepted
IF (accprob < 1.0 ) totworse = totworse + 1
! Adjust annealing schedule based on acceptance rate
! Swap to slow cooling once 60% of worse solutions are being accepted.
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

    IF (acceptance_rate < 0.01) THEN
      ! system is frozen
      !system_frozen = .TRUE.
      !EXIT
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

 WRITE(6,*)'The L #',z,'is'
 WRITE(6,*)L
! WRITE(6,*)' '
! WRITE(6,*)'biggest dl',dl_max
! WRITE(6,*)'smalest dL',dL_min
!-------------------------ABSOULUTE----------------------------------

dL_sum = dL_sum + dL_max
the_sum = the_sum + L
Lvalues(z) = L              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
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

!----------Internal FUnction---------
CONTAINS

! internal function so we can access all variables without passing them
! which would make the code illegible

! Good old a^2 = b^2 + c^2
DOUBLE PRECISION FUNCTION dist(i, j)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: i, j
  dist = SQRT( (City(1,i)-City(1,j))**2 + (City(2,i)-City(2,j))**2 )
END FUNCTION dist

END PROGRAM TSP


!------------------------------------------------------------------------
!get spherical coordinates
!only change the angle, not radius



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


!Terminal type set to 'qt'
!gnuplot> pl 'CitiesPlot.txt' u 2:3:1 w labels point offset character 0, character 1 tc rgb @blue@, ' ' u 2:3 w l






