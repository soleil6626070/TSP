!----------------------------------------------------------!
! PHY2073 - The Travelling Salesman Problem                !
! Name: Aidan Walsh                                        !
! URN: 6373906                                             !
! Deadline: May 22nd 2017                                  !
!----------------------------------------------------------!


PROGRAM TSP
 IMPLICIT NONE
 DOUBLE PRECISION :: the_sum, Lmean, Lerror, SD_sum
 DOUBLE PRECISION :: dL_sum, dLmean, dLerror, dLSD_sum
 DOUBLE PRECISION :: L, Lstep, Lini, Lnew, dL, r, rand, tempcity
 DOUBLE PRECISION :: accprob, p, annealsched, Lmin_scalar, dL_max, dL_min, absdLmax
 INTEGER :: iseed, first, i, k, a, b, z, y, progress, numswap, sucswap
 INTEGER :: accprobiter
 INTEGER, PARAMETER :: N = 40   !Number of Cities
 REAL, DIMENSION(1:2,1:N) :: City, City_savedpath, Lmin_array, City_ini
 INTEGER, PARAMETER :: sample_size = 100
 INTEGER, PARAMETER :: k16 = SELECTED_INT_KIND(R=16)
 INTEGER(KIND=k16) :: j
 REAL, DIMENSION(1:sample_size) :: Lvalues, dLvalues

! p     = 4.0 !SQRT(2.0)         !Parameter that varies with time
 first = 0           !Ideal p, length of the greatest gap between
 iseed = 971739      !two cities.
 annealsched = 0.99
 Lmin_scalar = 9999999.0
 absdLmax = 0.0
 WRITE(6,'(a,i9)')'seed value that generates this sequence is ',iseed

!-----This do-loop assigns N cities random x and y coordinates.--------

 DO i = 1,N

   City(1,i) = rand(iseed,first)
   City(2,i) = rand(iseed,first)

!  WRITE(6,*)City(1,i), City(2,i)

 END DO

   City_ini = City
!------------------DOOOOOOOOOOOOloop--------------------------




the_sum = 0.0
dl_sum = 0.0


!-------This calculates the length of the path in order----------------


! City = City_ini
 Lstep = 0.0
 Lini = 0.0

 DO i = 1,(N-1) 

    Lstep = (City(1,i)-City(1,i+1))**2.0
    Lstep = (City(2,i)-City(2,i+1))**2.0 + Lstep
    Lini = Lini + SQRT(Lstep)

 END DO


DO z=1, sample_size           !big do loop
 annealsched = 0.99 !wooooooooooooooooooo

p  = 100*N        !8.0 ! SQRT(2.0)
numswap = 0
sucswap = 0
 dL_max = 0.0
 dL_min = 0.0
!accprobiter = 0



! IF (z == 2) THEN
!  WRITE(6,*)'1',L
! END IF
! L = 0.0
! L = Lini

!IF (z >= 2) THEN                                      !mayb 1
!    City = Lmin_array !City_savedpath !Lmin_array
!END IF

!----------Generates two random #s between 1 & N-----------------------

 L = 0.0 !remember this was added
! City = City_ini
 DO j = 1,999999_k16        !smaller do loop, to find each L

   IF (z==1 .AND. j==1) THEN
       WRITE(6,*)'l1',Lnew,L,p
   END IF
   IF (z==2 .AND. j==1) THEN
       WRITE(6,*)'l2',Lnew,L,p
   END IF
 
   CALL RANDOM_NUMBER(r)
   a = NINT( r*N + 0.5 )  

   CALL RANDOM_NUMBER(r) 
   b = NINT( r*N + 0.5 )  


!--------- Swapping cities---------------------------------------------


   tempcity  = City(1,a)     ! Swap two random cities' x-coordinates.
   City(1,a) = City(1,b)
   City(1,b) = tempcity

   tempcity  = City(2,a)     ! Then swap the same cities' y's.
   City(2,a) = City(2,b)
   City(2,b) = tempcity


!---------Calculating new path length & whether to accept it-----------

   Lnew = 0.0
!   Lstep = 0.0
     DO i = 1,(N-1)

       Lstep = (City(1,i)-City(1,i+1))**2.0
       Lstep = (City(2,i)-City(2,i+1))**2.0 + Lstep
       Lnew = Lnew + SQRT(Lstep)

     END DO

   IF (z==1 .AND. j==1) THEN
       WRITE(6,*)'l1',Lnew,L
   END IF
   IF (z==2 .AND. j==1) THEN
       WRITE(6,*)'l2',Lnew,L
   END IF

   dL = Lnew - L             ! if Lnew > Lold , accprob is >1
   IF ( dL > dL_max ) THEN
        dL_max = dL
   ELSE IF ( dL < dL_min ) THEN
        dL_min = dL           
   END IF  


   IF (p <= 10E-3 ) THEN     ! This IF statement gets rid of the floating
       accprob = 0.0          ! underflow/ 
   ELSE
       accprob = EXP(-dL/p)    ! if p < 10E-3 then accprob is 10^-65
   END IF

   IF (accprob <= 0.6) THEN
       annealsched = 0.99999
   ELSE IF (accprob <= 0.6) THEN
       annealsched = 0.9
   END IF


   IF (z==1 .AND. j==1) THEN
       WRITE(6,*)'l1',Lnew,L
   END IF
   IF (z==2 .AND. j==1) THEN
       WRITE(6,*)'l2',Lnew,L
   END IF


!       accprobiter = accprobiter + 1
!   END IF

!   IF (accprobiter >= 5) THEN
!       annealsched = 0.99999 
!   END IF


!   IF (accprob <= 0.6) THEN 
         !sort this out init
!   END IF


   CALL RANDOM_NUMBER(r)     ! set p to biggest dL?

   

   IF (accprob >= r) THEN !Lnew <= L .OR. 
      L = Lnew
      City_savedpath = City
      sucswap = sucswap+1
   ELSE
      City = City_savedpath
   END IF

   IF (z==1 .AND. j==1) THEN
       WRITE(6,*)'l1',Lnew,L
   END IF
   IF (z==2 .AND. j==1) THEN
       WRITE(6,*)'l2',Lnew,L
   END IF


   numswap = numswap+1

!   WRITE(6,*)p   

!   IF ( sucswap <= 10*N .OR. numswap <= 100*N) CYCLE !EXP(N*1.0) 
        p = p*annealsched

!   if(mod(j,100)==0)write(6,*)j,L,p,dL
!write(6,*)j,L,dL,accprob,r

!----------------Annealing Schedule------------------------------------
!       p = p*annealsched
   IF (p >= 0.001) CYCLE   !0.000001
   IF (p <  0.001) EXIT
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
         WRITE(10,'(2f6.3)')Lmin_array(1,k), Lmin_array(2,k)
       END DO

      CLOSE(10)
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


END PROGRAM TSP


!------------------------------------------------------------------------
!get spherical coordinates
!only change the angle, not radius












DOUBLE PRECISION FUNCTION rand(iseed,first)

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
  rand = real(nextn)/real(modlus)
!
END FUNCTION rand


!Terminal type set to 'qt'
!gnuplot> pl 'CitiesPlot.txt' u 2:3:1 w labels point offset character 0, character 1 tc rgb @blue@, ' ' u 2:3 w l






