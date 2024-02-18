      program laba1
      call menu
      pause
      end
      
      subroutine menu()
      implicit none
      integer c
      double precision alpha,beta,a,minangle,mincos
      alpha = -1
      beta = alpha
      a = alpha
   99 print ('(a)'), 'Menu', '1)Entering values of triangle',
     &'2)Calculating the area of a triangle',
     &'3)Calculation of the minimum angle in degrees',
     &'4)Calculation of the cos of the minimum angle',
     &'5)Exit'
      read *,c
      goto (1,2,3,4,5), c
    1 call input(a,alpha,beta)
      goto 99
    2 write(*,*) a, alpha,beta
      goto 99
    3 print *,'Minimum angle: ',minAngle(alpha,beta)
      goto 99
    4 print *,'Cos of minimum angle: ', minCos(minAngle(alpha,beta))
      goto 99
    5 stop
      end
 
      subroutine input(a, alpha, beta)
      implicit none
      double precision a, alpha, beta
      print *,'Enter new values for the characteristics '
     &'of the triangle (side, angles):'
      read *,a, alpha, beta
      end
      
      double precision function minAngle(alpha, beta)
      implicit none
      double precision alpha, beta, min
      if(alpha .le. beta) then
        min = alpha
      else
        min = beta
      endif
      if(min .ge. (180 - (alpha + beta))) then
        min = 180 - (alpha + beta)
      endif
      minAngle = min
      end
      
      double precision function minCos(angle)
      implicit none
      double precision angle
      minCos = cos(angle)
      end
