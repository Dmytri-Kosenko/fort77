      program laba1
      call menu
      pause
      end
      
      subroutine menu()
      implicit none
      real a, alpha, beta, gamma, triangleArea, mina, minc
      common /triangle/a, alpha, beta, gamma,
     &triangleArea/angles/mina,minc
      integer c
      call input
   99 print ('(a)'), 'Menu', '1)Entering values of triangle',
     &'2)Calculating the area of a triangle',
     &'3)Calculation of the minimum angle in degrees',
     &'4)Calculation of the cos of the minimum angle',
     &'5)Exit'
      read *,c
      goto (1,2,3,4,5), c
    1 call input
      goto 99
    2 call AreaOfTriangle
      print *,'Triangle area: ', triangleArea
      goto 99
    3 call minAngle
      print *,'Minimum angle: ',mina
      goto 99
    4 call minCos
      print *,'Cos of minimum angle: ', minc
      goto 99
    5 stop
      end
 
      subroutine input
      implicit none
      real a, alpha, beta, gamma, triangleArea, mina, minc 
      common /triangle/a, alpha, beta, gamma, triangleArea
      common /angles/mina, minc
      logical isTriangle
      print *,'Enter new values for the characteristics '
     &'of the triangle (side, angles):'
      read *,a, alpha, beta
      gamma = 180 - alpha - beta
      if(.not. isTriangle(a,alpha,beta, gamma)) then
        call input
      endif
      end
      
      subroutine minAngle
      implicit none
      common /triangle/a, alpha, beta, gamma, triangleArea
      common /angles/mina, minc
      real a, alpha, beta, gamma, triangleArea, mina, minc
      mina = min(alpha, beta, gamma)
      end
      
      subroutine minCos
      implicit none
      real a, alpha, beta, gamma, triangleArea, mina, minc
      common /triangle/a, alpha, beta, gamma, triangleArea
      common /angles/mina, minc
      real pi
      pi = 3.1415
      call minAngle(alpha,beta,gamma)
      minc = cos(mina * pi/180)
      end
      
      subroutine AreaOfTriangle
      implicit none
      real a, alpha, beta, gamma, triangleArea
      common /triangle/a, alpha, beta, gamma, triangleArea
      real pi
      pi = 3.1415
      triangleArea = a**2/(2*(1/tan(alpha*pi/180) + 
     &1/tan(beta*pi/180)) )
      end
      
      logical function isTriangle(a,alpha,beta, gamma)
      implicit none
      real a,alpha,beta,gamma
      if(alpha + beta .ge. 180 )then
        print *,'The sum of the angles exceeds 180'
        isTriangle = .false.
      else if( alpha .le. 0 .or.
     &beta .le. 0 .or. a .le. 0)then
        print *,'incorrect input'
        isTriangle = .false.
      else
        isTriangle = .true.
      endif
      end
