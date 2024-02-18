
        PROGRAM Lab1
        call Menu()
        END
        SUBROUTINE Menu()
        common /Str/ S
        common /vych/ cos_u,ugol
99      print *, 'Viberite punkt menu:'
        print *, '1)Vvesti koordinaty treugolnika'
        print *, '2)Ploshad" Treugolnika'
        print *, '3)Minimal"nii ugol '
        print *, '4)COS ugla'
        print *, '5)Exit'
        read *, I
        goto (1,2,3,4,5) I
1       call Input()
        goto 99
2       call Strka()
        print *, 'S=',S
        pause
        print *, '_______________________'
        goto 99 
3       call minugol()
        print *, 'UGOL=',ugol
        pause
        print *, '_______________________'
        goto 99
4       call mincos()
        print *,'COS=', cos_u
        pause
        print *, '_______________________'
        goto 99       
5       END

        SUBROUTINE Input()
        common /sides/ l,x,y,z
        print *, 'Enter storony:'
        read *,l
        print *,'ygol y'
        read *,y
        print *,'ygol x'
        read *,x    
        if( (x+y).ge.180)then
        print *, 'NE KORREKTNIY VVOD YGLA'
        endif
        if( x.eq.0 .or. y.eq.0 .or. l.eq.0)then
        print *, 'NE KORREKTNIY VVOD znachenia'
        endif
        END
        
        SUBROUTINE Calcsides()
        common /sides/ l,x,y,z
        common /a/ l1,x1,y1,z1
        real Pi
        Pi=3.141592652 
        z=180-x-y
        z1 = ( ( 180 - x - y ) * pi ) / 180
        x1 = ( x * pi ) / 180
        y1 = ( y * pi ) / 180
        l1=l
        END

        SUBROUTINE Strka()
        common /a/ l1,x1,y1,z1/Str/ S
        call Calcsides()
        S = (l1**2 * sin(x1) * sin(y1) ) / ( 2 * sin(z1) )  
        pause
        END
        
        SUBROUTINE  minugol()
        Common  /sides/ l,x,y,z/vych/ cos_u,ugol
        if( x .le. y)then
          ugol= x
        else 
          ugol = y
        endif
        if( ugol .ge. (180 - (x + y)))then
          ugol= 180 - (x + y)
        endif
        
        END  

        SUBROUTINE  mincos()
        Common  /sides/ l,x,y,z/vych/ cos_u,ugol
        common /a/ l1,x1,y1,z1
        call minugol()
        cos_u= cos(ugol)
        end

      
