program bisection
      implicit   none
      real::a,b,c,tol,f
      tol=1e-2

      write(*,*)"Write the interval in which root lies"
      10 read(*,*)a,b
      if (f(a)*f(b) .gt. 0) then 
              print*,"The root doesnot lie in this interval"
              goto 10

      end if
      15 if (f(a)*f(b) .lt. 0) then
             c=(a+b)/2
      end if
      if (f(a)*f(c) .lt. 0) then 
              b=c
      else
              a=c
      end if
      if (abs(b-a) .gt. tol) then
              goto 15
      end if
      print*,"The root is ",c
      end
      real function f(x)
              real::x,m
              m=-7
              f=tanh(m*x)-x
              
              end
