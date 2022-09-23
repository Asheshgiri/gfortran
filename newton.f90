program newton
      implicit none
      real::x,it
      write(*,*)"The initial guess"
      read(*,*)x
      call ntrap(x,it)
        print*,"The root and iterartion is",x,it
      end
      real function f(x)
              real::x
              f=x-cos(x)
              end
              real function fd(x)
                      real::x
                      fd=1+sin(x)
                      end
      subroutine ntrap(x,it)
              implicit none
              real,external::f,fd
              real::x,tol,error,it
              tol=1e-3
              it=0
              10 error= -f(x)/fd(x)
              x=x+error
              it=it+1
              if(abs(error) .gt. tol) then
                      goto 10
              end if
              end
