      PROGRAM program_using_module
      USE math_consts_module

      real :: x
      x = cos(pi)

      write(*,*) "The cosine of pi is ", x
      x = log(e)

      write(*,*) "The natural log of e is ", x

      END PROGRAM program_using_module
