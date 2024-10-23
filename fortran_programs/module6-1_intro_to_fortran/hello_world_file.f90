    PROGRAM HelloWorld
    IMPLICIT NONE ! requires you to explicitly define the type for every variable that you use in your program, so to avoid undefined variables
    CHARACTER(len = 25) :: x

    WRITE(*,*) 'Hello, world!' ! this prints out on the screen
    OPEN(1,file='MyFile.dat') ! this assigns an ID number to the file
    WRITE(1,99)'Hello world!' ! this prints out to the file with the format specifier 99
    CLOSE(1)
    CALL flush(1)


    OPEN(1,file='MyFile.dat')
    READ(1,99) x ! we read the data from file and put it into x
    OPEN(2,file='MyFile2.dat',STATUS='REPLACE',ACTION='WRITE') ! this assigns an ID number to the file MyFile2
    WRITE(2,99)'Hello world, again!' ! this prints out to the file with the format specifier 99

    CALL flush(1)
    CALL flush(2)
    CLOSE(1)
    CLOSE(2)

    99 FORMAT(A25)
    END PROGRAM HelloWorld
