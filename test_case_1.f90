
MODULE my_test_case
    USE test_case, ONLY: base_test_case
    USE particle_simulation, ONLY: N, d, r, v, t, a

    IMPLICIT NONE 

    TYPE, EXTENDS(base_test_case) :: my_tc 

        CONTAINS 

        PROCEDURE :: initialize => initialize_circle

    END TYPE

    CONTAINS
        
   SUBROUTINE initialize_circle(this)
        CLASS(my_tc), INTENT(INOUT) :: this
     
        ! Local variables 
        REAL, PARAMETER :: rad = 1.0, rad_vel = 1.0
        REAL, PARAMETER :: pi = 4.0*atan(1.0)
        REAL, PARAMETER :: theta_step = 2.0*pi/REAL(N)

        INTEGER :: i 
        REAL :: theta

        ! Circle around the origin, radial velocity outwards
        DO i = 1, N
           theta = REAL(i-1)*theta_step

           r(1,i) = rad*cos(theta)
           r(2,i) = rad*sin(theta)

           v(1,i) = rad_vel*cos(theta)
           v(2,i) = rad_vel*sin(theta)
        END DO

     END SUBROUTINE initialize_circle

END MODULE my_test_case

PROGRAM test_particle_simulation
    USE my_test_case, ONLY: my_tc
    USE particle_simulation, ONLY: load_test_case, integrate_vverlet

    IMPLICIT NONE 

    TYPE(my_tc) :: tc
    
    PRINT *, "Running test case: circle"

    ! Initialize the test case 
    CALL load_test_case(tc)
    CALL integrate_vverlet(1000, 1e-2)

END PROGRAM
