
MODULE my_test_case
    USE test_case, ONLY: base_test_case
    USE particle_simulation, ONLY: N, d, r, v, t, a, dist

    IMPLICIT NONE 

    TYPE, EXTENDS(base_test_case) :: my_tc 

        CONTAINS 

        PROCEDURE :: initialize => initialize_circle
        PROCEDURE :: boundary_conditions => box_boundary
        PROCEDURE :: add_acceleration => add_acceleration

    END TYPE

    CONTAINS

    REAL FUNCTION radial_f(i, j)
        INTEGER, INTENT(IN) :: i, j

        REAL, PARAMETER :: a = 0.5
        REAL :: r 

        r = dist(i, j)

        ! Lennard-Jones potential
        IF (r < a) THEN
            radial_f = 24.0*(2.0*r/a - 1.0)*(r/a)**(-7)
        ELSE
            radial_f = 0.0
        END IF

    END FUNCTION radial_f 

    SUBROUTINE add_acceleration(this)
        CLASS(my_tc), INTENT(INOUT) :: this 

        ! Local variables 
        INTEGER :: i, j, di 
        REAL :: angle, ax, ay, ar

        DO i = 1, N
            DO j = i+1, N
                angle = ATAN2(r(2,j) - r(2,i), r(1,j) - r(1,i))

                ar = radial_f(i, j)
                a(1,i) = a(1,i) + ar*COS(angle) 
                a(2,i) = a(2,i) + ar*SIN(angle) 

                a(1,j) = a(1,j) - ar*COS(angle) 
                a(2,j) = a(2,j) - ar*SIN(angle)

            END DO
        END DO

    END SUBROUTINE add_acceleration
        
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

     SUBROUTINE box_boundary(this)
         CLASS(my_tc), INTENT(INOUT) :: this

            ! Local variables 
            REAL, PARAMETER :: box_size = 5.0
            REAL, PARAMETER :: box_center = box_size/2.0
            integer :: i, di

            DO i = 1, n
                DO di = 1, d 
                    IF (r(di,i) < -box_size/2) THEN
                        r(di,i) = -box_size - r(di,i)
                        v(di,i) = -v(di,i)
                    ELSE IF (r(di,i) > box_size/2) THEN
                        r(di,i) = box_size - r(di,i)
                        v(di,i) = -v(di,i)
                    END IF
                END DO
            END DO
                

    END SUBROUTINE box_boundary

END MODULE my_test_case

PROGRAM test_particle_simulation
    USE my_test_case, ONLY: my_tc
    USE particle_simulation, ONLY: load_test_case, integrate_vverlet

    IMPLICIT NONE 

    TYPE(my_tc) :: tc
    
    PRINT *, "Running test case: circle"

    ! Initialize the test case 
    CALL load_test_case(tc)
    CALL integrate_vverlet(1000, 1e-3)

END PROGRAM
