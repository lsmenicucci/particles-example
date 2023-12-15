MODULE particle_simulation
    USE test_case, ONLY: base_test_case

    ! Module to perform integration of a harmonic oscillator
    implicit none

    ! Global parameters ------------------------------------------------------
    INTEGER, PARAMETER :: n = 20          ! Number of particles
    INTEGER, PARAMETER :: d = 2          ! Number of dimensions
    REAL, DIMENSION(d, n) :: r, v, a     ! Position, velocity, acceleration
    REAL :: t                            ! Time
    CLASS(base_test_case), ALLOCATABLE :: cur_tc

    CONTAINS

    ! Compute the distance between particles i and j
    REAL FUNCTION dist(i, j)
        INTEGER, INTENT(IN) :: i, j
        dist = SQRT(SUM((r(:, i) - r(:, j))**2))
    END FUNCTION dist

    ! Compute the kinetic energy of the system
    REAL FUNCTION kinetic_energy()
        kinetic_energy = 0.5 * SUM(v**2)
    END FUNCTION kinetic_energy

    ! Load a test case to be simulated
    SUBROUTINE load_test_case(tc)
        CLASS(base_test_case), INTENT(INOUT) :: tc

        ! Set current test case 
        cur_tc = tc

        ! Initialize position, velocity, acceleration
        r = 0.0
        v = 0.0
        a = 0.0

        ! Initialize test case
        CALL cur_tc%initialize()
        CALL cur_tc%io%init(n)

    END SUBROUTINE load_test_case
        
    SUBROUTINE integrate_vverlet(steps, dt)

        INTEGER, INTENT(IN) :: steps
        REAL, INTENT(IN) :: dt
        INTEGER :: ti
        REAL, DIMENSION(d, n) :: r_old, v_old, a_old

        DO ti = 1, steps 
            ! Save old values
            r_old = r
            v_old = v
            a_old = a

            ! Compute v(t + dt/2)
            v = v_old + 0.5 * a_old * dt

            ! Compute x(t + dt)
            r = r_old + v * dt 

            ! Use x(t + dt) to compute a(t + dt)
            ! Note that we don't know the implementation of add_acceleration
            a = 0
            CALL cur_tc%add_acceleration()

            ! Compute v(t + dt)
            v = v_old + 0.5 * (a + a_old) * dt

            ! Apply boundary conditions 
            CALL cur_tc%boundary_conditions()

            ! Save snapshot 
            CALL cur_tc%io%save(r, v, a)

        END DO

    END SUBROUTINE integrate_vverlet

END MODULE
