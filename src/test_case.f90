MODULE test_case
    USE io_csv_mod, ONLY : io_csv

    TYPE :: base_test_case
        TYPE(io_csv) :: io

        CONTAINS 

        PROCEDURE :: initialize 
        PROCEDURE :: boundary_conditions 
        PROCEDURE :: add_acceleration 
        PROCEDURE :: save_snapshot 

    END TYPE

    CONTAINS

    ! Test case procedures ---------------------------------------------------
    ! Dummy subroutines which will be reimplemented in the test cases 

    SUBROUTINE initialize(this)
        IMPLICIT NONE
        CLASS(base_test_case), INTENT(INOUT) :: this
    END SUBROUTINE initialize

    SUBROUTINE boundary_conditions(this)
        IMPLICIT NONE
        CLASS(base_test_case), INTENT(INOUT) :: this
    END SUBROUTINE boundary_conditions

    SUBROUTINE add_acceleration(this)
        IMPLICIT NONE
        CLASS(base_test_case), INTENT(INOUT) :: this
    END SUBROUTINE add_acceleration 

    SUBROUTINE save_snapshot(this)
        IMPLICIT NONE
        CLASS(base_test_case), INTENT(INOUT) :: this
    END SUBROUTINE save_snapshot


END MODULE test_case
