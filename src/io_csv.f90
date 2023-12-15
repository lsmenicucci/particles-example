MODULE io_csv_mod
    USE io_base_mod, ONLY: io_base

    IMPLICIT NONE

    ! Save data to a CSV file
    TYPE, EXTENDS(io_base) :: io_csv
        CHARACTER(LEN = 256) :: filename = 'data.csv'
        INTEGER :: file_unit = 10
        INTEGER :: n = 0

        CONTAINS 
        
        PROCEDURE :: init => io_csv_init 
        PROCEDURE :: finalize => io_csv_finalize
        PROCEDURE :: load => io_csv_load 
        PROCEDURE :: save => io_csv_save 
    END TYPE io_csv

    CONTAINS

    SUBROUTINE io_csv_init(this, n)
        CLASS(io_csv), INTENT(INOUT) :: this
        INTEGER, INTENT(IN) :: n

        OPEN(UNIT = this%file_unit, FILE = this%filename)
        WRITE(this%file_unit, *) n

        this%n = n
    END SUBROUTINE io_csv_init

    SUBROUTINE io_csv_finalize(this)
        CLASS(io_csv), INTENT(INOUT) :: this

        CLOSE(UNIT = this%file_unit)
    END SUBROUTINE io_csv_finalize 

    SUBROUTINE io_csv_load(this, r, v, a)
        CLASS(io_csv), INTENT(INOUT) :: this
        REAL, DIMENSION(:, :), INTENT(OUT) :: r, v, a
        INTEGER :: i, j, n

        DO i = 1, SIZE(r, 1)
            READ(this%file_unit, *) (r(i, j), j = 1, SIZE(r, 2))
        END DO

        DO i = 1, SIZE(v, 1)
            READ(this%file_unit, *) (v(i, j), j = 1, SIZE(v, 2))
        END DO

        DO i = 1, SIZE(a, 1)
            READ(this%file_unit, *) (a(i, j), j = 1, SIZE(a, 2))
        END DO
    END SUBROUTINE io_csv_load

    SUBROUTINE io_csv_save(this, r, v, a)
        CLASS(io_csv), INTENT(INOUT) :: this
        REAL, DIMENSION(:, :), INTENT(IN) :: r, v, a
        INTEGER :: i, j

        DO i = 1, this%n
            WRITE(this%file_unit, "(*(F10.5))") r(:, i)
        END DO

    END SUBROUTINE io_csv_save


END MODULE io_csv_mod
