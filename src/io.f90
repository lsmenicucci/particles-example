MODULE io_base_mod
    IMPLICIT NONE 

    ! Abstract base class for IO, all IO classes must inherit from this
    TYPE, ABSTRACT :: io_base
        CONTAINS 

        PROCEDURE(io_base_load), DEFERRED :: load
        PROCEDURE(io_base_save), DEFERRED :: save
        PROCEDURE(io_base_init), DEFERRED :: init
        PROCEDURE(io_base_finalize), DEFERRED :: finalize

    END TYPE io_base

    INTERFACE
        SUBROUTINE io_base_init(this, n)
            IMPORT io_base
            CLASS(io_base), INTENT(INOUT) :: this
            INTEGER, INTENT(IN) :: n
        END SUBROUTINE io_base_init
    END INTERFACE

    INTERFACE
        SUBROUTINE io_base_finalize(this)
            IMPORT io_base
            CLASS(io_base), INTENT(INOUT) :: this
        END SUBROUTINE io_base_finalize
    END INTERFACE

    INTERFACE
        SUBROUTINE io_base_load(this, r, v, a)
            IMPORT io_base
            CLASS(io_base), INTENT(INOUT) :: this
            REAL, DIMENSION(:, :), INTENT(OUT) :: r, v, a
        END SUBROUTINE io_base_load
    END INTERFACE

    INTERFACE
        SUBROUTINE io_base_save(this, r, v, a)
            IMPORT io_base
            CLASS(io_base), INTENT(INOUT) :: this
            REAL, DIMENSION(:, :), INTENT(IN) :: r, v, a
        END SUBROUTINE io_base_save
    END INTERFACE


END MODULE io_base_mod
