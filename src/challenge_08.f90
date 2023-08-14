PROGRAM main
    IMPLICIT NONE
    !=========================================================================!
    INTEGER :: grid_width = 0
    INTEGER :: grid_height = 0

    INTEGER, DIMENSION(:, :), ALLOCATABLE :: tree_grid
    
    LOGICAL :: test = .false.
    !=========================================================================!
    
    IF(.NOT. test) THEN
        CALL get_gridsize("input_08.txt", grid_width, grid_height)
        ALLOCATE(tree_grid(grid_height, grid_width))
        CALL read_tree_grid("input_08.txt", grid_width, grid_height, tree_grid)
    ELSE
        CALL get_gridsize("input_08_test.txt", grid_width, grid_height)
        ALLOCATE(tree_grid(grid_height, grid_width))
        CALL read_tree_grid("input_08_test.txt", grid_width, grid_height, &
                           &tree_grid)
    ENDIF
    
    CALL challenge_part_i(tree_grid, grid_width, grid_height)
    CALL challenge_part_ii(tree_grid, grid_width, grid_height)
END PROGRAM main


SUBROUTINE get_gridsize(filename, size_x, size_y)
    IMPLICIT NONE
    !=========================================================================!
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT) :: size_x
    INTEGER, INTENT(OUT) :: size_y
    !-------------------------------------------------------------------------!
    INTEGER :: nunit = 20
    INTEGER :: iostatus = 0
    CHARACTER :: char
    !=========================================================================!
    
    size_x = 0
    size_y = 0
    
    OPEN(nunit, file=filename, action="read", status="old", delim="NONE")

    DO
        READ(nunit, "(A)", iostat=iostatus, advance="no") char
        IF(iostatus /= 0) EXIT
        size_x = size_x + 1
    ENDDO

    rewind(unit=nunit, iostat=iostatus)

    DO
        READ(nunit, "(A)", iostat=iostatus) char
        IF(iostatus /= 0) EXIT
        size_y = size_y + 1
    ENDDO
    
    CLOSE(nunit)

    WRITE(*, "(A, I0)") "Grid width : ", size_x
    WRITE(*, "(A, I0)") "Grid height: ", size_y
END SUBROUTINE get_gridsize


SUBROUTINE read_tree_grid(filename, size_x, size_y, grid)
    IMPLICIT NONE
    !=========================================================================!
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER, INTENT(in) :: size_x
    INTEGER, INTENT(in) :: size_y
    INTEGER, DIMENSION(size_x, size_y), INTENT(inout) :: grid
    !-------------------------------------------------------------------------!
    INTEGER :: nunit = 20
    INTEGER :: iostatus = 0
    INTEGER :: i
    INTEGER :: j
    CHARACTER(len=size_x) :: line
    !=========================================================================!
    
    OPEN(nunit, file=filename, action="read", status="old", delim="NONE")
    
    DO i = 1, size_x
        READ(nunit, *, iostat=iostatus) line
        IF(iostatus /= 0) EXIT
        
        DO j = 1, size_y
            READ(line(j:j), "(I1)") grid(i, j)
        ENDDO
    ENDDO
    
    CLOSE(nunit)
END SUBROUTINE read_tree_grid


SUBROUTINE challenge_part_i(grid, size_x, size_y)
    IMPLICIT NONE
    !=========================================================================!
    INTEGER, INTENT(in) :: size_x
    INTEGER, INTENT(in) :: size_y
    INTEGER, DIMENSION(size_x, size_y), INTENT(in) :: grid
    !-------------------------------------------------------------------------!
    INTEGER :: x
    INTEGER :: y
    INTEGER :: current_height
    INTEGER :: num_vis_trees
    !=========================================================================!
    num_vis_trees = 0
    DO x = 1, size_x
        DO y = 1, size_y
            current_height = grid(x, y)
            IF(all(grid(x, :y - 1) < current_height) .or. &
              &all(grid(x, y + 1:) < current_height) .or. &
              &all(grid(:x - 1, y) < current_height) .or. &
              &all(grid(x + 1:, y) < current_height)) THEN
                num_vis_trees = num_vis_trees + 1
                WRITE(*, "(A)", advance="no") "^"
            ELSE
                WRITE(*, "(A)", advance="no") "#"
            ENDIF
        ENDDO
        WRITE(*, *) ""
    ENDDO
    
    WRITE(*, "(A, I0)") "Part I: Number of visibile trees: ", num_vis_trees
END SUBROUTINE


SUBROUTINE challenge_part_ii(grid, size_x, size_y)
    IMPLICIT NONE
    !=========================================================================!
    INTEGER, INTENT(in) :: size_x
    INTEGER, INTENT(in) :: size_y
    INTEGER, DIMENSION(size_x, size_y), INTENT(in) :: grid
    !-------------------------------------------------------------------------!
    INTEGER :: x
    INTEGER :: y
    INTEGER :: scenic_score
    INTEGER :: highest_scenic_score = 0
    !-------------------------------------------------------------------------!
    interface
        INTEGER FUNCTION get_scenic_score(grid, size_x, size_y, pos_x, pos_y)
            INTEGER, INTENT(in) :: size_x
            INTEGER, INTENT(in) :: size_y
            INTEGER, DIMENSION(size_x, size_y), INTENT(in) :: grid
            INTEGER, INTENT(in) :: pos_x
            INTEGER, INTENT(in) :: pos_y
        END FUNCTION get_scenic_score
    END interface
    !=========================================================================!
    
    DO x = 1, size_x
        DO y = 1, size_y
            scenic_score = get_scenic_score(grid, size_x, size_y, x, y)
            WRITE(*, "(I7)", advance="no") scenic_score
            IF(scenic_score > highest_scenic_score) THEN
                highest_scenic_score = scenic_score
            ENDIF
        ENDDO
        WRITE(*, *) ""
    ENDDO
    
    WRITE(*, "(A, I0)") "Part II: Highest possible score: ", &
                       &highest_scenic_score
END SUBROUTINE


INTEGER FUNCTION get_scenic_score(grid, size_x, size_y, pos_x, pos_y)
    IMPLICIT NONE
    !=========================================================================!
    INTEGER, INTENT(in) :: size_x
    INTEGER, INTENT(in) :: size_y
    INTEGER, DIMENSION(size_x, size_y), INTENT(in) :: grid
    INTEGER, INTENT(in) :: pos_x
    INTEGER, INTENT(in) :: pos_y
    !-------------------------------------------------------------------------!
    INTEGER :: score_up
    INTEGER :: score_down
    INTEGER :: score_left
    INTEGER :: score_right
    INTEGER :: x
    INTEGER :: y
    INTEGER :: height
    !=========================================================================!
    
    score_up = 0
    score_down = 0
    score_left = 0
    score_right = 0
    
    height = grid(pos_x, pos_y)
    
    IF(pos_x == 1) THEN
        score_up = 0
    ELSE
        DO x = pos_x - 1, 1, -1
            score_up = score_up + 1
            IF(grid(x, pos_y) >= height) EXIT
        ENDDO
    ENDIF
    
    IF(pos_x == size_x) THEN 
        score_down = 0
    ELSE
        DO x = pos_x + 1, size_x
            score_down = score_down + 1
            IF(grid(x, pos_y) >= height) EXIT
        ENDDO
    ENDIF
    
    IF(pos_y == 1) THEN
        score_left = 0
    ELSE
        DO y = pos_y - 1, 1, -1
            score_left = score_left + 1
            IF(grid(pos_x, y) >= height) EXIT
        ENDDO
    ENDIF
    
    IF(pos_y == size_y) THEN
        score_right = 0
    ELSE
        DO y = pos_y + 1, size_y
            score_right = score_right + 1
            IF(grid(pos_x, y) >= height) EXIT
        ENDDO
    ENDIF
    
    get_scenic_score = score_up * score_down * score_left * score_right
END FUNCTION get_scenic_score