program main
    implicit none
    !=========================================================================!
    integer :: grid_width = 0
    integer :: grid_height = 0
    integer :: num_vis_trees = 0
    integer :: highest_scenic_score = 0

    integer, dimension(:, :), allocatable :: tree_grid
    
    logical :: test = .false.
    !=========================================================================!
    
    if(.not. test) then
        call get_gridsize("input_08.txt", grid_width, grid_height)
        allocate(tree_grid(grid_height, grid_width))
        call read_tree_grid("input_08.txt", grid_width, grid_height, tree_grid)
    else
        call get_gridsize("input_08_test.txt", grid_width, grid_height)
        allocate(tree_grid(grid_height, grid_width))
        call read_tree_grid("input_08_test.txt", grid_width, grid_height, &
                           &tree_grid)
    endif
    
    call challenge_part_i(tree_grid, grid_width, grid_height)
    call challenge_part_ii(tree_grid, grid_width, grid_height)
end program main


subroutine get_gridsize(filename, size_x, size_y)
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    integer, intent(out) :: size_x
    integer, intent(out) :: size_y
    !-------------------------------------------------------------------------!
    integer :: nunit = 20
    integer :: iostatus = 0
    character :: char
    !=========================================================================!
    
    size_x = 0
    size_y = 0
    
    open(nunit, file=filename, action="read", status="old", delim="none")

    do
        read(nunit, "(A)", iostat=iostatus, advance="no") char
        if(iostatus /= 0) exit
        size_x = size_x + 1
    enddo

    rewind(unit=nunit, iostat=iostatus)

    do
        read(nunit, "(A)", iostat=iostatus) char
        if(iostatus /= 0) exit
        size_y = size_y + 1
    enddo
    
    close(nunit)

    write(*, "(A, I0)") "Grid width : ", size_x
    write(*, "(A, I0)") "Grid height: ", size_y
end subroutine get_gridsize


subroutine read_tree_grid(filename, size_x, size_y, grid)
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    integer, intent(in) :: size_x
    integer, intent(in) :: size_y
    integer, dimension(size_x, size_y), intent(inout) :: grid
    !-------------------------------------------------------------------------!
    integer :: nunit = 20
    integer :: iostatus = 0
    integer :: i
    integer :: j
    character(len=size_x) :: line
    !=========================================================================!
    
    open(nunit, file=filename, action="read", status="old", delim="none")
    
    do i = 1, size_x
        read(nunit, *, iostat=iostatus) line
        if(iostatus /= 0) exit
        
        do j = 1, size_y
            read(line(j:j), "(I1)") grid(i, j)
        enddo
    enddo
    
    close(nunit)
end subroutine read_tree_grid


subroutine challenge_part_i(grid, size_x, size_y)
    implicit none
    !=========================================================================!
    integer, intent(in) :: size_x
    integer, intent(in) :: size_y
    integer, dimension(size_x, size_y), intent(in) :: grid
    !-------------------------------------------------------------------------!
    integer :: x
    integer :: y
    integer :: current_height
    integer :: num_vis_trees
    !=========================================================================!
    num_vis_trees = 0
    do x = 1, size_x
        do y = 1, size_y
            current_height = grid(x, y)
            if(all(grid(x, :y - 1) < current_height) .or. &
              &all(grid(x, y + 1:) < current_height) .or. &
              &all(grid(:x - 1, y) < current_height) .or. &
              &all(grid(x + 1:, y) < current_height)) then
                num_vis_trees = num_vis_trees + 1
                write(*, "(A)", advance="no") "^"
            else
                write(*, "(A)", advance="no") "#"
            endif
        enddo
        write(*, *) ""
    enddo
    
    write(*, "(A, I0)") "Part I: Number of visibile trees: ", num_vis_trees
end subroutine


subroutine challenge_part_ii(grid, size_x, size_y)
    implicit none
    !=========================================================================!
    integer, intent(in) :: size_x
    integer, intent(in) :: size_y
    integer, dimension(size_x, size_y), intent(in) :: grid
    !-------------------------------------------------------------------------!
    integer :: x
    integer :: y
    integer :: scenic_score
    integer :: highest_scenic_score = 0
    !-------------------------------------------------------------------------!
    interface
        integer function get_scenic_score(grid, size_x, size_y, pos_x, pos_y)
            integer, intent(in) :: size_x
            integer, intent(in) :: size_y
            integer, dimension(size_x, size_y), intent(in) :: grid
            integer, intent(in) :: pos_x
            integer, intent(in) :: pos_y
        end function get_scenic_score
    end interface
    !=========================================================================!
    
    do x = 1, size_x
        do y = 1, size_y
            scenic_score = get_scenic_score(grid, size_x, size_y, x, y)
            write(*, "(I7)", advance="no") scenic_score
            if(scenic_score > highest_scenic_score) then
                highest_scenic_score = scenic_score
            endif
        enddo
        write(*, *) ""
    enddo
    
    write(*, "(A, I0)") "Part II: Highest possible score: ", &
                       &highest_scenic_score
end subroutine


integer function get_scenic_score(grid, size_x, size_y, pos_x, pos_y)
    implicit none
    !=========================================================================!
    integer, intent(in) :: size_x
    integer, intent(in) :: size_y
    integer, dimension(size_x, size_y), intent(in) :: grid
    integer, intent(in) :: pos_x
    integer, intent(in) :: pos_y
    !-------------------------------------------------------------------------!
    integer :: score_up
    integer :: score_down
    integer :: score_left
    integer :: score_right
    integer :: x
    integer :: y
    integer :: height
    !=========================================================================!
    
    score_up = 0
    score_down = 0
    score_left = 0
    score_right = 0
    
    height = grid(pos_x, pos_y)
    
    if(pos_x == 1) then
        score_up = 0
    else
        do x = pos_x - 1, 1, -1
            score_up = score_up + 1
            if(grid(x, pos_y) >= height) exit
        enddo
    endif
    
    if(pos_x == size_x) then 
        score_down = 0
    else
        do x = pos_x + 1, size_x
            score_down = score_down + 1
            if(grid(x, pos_y) >= height) exit
        enddo
    endif
    
    if(pos_y == 1) then
        score_left = 0
    else
        do y = pos_y - 1, 1, -1
            score_left = score_left + 1
            if(grid(pos_x, y) >= height) exit
        enddo
    endif
    
    if(pos_y == size_y) then
        score_right = 0
    else
        do y = pos_y + 1, size_y
            score_right = score_right + 1
            if(grid(pos_x, y) >= height) exit
        enddo
    endif
    
    get_scenic_score = score_up * score_down * score_left * score_right
end function get_scenic_score