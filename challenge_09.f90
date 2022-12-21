module point_class
    type point
        integer :: x
        integer :: y
    end type point
end module point_class


program main
    implicit none
    !=========================================================================!
    logical :: test = .true.
    !=========================================================================!
    
    if(test) then
        call challenge_part_i("input_09_test_p1.txt")
        call challenge_part_ii("input_09_test_p2.txt")
    else
        call challenge_part_i("input_09.txt")
        call challenge_part_ii("input_09.txt")
    endif
    
end program main


subroutine move_point(p, dir)
    use point_class
    implicit none
    !=========================================================================!
    type(point), intent(inout) :: p
    character, intent(in) :: dir
    !=========================================================================!
    
    select case(dir)
        case("U")
            p%y = p%y + 1
        case("D")
            p%y = p%y - 1
        case("L")
            p%x = p%x - 1
        case("R")
            p%x = p%x + 1
    end select
end subroutine move_point


subroutine follow_point(p_first, p_follow, dir, dir_new)
    use point_class
    implicit none
    !=========================================================================!
    type(point), intent(in) :: p_first
    type(point), intent(inout) :: p_follow
    character, intent(in) :: dir
    character, intent(out) :: dir_new
    !-------------------------------------------------------------------------!
    type(point) :: dist
    !=========================================================================!
    
    dist%x = p_first%x - p_follow%x
    dist%y = p_first%y - p_follow%y
    
    write(*, *) "Dist:", dist
    
    if(abs(dist%x) > 1 .or. abs(dist%y) > 1) then
        select case(dir)
            case("U")
                p_follow%x = p_first%x
                p_follow%y = p_first%y - 1
                select case(dist%x)
                    case(1)
                        dir_new = "R"
                    case(-1)
                        dir_new = "L"
                    case default
                        dir_new = dir
                end select
            case("D")
                p_follow%x = p_first%x
                p_follow%y = p_first%y + 1
                select case(dist%x)
                    case(1)
                        dir_new = "R"
                    case(-1)
                        dir_new = "L"
                    case default
                        dir_new = dir
                end select
            case("L")
                p_follow%x = p_first%x + 1
                p_follow%y = p_first%y
                select case(dist%y)
                    case(1)
                        dir_new = "U"
                    case(-1)
                        dir_new = "D"
                    case default
                        dir_new = dir
                end select
            case("R")
                p_follow%x = p_first%x - 1
                p_follow%y = p_first%y
                select case(dist%y)
                    case(1)
                        dir_new = "U"
                    case(-1)
                        dir_new = "D"
                    case default
                        dir_new = dir
                end select
        end select
    endif
end subroutine follow_point


subroutine alloc_vis_map(filename, vis_map)
    use point_class
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    integer, dimension(:, :), allocatable, intent(inout) :: vis_map
    !-------------------------------------------------------------------------!
    type(point) :: head = point(1, 1)
    character :: dir, dummy
    integer :: amount
    integer :: min_x = 1, max_x = 1
    integer :: min_y = 1, max_y = 1
    integer :: nunit = 20
    integer :: iostatus
    integer :: i
    !=========================================================================!
    
    open(nunit, file=filename, action="read", status="old")
    
    do
        read(nunit, "(A1, I4)", iostat=iostatus) dir, amount
        if(iostatus /= 0) exit
        
        steps: do i = 1, amount
            call move_point(head, dir)
            
            if(head%x > max_x) max_x = head%x
            if(head%x < min_x) min_x = head%x
            if(head%y > max_y) max_y = head%y
            if(head%y < min_y) min_y = head%y
        enddo steps
    enddo
    
    ! Safety frame:
    min_x = min_x - 1
    min_y = min_y - 1
    max_x = max_x + 1
    max_y = max_y + 1
    
    allocate(vis_map(min_x:max_x, min_y:max_y))
    vis_map(:, :) = 0
    write(*, *) "Vis map allocated to size", min_x, max_x, min_y, max_y
    close(nunit)
end subroutine alloc_vis_map


subroutine challenge_part_i(filename)
    use point_class
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    !-------------------------------------------------------------------------!
    type(point) :: head = point(1, 1)
    type(point) :: tail = point(1, 1)
    character :: dir
    character :: dummy
    integer :: nunit = 20
    integer :: amount
    integer :: i
    integer :: iostatus
    integer, dimension(:, :), allocatable :: vis_map
    !-------------------------------------------------------------------------!
    interface
        subroutine print_map(vis_map)
            integer, dimension(:, :), intent(in) :: vis_map
        end subroutine print_map
        
        subroutine alloc_vis_map(filename, vis_map)
            character(len=*), intent(in) :: filename
            integer, dimension(:, :), allocatable, intent(inout) :: vis_map
        end subroutine alloc_vis_map
    end interface
    !=========================================================================!
    
    
    
    ! allocate(vis_map(-200:200, -200:200))
    ! vis_map(:, :) = 0
    call alloc_vis_map(filename, vis_map)
    
    open(nunit, file=filename, action="read", status="old")
    do
        read(nunit, "(A1, I4)", iostat=iostatus) dir, amount
        if(iostatus /= 0) exit
        
        steps: do i = 1, amount
            call move_point(head, dir)
            call follow_point(head, tail, dir, dummy)
            
            !write(*, "(I0, A3, I5, I5)") counter, "T", tail%x, tail%y
            vis_map(tail%x, tail%y) = 1
        enddo steps
    enddo
    
    call print_map(vis_map)
    write(*, "(A, I0)") "Part I: Number of visited points: ", sum(vis_map)
    
    deallocate(vis_map)
    close(nunit)
end subroutine challenge_part_i


subroutine challenge_part_ii(filename)
    use point_class
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    !-------------------------------------------------------------------------!
    type(point) :: head = point(1, 1)
    type(point), dimension(:), allocatable :: rope
    character :: dir
    character :: next_dir
    integer :: nunit = 20
    integer :: i, j
    integer :: iostatus
    integer :: amount
    integer :: num_segments = 9
    integer, dimension(:, :), allocatable :: vis_map
    !-------------------------------------------------------------------------!
    interface
        subroutine print_map(vis_map)
            integer, dimension(:, :), intent(in) :: vis_map
        end subroutine print_map
        
        subroutine alloc_vis_map(filename, vis_map)
            character(len=*), intent(in) :: filename
            integer, dimension(:, :), allocatable, intent(inout) :: vis_map
        end subroutine alloc_vis_map
    end interface
    !=========================================================================!
    
    allocate(rope(num_segments))
    rope(:) = point(1, 1)
    call alloc_vis_map(filename, vis_map)
    
    open(nunit, file=filename, action="read", status="old")
    
    simulation: do
        read(nunit, "(A, I4)", iostat=iostatus) dir, amount
        if(iostatus /= 0) exit
        
        steps: do i = 1, amount
            call move_point(head, dir)
            call follow_point(head, rope(1), dir, next_dir)
            
            rope_follow: do j = 2, num_segments
                dir = next_dir
                call follow_point(rope(j - 1), rope(j), dir, next_dir)
            enddo rope_follow
            write(*, "(A, I5, I5)") "T", rope(:)
            vis_map(rope(num_segments)%x, rope(num_segments)%y) = 1
        enddo steps
    enddo simulation
    
    call print_map(vis_map)
    write(*, "(A, I0)") "Part II: Number of visited points: ", sum(vis_map)
    
    deallocate(rope)
    deallocate(vis_map)
    close(nunit)
end subroutine challenge_part_ii


subroutine print_map(vis_map)
    implicit none
    !=========================================================================!
    integer, dimension(:, :), intent(in) :: vis_map
    !-------------------------------------------------------------------------!
    integer :: i
    integer :: j
    !=========================================================================!
    
    do i = ubound(vis_map, 2), lbound(vis_map, 2), -1
        do j = lbound(vis_map, 1), ubound(vis_map, 1)
            if(vis_map(j, i) == 1) then
                write(*, "(A)", advance="no") "#"
            else
                write(*, "(A)", advance="no") "."
            endif
        enddo
        write(*, *)
    enddo
end subroutine print_map