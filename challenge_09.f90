module point_class
    type point
        integer :: x
        integer :: y
    end type point
end module point_class


program main
    implicit none
    !=========================================================================!
    logical :: test = .false.
    real :: t_start, t_end
    !=========================================================================!
    
    call cpu_time(t_start)
    if(test) then
        call challenge_part_i("input_09_test_p1.txt")
        call challenge_part_ii("input_09_test_p2.txt")
    else
        call challenge_part_i("input_09.txt")
        call challenge_part_ii("input_09.txt")
    endif
    call cpu_time(t_end)
    
    write(*, "(A, F10.8)") "Exec time: ", t_end - t_start, "s"
    
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


subroutine follow_point(head, tail)
    use point_class
    implicit none
    !=========================================================================!
    type(point), intent(in) :: head
    type(point), intent(inout) :: tail
    !-------------------------------------------------------------------------!
    type(point) :: dist
    !=========================================================================!
    
    dist%x = head%x - tail%x
    dist%y = head%y - tail%y
    
    if(abs(dist%x) > 1 .or. abs(dist%y) > 1) then
        if(abs(dist%x) > abs(dist%y)) then
            tail%x = tail%x + dist%x - dist%x / abs(dist%x)
            tail%y = head%y
        else if(abs(dist%x) < abs(dist%y)) then
            tail%x = head%x
            tail%y = tail%y + dist%y - dist%y / abs(dist%y)
        else
            tail%x = tail%x + dist%x - dist%x / abs(dist%x)
            tail%y = tail%y + dist%y - dist%y / abs(dist%y)
        endif
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
    character :: dir
    integer :: amount
    integer :: min_x = 1, max_x = 1
    integer :: min_y = 1, max_y = 1
    integer :: nunit = 20
    integer :: safety_offset = 5
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
    min_x = min_x - safety_offset
    min_y = min_y - safety_offset
    max_x = max_x + safety_offset
    max_y = max_y + safety_offset
    
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
    
    call alloc_vis_map(filename, vis_map)
    
    open(nunit, file=filename, action="read", status="old")
    do
        read(nunit, "(A1, I4)", iostat=iostatus) dir, amount
        if(iostatus /= 0) exit
        
        steps: do i = 1, amount
            call move_point(head, dir)
            call follow_point(head, tail)
            
            !write(*, "(I0, A3, I5, I5)") counter, "T", tail%x, tail%y
            vis_map(tail%x, tail%y) = 1
        enddo steps
    enddo
    
    ! call print_map(vis_map)
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
            call follow_point(head, rope(1))
            
            rope_follow: do j = 2, num_segments
                call follow_point(rope(j - 1), rope(j))
            enddo rope_follow
            
            vis_map(rope(num_segments)%x, rope(num_segments)%y) = 1
        enddo steps
    enddo simulation
    
    ! call print_map(vis_map)
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