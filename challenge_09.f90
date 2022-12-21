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
    !=========================================================================!
    
    if(test) then
        call challenge_part_i("input_09_test.txt")
        call challenge_part_ii("input_09_test.txt")
    else
        call challenge_part_i("input_09.txt")
        call challenge_part_ii("input_09.txt")
    endif
    
end program main


subroutine challenge_part_i(filename)
    use point_class
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    !-------------------------------------------------------------------------!
    type(point) :: head = point(1, 1)
    type(point) :: tail = point(1, 1)
    type(point) :: dist = point(0, 0)
    character :: dir
    integer :: nunit = 20
    integer :: amount
    integer :: i, counter
    integer :: iostatus
    integer, dimension(:, :), allocatable :: vis_map
    !=========================================================================!
    
    open(nunit, file=filename, action="read", status="old")
    
    allocate(vis_map(-200:200, -200:200))
    vis_map(:, :) = 0
    counter = 0
    do
        read(nunit, "(A1, I4)", iostat=iostatus) dir, amount
        if(iostatus /= 0) exit
        
        steps: do i = 1, amount
            counter = counter + 1
            select case(dir)
                case("U")
                    head%y = head%y + 1
                case("D")
                    head%y = head%y - 1
                case("L")
                    head%x = head%x - 1
                case("R")
                    head%x = head%x + 1
            end select
            
            dist%x = abs(head%x - tail%x)
            dist%y = abs(head%y - tail%y)
            
            if(dist%x > 1 .or. dist%y > 1) then
                select case(dir)
                    case("U")
                        tail%x = head%x
                        tail%y = head%y - 1
                    case("D")
                        tail%x = head%x
                        tail%y = head%y + 1
                    case("L")
                        tail%x = head%x + 1
                        tail%y = head%y
                    case("R")
                        tail%x = head%x - 1
                        tail%y = head%y
                end select
            endif
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
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    !-------------------------------------------------------------------------!
    character :: dir
    integer :: nunit = 20
    integer :: iostatus
    integer :: amount
    integer, dimension(:, :), allocatable :: vis_map
    !=========================================================================!
    open(nunit, file=filename, action="read", status="old")
    
    allocate(vis_map(-200:200, -200:200))
    vis_map(:, :) = 0
    
    do
        read(nunit, "(A, I4)", iostat=iostatus) dir, amount
        if(iostatus /= 0) exit
        
        write(*, *) dir, amount
    enddo
    
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