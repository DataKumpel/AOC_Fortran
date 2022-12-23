module sprite_class
    type sprite
        integer :: lpos
        integer :: upos
    contains
        procedure :: move_to
    end type sprite

contains
    subroutine move_to(this, ipos)
        implicit none
        !=====================================================================!
        class(sprite) :: this
        integer :: ipos
        !=====================================================================!
        
        this%lpos = ipos
        this%upos = ipos + 2
    end subroutine move_to
end module sprite_class


program main
    implicit none
    !=========================================================================!
    logical :: test = .false.
    !=========================================================================!
    
    if(test) then
        call challenge_part_i("input_10_test.txt")
        call challenge_part_ii("input_10_test.txt")
    else
        call challenge_part_i("input_10.txt")
        call challenge_part_ii("input_10.txt")
    endif
    
end program main


subroutine challenge_part_i(filename)
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    !-------------------------------------------------------------------------!
    integer :: i
    integer :: signal_sum = 0
    integer, dimension(:), allocatable :: cycle_stack
    !-------------------------------------------------------------------------!
    interface
        subroutine fill_cycle_stack(filename, cycle_stack)
            character(len=*), intent(in) :: filename
            integer, dimension(:), allocatable, intent(inout) :: cycle_stack
        end subroutine fill_cycle_stack
    end interface
    !=========================================================================!
    
    call fill_cycle_stack(filename, cycle_stack)
    
    do i = 20, ubound(cycle_stack, 1), 40
        write(*, *) i, cycle_stack(i), i * cycle_stack(i)
        signal_sum = signal_sum + i * cycle_stack(i)
    enddo
    
    write(*, "(A, I0)") "Part I: Signal strength sum: ", signal_sum
    
    deallocate(cycle_stack)
end subroutine challenge_part_i


subroutine challenge_part_ii(filename)
    use sprite_class
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    !-------------------------------------------------------------------------!
    type(sprite) :: spr
    character, dimension(40, 6) :: crt_screen
    integer, dimension(:), allocatable :: cycle_stack
    integer :: cycles
    integer :: crt_row = 1
    integer :: crt_col = 1
    !-------------------------------------------------------------------------!
    interface
        subroutine fill_cycle_stack(filename, cycle_stack)
            character(len=*), intent(in) :: filename
            integer, dimension(:), allocatable, intent(inout) :: cycle_stack
        end subroutine fill_cycle_stack
    end interface
    !=========================================================================!
    
    call fill_cycle_stack(filename, cycle_stack)
    
    write(*, "(A)") "Part II:"
    do cycles = 1, ubound(cycle_stack, 1)
        call spr%move_to(cycle_stack(cycles))
        
        if(mod(cycles, 40) >= spr%lpos .and. &
          &mod(cycles, 40) <= spr%upos) then
            crt_screen(crt_col, crt_row) = "#"
        else
            crt_screen(crt_col, crt_row) = "."
        endif
        crt_col = crt_col + 1
        
        if(mod(cycles, 40) == 0) then
            write(*, *) crt_screen(:, crt_row)
            crt_row = crt_row + 1
            crt_col = 1
        endif
    enddo
    
    deallocate(cycle_stack)
    
end subroutine challenge_part_ii


subroutine fill_cycle_stack(filename, cycle_stack)
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    integer, dimension(:), allocatable, intent(inout) :: cycle_stack
    !-------------------------------------------------------------------------!
    character(len=20) :: line
    integer :: nunit = 20
    integer :: iostatus
    integer :: ivalue
    integer :: xregister
    integer :: cycles
    !=========================================================================!
    
    allocate(cycle_stack(240))
    
    cycles = 1
    xregister = 1
    cycle_stack(1) = 1
    
    open(nunit, file=filename, action="read", status="old")
    do
        read(nunit, "(A)", iostat=iostatus) line
        if(iostatus /= 0) exit
        
        if(line(1:4) == "addx") then
            read(line(5:), "(I6)") ivalue
            cycle_stack(cycles + 1) = xregister
            call addx(ivalue, xregister, cycles)
        else if(line(1:4) == "noop") then
            call noop(cycles)
        endif
        cycle_stack(cycles) = xregister
    enddo
    close(nunit)
end subroutine fill_cycle_stack


subroutine addx(v, xregister, cycles)
    implicit none
    !=========================================================================!
    integer, intent(in) :: v
    integer, intent(inout) :: xregister
    integer, intent(inout) :: cycles
    !=========================================================================!
    
    xregister = xregister + v
    cycles = cycles + 2
end subroutine addx


subroutine noop(cycles)
    !=========================================================================!
    integer, intent(inout) :: cycles
    !=========================================================================!
    
    cycles = cycles + 1
end subroutine noop