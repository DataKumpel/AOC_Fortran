program main
    implicit none
    !=========================================================================!
    logical :: test = .false.
    !=========================================================================!
    
    if(test) then
        call challenge_part_i("input_10_test.txt")
    else
        call challenge_part_i("input_10.txt")
    endif
    
end program main


subroutine challenge_part_i(filename)
    implicit none
    !=========================================================================!
    character(len=*), intent(in) :: filename
    !-------------------------------------------------------------------------!
    character(len=20) :: line
    character(len=4) :: instr
    integer :: ivalue
    integer :: cycles = 1
    integer :: xregister = 1
    integer :: nunit = 20
    integer :: iostatus
    integer :: i
    integer :: signal_sum = 0
    integer, dimension(:), allocatable :: cycle_stack
    !=========================================================================!
    
    open(nunit, file=filename, action="read", status="old")
    
    allocate(cycle_stack(250))
    
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
    
    do i = 20, 250, 40
        write(*, *) i, cycle_stack(i), i * cycle_stack(i)
        signal_sum = signal_sum + i * cycle_stack(i)
    enddo
    
    write(*, *) "Signal strength sum: ", signal_sum
    
    deallocate(cycle_stack)
    close(nunit)
end subroutine challenge_part_i


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