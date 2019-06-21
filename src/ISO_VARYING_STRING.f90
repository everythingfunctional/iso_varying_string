module ISO_VARYING_STRING
    implicit none
    private

    type, public :: VARYING_STRING ! Sec. 3.2
        private
        character(len=1), allocatable :: characters(:)
    end type VARYING_STRING

    interface assignment(=) ! Sec. 3.3.1
        module procedure assignCharacterToString
        module procedure assignStringToCharacter
    end interface

    interface CHAR ! Sec. 3.4.3
        module procedure stringToChar
        module procedure stringToCharWithLength
    end interface

    public :: &
            assignment(=), &
            CHAR, &
            VAR_STR
contains
    elemental subroutine assignCharacterToString(lhs, rhs)
        ! Sec. 3.3.1
        type(VARYING_STRING), intent(out) :: lhs
        character(len=*), intent(in) :: rhs

        integer :: i
        integer :: length

        length = len(rhs)
        allocate(lhs%characters(length))
        do concurrent (i = 1 : length)
            lhs%characters(i) = rhs(i:i)
        end do
    end subroutine assignCharacterToString

    elemental subroutine assignStringToCharacter(lhs, rhs)
        ! Sec. 3.3.1
        character(len=*), intent(out) :: lhs
        type(VARYING_STRING), intent(in) :: rhs

        integer :: i
        integer :: length_input
        integer :: length_output

        length_output = len(lhs)
        if (allocated(rhs%characters)) then
            length_input = size(rhs%characters)
            do concurrent (i = 1 : min(length_input, length_output))
                lhs(i:i) = rhs%characters(i)
            end do
            if (length_input < length_output) then
                do concurrent (i = length_input+1 : length_output)
                    lhs(i:i) = " "
                end do
            end if
        else
            do concurrent (i = 1 : length_output)
                lhs(i:i) = " "
            end do
        end if
    end subroutine assignStringToCharacter

    pure function stringToChar(string) result(chars)
        ! Sec. 3.4.3
        type(VARYING_STRING), intent(in) :: string
        character(len=size(string%characters)) :: chars

        if (allocated(string%characters)) then
            chars = string
        end if
    end function stringToChar

    pure function stringToCharWithLength(string, length) result(chars)
        ! Sec. 3.4.3
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: length
        character(len=length) :: chars

        if (allocated(string%characters)) then
            chars = string
        end if
    end function stringToCharWithLength

    elemental function VAR_STR(char)
        ! Sec. 3.5.1
        character(len=*), intent(in) :: char
        type(VARYING_STRING) :: VAR_STR

        VAR_STR = char
    end function VAR_STR
end module ISO_VARYING_STRING
