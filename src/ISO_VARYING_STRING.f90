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

    interface operator(//) ! Sec. 3.3.2
        module procedure concatStrings
        module procedure concatStringAndCharacter
        module procedure concatCharacterAndString
    end interface

    interface operator(==) ! Sec. 3.3.3
        module procedure string_EQ_String
        module procedure character_EQ_String
        module procedure string_EQ_Character
    end interface

    interface operator(/=) ! Sec. 3.3.3
        module procedure string_NE_String
        module procedure character_NE_String
        module procedure string_NE_Character
    end interface

    interface operator(<) ! Sec. 3.3.3
        module procedure string_LT_String
        module procedure character_LT_String
        module procedure string_LT_Character
    end interface

    interface operator(<=) ! Sec. 3.3.3
        module procedure string_LE_String
        module procedure character_LE_String
        module procedure string_LE_Character
    end interface

    interface operator(>) ! Sec. 3.3.3
        module procedure string_GT_String
        module procedure character_GT_String
        module procedure string_GT_Character
    end interface

    interface operator(>=) ! Sec. 3.3.3
        module procedure string_GE_String
        module procedure character_GE_String
        module procedure string_GE_Character
    end interface

    interface ADJUSTL ! Sec. 3.4.1
        module procedure stringADJUSTL
    end interface

    interface CHAR ! Sec. 3.4.3
        module procedure stringToChar
        module procedure stringToCharWithLength
    end interface

    public :: &
            assignment(=), &
            operator(//), &
            operator(==), &
            operator(/=), &
            operator(<), &
            operator(<=), &
            operator(>), &
            operator(>=), &
            ADJUSTL, &
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

    elemental function concatStrings(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        type(VARYING_STRING) :: concatenated

        concatenated = char(lhs) // char(rhs)
    end function concatStrings

    elemental function concatStringAndCharacter(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        type(VARYING_STRING) :: concatenated

        concatenated = char(lhs) // rhs
    end function concatStringAndCharacter

    elemental function concatCharacterAndString(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        type(VARYING_STRING) :: concatenated

        concatenated = lhs // char(rhs)
    end function concatCharacterAndString

    elemental function string_EQ_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) == char(rhs)
    end function string_EQ_String

    elemental function character_EQ_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = lhs == char(rhs)
    end function character_EQ_String

    elemental function string_EQ_Character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) == rhs
    end function string_EQ_Character

    elemental function string_NE_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) /= char(rhs)
    end function string_NE_String

    elemental function character_NE_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = lhs /= char(rhs)
    end function character_NE_String

    elemental function string_NE_Character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) /= rhs
    end function string_NE_Character

    elemental function string_LT_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) < char(rhs)
    end function string_LT_String

    elemental function character_LT_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = lhs < char(rhs)
    end function character_LT_String

    elemental function string_LT_Character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) < rhs
    end function string_LT_Character

    elemental function string_LE_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) <= char(rhs)
    end function string_LE_String

    elemental function character_LE_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = lhs <= char(rhs)
    end function character_LE_String

    elemental function string_LE_Character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) <= rhs
    end function string_LE_Character

    elemental function string_GT_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) > char(rhs)
    end function string_GT_String

    elemental function character_GT_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = lhs > char(rhs)
    end function character_GT_String

    elemental function string_GT_Character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) > rhs
    end function string_GT_Character

    elemental function string_GE_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) >= char(rhs)
    end function string_GE_String

    elemental function character_GE_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        equals = lhs >= char(rhs)
    end function character_GE_String

    elemental function string_GE_Character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) >= rhs
    end function string_GE_Character

    elemental function stringADJUSTL(string) result(adjusted)
        ! Sec. 3.4.1
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: adjusted

        adjusted = adjustl(char(string))
    end function stringADJUSTL

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
