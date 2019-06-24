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

    interface ADJUSTR ! Sec. 3.4.1
        module procedure stringADJUSTR
    end interface

    interface CHAR ! Sec. 3.4.3
        module procedure stringToChar
        module procedure stringToCharWithLength
    end interface

    interface IACHAR ! Sec. 3.4.4
        module procedure stringIACHAR
    end interface

    interface ICHAR ! Sec. 3.4.5
        module procedure stringICHAR
    end interface

    interface INDEX ! Sec. 3.4.6
        module procedure stringIndexString
        module procedure stringIndexCharacter
        module procedure CharacterIndexString
    end interface

    interface LEN ! Sec. 3.4.7
        module procedure lenString
    end interface

    interface LEN_TRIM ! Sec. 3.4.8
        module procedure lenTrimString
    end interface

    interface LGE ! Sec. 3.4.9
        module procedure string_LGE_String
        module procedure character_LGE_String
        module procedure string_LGE_Character
    end interface

    interface LGT ! Sec. 3.4.10
        module procedure string_LGT_String
        module procedure character_LGT_String
        module procedure string_LGT_Character
    end interface

    interface LLE ! Sec. 3.4.11
        module procedure string_LLE_String
        module procedure character_LLE_String
        module procedure string_LLE_Character
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
            ADJUSTR, &
            CHAR, &
            IACHAR, &
            ICHAR, &
            INDEX, &
            LEN, &
            LEN_TRIM, &
            LGE, &
            LGT, &
            LLE, &
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

    elemental function stringADJUSTR(string) result(adjusted)
        ! Sec. 3.4.2
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: adjusted

        adjusted = adjustr(char(string))
    end function stringADJUSTR

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

    elemental function stringIACHAR(c)
        ! Sec. 3.4.4
        type(VARYING_STRING), intent(in) :: c
        integer :: stringIACHAR

        stringIACHAR = iachar(char(c))
    end function stringIACHAR

    elemental function stringICHAR(c)
        ! Sec. 3.4.5
        type(VARYING_STRING), intent(in) :: c
        integer :: stringICHAR

        stringICHAR = ichar(char(c))
    end function stringICHAR

    elemental function stringIndexString(string, substring, back) result(position)
        ! Sec. 3.4.6
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        position = index(char(string), char(substring), back)
    end function stringIndexString

    elemental function stringIndexCharacter(string, substring, back) result(position)
        ! Sec. 3.4.6
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        position = index(char(string), substring, back)
    end function stringIndexCharacter

    elemental function characterIndexString(string, substring, back) result(position)
        ! Sec. 3.4.6
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        position = index(string, char(substring), back)
    end function characterIndexString

    elemental function lenString(string) result(length)
        ! Sec. 3.4.7
        type(VARYING_STRING), intent(in) :: string
        integer :: length

        length = len(char(string))
    end function lenString

    elemental function lenTrimString(string) result(length)
        ! Sec. 3.4.8
        type(VARYING_STRING), intent(in) :: string
        integer :: length

        length = len_trim(char(string))
    end function lenTrimString

    elemental function string_LGE_String(string_a, string_b) result(greater_than_or_equals)
        ! Sec 3.4.9
        type(VARYING_STRING), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: greater_than_or_equals

        greater_than_or_equals = lge(char(string_a), char(string_b))
    end function string_LGE_String

    elemental function character_LGE_String(string_a, string_b) result(greater_than_or_equals)
        ! Sec 3.4.9
        character(len=*), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: greater_than_or_equals

        greater_than_or_equals = lge(string_a, char(string_b))
    end function character_LGE_String

    elemental function string_LGE_Character(string_a, string_b) result(greater_than_or_equals)
        ! Sec 3.4.9
        type(VARYING_STRING), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: greater_than_or_equals

        greater_than_or_equals = lge(char(string_a), string_b)
    end function string_LGE_Character

    elemental function string_LGT_String(string_a, string_b) result(greater_than)
        ! Sec 3.4.10
        type(VARYING_STRING), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: greater_than

        greater_than = lgt(char(string_a), char(string_b))
    end function string_LGT_String

    elemental function character_LGT_String(string_a, string_b) result(greater_than)
        ! Sec 3.4.10
        character(len=*), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: greater_than

        greater_than = lgt(string_a, char(string_b))
    end function character_LGT_String

    elemental function string_LGT_Character(string_a, string_b) result(greater_than)
        ! Sec 3.4.10
        type(VARYING_STRING), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: greater_than

        greater_than = lgt(char(string_a), string_b)
    end function string_LGT_Character

    elemental function string_LLE_String(string_a, string_b) result(less_than_or_equals)
        ! Sec 3.4.11
        type(VARYING_STRING), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: less_than_or_equals

        less_than_or_equals = lle(char(string_a), char(string_b))
    end function string_LLE_String

    elemental function character_LLE_String(string_a, string_b) result(less_than_or_equals)
        ! Sec 3.4.11
        character(len=*), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: less_than_or_equals

        less_than_or_equals = lle(string_a, char(string_b))
    end function character_LLE_String

    elemental function string_LLE_Character(string_a, string_b) result(less_than_or_equals)
        ! Sec 3.4.11
        type(VARYING_STRING), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: less_than_or_equals

        less_than_or_equals = lle(char(string_a), string_b)
    end function string_LLE_Character

    elemental function VAR_STR(char)
        ! Sec. 3.5.1
        character(len=*), intent(in) :: char
        type(VARYING_STRING) :: VAR_STR

        VAR_STR = char
    end function VAR_STR
end module ISO_VARYING_STRING
