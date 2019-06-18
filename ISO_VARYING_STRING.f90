! This module conforms to the requirements of ISO/IEC 1539-2:2000(E)
! Comments provide the Section defining the requirement each portion satisfies
module ISO_VARYING_STRING ! Sec. 3.1
    implicit none
    private

    type, public :: VARYING_STRING ! Sec. 3.2
        private
        character(len=1), allocatable :: characters(:)
    end type VARYING_STRING

    interface assignment(=) ! Sec. 3.3.1
        module procedure assignCharacterToString
        module procedure assignStringToCharacter
        module procedure assignStringToString
    end interface

    interface operator(//) ! Sec. 3.3.2
        module procedure concatCharacterAndString
        module procedure concatStringAndCharacter
        module procedure concatStrings
    end interface

    interface operator(==) ! Sec. 3.3.3
        module procedure character_EQ_String
        module procedure string_EQ_Character
        module procedure string_EQ_String
    end interface

    interface operator(/=) ! Sec. 3.3.3
        module procedure character_NE_String
        module procedure string_NE_Character
        module procedure string_NE_String
    end interface

    interface operator(<) ! Sec. 3.3.3
        module procedure character_LT_String
        module procedure string_LT_Character
        module procedure string_LT_String
    end interface

    interface operator(<=) ! Sec. 3.3.3
        module procedure character_LE_String
        module procedure string_LE_Character
        module procedure string_LE_String
    end interface

    interface operator(>) ! Sec. 3.3.3
        module procedure character_GT_String
        module procedure string_GT_Character
        module procedure string_GT_String
    end interface

    interface operator(>=) ! Sec. 3.3.3
        module procedure character_GE_String
        module procedure string_GE_Character
        module procedure string_GE_String
    end interface

    interface ADJUSTL ! Sec. 3.4.1
        module procedure stringADJUSTL
    end interface

    interface ADJUSTR ! Sec. 3.4.2
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
        module procedure characterIndexString
        module procedure stringIndexCharacter
        module procedure stringIndexString
    end interface

    interface LEN ! Sec. 3.4.7
        module procedure lenString
    end interface

    interface LEN_TRIM ! Sec. 3.4.8
        module procedure lenTrimString
    end interface

    interface LLT ! Sec. 3.4.12
        module procedure character_LLT_String
        module procedure string_LLT_Character
        module procedure string_LLT_String
    end interface

    interface LLE ! Sec. 3.4.11
        module procedure character_LLE_String
        module procedure string_LLE_Character
        module procedure string_LLE_String
    end interface

    interface LGT ! Sec. 3.4.10
        module procedure character_LGT_String
        module procedure string_LGT_Character
        module procedure string_LGT_String
    end interface

    interface LGE ! Sec. 3.4.9
        module procedure character_LGE_String
        module procedure string_LGE_Character
        module procedure string_LGE_String
    end interface

    interface REPEAT ! Sec. 3.4.13
        module procedure stringRepeat
    end interface

    interface SCAN ! Sec. 3.4.14
        module procedure characterScanString
        module procedure stringScanCharacter
        module procedure stringScanString
    end interface

    interface TRIM ! Sec. 3.4.15
        module procedure trimString
    end interface

    interface VERIFY ! Sec. 3.4.16
        module procedure characterVerifyString
        module procedure stringVerifyCharacter
        module procedure stringVerifyString
    end interface

    interface GET ! Sec. 3.6.1
        module procedure getDefaultUnitToEndOfRecord
        module procedure getWithUnitToEndOfRecord
        module procedure getDefaultUnitToTerminatorString
        module procedure getWithUnitToTerminatorString
        module procedure getDefaultUnitToTerminatorCharacters
        module procedure getWithUnitToTerminatorCharacters
    end interface

    interface PUT ! Sec. 3.6.2
        module procedure putStringDefaultUnit
        module procedure putStringWithUnit
        module procedure putCharactersDefaultUnit
        module procedure putCharactersWithUnit
    end interface

    interface PUT_LINE ! Sec. 3.6.3
        module procedure putLineStringDefaultUnit
        module procedure putLineStringWithUnit
        module procedure putLineCharactersDefaultUnit
        module procedure putLineCharactersWithUnit
    end interface

    interface EXTRACT ! Sec. 3.7.1
        module procedure extractCharacters
        module procedure extractString
    end interface

    interface INSERT ! Sec. 3.7.2
        module procedure insertCharacterIntoCharacter
        module procedure insertCharacterIntoString
        module procedure insertStringIntoCharacter
        module procedure insertStringIntoString
    end interface

    interface REMOVE ! Sec. 3.7.3
        module procedure removeCharacters
        module procedure removeString
    end interface

    interface REPLACE ! Sec. 3.7.4
        module procedure replaceCharacterWithCharacterStart
        module procedure replaceCharacterWithStringStart
        module procedure replaceStringWithCharacterStart
        module procedure replaceStringWithStringStart
        module procedure replaceCharacterWithCharacterRange
        module procedure replaceCharacterWithStringRange
        module procedure replaceStringWithCharacterRange
        module procedure replaceStringWithStringRange
        module procedure replaceTargetCharacterWithCharacterInCharacter
        module procedure replaceTargetCharacterWithCharacterInString
        module procedure replaceTargetCharacterWithStringInCharacter
        module procedure replaceTargetCharacterWithStringInString
        module procedure replaceTargetStringWithCharacterInCharacter
        module procedure replaceTargetStringWithCharacterInString
        module procedure replaceTargetStringWithStringInCharacter
        module procedure replaceTargetStringWithStringInString
    end interface

    interface SPLIT ! Sec. 3.7.5
        module procedure splitCharacter
        module procedure splitString
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
            LLT, &
            LLE, &
            LGT, &
            LGE, &
            REPEAT, &
            SCAN, &
            TRIM, &
            VERIFY, &
            VAR_STR, &
            GET, &
            PUT, &
            PUT_LINE, &
            EXTRACT, &
            INSERT, &
            REMOVE, &
            REPLACE, &
            SPLIT
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
                do concurrent (i = length_input : length_output)
                    lhs(i:i) = " "
                end do
            end if
        else
            do concurrent (i = 1 : length_output)
                lhs(i:i) = " "
            end do
        end if
    end subroutine assignStringToCharacter

    elemental subroutine assignStringToString(lhs, rhs)
        ! Sec. 3.3.1
        type(VARYING_STRING), intent(out) :: lhs
        type(VARYING_STRING), intent(in) :: rhs

        if (allocated(rhs%characters)) allocate(lhs%characters, source = rhs%characters)
    end subroutine assignStringToString

    elemental function concatCharacterAndString(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        type(VARYING_STRING) :: concatenated

        integer :: i
        integer :: lhs_length
        integer :: rhs_length

        lhs_length = len(lhs)
        if (allocated(rhs%characters)) then
            rhs_length = size(rhs%characters)
            allocate(concatenated%characters(lhs_length + rhs_length))
            do concurrent (i = 1 : lhs_length)
                concatenated%characters(i) = lhs(i:i)
            end do
            concatenated%characters(lhs_length+1 : lhs_length+rhs_length) = &
                    rhs%characters(1:rhs_length)
        else
            allocate(concatenated%characters(lhs_length))
            do concurrent (i = 1 : lhs_length)
                concatenated%characters(i) = lhs(i:i)
            end do
        end if
    end function concatCharacterAndString

    elemental function concatStringAndCharacter(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        type(VARYING_STRING) :: concatenated

        integer :: i
        integer :: lhs_length
        integer :: rhs_length

        rhs_length = len(rhs)
        if (allocated(lhs%characters)) then
            lhs_length = size(lhs%characters)
            allocate(concatenated%characters(lhs_length + rhs_length))
            concatenated%characters(1:lhs_length) = lhs%characters(1:lhs_length)
            do concurrent (i = 1 : rhs_length)
                concatenated%characters(i + lhs_length) = rhs(i:i)
            end do
        else
            allocate(concatenated%characters(rhs_length))
            do concurrent (i = 1 : rhs_length)
                concatenated%characters(i) = rhs(i:i)
            end do
        end if
    end function concatStringAndCharacter

    elemental function concatStrings(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        type(VARYING_STRING) :: concatenated

        integer :: lhs_length
        integer :: rhs_length

        if (allocated(lhs%characters)) then
            if (allocated(rhs%characters)) then
                lhs_length = size(lhs%characters)
                rhs_length = size(rhs%characters)
                allocate(concatenated%characters(lhs_length + rhs_length))
                concatenated%characters(1:lhs_length) = lhs%characters(1:lhs_length)
                concatenated%characters(lhs_length+1 : lhs_length+rhs_length) = &
                        rhs%characters(1:rhs_length)
            else
                allocate(concatenated%characters, source = lhs%characters)
            end if
        else
            if (allocated(rhs%characters)) then
                allocate(concatenated%characters, source = rhs%characters)
            end if
        end if
    end function concatStrings

    elemental function character_EQ_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        character(len=size(rhs%characters)) :: temp_string

        if (allocated(rhs%characters)) then
            temp_string = rhs
            equals = lhs == temp_string
        else
            equals = lhs == ""
        end if
    end function character_EQ_String

    elemental function string_EQ_Character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            equals = temp_string == rhs
        else
            equals = "" == rhs
        end if
    end function string_EQ_Character

    elemental function string_EQ_String(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: equals

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            equals = temp_string == rhs
        else
            equals = "" == rhs
        end if
    end function string_EQ_String

    elemental function character_NE_String(lhs, rhs) result(not_equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: not_equals

        character(len=size(rhs%characters)) :: temp_string

        if (allocated(rhs%characters)) then
            temp_string = rhs
            not_equals = lhs /= temp_string
        else
            not_equals = lhs /= ""
        end if
    end function character_NE_String

    elemental function string_NE_Character(lhs, rhs) result(not_equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: not_equals

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            not_equals = temp_string /= rhs
        else
            not_equals = "" /= rhs
        end if
    end function string_NE_Character

    elemental function string_NE_String(lhs, rhs) result(not_equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: not_equals

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            not_equals = temp_string /= rhs
        else
            not_equals = "" /= rhs
        end if
    end function string_NE_String

    elemental function character_LT_String(lhs, rhs) result(less_than)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: less_than

        character(len=size(rhs%characters)) :: temp_string

        if (allocated(rhs%characters)) then
            temp_string = rhs
            less_than = lhs < temp_string
        else
            less_than = lhs < ""
        end if
    end function character_LT_String

    elemental function string_LT_Character(lhs, rhs) result(less_than)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: less_than

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            less_than = temp_string < rhs
        else
            less_than = "" < rhs
        end if
    end function string_LT_Character

    elemental function string_LT_String(lhs, rhs) result(less_than)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: less_than

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            less_than = temp_string < rhs
        else
            less_than = "" < rhs
        end if
    end function string_LT_String

    elemental function character_LE_String(lhs, rhs) result(less_than_or_equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: less_than_or_equals

        character(len=size(rhs%characters)) :: temp_string

        if (allocated(rhs%characters)) then
            temp_string = rhs
            less_than_or_equals = lhs <= temp_string
        else
            less_than_or_equals = lhs <= ""
        end if
    end function character_LE_String

    elemental function string_LE_Character(lhs, rhs) result(less_than_or_equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: less_than_or_equals

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            less_than_or_equals = temp_string <= rhs
        else
            less_than_or_equals = "" <= rhs
        end if
    end function string_LE_Character

    elemental function string_LE_String(lhs, rhs) result(less_than_or_equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: less_than_or_equals

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            less_than_or_equals = temp_string <= rhs
        else
            less_than_or_equals = "" <= rhs
        end if
    end function string_LE_String

    elemental function character_GT_String(lhs, rhs) result(greater_than)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: greater_than

        character(len=size(rhs%characters)) :: temp_string

        if (allocated(rhs%characters)) then
            temp_string = rhs
            greater_than = lhs > temp_string
        else
            greater_than = lhs > ""
        end if
    end function character_GT_String

    elemental function string_GT_Character(lhs, rhs) result(greater_than)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: greater_than

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            greater_than = temp_string > rhs
        else
            greater_than = "" > rhs
        end if
    end function string_GT_Character

    elemental function string_GT_String(lhs, rhs) result(greater_than)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: greater_than

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            greater_than = temp_string > rhs
        else
            greater_than = "" > rhs
        end if
    end function string_GT_String

    elemental function character_GE_String(lhs, rhs) result(greater_than_or_equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: greater_than_or_equals

        character(len=size(rhs%characters)) :: temp_string

        if (allocated(rhs%characters)) then
            temp_string = rhs
            greater_than_or_equals = lhs >= temp_string
        else
            greater_than_or_equals = lhs >= ""
        end if
    end function character_GE_String

    elemental function string_GE_Character(lhs, rhs) result(greater_than_or_equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: greater_than_or_equals

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            greater_than_or_equals = temp_string >= rhs
        else
            greater_than_or_equals = "" >= rhs
        end if
    end function string_GE_Character

    elemental function string_GE_String(lhs, rhs) result(greater_than_or_equals)
        ! Sec. 3.3.3
        type(VARYING_STRING), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        logical :: greater_than_or_equals

        character(len=size(lhs%characters)) :: temp_string

        if (allocated(lhs%characters)) then
            temp_string = lhs
            greater_than_or_equals = temp_string >= rhs
        else
            greater_than_or_equals = "" >= rhs
        end if
    end function string_GE_String

    elemental function stringADJUSTL(string) result(adjusted)
        ! Sec. 3.4.1
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: adjusted

        character(len=size(string%characters)) :: temp_adjusted
        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            temp_adjusted = ADJUSTL(temp_string)
            adjusted = temp_adjusted
        end if
    end function stringADJUSTL

    elemental function stringADJUSTR(string) result(adjusted)
        ! Sec. 3.4.2
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: adjusted

        character(len=size(string%characters)) :: temp_adjusted
        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            temp_adjusted = ADJUSTR(temp_string)
            adjusted = temp_adjusted
        end if
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

        stringIACHAR = IACHAR(c%characters(1))
    end function stringIACHAR

    elemental function stringICHAR(c)
        ! Sec. 3.4.5
        type(VARYING_STRING), intent(in) :: c
        integer :: stringICHAR

        stringICHAR = ICHAR(c%characters(1))
    end function stringICHAR

    elemental function characterIndexString(string, substring, back) result(position)
        ! Sec. 3.4.6
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(substring%characters)) :: temp_substring

        if (allocated(substring%characters)) then
            temp_substring = substring
            position = INDEX(string, temp_substring, back)
        else
            position = INDEX(string, "", back)
        end if
    end function characterIndexString

    elemental function stringIndexCharacter(string, substring, back) result(position)
        ! Sec. 3.4.6
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            position = INDEX(temp_string, substring, back)
        else
            position = INDEX("", substring, back)
        end if
    end function stringIndexCharacter

    elemental function stringIndexString(string, substring, back) result(position)
        ! Sec. 3.4.6
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            position = INDEX(temp_string, substring, back)
        else
            position = INDEX("", substring, back)
        end if
    end function stringIndexString

    elemental function lenString(string) result(length)
        ! Sec. 3.4.7
        type(VARYING_STRING), intent(in) :: string
        integer :: length

        if (allocated(string%characters)) then
            length = size(string%characters)
        else
            length = 0
        end if
    end function lenString

    elemental function lenTrimString(string) result(length)
        ! Sec. 3.4.8
        type(VARYING_STRING), intent(in) :: string
        integer :: length

        length = len(trim(string))
    end function lenTrimString

    elemental function character_LLT_String(string_a, string_b) result(less_than)
        ! Sec. 3.4.12
        character(len=*), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: less_than

        character(len=size(string_b%characters)) :: temp_string_b

        if (allocated(string_b%characters)) then
            temp_string_b = string_b
            less_than = LLT(string_a, temp_string_b)
        else
            less_than = LLT(string_a, "")
        end if
    end function character_LLT_String

    elemental function string_LLT_Character(string_a, string_b) result(less_than)
        ! Sec 3.4.12
        type(VARYING_STRING), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: less_than

        character(len=size(string_a%characters)) :: temp_string_a

        if (allocated(string_a%characters)) then
            temp_string_a = string_a
            less_than = LLT(temp_string_a, string_b)
        else
            less_than = LLT("", string_b)
        end if
    end function string_LLT_Character

    elemental function string_LLT_String(string_a, string_b) result(less_than)
        ! Sec 3.4.12
        type(VARYING_STRING), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: less_than

        character(len=size(string_a%characters)) :: temp_string_a

        if (allocated(string_a%characters)) then
            temp_string_a = string_a
            less_than = LLT(temp_string_a, string_b)
        else
            less_than = LLT("", string_b)
        end if
    end function string_LLT_String

    elemental function character_LLE_String(string_a, string_b) result(less_than_or_equals)
        ! Sec. 3.4.11
        character(len=*), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: less_than_or_equals

        character(len=size(string_b%characters)) :: temp_string_b

        if (allocated(string_b%characters)) then
            temp_string_b = string_b
            less_than_or_equals = LLE(string_a, temp_string_b)
        else
            less_than_or_equals = LLE(string_a, "")
        end if
    end function character_LLE_String

    elemental function string_LLE_Character(string_a, string_b) result(less_than_or_equals)
        ! Sec 3.4.11
        type(VARYING_STRING), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: less_than_or_equals

        character(len=size(string_a%characters)) :: temp_string_a

        if (allocated(string_a%characters)) then
            temp_string_a = string_a
            less_than_or_equals = LLE(temp_string_a, string_b)
        else
            less_than_or_equals = LLE("", string_b)
        end if
    end function string_LLE_Character

    elemental function string_LLE_String(string_a, string_b) result(less_than_or_equals)
        ! Sec 3.4.11
        type(VARYING_STRING), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: less_than_or_equals

        character(len=size(string_a%characters)) :: temp_string_a

        if (allocated(string_a%characters)) then
            temp_string_a = string_a
            less_than_or_equals = LLE(temp_string_a, string_b)
        else
            less_than_or_equals = LLE("", string_b)
        end if
    end function string_LLE_String

    elemental function character_LGT_String(string_a, string_b) result(greater_than)
        ! Sec. 3.4.10
        character(len=*), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: greater_than

        character(len=size(string_b%characters)) :: temp_string_b

        if (allocated(string_b%characters)) then
            temp_string_b = string_b
            greater_than = LGT(string_a, temp_string_b)
        else
            greater_than = LGT(string_a, "")
        end if
    end function character_LGT_String

    elemental function string_LGT_Character(string_a, string_b) result(greater_than)
        ! Sec 3.4.10
        type(VARYING_STRING), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: greater_than

        character(len=size(string_a%characters)) :: temp_string_a

        if (allocated(string_a%characters)) then
            temp_string_a = string_a
            greater_than = LGT(temp_string_a, string_b)
        else
            greater_than = LGT("", string_b)
        end if
    end function string_LGT_Character

    elemental function string_LGT_String(string_a, string_b) result(greater_than)
        ! Sec 3.4.10
        type(VARYING_STRING), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: greater_than

        character(len=size(string_a%characters)) :: temp_string_a

        if (allocated(string_a%characters)) then
            temp_string_a = string_a
            greater_than = LGT(temp_string_a, string_b)
        else
            greater_than = LGT("", string_b)
        end if
    end function string_LGT_String

    elemental function character_LGE_String(string_a, string_b) result(greater_than_or_equals)
        ! Sec. 3.4.9
        character(len=*), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: greater_than_or_equals

        character(len=size(string_b%characters)) :: temp_string_b

        if (allocated(string_b%characters)) then
            temp_string_b = string_b
            greater_than_or_equals = LGE(string_a, temp_string_b)
        else
            greater_than_or_equals = LGE(string_a, "")
        end if
    end function character_LGE_String

    elemental function string_LGE_Character(string_a, string_b) result(greater_than_or_equals)
        ! Sec 3.4.9
        type(VARYING_STRING), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: greater_than_or_equals

        character(len=size(string_a%characters)) :: temp_string_a

        if (allocated(string_a%characters)) then
            temp_string_a = string_a
            greater_than_or_equals = LGE(temp_string_a, string_b)
        else
            greater_than_or_equals = LGE("", string_b)
        end if
    end function string_LGE_Character

    elemental function string_LGE_String(string_a, string_b) result(greater_than_or_equals)
        ! Sec 3.4.9
        type(VARYING_STRING), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: greater_than_or_equals

        character(len=size(string_a%characters)) :: temp_string_a

        if (allocated(string_a%characters)) then
            temp_string_a = string_a
            greater_than_or_equals = LGE(temp_string_a, string_b)
        else
            greater_than_or_equals = LGE("", string_b)
        end if
    end function string_LGE_String

    elemental function stringRepeat(string, ncopies) result(repeated)
        ! Sec. 3.4.13
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: ncopies
        type(VARYING_STRING) :: repeated

        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            repeated = REPEAT(temp_string, ncopies)
        else
            repeated = REPEAT("", ncopies)
        end if
    end function stringRepeat

    elemental function characterScanString(string, set, back) result(position)
        ! Sec. 3.4.14
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(set%characters)) :: temp_set

        if (allocated(set%characters)) then
            temp_set = set
            position = SCAN(string, temp_set, back)
        else
            position = SCAN(string, "", back)
        end if
    end function characterScanString

    elemental function stringScanCharacter(string, set, back) result(position)
        ! Sec. 3.4.14
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            position = SCAN(temp_string, set, back)
        else
            position = SCAN("", set, back)
        end if
    end function stringScanCharacter

    elemental function stringScanString(string, set, back) result(position)
        ! Sec. 3.4.14
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            position = SCAN(temp_string, set, back)
        else
            position = SCAN("", set, back)
        end if
    end function stringScanString

    elemental function trimString(string) result(trimmed)
        ! Sec. 3.4.15
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: trimmed

        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            trimmed = TRIM(temp_string)
        end if
    end function trimString

    elemental function characterVerifyString(string, set, back) result(position)
        ! Sec. 3.5.16
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(set%characters)) :: temp_set

        if (allocated(set%characters)) then
            temp_set = set
            position = VERIFY(string, temp_set, back)
        else
            position = VERIFY(string, "", back)
        end if
    end function characterVerifyString

    elemental function stringVerifyCharacter(string, set, back) result(position)
        ! Sec. 3.5.16
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            position = VERIFY(temp_string, set, back)
        else
            position = VERIFY("", set, back)
        end if
    end function stringVerifyCharacter

    elemental function stringVerifyString(string, set, back) result(position)
        ! Sec. 3.5.16
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        character(len=size(string%characters)) :: temp_string

        if (allocated(string%characters)) then
            temp_string = string
            position = VERIFY(temp_string, set, back)
        else
            position = VERIFY("", set, back)
        end if
    end function stringVerifyString

    elemental function VAR_STR(char)
        ! Sec. 3.5.1
        character(len=*), intent(in) :: char
        type(VARYING_STRING) :: VAR_STR

        VAR_STR = char
    end function VAR_STR

    subroutine getDefaultUnitToEndOfRecord(string, maxlen, iostat)
        ! Sec. 3.6.1
        type(VARYING_STRING), intent(out) :: string
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        integer, parameter :: BUFFER_SIZE = 100
        character(len=BUFFER_SIZE) :: buffer
        integer :: next_read_length
        integer :: num_read
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                next_read_length = min(BUFFER_SIZE, num_to_read)
                read(*, fmt='(A)', advance='NO', eor=9999, size=num_read, iostat=iostat) buffer(1:next_read_length)
                if (iostat /= 0) return
                string = string // buffer(1:next_read_length)
                num_to_read = num_to_read - next_read_length
            end do
        else
            do
                if (num_to_read <= 0) exit
                next_read_length = min(BUFFER_SIZE, num_to_read)
                read(*, fmt='(A)', advance='NO', eor=9999, size=num_read) buffer(1:next_read_length)
                string = string // buffer(1:next_read_length)
                num_to_read = num_to_read - next_read_length
            end do
        end if
        9999 string = string // buffer(1:num_read)
    end subroutine getDefaultUnitToEndOfRecord

    subroutine getWithUnitToEndOfRecord(unit, string, maxlen, iostat)
        ! Sec. 3.6.1
        integer, intent(in) :: unit
        type(VARYING_STRING), intent(out) :: string
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        integer, parameter :: BUFFER_SIZE = 100
        character(len=BUFFER_SIZE) :: buffer
        integer :: next_read_length
        integer :: num_read
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                next_read_length = min(BUFFER_SIZE, num_to_read)
                read(unit, fmt='(A)', advance='NO', eor=9999, size=num_read, iostat=iostat) buffer(1:next_read_length)
                if (iostat /= 0) return
                string = string // buffer(1:next_read_length)
                num_to_read = num_to_read - next_read_length
            end do
        else
            do
                if (num_to_read <= 0) exit
                next_read_length = min(BUFFER_SIZE, num_to_read)
                read(unit, fmt='(A)', advance='NO', eor=9999, size=num_read) buffer(1:next_read_length)
                string = string // buffer(1:next_read_length)
                num_to_read = num_to_read - next_read_length
            end do
        end if
        9999 string = string // buffer(1:num_read)
    end subroutine getWithUnitToEndOfRecord

    subroutine getDefaultUnitToTerminatorString(string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        type(VARYING_STRING), intent(out) :: string
        type(VARYING_STRING), intent(in) :: set ! possible terminator characters
        type(VARYING_STRING), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        character(len=1) :: buffer
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                read(*, fmt='(A)', advance='NO', eor=9999, iostat=iostat) buffer
                if (iostat /= 0) return
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        else
            do
                if (num_to_read <= 0) exit
                read(*, fmt='(A)', advance='NO', eor=9999) buffer
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        end if
        9999 continue
    end subroutine getDefaultUnitToTerminatorString

    subroutine getWithUnitToTerminatorString(unit, string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        integer, intent(in) :: unit
        type(VARYING_STRING), intent(out) :: string
        type(VARYING_STRING), intent(in) :: set ! possible terminator characters
        type(VARYING_STRING), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        character(len=1) :: buffer
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                read(unit, fmt='(A)', advance='NO', eor=9999, iostat=iostat) buffer
                if (iostat /= 0) return
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        else
            do
                if (num_to_read <= 0) exit
                read(unit, fmt='(A)', advance='NO', eor=9999) buffer
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        end if
        9999 continue
    end subroutine getWithUnitToTerminatorString

    subroutine getDefaultUnitToTerminatorCharacters(string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        type(VARYING_STRING), intent(out) :: string
        character(len=*), intent(in) :: set ! possible terminator characters
        type(VARYING_STRING), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        character(len=1) :: buffer
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                read(*, fmt='(A)', advance='NO', eor=9999, iostat=iostat) buffer
                if (iostat /= 0) return
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        else
            do
                if (num_to_read <= 0) exit
                read(*, fmt='(A)', advance='NO', eor=9999) buffer
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        end if
        9999 continue
    end subroutine getDefaultUnitToTerminatorCharacters

    subroutine getWithUnitToTerminatorCharacters(unit, string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        integer, intent(in) :: unit
        type(VARYING_STRING), intent(out) :: string
        character(len=*), intent(in) :: set ! possible terminator characters
        type(VARYING_STRING), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        character(len=1) :: buffer
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                read(unit, fmt='(A)', advance='NO', eor=9999, iostat=iostat) buffer
                if (iostat /= 0) return
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        else
            do
                if (num_to_read <= 0) exit
                read(unit, fmt='(A)', advance='NO', eor=9999) buffer
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        end if
        9999 continue
    end subroutine getWithUnitToTerminatorCharacters

    subroutine putStringDefaultUnit(string, iostat)
        ! Sec. 3.6.2
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(*, fmt='(A)', advance='NO', iostat=iostat) char(string)
        else
            write(*, fmt='(A)', advance='NO') char(string)
        end if
    end subroutine putStringDefaultUnit

    subroutine putStringWithUnit(unit, string, iostat)
        ! Sec. 3.6.2
        integer, intent(in) :: unit
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(unit, fmt='(A)', advance='NO', iostat=iostat) char(string)
        else
            write(unit, fmt='(A)', advance='NO') char(string)
        end if
    end subroutine putStringWithUnit

    subroutine putCharactersDefaultUnit(string, iostat)
        ! Sec. 3.6.2
        character(len=*), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(*, fmt='(A)', advance='NO', iostat=iostat) string
        else
            write(*, fmt='(A)', advance='NO') string
        end if
    end subroutine putCharactersDefaultUnit

    subroutine putCharactersWithUnit(unit, string, iostat)
        ! Sec. 3.6.2
        integer, intent(in) :: unit
        character(len=*), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(unit, fmt='(A)', advance='NO', iostat=iostat) string
        else
            write(unit, fmt='(A)', advance='NO') string
        end if
    end subroutine putCharactersWithUnit

    subroutine putLineStringDefaultUnit(string, iostat)
        ! Sec. 3.6.3
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(*, fmt='(A,/)', advance='NO', iostat=iostat) char(string)
        else
            write(*, fmt='(A,/)', advance='NO') char(string)
        end if
    end subroutine putLineStringDefaultUnit

    subroutine putLineStringWithUnit(unit, string, iostat)
        ! Sec. 3.6.3
        integer, intent(in) :: unit
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(unit, fmt='(A,/)', advance='NO', iostat=iostat) char(string)
        else
            write(unit, fmt='(A,/)', advance='NO') char(string)
        end if
    end subroutine putLineStringWithUnit

    subroutine putLineCharactersDefaultUnit(string, iostat)
        ! Sec. 3.6.3
        character(len=*), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(*, fmt='(A,/)', advance='NO', iostat=iostat) string
        else
            write(*, fmt='(A,/)', advance='NO') string
        end if
    end subroutine putLineCharactersDefaultUnit

    subroutine putLineCharactersWithUnit(unit, string, iostat)
        ! Sec. 3.6.3
        integer, intent(in) :: unit
        character(len=*), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(unit, fmt='(A,/)', advance='NO', iostat=iostat) string
        else
            write(unit, fmt='(A,/)', advance='NO') string
        end if
    end subroutine putLineCharactersWithUnit

    elemental function extractCharacters(string, start, finish) result(extracted)
        ! Sec. 3.7.1
        character(len=*), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(VARYING_STRING) :: extracted

        integer :: start_
        integer :: finish_

        if (present(start)) then
            start_ = max(1, start)
        else
            start_ = 1
        end if
        if (present(finish)) then
            finish_ = min(len(string),finish)
        else
            finish_ = len(string)
        end if
        if (start_ < finish_) then
            extracted = string(start_:finish_)
        end if
    end function extractCharacters

    elemental function extractString(string, start, finish) result(extracted)
        ! Sec. 3.7.1
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(VARYING_STRING) :: extracted

        extracted = extract(char(string), start, finish)
    end function extractString

    elemental function insertCharacterIntoCharacter(string, start, substring) result(combined)
        ! Sec. 3.7.2
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: combined

        combined = insert(var_str(string), start, var_str(substring))
    end function insertCharacterIntoCharacter

    elemental function insertCharacterIntoString(string, start, substring) result(combined)
        ! Sec. 3.7.2
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: combined

        combined = insert(string, start, var_str(substring))
    end function insertCharacterIntoString

    elemental function insertStringIntoCharacter(string, start, substring) result(combined)
        ! Sec. 3.7.2
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: combined

        combined = insert(var_str(string), start, substring)
    end function insertStringIntoCharacter

    elemental function insertStringIntoString(string, start, substring) result(combined)
        ! Sec. 3.7.2
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: combined

        integer :: length_string
        integer :: length_substring

        length_string = len(string)
        length_substring = len(substring)
        allocate(combined%characters(length_string + length_substring))
        if (start <= 1) then
            combined%characters(1:length_substring) = substring%characters(:)
            combined%characters(length_substring+1:) = string%characters(:)
        else if (start > length_string) then
            combined%characters(1:length_string) = string%characters(:)
            combined%characters(length_string+1:) = substring%characters(:)
        else
            combined%characters(1:start-1) = string%characters(1:start-1)
            combined%characters(start:start+length_substring) = substring%characters(:)
            combined%characters(start+length_substring+1:) = string%characters(start:)
        end if
    end function insertStringIntoString

    elemental function removeCharacters(string, start, finish) result(removed)
        ! Sec. 3.7.3
        character(len=*), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(VARYING_STRING) :: removed

        removed = remove(var_str(string), start, finish)
    end function removeCharacters

    elemental function removeString(string, start, finish) result(removed)
        ! Sec. 3.7.3
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(VARYING_STRING) :: removed

        integer :: start_
        integer :: finish_
        integer :: string_length

        string_length = len(string)
        if (present(start)) then
            start_ = max(1, start)
        else
            start_ = 1
        end if
        if (present(finish)) then
            finish_ = min(string_length, finish)
        else
            finish_ = string_length
        end if

        if (finish_ < start_) then
            removed = string
        else
            allocate(removed%characters(string_length - (finish_ - start_) - 1))
            removed%characters(1:start_ - 1) = string%characters(1:start_ - 1)
            removed%characters(start_:) = string%characters(finish_ + 1:)
        end if
    end function removeString

    elemental function replaceCharacterWithCharacterStart(string, start, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        integer :: start_
        type(VARYING_STRING) :: beginning
        type(VARYING_STRING) :: ending

        start_ = max(1, start)
        if (start_ <= 1) then
            beginning = ""
        else
            beginning = string(1:min(start_, len(string)))
        end if

        if (start_ + len(substring) >= len(string)) then
            ending = ""
        else
            ending = string(start_ + len(substring):)
        end if

        replaced = beginning // substring // ending
    end function replaceCharacterWithCharacterStart

    elemental function replaceCharacterWithStringStart(string, start, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(string, start, char(substring))
    end function replaceCharacterWithStringStart

    elemental function replaceStringWithCharacterStart(string, start, substring) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), start, substring)
    end function replaceStringWithCharacterStart

    elemental function replaceStringWithStringStart(string, start, substring) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), start, char(substring))
    end function replaceStringWithStringStart

    elemental function replaceCharacterWithCharacterRange(string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        integer :: start_
        integer :: finish_
        type(VARYING_STRING) :: beginning
        type(VARYING_STRING) :: ending

        start_ = max(1, start)
        finish_ = min(len(string), finish)

        if (start_ <= 1) then
            beginning = ""
        else
            beginning = string(1:min(start_, len(string)))
        end if

        if (start_ > len(string) .or. finish_ >= len(string)) then
            ending = ""
        else
            ending = string(max(start_, finish_):)
        end if

        replaced = beginning // substring // ending
    end function replaceCharacterWithCharacterRange

    elemental function replaceCharacterWithStringRange(string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(string, start, finish, char(substring))
    end function replaceCharacterWithStringRange

    elemental function replaceStringWithCharacterRange(string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), start, finish, substring)
    end function replaceStringWithCharacterRange

    elemental function replaceStringWithStringRange(string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), start, finish, char(substring))
    end function replaceStringWithStringRange

    elemental function replaceTargetCharacterWithCharacterInCharacter(string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        integer :: target_start

        target_start = index(string, target, back)
        if (present(every)) then
            if (every) then
                replaced = string
                do while (target_start /= 0)
                    replaced = replace(replaced, target_start, target_start + len(target), substring)
                    target_start = index(replaced, target, back)
                end do
            else
                if (target_start /= 0) then
                    replaced = replace(string, target_start, target_start + len(target), substring)
                else
                    replaced = string
                end if
            end if
        else
            if (target_start /= 0) then
                replaced = replace(string, target_start, target_start + len(target), substring)
            else
                replaced = string
            end if
        end if
    end function replaceTargetCharacterWithCharacterInCharacter

    elemental function replaceTargetCharacterWithCharacterInString(string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), target, substring, every, back)
    end function replaceTargetCharacterWithCharacterInString

    elemental function replaceTargetCharacterWithStringInCharacter(string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: target
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(string, target, char(substring), every, back)
    end function replaceTargetCharacterWithStringInCharacter

    elemental function replaceTargetCharacterWithStringInString(string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: target
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), target, char(substring), every, back)
    end function replaceTargetCharacterWithStringInString

    elemental function replaceTargetStringWithCharacterInCharacter(string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(string, char(target), substring, every, back)
    end function replaceTargetStringWithCharacterInCharacter

    elemental function replaceTargetStringWithCharacterInString(string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), char(target), substring, every, back)
    end function replaceTargetStringWithCharacterInString

    elemental function replaceTargetStringWithStringInCharacter(string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: target
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(string, char(target), char(substring), every, back)
    end function replaceTargetStringWithStringInCharacter

    elemental function replaceTargetStringWithStringInString(string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: target
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), char(target), char(substring), every, back)
    end function replaceTargetStringWithStringInString

    elemental subroutine splitCharacter(string, word, set, separator, back)
        ! Sec. 3.7.5
        type(VARYING_STRING), intent(inout) :: string
        type(VARYING_STRING), intent(out) :: word
        character(len=*), intent(in) :: set
        type(VARYING_STRING), optional, intent(out) :: separator
        logical, optional, intent(in) :: back

        logical :: backwards
        integer :: i
        integer :: string_length
        character(len=1), allocatable :: temp(:)

        string_length = len(string)
        if (present(back)) then
            backwards = back
        else
            backwards = .false.
        end if
        if (backwards) then
            do i = string_length, 1, -1
                if (index(set, string%characters(i)) /= 0) exit
            end do
            if (i < 1) then
                word = string
                string = ""
                if (present(separator)) separator = ""
            else
                allocate(word%characters, source = string%characters(i+1:))
                allocate(temp, source = string%characters(:i-1))
                if (present(separator)) allocate(separator%characters, source = string%characters(i:i))
                deallocate(string%characters)
                allocate(string%characters, source = temp)
                deallocate(temp)
            end if
        else
            do i = 1, string_length
                if (index(set, string%characters(i)) /= 0) exit
            end do
            if (i > string_length) then
                word = string
                string = ""
                if (present(separator)) separator = ""
            else
                allocate(word%characters, source = string%characters(1:i-1))
                allocate(temp, source = string%characters(i+1:))
                if (present(separator)) allocate(separator%characters, source = string%characters(i:i))
                deallocate(string%characters)
                allocate(string%characters, source = temp)
                deallocate(temp)
            end if
        end if
    end subroutine splitCharacter

    elemental subroutine splitString(string, word, set, separator, back)
        ! Sec. 3.7.5
        type(VARYING_STRING), intent(inout) :: string
        type(VARYING_STRING), intent(out) :: word
        type(VARYING_STRING), intent(in) :: set
        type(VARYING_STRING), optional, intent(out) :: separator
        logical, optional, intent(in) :: back

        call split(string, word, char(set), separator, back)
    end subroutine splitString
end module ISO_VARYING_STRING
