module ISO_VARYING_STRING
    implicit none
    private

    type, public :: VARYING_STRING ! Sec. 3.2
        private
        character(len=1), allocatable :: characters(:)
    contains
        private
        final :: destructor
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

    interface LLT ! Sec. 3.4.12
        module procedure string_LLT_String
        module procedure character_LLT_String
        module procedure string_LLT_Character
    end interface

    interface REPEAT ! Sec. 3.4.13
        module procedure stringRepeat
    end interface

    interface SCAN ! Sec. 3.4.14
        module procedure stringScanString
        module procedure stringScanCharacter
        module procedure characterScanString
    end interface

    interface TRIM ! Sec. 3.4.15
        module procedure trimString
    end interface

    interface VERIFY ! Sec. 3.4.16
        module procedure stringVerifyString
        module procedure stringVerifyCharacter
        module procedure characterVerifyString
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
        module procedure extractCharacter
        module procedure extractString
    end interface

    interface INSERT ! Sec. 3.7.2
        module procedure insertCharacterIntoCharacter
        module procedure insertCharacterIntoString
        module procedure insertStringIntoCharacter
        module procedure insertStringIntoString
    end interface

    interface REMOVE ! Sec. 3.7.3
        module procedure removeCharacter
        module procedure removeString
    end interface

    interface REPLACE ! Sec. 3.7.4
        module procedure replaceCharacterWithCharacterStart
        module procedure replaceStringWithCharacterStart
        module procedure replaceCharacterWithStringStart
        module procedure replaceStringWithStringStart
        module procedure replaceCharacterWithCharacterRange
        module procedure replaceStringWithCharacterRange
        module procedure replaceCharacterWithStringRange
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
            LGE, &
            LGT, &
            LLE, &
            LLT, &
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

    elemental function string_LLT_String(string_a, string_b) result(less_than)
        ! Sec 3.4.12
        type(VARYING_STRING), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: less_than

        less_than = llt(char(string_a), char(string_b))
    end function string_LLT_String

    elemental function character_LLT_String(string_a, string_b) result(less_than)
        ! Sec 3.4.12
        character(len=*), intent(in) :: string_a
        type(VARYING_STRING), intent(in) :: string_b
        logical :: less_than

        less_than = llt(string_a, char(string_b))
    end function character_LLT_String

    elemental function string_LLT_Character(string_a, string_b) result(less_than)
        ! Sec 3.4.12
        type(VARYING_STRING), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: less_than

        less_than = llt(char(string_a), string_b)
    end function string_LLT_Character

    elemental function stringRepeat(string, ncopies) result(repeated)
        ! Sec. 3.4.13
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: ncopies
        type(VARYING_STRING) :: repeated

        repeated = repeat(char(string), ncopies)
    end function stringRepeat

    elemental function stringScanString(string, set, back) result(position)
        ! Sec. 3.4.14
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = scan(char(string), char(set), back)
    end function stringScanString

    elemental function stringScanCharacter(string, set, back) result(position)
        ! Sec. 3.4.14
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = scan(char(string), set, back)
    end function stringScanCharacter

    elemental function characterScanString(string, set, back) result(position)
        ! Sec. 3.4.14
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = scan(string, char(set), back)
    end function characterScanString

    elemental function trimString(string) result(trimmed)
        ! Sec. 3.4.15
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: trimmed

        trimmed = trim(char(string))
    end function trimString

    elemental function stringVerifyString(string, set, back) result(position)
        ! Sec. 3.5.16
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = verify(char(string), char(set), back)
    end function stringVerifyString

    elemental function stringVerifyCharacter(string, set, back) result(position)
        ! Sec. 3.5.16
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = verify(char(string), set, back)
    end function stringVerifyCharacter

    elemental function characterVerifyString(string, set, back) result(position)
        ! Sec. 3.5.16
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = verify(string, char(set), back)
    end function characterVerifyString

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
        string = ""
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
        return
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
        string = ""
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
        return
        9999 string = string // buffer(1:num_read)
    end subroutine getWithUnitToEndOfRecord

    subroutine getDefaultUnitToTerminatorString(string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        type(VARYING_STRING), intent(out) :: string
        type(VARYING_STRING), intent(in) :: set ! possible terminator characters
        type(VARYING_STRING), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        call get(string, char(set), separator, maxlen, iostat)
    end subroutine getDefaultUnitToTerminatorString

    subroutine getWithUnitToTerminatorString(unit, string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        integer, intent(in) :: unit
        type(VARYING_STRING), intent(out) :: string
        type(VARYING_STRING), intent(in) :: set ! possible terminator characters
        type(VARYING_STRING), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        call get(unit, string, char(set), separator, maxlen, iostat)
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
        string = ""
        if (present(separator)) separator = ""
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
        string = ""
        if (present(separator)) separator = ""
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

        call put(char(string), iostat)
    end subroutine putStringDefaultUnit

    subroutine putStringWithUnit(unit, string, iostat)
        ! Sec. 3.6.2
        integer, intent(in) :: unit
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(out) :: iostat

        call put(unit, char(string), iostat)
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

        call put_line(char(string), iostat)
    end subroutine putLineStringDefaultUnit

    subroutine putLineStringWithUnit(unit, string, iostat)
        ! Sec. 3.6.3
        integer, intent(in) :: unit
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(out) :: iostat

        call put_line(unit, char(string), iostat)
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

    elemental function extractCharacter(string, start, finish) result(extracted)
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
            finish_ = min(len(string), finish)
        else
            finish_ = len(string)
        end if

        extracted = string(start_:finish_)
    end function extractCharacter

    elemental function extractString(string, start, finish) result(extracted)
        ! Sec. 3.7.1
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(VARYING_STRING) :: extracted

        extracted = extract(char(string), start, finish)
    end function extractString

    elemental function insertCharacterIntoCharacter(string, start, substring) result(inserted)
        ! Sec. 3.7.2
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: inserted

        type(VARYING_STRING) :: beginning
        type(VARYING_STRING) :: middle
        type(VARYING_STRING) :: end_

        if (start <= 1) then
            beginning = substring
            middle = string
            end_ = ""
        else if (start > len(string)) then
            beginning = string
            middle = substring
            end_ = ""
        else
            beginning = string(1:start-1)
            middle = substring
            end_ = string(start:)
        end if
        inserted = beginning // middle // end_
    end function insertCharacterIntoCharacter

    elemental function insertCharacterIntoString(string, start, substring) result(inserted)
        ! Sec. 3.7.2
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: inserted

        inserted = insert(char(string), start, substring)
    end function insertCharacterIntoString

    elemental function insertStringIntoCharacter(string ,start, substring) result(inserted)
        ! Sec. 3.7.2
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: inserted

        inserted = insert(string, start, char(substring))
    end function insertStringIntoCharacter

    elemental function insertStringIntoString(string ,start, substring) result(inserted)
        ! Sec. 3.7.2
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: inserted

        inserted = insert(char(string), start, char(substring))
    end function insertStringIntoString

    elemental function removeCharacter(string, start, finish) result(removed)
        ! Sec. 3.7.3
        character(len=*), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(VARYING_STRING) :: removed

        integer :: start_
        integer :: finish_
        type(VARYING_STRING) :: beginning
        type(VARYING_STRING) :: end_

        if (present(start)) then
            start_ = start
        else
            start_ = 1
        end if
        if (present(finish)) then
            finish_ = finish
        else
            finish_ = len(string)
        end if

        if (start_ > finish_) then
            removed = string
        else
            beginning = string(1:start_ - 1)
            end_ = string(finish_ + 1:len(string))
            removed = beginning // end_
        end if
    end function removeCharacter

    elemental function removeString(string, start, finish) result(removed)
        ! Sec. 3.7.3
        type(VARYING_STRING), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(VARYING_STRING) :: removed

        removed = remove(char(string), start, finish)
    end function removeString

    elemental function replaceCharacterWithCharacterStart( &
                string, start, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        integer :: start_

        start_ = max(1, start)
        replaced = insert( &
                remove(string, start_, start_ + len(substring) - 1), &
                start_, &
                substring)
    end function replaceCharacterWithCharacterStart

    elemental function replaceStringWithCharacterStart( &
            string, start, substring) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), start, substring)
    end function replaceStringWithCharacterStart

    elemental function replaceCharacterWithStringStart( &
            string, start, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(string, start, char(substring))
    end function replaceCharacterWithStringStart

    elemental function replaceStringWithStringStart( &
            string, start, substring) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), start, char(substring))
    end function replaceStringWithStringStart

    elemental function replaceCharacterWithCharacterRange( &
            string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        type(VARYING_STRING) :: beginning
        type(VARYING_STRING) :: ending

        beginning = string(1 : start-1)
        ending = string(max(finish+1, start) : )
        replaced = beginning // substring // ending
    end function replaceCharacterWithCharacterRange

    elemental function replaceStringWithCharacterRange( &
            string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        character(len=*), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), start, finish, substring)
    end function replaceStringWithCharacterRange

    elemental function replaceCharacterWithStringRange( &
            string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(string, start, finish, char(substring))
    end function replaceCharacterWithStringRange

    elemental function replaceStringWithStringRange( &
            string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        type(VARYING_STRING), intent(in) :: substring
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), start, finish, char(substring))
    end function replaceStringWithStringRange

    elemental function replaceTargetCharacterWithCharacterInCharacter( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        logical :: back_
        logical :: every_

        if (present(back)) then
            back_ = back
        else
            back_ = .false.
        end if

        if (present(every)) then
            every_ = every
        else
            every_ = .false.
        end if

        replaced = recursiveReplace(string)
    contains
        pure recursive function recursiveReplace(string_) result(replaced_)
            character(len=*), intent(in) :: string_
            type(VARYING_STRING) :: replaced_

            integer :: position

            position = index(string_, target, back_)
            if (position /= 0) then
                if (every_) then
                    if (back_) then
                        replaced_ = &
                                recursiveReplace(string_(1:position-1)) &
                                // substring &
                                // string_(position+len(target):)
                    else
                        replaced_ = &
                                string_(1:position-1) &
                                // substring &
                                // recursiveReplace(string_(position+len(target):))
                    end if
                else
                    replaced_ = replace( &
                            string_, position, position+len(target)-1, substring)
                end if
            else
                replaced_ = string_
            end if
        end function recursiveReplace
    end function replaceTargetCharacterWithCharacterInCharacter

    elemental function replaceTargetCharacterWithCharacterInString( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), target, substring, every, back)
    end function replaceTargetCharacterWithCharacterInString

    elemental function replaceTargetCharacterWithStringInCharacter( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: target
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(string, target, char(substring), every, back)
    end function replaceTargetCharacterWithStringInCharacter

    elemental function replaceTargetCharacterWithStringInString( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: target
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), target, char(substring), every, back)
    end function replaceTargetCharacterWithStringInString

    elemental function replaceTargetStringWithCharacterInCharacter( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(string, char(target), substring, every, back)
    end function replaceTargetStringWithCharacterInCharacter

    elemental function replaceTargetStringWithCharacterInString( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(char(string), char(target), substring, every, back)
    end function replaceTargetStringWithCharacterInString

    elemental function replaceTargetStringWithStringInCharacter( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: target
        type(VARYING_STRING), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(VARYING_STRING) :: replaced

        replaced = replace(string, char(target), char(substring), every, back)
    end function replaceTargetStringWithStringInCharacter

    elemental function replaceTargetStringWithStringInString( &
            string, target, substring, every, back) result(replaced)
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

    pure subroutine destructor(self)
        type(VARYING_STRING), intent(inout) :: self

        if (allocated(self%characters)) deallocate(self%characters)
    end subroutine destructor
end module ISO_VARYING_STRING
