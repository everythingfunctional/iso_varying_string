module insert_test
    implicit none
    private

    public :: &
            test_insert_character_into_character, &
            test_insert_character_into_string, &
            test_insert_string_into_character, &
            test_insert_string_into_string
contains
    function test_insert_character_into_character() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string with the characters of substring inserted into the" &
                // " copy of string before the character at the position start.", &
                checkInsertCharacterIntoCharacter)
        individual_tests(2) = it( &
                "If start is greater than LEN(string), then substring is appended to the copy of string", &
                checkInsertCharacterIntoCharacterAtEnd)
        individual_tests(3) = it( &
                "If start is less than one, then substring is prepended to the copy of string", &
                checkInsertCharacterIntoCharacterAtBeginning)
        tests = describe("Sec. 3.7.2: INSERT character into character", individual_tests)
    end function test_insert_character_into_character

    function test_insert_character_into_string() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string with the characters of substring inserted into the" &
                // " copy of string before the character at the position start.", &
                checkInsertCharacterIntoString)
        individual_tests(2) = it( &
                "If start is greater than LEN(string), then substring is appended to the copy of string", &
                checkInsertCharacterIntoStringAtEnd)
        individual_tests(3) = it( &
                "If start is less than one, then substring is prepended to the copy of string", &
                checkInsertCharacterIntoStringAtBeginning)
        tests = describe("Sec. 3.7.2: INSERT character into string", individual_tests)
    end function test_insert_character_into_string

    function test_insert_string_into_character() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string with the characters of substring inserted into the" &
                // " copy of string before the character at the position start.", &
                checkInsertStringIntoCharacter)
        individual_tests(2) = it( &
                "If start is greater than LEN(string), then substring is appended to the copy of string", &
                checkInsertStringIntoCharacterAtEnd)
        individual_tests(3) = it( &
                "If start is less than one, then substring is prepended to the copy of string", &
                checkInsertStringIntoCharacterAtBeginning)
        tests = describe("Sec. 3.7.2: INSERT string into character", individual_tests)
    end function test_insert_string_into_character

    function test_insert_string_into_string() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string with the characters of substring inserted into the" &
                // " copy of string before the character at the position start.", &
                checkInsertStringIntoString)
        individual_tests(2) = it( &
                "If start is greater than LEN(string), then substring is appended to the copy of string", &
                checkInsertStringIntoStringAtEnd)
        individual_tests(3) = it( &
                "If start is less than one, then substring is prepended to the copy of string", &
                checkInsertStringIntoStringAtBeginning)
        tests = describe("Sec. 3.7.2: INSERT string into string", individual_tests)
    end function test_insert_string_into_string

    pure function checkInsertCharacterIntoCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, insert
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRSUBSTRINGING"
        integer, parameter :: start = 4

        result_ = assertEquals(expected, char(insert(string, start, substring)))
    end function checkInsertCharacterIntoCharacter

    pure function checkInsertCharacterIntoCharacterAtEnd() result(result_)
        use ISO_VARYING_STRING, only: char, insert
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRINGSUBSTRING"
        integer, parameter :: start = 7

        result_ = assertEquals(expected, char(insert(string, start, substring)))
    end function checkInsertCharacterIntoCharacterAtEnd

    pure function checkInsertCharacterIntoCharacterAtBeginning() result(result_)
        use ISO_VARYING_STRING, only: char, insert
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "SUBSTRINGSTRING"
        integer, parameter :: start = -1

        result_ = assertEquals(expected, char(insert(string, start, substring)))
    end function checkInsertCharacterIntoCharacterAtBeginning

    pure function checkInsertCharacterIntoString() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRSUBSTRINGING"
        integer, parameter :: start = 4

        result_ = assertEquals(expected, char(insert(var_str(string), start, substring)))
    end function checkInsertCharacterIntoString

    pure function checkInsertCharacterIntoStringAtEnd() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRINGSUBSTRING"
        integer, parameter :: start = 7

        result_ = assertEquals(expected, char(insert(var_str(string), start, substring)))
    end function checkInsertCharacterIntoStringAtEnd

    pure function checkInsertCharacterIntoStringAtBeginning() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "SUBSTRINGSTRING"
        integer, parameter :: start = -1

        result_ = assertEquals(expected, char(insert(var_str(string), start, substring)))
    end function checkInsertCharacterIntoStringAtBeginning

    pure function checkInsertStringIntoCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRSUBSTRINGING"
        integer, parameter :: start = 4

        result_ = assertEquals(expected, char(insert(string, start, var_str(substring))))
    end function checkInsertStringIntoCharacter

    pure function checkInsertStringIntoCharacterAtEnd() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRINGSUBSTRING"
        integer, parameter :: start = 7

        result_ = assertEquals(expected, char(insert(string, start, var_str(substring))))
    end function checkInsertStringIntoCharacterAtEnd

    pure function checkInsertStringIntoCharacterAtBeginning() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "SUBSTRINGSTRING"
        integer, parameter :: start = -1

        result_ = assertEquals(expected, char(insert(string, start, var_str(substring))))
    end function checkInsertStringIntoCharacterAtBeginning

    pure function checkInsertStringIntoString() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRSUBSTRINGING"
        integer, parameter :: start = 4

        result_ = assertEquals(expected, char(insert(var_str(string), start, var_str(substring))))
    end function checkInsertStringIntoString

    pure function checkInsertStringIntoStringAtEnd() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRINGSUBSTRING"
        integer, parameter :: start = 7

        result_ = assertEquals(expected, char(insert(var_str(string), start, var_str(substring))))
    end function checkInsertStringIntoStringAtEnd

    pure function checkInsertStringIntoStringAtBeginning() result(result_)
        use ISO_VARYING_STRING, only: char, insert, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "SUBSTRINGSTRING"
        integer, parameter :: start = -1

        result_ = assertEquals(expected, char(insert(var_str(string), start, var_str(substring))))
    end function checkInsertStringIntoStringAtBeginning
end module insert_test
