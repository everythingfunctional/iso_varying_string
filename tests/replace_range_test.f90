module replace_range_test
    implicit none
    private

    public :: &
            test_replace_character_in_character_range, &
            test_replace_character_in_string_range, &
            test_replace_string_in_character_range, &
            test_replace_string_in_string_range
contains
    function test_replace_character_in_character_range() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters in the copy of string between positions start" &
                // " and finish, including those at start and finish, are" &
                // " deleted and replaced by characters of substring.", &
                checkReplaceCharacterInCharacter)
        individual_tests(2) = it( &
                "If start is less than one, the value one is used for start.", &
                checkReplaceCharacterInCharacterStartLTOne)
        individual_tests(3) = it( &
                "If finish is greater than len(string), the value len(string)" &
                // " is used for finish.", &
                checkReplaceCharacterInCharacterStartGTEnd)
        individual_tests(4) = it( &
                "If finish is less than start, the characters of substring" &
                // " are inserted before the character at start and no" &
                // " characters are deleted.", &
                checkReplaceCharacterInCharacterStartGTFinish)
        tests = describe( &
                "Sec. 3.7.4: REPLACE in character with character in range", &
                individual_tests)
    end function test_replace_character_in_character_range

    function test_replace_character_in_string_range() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters in the copy of string between positions start" &
                // " and finish, including those at start and finish, are" &
                // " deleted and replaced by characters of substring.", &
                checkReplaceCharacterInString)
        individual_tests(2) = it( &
                "If start is less than one, the value one is used for start.", &
                checkReplaceCharacterInStringStartLTOne)
        individual_tests(3) = it( &
                "If finish is greater than len(string), the value len(string)" &
                // " is used for finish.", &
                checkReplaceCharacterInStringStartGTEnd)
        individual_tests(4) = it( &
                "If finish is less than start, the characters of substring" &
                // " are inserted before the character at start and no" &
                // " characters are deleted.", &
                checkReplaceCharacterInStringStartGTFinish)
        tests = describe( &
                "Sec. 3.7.4: REPLACE in string with character in range", &
                individual_tests)
    end function test_replace_character_in_string_range

    function test_replace_string_in_character_range() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters in the copy of string between positions start" &
                // " and finish, including those at start and finish, are" &
                // " deleted and replaced by characters of substring.", &
                checkReplaceStringInCharacter)
        individual_tests(2) = it( &
                "If start is less than one, the value one is used for start.", &
                checkReplaceStringInCharacterStartLTOne)
        individual_tests(3) = it( &
                "If finish is greater than len(string), the value len(string)" &
                // " is used for finish.", &
                checkReplaceStringInCharacterStartGTEnd)
        individual_tests(4) = it( &
                "If finish is less than start, the characters of substring" &
                // " are inserted before the character at start and no" &
                // " characters are deleted.", &
                checkReplaceStringInCharacterStartGTFinish)
        tests = describe( &
                "Sec. 3.7.4: REPLACE in character with string in range", &
                individual_tests)
    end function test_replace_string_in_character_range

    function test_replace_string_in_string_range() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters in the copy of string between positions start" &
                // " and finish, including those at start and finish, are" &
                // " deleted and replaced by characters of substring.", &
                checkReplaceStringInString)
        individual_tests(2) = it( &
                "If start is less than one, the value one is used for start.", &
                checkReplaceStringInStringStartLTOne)
        individual_tests(3) = it( &
                "If finish is greater than len(string), the value len(string)" &
                // " is used for finish.", &
                checkReplaceStringInStringStartGTEnd)
        individual_tests(4) = it( &
                "If finish is less than start, the characters of substring" &
                // " are inserted before the character at start and no" &
                // " characters are deleted.", &
                checkReplaceStringInStringStartGTFinish)
        tests = describe( &
                "Sec. 3.7.4: REPLACE in string with string in range", &
                individual_tests)
    end function test_replace_string_in_string_range

    pure function checkReplaceCharacterInCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WAS CRAZY", &
                char(replace("THAT IS CRAZY", 6, 7, "WAS")))
    end function checkReplaceCharacterInCharacter

    pure function checkReplaceCharacterInCharacterStartLTOne() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "WAS CRAZY", &
                char(replace("THAT IS CRAZY", -1, 7, "WAS")))
    end function checkReplaceCharacterInCharacterStartLTOne

    pure function checkReplaceCharacterInCharacterStartGTEnd() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WAS", &
                char(replace("THAT IS CRAZY", 6, 15, "WAS")))
    end function checkReplaceCharacterInCharacterStartGTEnd

    pure function checkReplaceCharacterInCharacterStartGTFinish() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WASIS CRAZY", &
                char(replace("THAT IS CRAZY", 6, 1, "WAS")))
    end function checkReplaceCharacterInCharacterStartGTFinish

    pure function checkReplaceCharacterInString() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WAS CRAZY", &
                char(replace(var_str("THAT IS CRAZY"), 6, 7, "WAS")))
    end function checkReplaceCharacterInString

    pure function checkReplaceCharacterInStringStartLTOne() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "WAS CRAZY", &
                char(replace(var_str("THAT IS CRAZY"), -1, 7, "WAS")))
    end function checkReplaceCharacterInStringStartLTOne

    pure function checkReplaceCharacterInStringStartGTEnd() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WAS", &
                char(replace(var_str("THAT IS CRAZY"), 6, 15, "WAS")))
    end function checkReplaceCharacterInStringStartGTEnd

    pure function checkReplaceCharacterInStringStartGTFinish() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WASIS CRAZY", &
                char(replace(var_str("THAT IS CRAZY"), 6, 1, "WAS")))
    end function checkReplaceCharacterInStringStartGTFinish

    pure function checkReplaceStringInCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WAS CRAZY", &
                char(replace("THAT IS CRAZY", 6, 7, var_str("WAS"))))
    end function checkReplaceStringInCharacter

    pure function checkReplaceStringInCharacterStartLTOne() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "WAS CRAZY", &
                char(replace("THAT IS CRAZY", -1, 7, var_str("WAS"))))
    end function checkReplaceStringInCharacterStartLTOne

    pure function checkReplaceStringInCharacterStartGTEnd() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WAS", &
                char(replace("THAT IS CRAZY", 6, 15, var_str("WAS"))))
    end function checkReplaceStringInCharacterStartGTEnd

    pure function checkReplaceStringInCharacterStartGTFinish() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WASIS CRAZY", &
                char(replace("THAT IS CRAZY", 6, 1, var_str("WAS"))))
    end function checkReplaceStringInCharacterStartGTFinish

    pure function checkReplaceStringInString() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WAS CRAZY", &
                char(replace(var_str("THAT IS CRAZY"), 6, 7, var_str("WAS"))))
    end function checkReplaceStringInString

    pure function checkReplaceStringInStringStartLTOne() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "WAS CRAZY", &
                char(replace(var_str("THAT IS CRAZY"), -1, 7, var_str("WAS"))))
    end function checkReplaceStringInStringStartLTOne

    pure function checkReplaceStringInStringStartGTEnd() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WAS", &
                char(replace(var_str("THAT IS CRAZY"), 6, 15, var_str("WAS"))))
    end function checkReplaceStringInStringStartGTEnd

    pure function checkReplaceStringInStringStartGTFinish() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "THAT WASIS CRAZY", &
                char(replace(var_str("THAT IS CRAZY"), 6, 1, var_str("WAS"))))
    end function checkReplaceStringInStringStartGTFinish
end module replace_range_test
