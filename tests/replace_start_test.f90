module replace_start_test
    use ISO_VARYING_STRING, only: char, replace, var_str
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: &
            test_replace_character_in_character_start, &
            test_replace_character_in_string_start, &
            test_replace_string_in_character_start, &
            test_replace_string_in_string_start
contains
    function test_replace_character_in_character_start() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters of substring are inserted into a copy of string" &
                // " at the position start, and the characters in postions from" &
                // " start to min(start+len(substring)-1, len(string)) are deleted.", &
                checkReplaceCharacterInCharacter)
        individual_tests(2) = it( &
                "If start is greater than len(string), the substring is appended to the copy of string.", &
                checkReplaceCharacterInCharacterAfter)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start", &
                checkReplaceCharacterInCharacterBefore)
        individual_tests(4) = it( &
                "If substring runs off the end, the resulting string is longer?", &
                checkReplaceCharacterInCharacterOverrun)
        tests = describe("Sec. 3.7.4: REPLACE in character with character at start", individual_tests)
    end function test_replace_character_in_character_start

    function test_replace_character_in_string_start() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters of substring are inserted into a copy of string" &
                // " at the position start, and the characters in postions from" &
                // " start to min(start+len(substring)-1, len(string)) are deleted.", &
                checkReplaceCharacterInString)
        individual_tests(2) = it( &
                "If start is greater than len(string), the substring is appended to the copy of string.", &
                checkReplaceCharacterInStringAfter)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start", &
                checkReplaceCharacterInStringBefore)
        individual_tests(4) = it( &
                "If substring runs off the end, the resulting string is longer?", &
                checkReplaceCharacterInStringOverrun)
        tests = describe("Sec. 3.7.4: REPLACE in string with character at start", individual_tests)
    end function test_replace_character_in_string_start

    function test_replace_string_in_character_start() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters of substring are inserted into a copy of string" &
                // " at the position start, and the characters in postions from" &
                // " start to min(start+len(substring)-1, len(string)) are deleted.", &
                checkReplaceStringInCharacter)
        individual_tests(2) = it( &
                "If start is greater than len(string), the substring is appended to the copy of string.", &
                checkReplaceStringInCharacterAfter)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start", &
                checkReplaceStringInCharacterBefore)
        individual_tests(4) = it( &
                "If substring runs off the end, the resulting string is longer?", &
                checkReplaceStringInCharacterOverrun)
        tests = describe("Sec. 3.7.4: REPLACE in character with string at start", individual_tests)
    end function test_replace_string_in_character_start

    function test_replace_string_in_string_start() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters of substring are inserted into a copy of string" &
                // " at the position start, and the characters in postions from" &
                // " start to min(start+len(substring)-1, len(string)) are deleted.", &
                checkReplaceStringInString)
        individual_tests(2) = it( &
                "If start is greater than len(string), the substring is appended to the copy of string.", &
                checkReplaceStringInStringAfter)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start", &
                checkReplaceStringInStringBefore)
        individual_tests(4) = it( &
                "If substring runs off the end, the resulting string is longer?", &
                checkReplaceStringInStringOverrun)
        tests = describe("Sec. 3.7.4: REPLACE in string with string at start", individual_tests)
    end function test_replace_string_in_string_start

    pure function checkReplaceCharacterInCharacter() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("SOMETHRING", char(replace("SOMESTRING", 5, "TH")))
    end function checkReplaceCharacterInCharacter

    pure function checkReplaceCharacterInCharacterAfter() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("SOMETHINGELSE", char(replace("SOMETHING", 10, "ELSE")))
    end function checkReplaceCharacterInCharacterAfter

    pure function checkReplaceCharacterInCharacterBefore() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("ELSETHING", char(replace("SOMETHING", -1, "ELSE")))
    end function checkReplaceCharacterInCharacterBefore

    pure function checkReplaceCharacterInCharacterOverrun() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("OVERRUN", char(replace("OVERT", 5, "RUN")))
    end function checkReplaceCharacterInCharacterOverrun

    pure function checkReplaceCharacterInString() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("SOMETHRING", char(replace(var_str("SOMESTRING"), 5, "TH")))
    end function checkReplaceCharacterInString

    pure function checkReplaceCharacterInStringAfter() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("SOMETHINGELSE", char(replace(var_str("SOMETHING"), 10, "ELSE")))
    end function checkReplaceCharacterInStringAfter

    pure function checkReplaceCharacterInStringBefore() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("ELSETHING", char(replace(var_str("SOMETHING"), -1, "ELSE")))
    end function checkReplaceCharacterInStringBefore

    pure function checkReplaceCharacterInStringOverrun() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("OVERRUN", char(replace(var_str("OVERT"), 5, "RUN")))
    end function checkReplaceCharacterInStringOverrun

    pure function checkReplaceStringInCharacter() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("SOMETHRING", char(replace("SOMESTRING", 5, var_str("TH"))))
    end function checkReplaceStringInCharacter

    pure function checkReplaceStringInCharacterAfter() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("SOMETHINGELSE", char(replace("SOMETHING", 10, var_str("ELSE"))))
    end function checkReplaceStringInCharacterAfter

    pure function checkReplaceStringInCharacterBefore() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("ELSETHING", char(replace("SOMETHING", -1, var_str("ELSE"))))
    end function checkReplaceStringInCharacterBefore

    pure function checkReplaceStringInCharacterOverrun() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("OVERRUN", char(replace("OVERT", 5, var_str("RUN"))))
    end function checkReplaceStringInCharacterOverrun

    pure function checkReplaceStringInString() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("SOMETHRING", char(replace(var_str("SOMESTRING"), 5, var_str("TH"))))
    end function checkReplaceStringInString

    pure function checkReplaceStringInStringAfter() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("SOMETHINGELSE", char(replace(var_str("SOMETHING"), 10, var_str("ELSE"))))
    end function checkReplaceStringInStringAfter

    pure function checkReplaceStringInStringBefore() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("ELSETHING", char(replace(var_str("SOMETHING"), -1, var_str("ELSE"))))
    end function checkReplaceStringInStringBefore

    pure function checkReplaceStringInStringOverrun() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("OVERRUN", char(replace(var_str("OVERT"), 5, var_str("RUN"))))
    end function checkReplaceStringInStringOverrun
end module replace_start_test
