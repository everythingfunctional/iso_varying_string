module replace_start_test
    implicit none
    private

    public :: test_replace_character_in_character_start
contains
    function test_replace_character_in_character_start() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

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

    pure function checkReplaceCharacterInCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals("SOMETHRING", char(replace("SOMESTRING", 5, "TH")))
    end function checkReplaceCharacterInCharacter

    pure function checkReplaceCharacterInCharacterAfter() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals("SOMETHINGELSE", char(replace("SOMETHING", 10, "ELSE")))
    end function checkReplaceCharacterInCharacterAfter

    pure function checkReplaceCharacterInCharacterBefore() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals("ELSETHING", char(replace("SOMETHING", -1, "ELSE")))
    end function checkReplaceCharacterInCharacterBefore

    pure function checkReplaceCharacterInCharacterOverrun() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals("OVERRUN", char(replace("OVERT", 5, "RUN")))
    end function checkReplaceCharacterInCharacterOverrun
end module replace_start_test
