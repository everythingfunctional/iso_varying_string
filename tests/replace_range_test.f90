module replace_range_test
    implicit none
    private

    public :: test_replace_character_in_character_range
contains
    function test_replace_character_in_character_range() result(tests)
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
end module replace_range_test
