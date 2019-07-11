module replace_target_test
    implicit none
    private

    public :: test_replace_character_with_character_in_character
contains
    function test_replace_character_with_character_in_character() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The copy of string is searched for occurences of target. If" &
                // " target is found, it is replaced by substring.", &
                checkReplaceCharacterWithCharacterInCharacter)
        individual_tests(2) = it( &
                "The search is done in the backward direction if the argument" &
                // " back is present with the value true.", &
                checkReplaceCharacterWithCharacterInCharacterBackward)
        individual_tests(3) = it( &
                "If every is present with the value true, the search and replace" &
                // " is continued from the character following target in the" &
                // " search direction specified until all occurrences of" &
                // " target in the copy string are replaced.", &
                checkReplaceCharacterWithCharacterInCharacterEvery)
        tests = describe( &
                "Sec. 3.7.4: REPLACE target character with character in character", &
                individual_tests)
    end function test_replace_character_with_character_in_character

    pure function checkReplaceCharacterWithCharacterInCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "with this in this string", &
                char(replace( &
                        "this in this string", &
                        "this", &
                        "with this")))
    end function checkReplaceCharacterWithCharacterInCharacter

    pure function checkReplaceCharacterWithCharacterInCharacterBackward() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "this in with this string", &
                char(replace(&
                        "this in this string", &
                        "this", &
                        "with this", &
                        back = .TRUE.)))
    end function checkReplaceCharacterWithCharacterInCharacterBackward

    pure function checkReplaceCharacterWithCharacterInCharacterEvery() result(result_)
        use ISO_VARYING_STRING, only: char, replace
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        "with this in with this string", &
                        char(replace(&
                                "this in this string", &
                                "this", &
                                "with this", &
                                every = .TRUE.))) &
                .and.assertEquals( &
                        "with this in with this string", &
                        char(replace( &
                                "this in this string", &
                                "this", &
                                "with this", &
                                every = .TRUE., &
                                back = .TRUE.)))
    end function checkReplaceCharacterWithCharacterInCharacterEvery
end module replace_target_test
