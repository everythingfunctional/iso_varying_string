module replace_target_test
    implicit none
    private

    public :: &
            test_replace_character_with_character_in_character, &
            test_replace_character_with_character_in_string, &
            test_replace_character_with_string_in_character, &
            test_replace_character_with_string_in_string, &
            test_replace_string_with_character_in_character, &
            test_replace_string_with_character_in_string, &
            test_replace_string_with_string_in_character, &
            test_replace_string_with_string_in_string
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

    function test_replace_character_with_character_in_string() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The copy of string is searched for occurences of target. If" &
                // " target is found, it is replaced by substring.", &
                checkReplaceCharacterWithCharacterInString)
        individual_tests(2) = it( &
                "The search is done in the backward direction if the argument" &
                // " back is present with the value true.", &
                checkReplaceCharacterWithCharacterInStringBackward)
        individual_tests(3) = it( &
                "If every is present with the value true, the search and replace" &
                // " is continued from the character following target in the" &
                // " search direction specified until all occurrences of" &
                // " target in the copy string are replaced.", &
                checkReplaceCharacterWithCharacterInStringEvery)
        tests = describe( &
                "Sec. 3.7.4: REPLACE target character with character in string", &
                individual_tests)
    end function test_replace_character_with_character_in_string

    function test_replace_character_with_string_in_character() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The copy of string is searched for occurences of target. If" &
                // " target is found, it is replaced by substring.", &
                checkReplaceCharacterWithStringInCharacter)
        individual_tests(2) = it( &
                "The search is done in the backward direction if the argument" &
                // " back is present with the value true.", &
                checkReplaceCharacterWithStringInCharacterBackward)
        individual_tests(3) = it( &
                "If every is present with the value true, the search and replace" &
                // " is continued from the character following target in the" &
                // " search direction specified until all occurrences of" &
                // " target in the copy string are replaced.", &
                checkReplaceCharacterWithStringInCharacterEvery)
        tests = describe( &
                "Sec. 3.7.4: REPLACE target character with string in character", &
                individual_tests)
    end function test_replace_character_with_string_in_character

    function test_replace_character_with_string_in_string() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The copy of string is searched for occurences of target. If" &
                // " target is found, it is replaced by substring.", &
                checkReplaceCharacterWithStringInString)
        individual_tests(2) = it( &
                "The search is done in the backward direction if the argument" &
                // " back is present with the value true.", &
                checkReplaceCharacterWithStringInStringBackward)
        individual_tests(3) = it( &
                "If every is present with the value true, the search and replace" &
                // " is continued from the character following target in the" &
                // " search direction specified until all occurrences of" &
                // " target in the copy string are replaced.", &
                checkReplaceCharacterWithStringInStringEvery)
        tests = describe( &
                "Sec. 3.7.4: REPLACE target character with string in string", &
                individual_tests)
    end function test_replace_character_with_string_in_string

    function test_replace_string_with_character_in_character() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The copy of string is searched for occurences of target. If" &
                // " target is found, it is replaced by substring.", &
                checkReplaceStringWithCharacterInCharacter)
        individual_tests(2) = it( &
                "The search is done in the backward direction if the argument" &
                // " back is present with the value true.", &
                checkReplaceStringWithCharacterInCharacterBackward)
        individual_tests(3) = it( &
                "If every is present with the value true, the search and replace" &
                // " is continued from the character following target in the" &
                // " search direction specified until all occurrences of" &
                // " target in the copy string are replaced.", &
                checkReplaceStringWithCharacterInCharacterEvery)
        tests = describe( &
                "Sec. 3.7.4: REPLACE target string with character in character", &
                individual_tests)
    end function test_replace_string_with_character_in_character

    function test_replace_string_with_character_in_string() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The copy of string is searched for occurences of target. If" &
                // " target is found, it is replaced by substring.", &
                checkReplaceStringWithCharacterInString)
        individual_tests(2) = it( &
                "The search is done in the backward direction if the argument" &
                // " back is present with the value true.", &
                checkReplaceStringWithCharacterInStringBackward)
        individual_tests(3) = it( &
                "If every is present with the value true, the search and replace" &
                // " is continued from the character following target in the" &
                // " search direction specified until all occurrences of" &
                // " target in the copy string are replaced.", &
                checkReplaceStringWithCharacterInStringEvery)
        tests = describe( &
                "Sec. 3.7.4: REPLACE target string with character in string", &
                individual_tests)
    end function test_replace_string_with_character_in_string

    function test_replace_string_with_string_in_character() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The copy of string is searched for occurences of target. If" &
                // " target is found, it is replaced by substring.", &
                checkReplaceStringWithStringInCharacter)
        individual_tests(2) = it( &
                "The search is done in the backward direction if the argument" &
                // " back is present with the value true.", &
                checkReplaceStringWithStringInCharacterBackward)
        individual_tests(3) = it( &
                "If every is present with the value true, the search and replace" &
                // " is continued from the character following target in the" &
                // " search direction specified until all occurrences of" &
                // " target in the copy string are replaced.", &
                checkReplaceStringWithStringInCharacterEvery)
        tests = describe( &
                "Sec. 3.7.4: REPLACE target string with string in character", &
                individual_tests)
    end function test_replace_string_with_string_in_character

    function test_replace_string_with_string_in_string() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "The copy of string is searched for occurences of target. If" &
                // " target is found, it is replaced by substring.", &
                checkReplaceStringWithStringInString)
        individual_tests(2) = it( &
                "The search is done in the backward direction if the argument" &
                // " back is present with the value true.", &
                checkReplaceStringWithStringInStringBackward)
        individual_tests(3) = it( &
                "If every is present with the value true, the search and replace" &
                // " is continued from the character following target in the" &
                // " search direction specified until all occurrences of" &
                // " target in the copy string are replaced.", &
                checkReplaceStringWithStringInStringEvery)
        tests = describe( &
                "Sec. 3.7.4: REPLACE target string with string in string", &
                individual_tests)
    end function test_replace_string_with_string_in_string

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

    pure function checkReplaceCharacterWithCharacterInString() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "with this in this string", &
                char(replace( &
                        var_str("this in this string"), &
                        "this", &
                        "with this")))
    end function checkReplaceCharacterWithCharacterInString

    pure function checkReplaceCharacterWithCharacterInStringBackward() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "this in with this string", &
                char(replace(&
                        var_str("this in this string"), &
                        "this", &
                        "with this", &
                        back = .TRUE.)))
    end function checkReplaceCharacterWithCharacterInStringBackward

    pure function checkReplaceCharacterWithCharacterInStringEvery() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        "with this in with this string", &
                        char(replace(&
                                var_str("this in this string"), &
                                "this", &
                                "with this", &
                                every = .TRUE.))) &
                .and.assertEquals( &
                        "with this in with this string", &
                        char(replace( &
                                var_str("this in this string"), &
                                "this", &
                                "with this", &
                                every = .TRUE., &
                                back = .TRUE.)))
    end function checkReplaceCharacterWithCharacterInStringEvery

    pure function checkReplaceCharacterWithStringInCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "with this in this string", &
                char(replace( &
                        "this in this string", &
                        "this", &
                        var_str("with this"))))
    end function checkReplaceCharacterWithStringInCharacter

    pure function checkReplaceCharacterWithStringInCharacterBackward() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "this in with this string", &
                char(replace(&
                        "this in this string", &
                        "this", &
                        var_str("with this"), &
                        back = .TRUE.)))
    end function checkReplaceCharacterWithStringInCharacterBackward

    pure function checkReplaceCharacterWithStringInCharacterEvery() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        "with this in with this string", &
                        char(replace(&
                                "this in this string", &
                                "this", &
                                var_str("with this"), &
                                every = .TRUE.))) &
                .and.assertEquals( &
                        "with this in with this string", &
                        char(replace( &
                                "this in this string", &
                                "this", &
                                var_str("with this"), &
                                every = .TRUE., &
                                back = .TRUE.)))
    end function checkReplaceCharacterWithStringInCharacterEvery

    pure function checkReplaceCharacterWithStringInString() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "with this in this string", &
                char(replace( &
                        var_str("this in this string"), &
                        "this", &
                        var_str("with this"))))
    end function checkReplaceCharacterWithStringInString

    pure function checkReplaceCharacterWithStringInStringBackward() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "this in with this string", &
                char(replace(&
                        var_str("this in this string"), &
                        "this", &
                        var_str("with this"), &
                        back = .TRUE.)))
    end function checkReplaceCharacterWithStringInStringBackward

    pure function checkReplaceCharacterWithStringInStringEvery() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        "with this in with this string", &
                        char(replace(&
                                var_str("this in this string"), &
                                "this", &
                                var_str("with this"), &
                                every = .TRUE.))) &
                .and.assertEquals( &
                        "with this in with this string", &
                        char(replace( &
                                var_str("this in this string"), &
                                "this", &
                                var_str("with this"), &
                                every = .TRUE., &
                                back = .TRUE.)))
    end function checkReplaceCharacterWithStringInStringEvery

    pure function checkReplaceStringWithCharacterInCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "with this in this string", &
                char(replace( &
                        "this in this string", &
                        var_str("this"), &
                        "with this")))
    end function checkReplaceStringWithCharacterInCharacter

    pure function checkReplaceStringWithCharacterInCharacterBackward() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "this in with this string", &
                char(replace(&
                        "this in this string", &
                        var_str("this"), &
                        "with this", &
                        back = .TRUE.)))
    end function checkReplaceStringWithCharacterInCharacterBackward

    pure function checkReplaceStringWithCharacterInCharacterEvery() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        "with this in with this string", &
                        char(replace(&
                                "this in this string", &
                                var_str("this"), &
                                "with this", &
                                every = .TRUE.))) &
                .and.assertEquals( &
                        "with this in with this string", &
                        char(replace( &
                                "this in this string", &
                                var_str("this"), &
                                "with this", &
                                every = .TRUE., &
                                back = .TRUE.)))
    end function checkReplaceStringWithCharacterInCharacterEvery

    pure function checkReplaceStringWithCharacterInString() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "with this in this string", &
                char(replace( &
                        var_str("this in this string"), &
                        var_str("this"), &
                        "with this")))
    end function checkReplaceStringWithCharacterInString

    pure function checkReplaceStringWithCharacterInStringBackward() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "this in with this string", &
                char(replace(&
                        var_str("this in this string"), &
                        var_str("this"), &
                        "with this", &
                        back = .TRUE.)))
    end function checkReplaceStringWithCharacterInStringBackward

    pure function checkReplaceStringWithCharacterInStringEvery() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        "with this in with this string", &
                        char(replace(&
                                var_str("this in this string"), &
                                var_str("this"), &
                                "with this", &
                                every = .TRUE.))) &
                .and.assertEquals( &
                        "with this in with this string", &
                        char(replace( &
                                var_str("this in this string"), &
                                var_str("this"), &
                                "with this", &
                                every = .TRUE., &
                                back = .TRUE.)))
    end function checkReplaceStringWithCharacterInStringEvery

    pure function checkReplaceStringWithStringInCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "with this in this string", &
                char(replace( &
                        "this in this string", &
                        var_str("this"), &
                        var_str("with this"))))
    end function checkReplaceStringWithStringInCharacter

    pure function checkReplaceStringWithStringInCharacterBackward() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "this in with this string", &
                char(replace(&
                        "this in this string", &
                        var_str("this"), &
                        var_str("with this"), &
                        back = .TRUE.)))
    end function checkReplaceStringWithStringInCharacterBackward

    pure function checkReplaceStringWithStringInCharacterEvery() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        "with this in with this string", &
                        char(replace(&
                                "this in this string", &
                                var_str("this"), &
                                var_str("with this"), &
                                every = .TRUE.))) &
                .and.assertEquals( &
                        "with this in with this string", &
                        char(replace( &
                                "this in this string", &
                                var_str("this"), &
                                var_str("with this"), &
                                every = .TRUE., &
                                back = .TRUE.)))
    end function checkReplaceStringWithStringInCharacterEvery

    pure function checkReplaceStringWithStringInString() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "with this in this string", &
                char(replace( &
                        var_str("this in this string"), &
                        var_str("this"), &
                        var_str("with this"))))
    end function checkReplaceStringWithStringInString

    pure function checkReplaceStringWithStringInStringBackward() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "this in with this string", &
                char(replace(&
                        var_str("this in this string"), &
                        var_str("this"), &
                        var_str("with this"), &
                        back = .TRUE.)))
    end function checkReplaceStringWithStringInStringBackward

    pure function checkReplaceStringWithStringInStringEvery() result(result_)
        use ISO_VARYING_STRING, only: char, replace, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        "with this in with this string", &
                        char(replace(&
                                var_str("this in this string"), &
                                var_str("this"), &
                                var_str("with this"), &
                                every = .TRUE.))) &
                .and.assertEquals( &
                        "with this in with this string", &
                        char(replace( &
                                var_str("this in this string"), &
                                var_str("this"), &
                                var_str("with this"), &
                                every = .TRUE., &
                                back = .TRUE.)))
    end function checkReplaceStringWithStringInStringEvery
end module replace_target_test
