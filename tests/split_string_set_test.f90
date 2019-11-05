module split_string_set_test
    implicit none
    private

    public :: test_split_character
contains
    function test_split_character() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: back_tests(3)
        type(TestItem_t) :: forward_separator_tests(2)
        type(TestItem_t) :: not_back_separator_tests(2)
        type(TestItem_t) :: back_separator_tests(2)
        type(TestItem_t) :: forward_no_separator_set_tests(3)
        type(TestItem_t) :: forward_separator_set_tests(3)
        type(TestItem_t) :: not_back_no_separator_set_tests(3)
        type(TestItem_t) :: not_back_separator_set_tests(3)
        type(TestItem_t) :: back_no_separator_set_tests(3)
        type(TestItem_t) :: back_separator_set_tests(3)

        forward_no_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                checkForwardNoSeparator)
        forward_no_separator_set_tests(2) = it( &
                "If no character from set is found, string is returned as zero length", &
                checkForwardNoSeparatorNotFound)
        forward_no_separator_set_tests(3) = it( &
                "If set is of zero length, string is returned as zero length", &
                checkForwardNoSeparatorEmptySet)
        forward_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                checkForwardWithSeparator)
        forward_separator_set_tests(2) = it( &
                "If no character from set is found, separator is returned as zero length", &
                checkForwardWithSeparatorNotFound)
        forward_separator_set_tests(3) = it( &
                "If set is of zero length, separator is returned as zero length", &
                checkForwardWithSeparatorEmptySet)
        not_back_no_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                checkNotBackwardNoSeparator)
        not_back_no_separator_set_tests(2) = it( &
                "If no character from set is found, string is returned as zero length", &
                checkNotBackwardNoSeparatorNotFound)
        not_back_no_separator_set_tests(3) = it( &
                "If set is of zero length, string is returned as zero length", &
                checkNotBackwardNoSeparatorEmptySet)
        not_back_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                checkNotBackwardWithSeparator)
        not_back_separator_set_tests(2) = it( &
                "If no character from set is found, separator is returned as zero length", &
                checkNotBackwardWithSeparatorNotFound)
        not_back_separator_set_tests(3) = it( &
                "If set is of zero length, separator is returned as zero length", &
                checkNotBackwardWithSeparatorEmptySet)
        back_no_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                checkBackwardNoSeparator)
        back_no_separator_set_tests(2) = it( &
                "If no character from set is found, string is returned as zero length", &
                checkBackwardNoSeparatorNotFound)
        back_no_separator_set_tests(3) = it( &
                "If set is of zero length, string is returned as zero length", &
                checkBackwardNoSeparatorEmptySet)
        back_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                checkBackwardWithSeparator)
        back_separator_set_tests(2) = it( &
                "If no character from set is found, separator is returned as zero length", &
                checkBackwardWithSeparatorNotFound)
        back_separator_set_tests(3) = it( &
                "If set is of zero length, separator is returned as zero length", &
                checkBackwardWithSeparatorEmptySet)
        forward_separator_tests(1) = describe( &
                "Without separator argument", &
                forward_no_separator_set_tests)
        forward_separator_tests(2) = describe( &
                "If the argument seprator is present, the actual character" &
                // " found which separates the word from the remainder of the" &
                // " string is returned in separator", &
                forward_separator_set_tests)
        not_back_separator_tests(1) = describe( &
                "Without separator argument", &
                not_back_no_separator_set_tests)
        not_back_separator_tests(2) = describe( &
                "If the argument seprator is present, the actual character" &
                // " found which separates the word from the remainder of the" &
                // " string is returned in separator", &
                not_back_separator_set_tests)
        back_separator_tests(1) = describe( &
                "Without separator argument", &
                back_no_separator_set_tests)
        back_separator_tests(2) = describe( &
                "If the argument seprator is present, the actual character" &
                // " found which separates the word from the remainder of the" &
                // " string is returned in separator", &
                back_separator_set_tests)
        back_tests(1) = describe( &
                "The string is searched in the forward direction", &
                forward_separator_tests)
        back_tests(2) = describe( &
                "The string is searched in the forward direction if back is false", &
                not_back_separator_tests)
        back_tests(3) = describe( &
                "The string is searched in the backward direction if back is true", &
                back_separator_tests)
        tests = describe( &
                "Sec. 3.7.5: SPLIT divides the string at the first occurence of a character that is in set (string)", &
                back_tests)
    end function test_split_character

    function checkForwardNoSeparator() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"))
        result_ = &
                assertEquals("split", char(word)) &
                .and.assertEquals(" this", char(string))
    end function checkForwardNoSeparator

    function checkForwardNoSeparatorNotFound() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str("!"))
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word))
    end function checkForwardNoSeparatorNotFound

    function checkForwardNoSeparatorEmptySet() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(""))
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word))
    end function checkForwardNoSeparatorEmptySet

    function checkForwardWithSeparator() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), separator)
        result_ = &
                assertEquals("split", char(word)) &
                .and.assertEquals(" this", char(string)) &
                .and.assertEquals(",", char(separator))
    end function checkForwardWithSeparator

    function checkForwardWithSeparatorNotFound() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str("!"), separator)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word)) &
                .and.assertEmpty(char(separator))
    end function checkForwardWithSeparatorNotFound

    function checkForwardWithSeparatorEmptySet() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(""), separator)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word)) &
                .and.assertEmpty(char(separator))
    end function checkForwardWithSeparatorEmptySet

    function checkNotBackwardNoSeparator() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), back=.false.)
        result_ = &
                assertEquals("split", char(word)) &
                .and.assertEquals(" this", char(string))
    end function checkNotBackwardNoSeparator

    function checkNotBackwardNoSeparatorNotFound() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str("!"), back=.false.)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word))
    end function checkNotBackwardNoSeparatorNotFound

    function checkNotBackwardNoSeparatorEmptySet() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(""), back=.false.)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word))
    end function checkNotBackwardNoSeparatorEmptySet

    function checkNotBackwardWithSeparator() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), separator, .false.)
        result_ = &
                assertEquals("split", char(word)) &
                .and.assertEquals(" this", char(string)) &
                .and.assertEquals(",", char(separator))
    end function checkNotBackwardWithSeparator

    function checkNotBackwardWithSeparatorNotFound() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str("!"), separator, .false.)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word)) &
                .and.assertEmpty(char(separator))
    end function checkNotBackwardWithSeparatorNotFound

    function checkNotBackwardWithSeparatorEmptySet() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(""), separator, .false.)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word)) &
                .and.assertEmpty(char(separator))
    end function checkNotBackwardWithSeparatorEmptySet

    function checkBackwardNoSeparator() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), back=.true.)
        result_ = &
                assertEquals("this", char(word)) &
                .and.assertEquals("split,", char(string))
    end function checkBackwardNoSeparator

    function checkBackwardNoSeparatorNotFound() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str("!"), back=.true.)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word))
    end function checkBackwardNoSeparatorNotFound

    function checkBackwardNoSeparatorEmptySet() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(""), back=.true.)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word))
    end function checkBackwardNoSeparatorEmptySet

    function checkBackwardWithSeparator() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), separator, .true.)
        result_ = &
                assertEquals("this", char(word)) &
                .and.assertEquals("split,", char(string)) &
                .and.assertEquals(" ", char(separator))
    end function checkBackwardWithSeparator

    function checkBackwardWithSeparatorNotFound() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str("!"), separator, .true.)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word)) &
                .and.assertEmpty(char(separator))
    end function checkBackwardWithSeparatorNotFound

    function checkBackwardWithSeparatorEmptySet() result(result_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), char, split, var_str
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: separator
        type(VARYING_STRING) :: string
        type(VARYING_STRING) :: word

        string = "split, this"
        call split(string, word, var_str(""), separator, .true.)
        result_ = &
                assertEmpty(char(string)) &
                .and.assertEquals("split, this", char(word)) &
                .and.assertEmpty(char(separator))
    end function checkBackwardWithSeparatorEmptySet
end module split_string_set_test
