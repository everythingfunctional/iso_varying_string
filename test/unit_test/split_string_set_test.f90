module split_string_set_test
    implicit none
    private

    public :: test_split_character
contains
    function test_split_character() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: back_tests(3)
        type(test_item_t) :: forward_separator_tests(2)
        type(test_item_t) :: not_back_separator_tests(2)
        type(test_item_t) :: back_separator_tests(2)
        type(test_item_t) :: forward_no_separator_set_tests(3)
        type(test_item_t) :: forward_separator_set_tests(3)
        type(test_item_t) :: not_back_no_separator_set_tests(3)
        type(test_item_t) :: not_back_separator_set_tests(3)
        type(test_item_t) :: back_no_separator_set_tests(3)
        type(test_item_t) :: back_separator_set_tests(3)

        forward_no_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                check_forward_no_separator)
        forward_no_separator_set_tests(2) = it( &
                "If no character from set is found, string is returned as zero length", &
                check_forward_no_separator_not_found)
        forward_no_separator_set_tests(3) = it( &
                "If set is of zero length, string is returned as zero length", &
                check_forward_no_separator_empty_set)
        forward_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                check_forward_with_separator)
        forward_separator_set_tests(2) = it( &
                "If no character from set is found, separator is returned as zero length", &
                check_forward_with_separator_not_found)
        forward_separator_set_tests(3) = it( &
                "If set is of zero length, separator is returned as zero length", &
                check_forward_with_separator_empty_set)
        not_back_no_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                check_not_backward_no_separator)
        not_back_no_separator_set_tests(2) = it( &
                "If no character from set is found, string is returned as zero length", &
                check_not_backward_no_separator_not_found)
        not_back_no_separator_set_tests(3) = it( &
                "If set is of zero length, string is returned as zero length", &
                check_not_backward_no_separator_empty_set)
        not_back_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                check_not_backward_with_separator)
        not_back_separator_set_tests(2) = it( &
                "If no character from set is found, separator is returned as zero length", &
                check_not_backward_with_separator_not_found)
        not_back_separator_set_tests(3) = it( &
                "If set is of zero length, separator is returned as zero length", &
                check_not_backward_with_separator_empty_set)
        back_no_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                check_backward_no_separator)
        back_no_separator_set_tests(2) = it( &
                "If no character from set is found, string is returned as zero length", &
                check_backward_no_separator_not_found)
        back_no_separator_set_tests(3) = it( &
                "If set is of zero length, string is returned as zero length", &
                check_backward_no_separator_empty_set)
        back_separator_set_tests(1) = it( &
                "The characters passed over in the search are returned in the" &
                // " argument word, and the remainder of the string, not" &
                // " including the sperator character is returned in the argument string", &
                check_backward_with_separator)
        back_separator_set_tests(2) = it( &
                "If no character from set is found, separator is returned as zero length", &
                check_backward_with_separator_not_found)
        back_separator_set_tests(3) = it( &
                "If set is of zero length, separator is returned as zero length", &
                check_backward_with_separator_empty_set)
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
    end function

    pure function check_forward_no_separator() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"))
        result_ = &
                assert_equals("split", word) &
                .and.assert_equals(" this", string)
    end function

    pure function check_forward_no_separator_not_found() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str("!"))
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_forward_no_separator_empty_set() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(""))
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_forward_with_separator() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), separator)
        result_ = &
                assert_equals("split", word) &
                .and.assert_equals(" this", string) &
                .and.assert_equals(",", separator)
    end function

    pure function check_forward_with_separator_not_found() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str("!"), separator)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_forward_with_separator_empty_set() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(""), separator)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_not_backward_no_separator() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), back=.false.)
        result_ = &
                assert_equals("split", word) &
                .and.assert_equals(" this", string)
    end function

    pure function check_not_backward_no_separator_not_found() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str("!"), back=.false.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_not_backward_no_separator_empty_set() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(""), back=.false.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_not_backward_with_separator() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), separator, .false.)
        result_ = &
                assert_equals("split", word) &
                .and.assert_equals(" this", string) &
                .and.assert_equals(",", separator)
    end function

    pure function check_not_backward_with_separator_not_found() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str("!"), separator, .false.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_not_backward_with_separator_empty_set() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(""), separator, .false.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_backward_no_separator() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), back=.true.)
        result_ = &
                assert_equals("this", word) &
                .and.assert_equals("split,", string)
    end function

    pure function check_backward_no_separator_not_found() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str("!"), back=.true.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_backward_no_separator_empty_set() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(""), back=.true.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_backward_with_separator() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(" ,"), separator, .true.)
        result_ = &
                assert_equals("this", word) &
                .and.assert_equals("split,", string) &
                .and.assert_equals(" ", separator)
    end function

    pure function check_backward_with_separator_not_found() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str("!"), separator, .true.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_backward_with_separator_empty_set() result(result_)
        use iso_varying_string, only: varying_string, assignment(=), split, var_str
        use vegetables, only: result_t, assert_empty, assert_equals

        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, var_str(""), separator, .true.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function
end module
