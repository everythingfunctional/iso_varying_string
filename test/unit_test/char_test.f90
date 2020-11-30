module char_test
    implicit none
    private

    public :: test_char
contains
    function test_char() result(tests)
        use vegetables, only: test_item_t, describe, it, ASCII_STRING_GENERATOR

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(5)

        individual_tests(1) = it( &
                "converts a varying string to a character with the same length", &
                ASCII_STRING_GENERATOR, &
                check_char_without_length)
        individual_tests(2) = it( &
                "converts a varying string to a shorter character", &
                check_char_with_shorter_length)
        individual_tests(3) = it( &
                "converts a varying string to a longer character", &
                check_char_with_longer_length)
        individual_tests(4) = it( &
                "gives a zero length character for length = 0", &
                check_char_with_zero_length)
        individual_tests(5) = it( &
                "gives a zero length character for negative length", &
                check_char_with_negative_length)
        tests = describe("Sec. 3.4.3: CHAR", individual_tests)
    end function

    pure function check_char_without_length(example) result(result_)
        use iso_varying_string, only: char
        use vegetables, only: &
                input_t, result_t, string_input_t, assert_equals, fail

        class(input_t), intent(in) :: example
        type(result_t) :: result_

        select type (example)
        type is (string_input_t)
            result_ = assert_equals( &
                    example%value_, &
                    char(example%value_), &
                    "If length is absent, the result is a copy of the" &
                    // " characters in the argument string")
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function

    pure function check_char_with_shorter_length() result(result_)
        use iso_varying_string, only: char, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "EXAM", &
                char(var_str("EXAMPLE"), 4), &
                "If string is longer than length, result is truncated on the right.")
    end function

    pure function check_char_with_longer_length() result(result_)
        use iso_varying_string, only: char, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "EXAMPLE   ", &
                char(var_str("EXAMPLE"), 10), &
                "If string is shorter than length, the result is padded on the" &
                // " right with blanks.")
    end function

    pure function check_char_with_zero_length() result(result_)
        use iso_varying_string, only: char, var_str
        use vegetables, only: result_t, assert_empty

        type(result_t) :: result_

        result_ = assert_empty( &
                char(var_str("EXAMPLE"), 0), &
                "If length is less than one, the result is of zero length.")
    end function

    pure function check_char_with_negative_length() result(result_)
        use iso_varying_string, only: char, var_str
        use vegetables, only: result_t, assert_empty

        type(result_t) :: result_

        result_ = assert_empty( &
                char(var_str("EXAMPLE"), -1), &
                "If length is less than one, the result is of zero length.")
    end function
end module
