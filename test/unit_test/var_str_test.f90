module var_str_test
    implicit none
    private

    public :: test_var_str
contains
    function test_var_str() result(tests)
        use vegetables, only: test_item_t, describe, it, ASCII_STRING_GENERATOR

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "Converts an intrinsic fixed-length character value into the" &
                // " equivalent varying-length string value.", &
                ASCII_STRING_GENERATOR, &
                check_var_str)
        tests = describe("Sec. 3.5.1: VAR_STR", individual_tests)
    end function

    pure function check_var_str(string) result(result_)
        use iso_varying_string, only: char, var_str
        use vegetables, only: &
                input_t, result_t, string_input_t, assert_equals, fail

        class(input_t), intent(in) :: string
        type(result_t) :: result_

        select type (string)
        type is (string_input_t)
            result_ = assert_equals( &
                    string%value_, &
                    var_str(char(string%value_)), &
                    "The result value is the same string of characters as the argument.")
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function
end module
