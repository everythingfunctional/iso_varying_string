module len_test
    implicit none
    private

    public :: test_len
contains
    function test_len() result(tests)
        use vegetables, only: test_item_t, describe, it, ASCII_STRING_GENERATOR

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                check_len)
        tests = describe("Sec 3.4.7: LEN", individual_tests)
    end function

    pure function check_len(string) result(result_)
        use iso_varying_string, only: char, len
        use vegetables, only: &
                input_t, result_t, string_input_t, assert_equals, fail

        class(input_t), intent(in) :: string
        type(result_t) :: result_

        select type (string)
        type is (string_input_t)
            result_ = assert_equals( &
                    len(char(string%value_)), &
                    len(string%value_), &
                    string%value_)
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function
end module
