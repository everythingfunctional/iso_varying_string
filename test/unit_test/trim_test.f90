module trim_test
    implicit none
    private

    public :: test_trim
contains
    function test_trim() result(tests)
        use vegetables, only: test_item_t, describe, it, ASCII_STRING_GENERATOR

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(1)

        individual_tests = It( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                check_trim)
        tests = Describe("Sec 3.4.15: TRIM", individual_tests)
    end function

    pure function check_trim(string) result(result_)
        use iso_varying_string, only: char, trim
        use vegetables, only: &
                input_t, result_t, string_input_t, assert_equals, fail

        class(input_t), intent(in) :: string
        type(result_t) :: result_

        select type (string)
        type is (string_input_t)
            result_ = assert_equals( &
                    trim(char(string%value_)), &
                    trim(string%value_))
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function
end module
