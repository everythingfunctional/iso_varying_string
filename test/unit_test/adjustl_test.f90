module adjustl_test
    implicit none
    private

    public :: test_adjustl
contains
    function test_adjustl() result(tests)
        use vegetables, only: test_item_t, describe, it, ASCII_STRING_GENERATOR

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                check_adjustl)
        tests = describe("Sec 3.4.1: ADJUSTL", individual_tests)
    end function

    pure function check_adjustl(string) result(result_)
        use iso_varying_string, only: adjustl, char
        use vegetables, only: &
                input_t, result_t, string_input_t, assert_equals, fail

        class(input_t), intent(in) :: string
        type(result_t) :: result_

        select type (string)
        type is (string_input_t)
            result_ = assert_equals( &
                    adjustl(char(string%value_)), &
                    adjustl(string%value_))
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function
end module
