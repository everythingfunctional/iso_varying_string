module adjustr_test
    implicit none
    private

    public :: test_adjustr
contains
    function test_adjustr() result(tests)
        use vegetables, only: test_item_t, describe, it, ASCII_STRING_GENERATOR

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                check_adjustr)
        tests = describe("Sec 3.4.2: ADJUSTR", individual_tests)
    end function

    pure function check_adjustr(string) result(result_)
        use iso_varying_string, only: adjustr, char
        use vegetables, only: &
                input_t, result_t, string_input_t, assert_equals, fail

        class(input_t), intent(in) :: string
        type(result_t) :: result_

        select type (string)
        type is (string_input_t)
            result_ = assert_equals( &
                    adjustr(char(string%value_)), &
                    adjustr(string%value_))
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function
end module
