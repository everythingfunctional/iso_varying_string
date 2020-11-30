module repeat_test
    implicit none
    private

    public :: test_repeat
contains
    function test_repeat() result(tests)
        use custom_generator, only: ASCII_STRING_AND_INTEGER_GENERATOR
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "works the same for characters and strings", &
                ASCII_STRING_AND_INTEGER_GENERATOR, &
                check_repeat)
        tests = describe( &
                "Sec. 3.4.13: REPEAT", individual_tests)
    end function

    pure function check_repeat(example) result(result_)
        use custom_generator, only: string_and_integer_input_t
        use iso_varying_string, only: char, repeat
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example
        type(result_t) :: result_

        select type (example)
        type is (string_and_integer_input_t)
            result_ = assert_equals( &
                    repeat(char(example%string), example%integer_), &
                    repeat(example%string, example%integer_))
        class default
            result_ = fail("Expected to get a string_and_integer_input_t")
        end select
    end function
end module
