module ichar_test
    implicit none
    private

    public :: test_ichar
contains
    function test_ichar() result(tests)
        use custom_generator, only: ASCII_CHARACTER_GENERATOR
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "works the same for characters and strings", &
                ASCII_CHARACTER_GENERATOR, &
                check_ichar)
        tests = describe("Sec. 3.4.5: ICHAR", individual_tests)
    end function

    pure function check_ichar(char_) result(result_)
        use custom_generator, only: character_input_t
        use iso_varying_string, only: ichar, var_str
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: char_
        type(result_t) :: result_

        select type (char_)
        type is (character_input_t)
            result_ = assert_equals( &
                    ichar(char_%value_), &
                    ichar(var_str(char_%value_)), &
                    char_%value_)
        class default
            result_ = fail("Expected to get a character_input_t.")
        end select
    end function
end module
