module concat_test
    implicit none
    private

    public :: test_concat
contains
    function test_concat() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_concat_strings)
        individual_tests(2) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_concat_string_and_character)
        individual_tests(3) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_concat_character_and_string)
        tests = describe( &
                "Sec. 3.3.2: operator(//) functions the same as for two characters for", &
                individual_tests)
    end function test_concat

    pure function check_concat_strings(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_equals( &
                    char(strings%first) // char(strings%second), &
                    strings%first // strings%second)
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_concat_string_and_character(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_equals( &
                    char(strings%first) // char(strings%second), &
                    strings%first // char(strings%second))
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_concat_character_and_string(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_equals( &
                    char(strings%first) // char(strings%second), &
                    char(strings%first) // strings%second)
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
