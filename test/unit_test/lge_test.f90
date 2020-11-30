module lge_test
    implicit none
    private

    public :: test_lge
contains
    function test_lge() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_string_lge_string)
        individual_tests(2) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_character_lge_string)
        individual_tests(3) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_string_lge_character)
        tests = describe( &
                "Sec. 3.4.9: LGE functions the same as for two characters for", &
                individual_tests)
    end function

    pure function check_string_lge_string(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, lge
        use vegetables, only: input_t, result_t, assert_that, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    lge(char(strings%first), char(strings%second)) &
                    .eqv. lge(strings%first, strings%second), &
                    'lge("' // strings%first // '", "' // strings%second // '")')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_character_lge_string(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, lge
        use vegetables, only: input_t, result_t, assert_that, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    lge(char(strings%first), char(strings%second)) &
                    .eqv. lge(char(strings%first), strings%second), &
                    'lge("' // strings%first // '", "' // strings%second // '")')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_string_lge_character(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, lge
        use vegetables, only: input_t, result_t, assert_that, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    lge(char(strings%first), char(strings%second)) &
                    .eqv. lge(strings%first, char(strings%second)), &
                    'lge("' // strings%first // '", "' // strings%second // '")')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
