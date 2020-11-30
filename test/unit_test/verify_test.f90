module verify_test
    implicit none
    private

    public :: test_verify
contains
    function test_verify() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_verify_strings)
        individual_tests(2) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_verify_string_and_character)
        individual_tests(3) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_verify_character_and_string)
        tests = describe( &
                "Sec. 3.4.16: VERIFY functions the same as for two characters for", &
                individual_tests)
    end function

    pure function check_verify_strings(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, verify
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    verify(char(strings%first), char(strings%second)), &
                    verify(strings%first, strings%second), &
                    'verify("' // strings%first // '", "' // strings%second // '")') &
                .and.assert_equals( &
                    verify(char(strings%first), char(strings%second), .false.), &
                    verify(strings%first, strings%second, .false.), &
                    'verify("' // strings%first // '", "' // strings%second // '", .false.)') &
                .and.assert_equals( &
                    verify(char(strings%first), char(strings%second), .true.), &
                    verify(strings%first, strings%second, .true.), &
                    'verify("' // strings%first // '", "' // strings%second // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_verify_string_and_character(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, verify
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    verify(char(strings%first), char(strings%second)), &
                    verify(strings%first, char(strings%second)), &
                    'verify("' // strings%first // '", "' // strings%second // '")') &
                .and.assert_equals( &
                    verify(char(strings%first), char(strings%second), .false.), &
                    verify(strings%first, char(strings%second), .false.), &
                    'verify("' // strings%first // '", "' // strings%second // '", .false.)') &
                .and.assert_equals( &
                    verify(char(strings%first), char(strings%second), .true.), &
                    verify(strings%first, char(strings%second), .true.), &
                    'verify("' // strings%first // '", "' // strings%second // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_verify_character_and_string(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, verify
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    verify(char(strings%first), char(strings%second)), &
                    verify(char(strings%first), strings%second), &
                    'verify("' // strings%first // '", "' // strings%second // '")') &
                .and.assert_equals( &
                    verify(char(strings%first), char(strings%second), .false.), &
                    verify(char(strings%first), strings%second, .false.), &
                    'verify("' // strings%first // '", "' // strings%second // '", .false.)') &
                .and.assert_equals( &
                    verify(char(strings%first), char(strings%second), .true.), &
                    verify(char(strings%first), strings%second, .true.), &
                    'verify("' // strings%first // '", "' // strings%second // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
