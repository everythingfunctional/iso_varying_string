module scan_test
    implicit none
    private

    public :: test_scan
contains
    function test_scan() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_scan_strings)
        individual_tests(2) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_scan_string_and_character)
        individual_tests(3) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_scan_character_and_string)
        tests = describe( &
                "Sec. 3.4.14: SCAN functions the same as for two characters for", &
                individual_tests)
    end function

    pure function check_scan_strings(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, scan
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    scan(char(strings%first), char(strings%second)), &
                    scan(strings%first, strings%second), &
                    'scan("' // strings%first // '", "' // strings%second // '")') &
                .and.assert_equals( &
                    scan(char(strings%first), char(strings%second), .false.), &
                    scan(strings%first, strings%second, .false.), &
                    'scan("' // strings%first // '", "' // strings%second // '", .false.)') &
                .and.assert_equals( &
                    scan(char(strings%first), char(strings%second), .true.), &
                    scan(strings%first, strings%second, .true.), &
                    'scan("' // strings%first // '", "' // strings%second // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_scan_string_and_character(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, scan
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    scan(char(strings%first), char(strings%second)), &
                    scan(strings%first, char(strings%second)), &
                    'scan("' // strings%first // '", "' // strings%second // '")') &
                .and.assert_equals( &
                    scan(char(strings%first), char(strings%second), .false.), &
                    scan(strings%first, char(strings%second), .false.), &
                    'scan("' // strings%first // '", "' // strings%second // '", .false.)') &
                .and.assert_equals( &
                    scan(char(strings%first), char(strings%second), .true.), &
                    scan(strings%first, char(strings%second), .true.), &
                    'scan("' // strings%first // '", "' // strings%second // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_scan_character_and_string(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), char, scan
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    scan(char(strings%first), char(strings%second)), &
                    scan(char(strings%first), strings%second), &
                    'scan("' // strings%first // '", "' // strings%second // '")') &
                .and.assert_equals( &
                    scan(char(strings%first), char(strings%second), .false.), &
                    scan(char(strings%first), strings%second, .false.), &
                    'scan("' // strings%first // '", "' // strings%second // '", .false.)') &
                .and.assert_equals( &
                    scan(char(strings%first), char(strings%second), .true.), &
                    scan(char(strings%first), strings%second, .true.), &
                    'scan("' // strings%first // '", "' // strings%second // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
