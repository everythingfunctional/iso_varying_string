module equal_test
    implicit none
    private

    public :: test_equals
contains
    function test_equals() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_string_equal_string)
        individual_tests(2) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_character_equal_string)
        individual_tests(3) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                check_string_equal_character)
        tests = describe( &
                "Sec. 3.3.3: operator(==) functions the same as for two characters for", &
                individual_tests)
    end function

    pure function check_string_equal_string(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), operator(==), char
        use vegetables, only: input_t, result_t, assert_that, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    char(strings%first) == char(strings%second) &
                    .eqv. strings%first == strings%second, &
                    '"' // strings%first // '" == "' // strings%second // '"')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_character_equal_string(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), operator(==), char
        use vegetables, only: input_t, result_t, assert_that, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    char(strings%first) == char(strings%second) &
                    .eqv. char(strings%first) == strings%second, &
                    '"' // strings%first // '" == "' // strings%second // '"')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_string_equal_character(strings) result(result_)
        use custom_generator, only: string_pair_input_t
        use iso_varying_string, only: operator(//), operator(==), char
        use vegetables, only: input_t, result_t, assert_that, fail

        class(input_t), intent(in) :: strings
        type(result_t) :: result_

        select type (strings)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    char(strings%first) == char(strings%second) &
                    .eqv. strings%first == char(strings%second), &
                    '"' // strings%first // '" == "' // strings%second // '"')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
