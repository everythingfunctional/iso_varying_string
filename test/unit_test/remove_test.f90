module remove_test
    implicit none
    private

    public :: test_remove_character, test_remove_string
contains
    function test_remove_character() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string between positions start and finish, inclusive.", &
                check_remove_character)
        individual_tests(2) = it( &
                "If start is absent, the value one is used for start.", &
                check_remove_character_without_start)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start.", &
                check_remove_character_with_start_lt_one)
        individual_tests(4) = it( &
                "If finish is absent, the value LEN(string) is used for finish.", &
                check_remove_character_without_finish)
        individual_tests(5) = it( &
                "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                check_remove_character_with_finish_gt_len_string)
        individual_tests(6) = it( &
                "If finish is less than start, the characters of string are delivered unchanged.", &
                check_remove_character_zero_length)
        tests = describe("Sec. 3.7.3 REMOVE character", individual_tests)
    end function

    function test_remove_string() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string between positions start and finish, inclusive.", &
                check_remove_string)
        individual_tests(2) = it( &
                "If start is absent, the value one is used for start.", &
                check_remove_string_without_start)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start.", &
                check_remove_string_with_start_lt_one)
        individual_tests(4) = it( &
                "If finish is absent, the value LEN(string) is used for finish.", &
                check_remove_string_without_finish)
        individual_tests(5) = it( &
                "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                check_remove_string_with_finish_gt_len_string)
        individual_tests(6) = it( &
                "If finish is less than start, the characters of string are delivered unchanged.", &
                check_remove_string_zero_length)
        tests = describe("Sec. 3.7.3 REMOVE string", individual_tests)
    end function

    pure function check_remove_character() result(result_)
        use iso_varying_string, only: remove
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("EPLE", remove("EXAMPLE", 2, 4))
    end function

    pure function check_remove_character_without_start() result(result_)
        use iso_varying_string, only: remove
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("PLE", remove("EXAMPLE", finish = 4))
    end function

    pure function check_remove_character_with_start_lt_one() result(result_)
        use iso_varying_string, only: remove
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("PLE", remove("EXAMPLE", -1, 4))
    end function

    pure function check_remove_character_without_finish() result(result_)
        use iso_varying_string, only: remove
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("E", remove("EXAMPLE", 2))
    end function

    pure function check_remove_character_with_finish_gt_len_string() result(result_)
        use iso_varying_string, only: remove
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("E", remove("EXAMPLE", 2, 8))
    end function

    pure function check_remove_character_zero_length() result(result_)
        use iso_varying_string, only: remove
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("EXAMPLE", remove("EXAMPLE", 10, -2))
    end function

    pure function check_remove_string() result(result_)
        use iso_varying_string, only: remove, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("EPLE", remove(var_str("EXAMPLE"), 2, 4))
    end function

    pure function check_remove_string_without_start() result(result_)
        use iso_varying_string, only: remove, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("PLE", remove(var_str("EXAMPLE"), finish = 4))
    end function

    pure function check_remove_string_with_start_lt_one() result(result_)
        use iso_varying_string, only: remove, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("PLE", remove(var_str("EXAMPLE"), -1, 4))
    end function

    pure function check_remove_string_without_finish() result(result_)
        use iso_varying_string, only: remove, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("E", remove(var_str("EXAMPLE"), 2))
    end function

    pure function check_remove_string_with_finish_gt_len_string() result(result_)
        use iso_varying_string, only: remove, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("E", remove(var_str("EXAMPLE"), 2, 8))
    end function

    pure function check_remove_string_zero_length() result(result_)
        use iso_varying_string, only: remove, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("EXAMPLE", remove(var_str("EXAMPLE"), 10, -2))
    end function
end module
