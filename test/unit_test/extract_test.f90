module extract_test
    implicit none
    private

    public :: test_extract_character, test_extract_string
contains
    function test_extract_character() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string between positions start and finish, inclusive.", &
                check_extract_character)
        individual_tests(2) = it( &
                "If start is absent, the value one is used for start.", &
                check_extract_character_without_start)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start.", &
                check_extract_character_with_start_lt_one)
        individual_tests(4) = it( &
                "If finish is absent, the value LEN(string) is used for finish.", &
                check_extract_character_without_finish)
        individual_tests(5) = it( &
                "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                check_extract_character_with_finish_gt_len_string)
        individual_tests(6) = it( &
                "If finish is less than start, the result is a zero-length string.", &
                check_extract_character_zero_length)
        tests = describe("Sec. 3.7.1 EXTRACT character", individual_tests)
    end function

    function test_extract_string() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string between positions start and finish, inclusive.", &
                check_extract_string)
        individual_tests(2) = it( &
                "If start is absent, the value one is used for start.", &
                check_extract_string_without_start)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start.", &
                check_extract_string_with_start_lt_one)
        individual_tests(4) = it( &
                "If finish is absent, the value LEN(string) is used for finish.", &
                check_extract_string_without_finish)
        individual_tests(5) = it( &
                "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                check_extract_string_with_finish_gt_len_string)
        individual_tests(6) = it( &
                "If finish is less than start, the result is a zero-length string.", &
                check_extract_string_zero_length)
        tests = describe("Sec. 3.7.1 EXTRACT string", individual_tests)
    end function

    pure function check_extract_character() result(result_)
        use iso_varying_string, only: extract
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:4), extract(example, 2, 4))
    end function

    pure function check_extract_character_without_start() result(result_)
        use iso_varying_string, only: extract
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(1:4), extract(example, finish = 4))
    end function

    pure function check_extract_character_with_start_lt_one() result(result_)
        use iso_varying_string, only: extract
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(1:4), extract(example, -1, 4))
    end function

    pure function check_extract_character_without_finish() result(result_)
        use iso_varying_string, only: extract
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:), extract(example, 2))
    end function

    pure function check_extract_character_with_finish_gt_len_string() result(result_)
        use iso_varying_string, only: extract
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:), extract(example, 2, len(example) + 1))
    end function

    pure function check_extract_character_zero_length() result(result_)
        use iso_varying_string, only: extract
        use vegetables, only: result_t, assert_empty

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_empty(extract(example, 10, -2))
    end function

    pure function check_extract_string() result(result_)
        use iso_varying_string, only: extract, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:4), extract(var_str(example), 2, 4))
    end function

    pure function check_extract_string_without_start() result(result_)
        use iso_varying_string, only: extract, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(1:4), extract(var_str(example), finish = 4))
    end function

    pure function check_extract_string_with_start_lt_one() result(result_)
        use iso_varying_string, only: extract, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(1:4), extract(var_str(example), -1, 4))
    end function

    pure function check_extract_string_without_finish() result(result_)
        use iso_varying_string, only: extract, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:), extract(var_str(example), 2))
    end function

    pure function check_extract_string_with_finish_gt_len_string() result(result_)
        use iso_varying_string, only: extract, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:), extract(var_str(example), 2, len(example) + 1))
    end function

    pure function check_extract_string_zero_length() result(result_)
        use iso_varying_string, only: extract, var_str
        use vegetables, only: result_t, assert_empty

        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_empty(extract(var_str(example), 10, -2))
    end function
end module
