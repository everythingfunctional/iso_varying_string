module replace_range_test
    implicit none
    private

    public :: &
            test_replace_character_in_character_range, &
            test_replace_character_in_string_range, &
            test_replace_string_in_character_range, &
            test_replace_string_in_string_range
contains
    function test_replace_character_in_character_range() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters in the copy of string between positions start" &
                // " and finish, including those at start and finish, are" &
                // " deleted and replaced by characters of substring.", &
                check_replace_character_in_character)
        individual_tests(2) = it( &
                "If start is less than one, the value one is used for start.", &
                check_replace_character_in_character_start_lt_one)
        individual_tests(3) = it( &
                "If finish is greater than len(string), the value len(string)" &
                // " is used for finish.", &
                check_replace_character_in_character_start_gt_end)
        individual_tests(4) = it( &
                "If finish is less than start, the characters of substring" &
                // " are inserted before the character at start and no" &
                // " characters are deleted.", &
                check_replace_character_in_character_start_gt_finish)
        tests = describe( &
                "Sec. 3.7.4: REPLACE in character with character in range", &
                individual_tests)
    end function

    function test_replace_character_in_string_range() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters in the copy of string between positions start" &
                // " and finish, including those at start and finish, are" &
                // " deleted and replaced by characters of substring.", &
                check_replace_character_in_string)
        individual_tests(2) = it( &
                "If start is less than one, the value one is used for start.", &
                check_replace_character_in_string_start_lt_one)
        individual_tests(3) = it( &
                "If finish is greater than len(string), the value len(string)" &
                // " is used for finish.", &
                check_replace_character_in_string_start_gt_end)
        individual_tests(4) = it( &
                "If finish is less than start, the characters of substring" &
                // " are inserted before the character at start and no" &
                // " characters are deleted.", &
                check_replace_character_in_string_start_gt_finish)
        tests = describe( &
                "Sec. 3.7.4: REPLACE in string with character in range", &
                individual_tests)
    end function

    function test_replace_string_in_character_range() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters in the copy of string between positions start" &
                // " and finish, including those at start and finish, are" &
                // " deleted and replaced by characters of substring.", &
                check_replace_string_in_character)
        individual_tests(2) = it( &
                "If start is less than one, the value one is used for start.", &
                check_replace_string_in_character_start_lt_one)
        individual_tests(3) = it( &
                "If finish is greater than len(string), the value len(string)" &
                // " is used for finish.", &
                check_replace_string_in_character_start_gt_end)
        individual_tests(4) = it( &
                "If finish is less than start, the characters of substring" &
                // " are inserted before the character at start and no" &
                // " characters are deleted.", &
                check_replace_string_in_character_start_gt_finish)
        tests = describe( &
                "Sec. 3.7.4: REPLACE in character with string in range", &
                individual_tests)
    end function

    function test_replace_string_in_string_range() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "The characters in the copy of string between positions start" &
                // " and finish, including those at start and finish, are" &
                // " deleted and replaced by characters of substring.", &
                check_replace_string_in_string)
        individual_tests(2) = it( &
                "If start is less than one, the value one is used for start.", &
                check_replace_string_in_string_start_lt_one)
        individual_tests(3) = it( &
                "If finish is greater than len(string), the value len(string)" &
                // " is used for finish.", &
                check_replace_string_in_string_start_gt_end)
        individual_tests(4) = it( &
                "If finish is less than start, the characters of substring" &
                // " are inserted before the character at start and no" &
                // " characters are deleted.", &
                check_replace_string_in_string_start_gt_finish)
        tests = describe( &
                "Sec. 3.7.4: REPLACE in string with string in range", &
                individual_tests)
    end function

    pure function check_replace_character_in_character() result(result_)
        use iso_varying_string, only: replace
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS CRAZY", &
                replace("THAT IS CRAZY", 6, 7, "WAS"))
    end function

    pure function check_replace_character_in_character_start_lt_one() result(result_)
        use iso_varying_string, only: replace
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "WAS CRAZY", &
                replace("THAT IS CRAZY", -1, 7, "WAS"))
    end function

    pure function check_replace_character_in_character_start_gt_end() result(result_)
        use iso_varying_string, only: replace
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS", &
                replace("THAT IS CRAZY", 6, 15, "WAS"))
    end function

    pure function check_replace_character_in_character_start_gt_finish() result(result_)
        use iso_varying_string, only: replace
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WASIS CRAZY", &
                replace("THAT IS CRAZY", 6, 1, "WAS"))
    end function

    pure function check_replace_character_in_string() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS CRAZY", &
                replace(var_str("THAT IS CRAZY"), 6, 7, "WAS"))
    end function

    pure function check_replace_character_in_string_start_lt_one() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "WAS CRAZY", &
                replace(var_str("THAT IS CRAZY"), -1, 7, "WAS"))
    end function

    pure function check_replace_character_in_string_start_gt_end() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS", &
                replace(var_str("THAT IS CRAZY"), 6, 15, "WAS"))
    end function

    pure function check_replace_character_in_string_start_gt_finish() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WASIS CRAZY", &
                replace(var_str("THAT IS CRAZY"), 6, 1, "WAS"))
    end function

    pure function check_replace_string_in_character() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS CRAZY", &
                replace("THAT IS CRAZY", 6, 7, var_str("WAS")))
    end function

    pure function check_replace_string_in_character_start_lt_one() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "WAS CRAZY", &
                replace("THAT IS CRAZY", -1, 7, var_str("WAS")))
    end function

    pure function check_replace_string_in_character_start_gt_end() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS", &
                replace("THAT IS CRAZY", 6, 15, var_str("WAS")))
    end function

    pure function check_replace_string_in_character_start_gt_finish() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WASIS CRAZY", &
                replace("THAT IS CRAZY", 6, 1, var_str("WAS")))
    end function

    pure function check_replace_string_in_string() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS CRAZY", &
                replace(var_str("THAT IS CRAZY"), 6, 7, var_str("WAS")))
    end function

    pure function check_replace_string_in_string_start_lt_one() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "WAS CRAZY", &
                replace(var_str("THAT IS CRAZY"), -1, 7, var_str("WAS")))
    end function

    pure function check_replace_string_in_string_start_gt_end() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS", &
                replace(var_str("THAT IS CRAZY"), 6, 15, var_str("WAS")))
    end function

    pure function check_replace_string_in_string_start_gt_finish() result(result_)
        use iso_varying_string, only: replace, var_str
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WASIS CRAZY", &
                replace(var_str("THAT IS CRAZY"), 6, 1, var_str("WAS")))
    end function
end module
