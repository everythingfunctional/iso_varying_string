! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use adjustl_test, only: &
                adjustl_adjustl => test_adjustl
        use adjustr_test, only: &
                adjustr_adjustr => test_adjustr
        use assignment_test, only: &
                assignment_assignment => test_assignment
        use char_test, only: &
                char_char => test_char
        use co_broadcast_test, only: &
                co_broadcast_co_broadcast => test_co_broadcast
        use concat_test, only: &
                concat_concat => test_concat
        use equal_test, only: &
                equal_equals => test_equals
        use extract_test, only: &
                extract_extract_character => test_extract_character, &
                extract_extract_string => test_extract_string
        use greater_than_equal_test, only: &
                greater_than_equal_greater_than_equals => test_greater_than_equals
        use greater_than_test, only: &
                greater_than_greater_than => test_greater_than
        use iachar_test, only: &
                iachar_iachar => test_iachar
        use ichar_test, only: &
                ichar_ichar => test_ichar
        use index_test, only: &
                index_index => test_index
        use insert_test, only: &
                insert_insert_character_into_character => test_insert_character_into_character, &
                insert_insert_character_into_string => test_insert_character_into_string, &
                insert_insert_string_into_character => test_insert_string_into_character, &
                insert_insert_string_into_string => test_insert_string_into_string
        use len_test, only: &
                len_len => test_len
        use len_trim_test, only: &
                len_trim_len_trim => test_len_trim
        use less_than_equal_test, only: &
                less_than_equal_less_than_equals => test_less_than_equals
        use less_than_test, only: &
                less_than_less_than => test_less_than
        use lge_test, only: &
                lge_lge => test_lge
        use lgt_test, only: &
                lgt_lgt => test_lgt
        use lle_test, only: &
                lle_lle => test_lle
        use llt_test, only: &
                llt_llt => test_llt
        use not_equal_test, only: &
                not_equal_not_equals => test_not_equals
        use remove_test, only: &
                remove_remove_character => test_remove_character, &
                remove_remove_string => test_remove_string
        use repeat_test, only: &
                repeat_repeat => test_repeat
        use replace_range_test, only: &
                replace_range_replace_character_in_character_range => test_replace_character_in_character_range, &
                replace_range_replace_character_in_string_range => test_replace_character_in_string_range, &
                replace_range_replace_string_in_character_range => test_replace_string_in_character_range, &
                replace_range_replace_string_in_string_range => test_replace_string_in_string_range
        use replace_start_test, only: &
                replace_start_replace_character_in_character_start => test_replace_character_in_character_start, &
                replace_start_replace_character_in_string_start => test_replace_character_in_string_start, &
                replace_start_replace_string_in_character_start => test_replace_string_in_character_start, &
                replace_start_replace_string_in_string_start => test_replace_string_in_string_start
        use replace_target_test, only: &
                replace_target_replace_character_with_character_in_character => &
                    test_replace_character_with_character_in_character, &
                replace_target_replace_character_with_character_in_string => &
                    test_replace_character_with_character_in_string, &
                replace_target_replace_character_with_string_in_character => &
                    test_replace_character_with_string_in_character, &
                replace_target_replace_character_with_string_in_string => &
                    test_replace_character_with_string_in_string, &
                replace_target_replace_string_with_character_in_character => &
                    test_replace_string_with_character_in_character, &
                replace_target_replace_string_with_character_in_string => &
                    test_replace_string_with_character_in_string, &
                replace_target_replace_string_with_string_in_character => &
                    test_replace_string_with_string_in_character, &
                replace_target_replace_string_with_string_in_string => &
                    test_replace_string_with_string_in_string
        use scan_test, only: &
                scan_scan => test_scan
        use split_character_set_test, only: &
                split_character_set_split_character => test_split_character
        use split_string_set_test, only: &
                split_string_set_split_character => test_split_character
        use trim_test, only: &
                trim_trim => test_trim
        use var_str_test, only: &
                var_str_var_str => test_var_str
        use verify_test, only: &
                verify_verify => test_verify
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(52)

        individual_tests(1) = adjustl_adjustl()
        individual_tests(2) = adjustr_adjustr()
        individual_tests(3) = assignment_assignment()
        individual_tests(4) = char_char()
        individual_tests(5) = co_broadcast_co_broadcast()
        individual_tests(6) = concat_concat()
        individual_tests(7) = equal_equals()
        individual_tests(8) = extract_extract_character()
        individual_tests(9) = extract_extract_string()
        individual_tests(10) = greater_than_equal_greater_than_equals()
        individual_tests(11) = greater_than_greater_than()
        individual_tests(12) = iachar_iachar()
        individual_tests(13) = ichar_ichar()
        individual_tests(14) = index_index()
        individual_tests(15) = insert_insert_character_into_character()
        individual_tests(16) = insert_insert_character_into_string()
        individual_tests(17) = insert_insert_string_into_character()
        individual_tests(18) = insert_insert_string_into_string()
        individual_tests(19) = len_len()
        individual_tests(20) = len_trim_len_trim()
        individual_tests(21) = less_than_equal_less_than_equals()
        individual_tests(22) = less_than_less_than()
        individual_tests(23) = lge_lge()
        individual_tests(24) = lgt_lgt()
        individual_tests(25) = lle_lle()
        individual_tests(26) = llt_llt()
        individual_tests(27) = not_equal_not_equals()
        individual_tests(28) = remove_remove_character()
        individual_tests(29) = remove_remove_string()
        individual_tests(30) = repeat_repeat()
        individual_tests(31) = replace_range_replace_character_in_character_range()
        individual_tests(32) = replace_range_replace_character_in_string_range()
        individual_tests(33) = replace_range_replace_string_in_character_range()
        individual_tests(34) = replace_range_replace_string_in_string_range()
        individual_tests(35) = replace_start_replace_character_in_character_start()
        individual_tests(36) = replace_start_replace_character_in_string_start()
        individual_tests(37) = replace_start_replace_string_in_character_start()
        individual_tests(38) = replace_start_replace_string_in_string_start()
        individual_tests(39) = replace_target_replace_character_with_character_in_character()
        individual_tests(40) = replace_target_replace_character_with_character_in_string()
        individual_tests(41) = replace_target_replace_character_with_string_in_character()
        individual_tests(42) = replace_target_replace_character_with_string_in_string()
        individual_tests(43) = replace_target_replace_string_with_character_in_character()
        individual_tests(44) = replace_target_replace_string_with_character_in_string()
        individual_tests(45) = replace_target_replace_string_with_string_in_character()
        individual_tests(46) = replace_target_replace_string_with_string_in_string()
        individual_tests(47) = scan_scan()
        individual_tests(48) = split_character_set_split_character()
        individual_tests(49) = split_string_set_split_character()
        individual_tests(50) = trim_trim()
        individual_tests(51) = var_str_var_str()
        individual_tests(52) = verify_verify()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
