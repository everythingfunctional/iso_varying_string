module len_trim_test
    implicit none
    private

    public :: test_len_trim
contains
    function test_len_trim() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkAdjustl)
        tests = describe("Sec 3.4.8: LEN_TRIM", individual_tests)
    end function test_len_trim

    pure function checkAdjustl(string) result(result_)
        use ISO_VARYING_STRING, only: len_trim, var_str
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (character(len=*))
            result_ = assertEquals( &
                    int(len_trim(string)), &
                    len_trim(var_str(string)), &
                    string)
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkAdjustl
end module len_trim_test
