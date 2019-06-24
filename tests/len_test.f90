module len_test
    implicit none
    private

    public :: test_len
contains
    function test_len() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkAdjustl)
        tests = describe("Sec 3.4.7: LEN", individual_tests)
    end function test_len

    pure function checkAdjustl(string) result(result_)
        use ISO_VARYING_STRING, only: len, var_str
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (character(len=*))
            result_ = assertEquals( &
                    int(len(string)), &
                    len(var_str(string)), &
                    string)
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkAdjustl
end module len_test
