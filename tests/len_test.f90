module len_test
    implicit none
    private

    public :: test_len
contains
    function test_len() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
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
        use ISO_VARYING_STRING, only: char, len
        use Vegetables_m, only: &
                Input_t, Result_t, StringInput_t, assertEquals, fail

        class(Input_t), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (StringInput_t)
            result_ = assertEquals( &
                    len(char(string%value_)), &
                    len(string%value_), &
                    string%value_)
        class default
            result_ = fail("Expected to get a StringInput_t")
        end select
    end function checkAdjustl
end module len_test
