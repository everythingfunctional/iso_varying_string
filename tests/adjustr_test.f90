module adjustr_test
    implicit none
    private

    public :: test_adjustr
contains
    function test_adjustr() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkAdjustr)
        tests = describe("Sec 3.4.2: ADJUSTR", individual_tests)
    end function test_adjustr

    pure function checkAdjustr(string) result(result_)
        use ISO_VARYING_STRING, only: adjustr, char, var_str
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (character(len=*))
            result_ = assertEquals( &
                    adjustr(string), &
                    char(adjustr(var_str(string))))
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkAdjustr
end module adjustr_test
