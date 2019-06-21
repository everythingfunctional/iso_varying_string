module adjustl_test
    implicit none
    private

    public :: test_adjustl
contains
    function test_adjustl() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkAdjustl)
        tests = describe("Sec 3.4.1: ADJUSTL", individual_tests)
    end function test_adjustl

    pure function checkAdjustl(string) result(result_)
        use ISO_VARYING_STRING, only: adjustl, char, var_str
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (character(len=*))
            result_ = assertEquals( &
                    adjustl(string), &
                    char(adjustl(var_str(string))))
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkAdjustl
end module adjustl_test
