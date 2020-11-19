module adjustr_test
    use iso_varying_string, only: adjustr, char
    use Vegetables_m, only: &
            Input_t, &
            Result_t, &
            StringInput_t, &
            TestItem_t, &
            assertEquals, &
            describe, &
            fail, &
            it, &
            ASCII_STRING_GENERATOR

    implicit none
    private

    public :: test_adjustr
contains
    function test_adjustr() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkAdjustr)
        tests = describe("Sec 3.4.2: ADJUSTR", individual_tests)
    end function test_adjustr

    pure function checkAdjustr(string) result(result_)
        class(Input_t), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (StringInput_t)
            result_ = assertEquals( &
                    adjustr(char(string%value_)), &
                    adjustr(string%value_))
        class default
            result_ = fail("Expected to get a StringInput_t")
        end select
    end function checkAdjustr
end module adjustr_test
