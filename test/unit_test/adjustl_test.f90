module adjustl_test
    use iso_varying_string, only: adjustl, char, var_str
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

    public :: test_adjustl
contains
    function test_adjustl() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkAdjustl)
        tests = describe("Sec 3.4.1: ADJUSTL", individual_tests)
    end function test_adjustl

    pure function checkAdjustl(string) result(result_)
        class(Input_t), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (StringInput_t)
            result_ = assertEquals( &
                    adjustl(char(string%value_)), &
                    adjustl(string%value_))
        class default
            result_ = fail("Expected to get a StringInput_t")
        end select
    end function checkAdjustl
end module adjustl_test
