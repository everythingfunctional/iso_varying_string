module trim_test
    implicit none
    private

    public :: test_trim
contains
    function test_trim() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = It( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkTrim)
        tests = Describe("Sec 3.4.15: TRIM", individual_tests)
    end function test_trim

    function checkTrim(string) result(result_)
        use ISO_VARYING_STRING, only: char, trim, var_str
        use Vegetables_m, only: &
                Input_t, Result_t, StringInput_t, assertEquals, fail

        class(Input_t), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (StringInput_t)
            result_ = assertEquals( &
                    trim(char(string%value_)), &
                    trim(string%value_))
        class default
            result_ = fail("Expected to get a StringInput_t")
        end select
    end function checkTrim
end module trim_test
