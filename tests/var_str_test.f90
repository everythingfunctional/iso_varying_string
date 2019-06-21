module var_str_test
    implicit none
    private

    public :: test_var_str
contains
    function test_var_str() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "Converts an intrinsic fixed-length character value into the" &
                // " equivalent varying-length string value.", &
                ASCII_STRING_GENERATOR, &
                checkVarStr)
        tests = describe("VAR_STR (Sec. 3.5.1)", individual_tests)
    end function test_var_str

    pure function checkVarStr(string) result(result_)
        use ISO_VARYING_STRING, only: char, var_str
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (character(len=*))
            result_ = assertEquals( &
                    string, &
                    char(var_str(string)), &
                    "The result value is the same string of characters as the argument.")
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkVarStr
end module var_str_test
