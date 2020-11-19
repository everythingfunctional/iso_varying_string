module var_str_test
    use iso_varying_string, only: char, var_str
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

    public :: test_var_str
contains
    function test_var_str() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "Converts an intrinsic fixed-length character value into the" &
                // " equivalent varying-length string value.", &
                ASCII_STRING_GENERATOR, &
                checkVarStr)
        tests = describe("Sec. 3.5.1: VAR_STR", individual_tests)
    end function test_var_str

    pure function checkVarStr(string) result(result_)
        class(Input_t), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (StringInput_t)
            result_ = assertEquals( &
                    string%value_, &
                    var_str(char(string%value_)), &
                    "The result value is the same string of characters as the argument.")
        class default
            result_ = fail("Expected to get a StringInput_t")
        end select
    end function checkVarStr
end module var_str_test
