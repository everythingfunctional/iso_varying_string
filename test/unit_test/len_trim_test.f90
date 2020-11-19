module len_trim_test
    use iso_varying_string, only: char, len_trim
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

    public :: test_len_trim
contains
    function test_len_trim() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkLenTrim)
        tests = describe("Sec 3.4.8: LEN_TRIM", individual_tests)
    end function test_len_trim

    pure function checkLenTrim(string) result(result_)
        class(Input_t), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (StringInput_t)
            result_ = assertEquals( &
                    len_trim(char(string%value_)), &
                    len_trim(string%value_), &
                    string%value_)
        class default
            result_ = fail("Expected to get a StringInput_t")
        end select
    end function checkLenTrim
end module len_trim_test
