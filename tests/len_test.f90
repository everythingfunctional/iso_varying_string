module len_test
    use ISO_VARYING_STRING, only: char, len
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

    public :: test_len
contains
    function test_len() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkLen)
        tests = describe("Sec 3.4.7: LEN", individual_tests)
    end function test_len

    pure function checkLen(string) result(result_)
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
    end function checkLen
end module len_test
