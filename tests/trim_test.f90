module trim_test
    implicit none
    private

    public :: test_trim
contains
    function test_trim() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests = it( &
                "works the same for characters and strings", &
                ASCII_STRING_GENERATOR, &
                checkTrim)
        tests = describe("Sec 3.4.15: TRIM", individual_tests)
    end function test_trim

    pure function checkTrim(string) result(result_)
        use ISO_VARYING_STRING, only: char, trim, var_str
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: string
        type(Result_t) :: result_

        select type (string)
        type is (character(len=*))
            result_ = assertEquals( &
                    trim(string), &
                    char(trim(var_str(string))))
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkTrim
end module trim_test
