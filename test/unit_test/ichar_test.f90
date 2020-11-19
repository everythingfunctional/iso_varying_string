module ichar_test
    use custom_generator, only: CharacterInput_t, ASCII_CHARACTER_GENERATOR
    use iso_varying_string, only: ichar, var_str
    use Vegetables_m, only: &
            Input_t, &
            Result_t, &
            TestItem_t, &
            assertEquals, &
            describe, &
            fail, &
            it

    implicit none
    private

    public :: test_ichar
contains
    function test_ichar() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "works the same for characters and strings", &
                ASCII_CHARACTER_GENERATOR, &
                checkIchar)
        tests = describe("Sec. 3.4.5: ICHAR", individual_tests)
    end function test_ichar

    pure function checkIchar(char_) result(result_)
        class(Input_t), intent(in) :: char_
        type(Result_t) :: result_

        select type (char_)
        type is (CharacterInput_t)
            result_ = assertEquals( &
                    ichar(char_%value_), &
                    ichar(var_str(char_%value_)), &
                    char_%value_)
        class default
            result_ = fail("Expected to get a CharacterInput_t.")
        end select
    end function checkIchar
end module ichar_test
