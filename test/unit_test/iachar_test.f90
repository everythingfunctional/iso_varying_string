module iachar_test
    use custom_generator, only: CharacterInput_t, ASCII_CHARACTER_GENERATOR
    use iso_varying_string, only: iachar, var_str
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

    public :: test_iachar
contains
    function test_iachar() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "works the same for characters and strings", &
                ASCII_CHARACTER_GENERATOR, &
                checkIachar)
        tests = describe("Sec. 3.4.4: IACHAR", individual_tests)
    end function test_iachar

    pure function checkIachar(char_) result(result_)
        class(Input_t), intent(in) :: char_
        type(Result_t) :: result_

        select type (char_)
        type is (CharacterInput_t)
            result_ = assertEquals( &
                    iachar(char_%value_), &
                    iachar(var_str(char_%value_)), &
                    char_%value_)
        class default
            result_ = fail("Expected to get a CharacterInput_t.")
        end select
    end function checkIachar
end module iachar_test
