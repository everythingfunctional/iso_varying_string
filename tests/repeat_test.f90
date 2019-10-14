module repeat_test
    implicit none
    private

    public :: test_repeat
contains
    function test_repeat() result(tests)
        use custom_generator, only: ASCII_STRING_AND_INTEGER_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "works the same for characters and strings", &
                ASCII_STRING_AND_INTEGER_GENERATOR, &
                checkRepeat)
        tests = describe( &
                "Sec. 3.4.13: REPEAT", individual_tests)
    end function test_repeat

    function checkRepeat(example) result(result_)
        use custom_generator, only: StringAndIntegerInput_t
        use ISO_VARYING_STRING, only: char, repeat
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example
        type(Result_t) :: result_

        select type (example)
        type is (StringAndIntegerInput_t)
            result_ = assertEquals( &
                    repeat(char(example%string), example%integer_), &
                    char(repeat(example%string, example%integer_)))
        class default
            result_ = fail("Expected to get a StringAndIntegerInput_t")
        end select
    end function checkRepeat
end module repeat_test
