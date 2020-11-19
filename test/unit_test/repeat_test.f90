module repeat_test
    use custom_generator, only: &
            StringAndIntegerInput_t, ASCII_STRING_AND_INTEGER_GENERATOR
    use ISO_VARYING_STRING, only: char, repeat
    use Vegetables_m, only: &
            Input_t, Result_t, TestItem_t, assertEquals, describe, fail, it

    implicit none
    private

    public :: test_repeat
contains
    function test_repeat() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "works the same for characters and strings", &
                ASCII_STRING_AND_INTEGER_GENERATOR, &
                checkRepeat)
        tests = describe( &
                "Sec. 3.4.13: REPEAT", individual_tests)
    end function test_repeat

    pure function checkRepeat(example) result(result_)
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