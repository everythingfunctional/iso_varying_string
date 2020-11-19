module not_equal_test
    use custom_generator, only: StringPairInput_t, ASCII_STRING_PAIR_GENERATOR
    use iso_varying_string, only: operator(//), operator(/=), char
    use Vegetables_m, only: &
            Input_t, &
            Result_t, &
            TestItem_t, &
            assertThat, &
            describe, &
            fail, &
            it

    implicit none
    private

    public :: test_not_equals
contains
    function test_not_equals() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringNotEqualString)
        individual_tests(2) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkCharacterNotEqualString)
        individual_tests(3) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringNotEqualCharacter)
        tests = describe( &
                "Sec. 3.3.3: operator(/=) functions the same as for two characters for", &
                individual_tests)
    end function test_not_equals

    pure function checkStringNotEqualString(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    char(strings%first) /= char(strings%second) &
                    .eqv. strings%first /= strings%second, &
                    char('"' // strings%first // '" /= "' // strings%second // '"'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringNotEqualString

    pure function checkCharacterNotEqualString(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    char(strings%first) /= char(strings%second) &
                    .eqv. char(strings%first) /= strings%second, &
                    char('"' // strings%first // '" /= "' // strings%second // '"'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkCharacterNotEqualString

    pure function checkStringNotEqualCharacter(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    char(strings%first) /= char(strings%second) &
                    .eqv. strings%first /= char(strings%second), &
                    char('"' // strings%first // '" /= "' // strings%second // '"'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringNotEqualCharacter
end module not_equal_test
