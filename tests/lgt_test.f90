module lgt_test
    use custom_generator, only: StringPairInput_t, ASCII_STRING_PAIR_GENERATOR
    use ISO_VARYING_STRING, only: operator(//), char, lgt
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

    public :: test_lgt
contains
    function test_lgt() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringLGEString)
        individual_tests(2) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkCharacterLGEString)
        individual_tests(3) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringLGECharacter)
        tests = describe( &
                "Sec. 3.4.10: LGT functions the same as for two characters for", &
                individual_tests)
    end function test_lgt

    pure function checkStringLGEString(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    lgt(char(strings%first), char(strings%second)) &
                    .eqv. lgt(strings%first, strings%second), &
                    char('lgt("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringLGEString

    pure function checkCharacterLGEString(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    lgt(char(strings%first), char(strings%second)) &
                    .eqv. lgt(char(strings%first), strings%second), &
                    char('lgt("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkCharacterLGEString

    pure function checkStringLGECharacter(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    lgt(char(strings%first), char(strings%second)) &
                    .eqv. lgt(strings%first, char(strings%second)), &
                    char('lgt("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringLGECharacter
end module lgt_test
