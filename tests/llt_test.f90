module llt_test
    implicit none
    private

    public :: test_llt
contains
    function test_llt() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

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
                "Sec. 3.4.12: LLT functions the same as for two characters for", &
                individual_tests)
    end function test_llt

    function checkStringLGEString(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, llt
        use Vegetables_m, only: Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    llt(char(strings%first), char(strings%second)) &
                    .eqv. llt(strings%first, strings%second), &
                    char('llt("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringLGEString

    function checkCharacterLGEString(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, llt
        use Vegetables_m, only: Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    llt(char(strings%first), char(strings%second)) &
                    .eqv. llt(char(strings%first), strings%second), &
                    char('llt("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkCharacterLGEString

    function checkStringLGECharacter(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, llt
        use Vegetables_m, only: Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    llt(char(strings%first), char(strings%second)) &
                    .eqv. llt(strings%first, char(strings%second)), &
                    char('llt("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringLGECharacter
end module llt_test
