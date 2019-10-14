module lle_test
    implicit none
    private

    public :: test_lle
contains
    function test_lle() result(tests)
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
                "Sec. 3.4.11: LLE functions the same as for two characters for", &
                individual_tests)
    end function test_lle

    function checkStringLGEString(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, lle
        use Vegetables_m, only: Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    lle(char(strings%first), char(strings%second)) &
                    .eqv. lle(strings%first, strings%second), &
                    char('lle("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringLGEString

    function checkCharacterLGEString(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, lle
        use Vegetables_m, only: Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    lle(char(strings%first), char(strings%second)) &
                    .eqv. lle(char(strings%first), strings%second), &
                    char('lle("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkCharacterLGEString

    function checkStringLGECharacter(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, lle
        use Vegetables_m, only: Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    lle(char(strings%first), char(strings%second)) &
                    .eqv. lle(strings%first, char(strings%second)), &
                    char('lle("' // strings%first // '", "' // strings%second // '")'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringLGECharacter
end module lle_test
