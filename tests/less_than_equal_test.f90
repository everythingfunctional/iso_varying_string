module less_than_equal_test
    implicit none
    private

    public :: test_less_than_equals
contains
    function test_less_than_equals() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringLessThanEqualString)
        individual_tests(2) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkCharacterLessThanEqualString)
        individual_tests(3) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringLessThanEqualCharacter)
        tests = describe( &
                "operator(<=) (Sec. 3.3.3) functions the same as for two characters for", &
                individual_tests)
    end function test_less_than_equals

    pure function checkStringLessThanEqualString(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(<=), char
        use Vegetables_m, only: Result_t, assertThat, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertThat( &
                    char(strings%first) <= char(strings%second) &
                    .eqv. strings%first <= strings%second)
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkStringLessThanEqualString

    pure function checkCharacterLessThanEqualString(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(<=), char
        use Vegetables_m, only: Result_t, assertThat, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertThat( &
                    char(strings%first) <= char(strings%second) &
                    .eqv. char(strings%first) <= strings%second)
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkCharacterLessThanEqualString

    pure function checkStringLessThanEqualCharacter(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(<=), char
        use Vegetables_m, only: Result_t, assertThat, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertThat( &
                    char(strings%first) <= char(strings%second) &
                    .eqv. strings%first <= char(strings%second))
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkStringLessThanEqualCharacter
end module less_than_equal_test
