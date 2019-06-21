module equal_test
    implicit none
    private

    public :: test_equals
contains
    function test_equals() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringEqualString)
        individual_tests(2) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkCharacterEqualString)
        individual_tests(3) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringEqualCharacter)
        tests = describe( &
                "operator(==) (Sec. 3.3.3) functions the same as for two characters for", &
                individual_tests)
    end function test_equals

    pure function checkStringEqualString(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(==), char
        use Vegetables_m, only: Result_t, assertThat, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertThat( &
                    char(strings%first) == char(strings%second) &
                    .eqv. strings%first == strings%second)
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkStringEqualString

    pure function checkCharacterEqualString(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(==), char
        use Vegetables_m, only: Result_t, assertThat, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertThat( &
                    char(strings%first) == char(strings%second) &
                    .eqv. char(strings%first) == strings%second)
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkCharacterEqualString

    pure function checkStringEqualCharacter(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(==), char
        use Vegetables_m, only: Result_t, assertThat, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertThat( &
                    char(strings%first) == char(strings%second) &
                    .eqv. strings%first == char(strings%second))
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkStringEqualCharacter
end module equal_test