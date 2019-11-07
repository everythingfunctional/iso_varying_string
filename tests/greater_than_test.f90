module greater_than_test
    use custom_generator, only: StringPairInput_t, ASCII_STRING_PAIR_GENERATOR
    use ISO_VARYING_STRING, only: operator(//), operator(>), char
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

    public :: test_greater_than
contains
    function test_greater_than() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringGreaterThanString)
        individual_tests(2) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkCharacterGreaterThanString)
        individual_tests(3) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkStringGreaterThanCharacter)
        tests = describe( &
                "Sec. 3.3.3: operator(>) functions the same as for two characters for", &
                individual_tests)
    end function test_greater_than

    pure function checkStringGreaterThanString(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    char(strings%first) > char(strings%second) &
                    .eqv. strings%first > strings%second, &
                    char('"' // strings%first // '" > "' // strings%second // '"'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringGreaterThanString

    pure function checkCharacterGreaterThanString(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    char(strings%first) > char(strings%second) &
                    .eqv. char(strings%first) > strings%second, &
                    char('"' // strings%first // '" > "' // strings%second // '"'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkCharacterGreaterThanString

    pure function checkStringGreaterThanCharacter(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = assertThat( &
                    char(strings%first) > char(strings%second) &
                    .eqv. strings%first > char(strings%second), &
                    char('"' // strings%first // '" > "' // strings%second // '"'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkStringGreaterThanCharacter
end module greater_than_test
