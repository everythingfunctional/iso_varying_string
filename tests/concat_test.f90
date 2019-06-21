module concat_test
    implicit none
    private

    public :: test_concat
contains
    function test_concat() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkConcatStrings)
        individual_tests(2) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkConcatStringAndCharacter)
        individual_tests(3) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkConcatCharacterAndString)
        tests = describe( &
                "operator(//) (Sec. 3.3.2) functions the same as for two characters for", &
                individual_tests)
    end function test_concat

    pure function checkConcatStrings(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(//), char
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertEquals( &
                    char(strings%first) // char(strings%second), &
                    char(strings%first // strings%second))
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkConcatStrings

    pure function checkConcatStringAndCharacter(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(//), char
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertEquals( &
                    char(strings%first) // char(strings%second), &
                    char(strings%first // char(strings%second)))
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkConcatStringAndCharacter

    pure function checkConcatCharacterAndString(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(//), char
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = assertEquals( &
                    char(strings%first) // char(strings%second), &
                    char(char(strings%first) // strings%second))
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkConcatCharacterAndString
end module concat_test