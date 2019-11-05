module verify_test
    implicit none
    private

    public :: test_verify
contains
    function test_verify() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkVerifyStrings)
        individual_tests(2) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkVerifyStringAndCharacter)
        individual_tests(3) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkVerifyCharacterAndString)
        tests = describe( &
                "Sec. 3.4.16: VERIFY functions the same as for two characters for", &
                individual_tests)
    end function test_verify

    pure function checkVerifyStrings(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, verify
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = &
                assertEquals( &
                    verify(char(strings%first), char(strings%second)), &
                    verify(strings%first, strings%second), &
                    char('verify("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    verify(char(strings%first), char(strings%second), .false.), &
                    verify(strings%first, strings%second, .false.), &
                    char('verify("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    verify(char(strings%first), char(strings%second), .true.), &
                    verify(strings%first, strings%second, .true.), &
                    char('verify("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkVerifyStrings

    pure function checkVerifyStringAndCharacter(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, verify
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = &
                assertEquals( &
                    verify(char(strings%first), char(strings%second)), &
                    verify(strings%first, char(strings%second)), &
                    char('verify("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    verify(char(strings%first), char(strings%second), .false.), &
                    verify(strings%first, char(strings%second), .false.), &
                    char('verify("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    verify(char(strings%first), char(strings%second), .true.), &
                    verify(strings%first, char(strings%second), .true.), &
                    char('verify("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkVerifyStringAndCharacter

    pure function checkVerifyCharacterAndString(strings) result(result_)
        use custom_generator, only: StringPairInput_t
        use ISO_VARYING_STRING, only: operator(//), char, verify
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = &
                assertEquals( &
                    verify(char(strings%first), char(strings%second)), &
                    verify(char(strings%first), strings%second), &
                    char('verify("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    verify(char(strings%first), char(strings%second), .false.), &
                    verify(char(strings%first), strings%second, .false.), &
                    char('verify("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    verify(char(strings%first), char(strings%second), .true.), &
                    verify(char(strings%first), strings%second, .true.), &
                    char('verify("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkVerifyCharacterAndString
end module verify_test
