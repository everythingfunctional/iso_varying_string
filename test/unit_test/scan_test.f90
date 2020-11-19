module scan_test
    use custom_generator, only: StringPairInput_t, ASCII_STRING_PAIR_GENERATOR
    use iso_varying_string, only: operator(//), char, scan
    use Vegetables_m, only: &
            Input_t, &
            Result_t, &
            TestItem_t, &
            assertEquals, &
            describe, &
            fail, &
            it

    implicit none
    private

    public :: test_scan
contains
    function test_scan() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkScanStrings)
        individual_tests(2) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkScanStringAndCharacter)
        individual_tests(3) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkScanCharacterAndString)
        tests = describe( &
                "Sec. 3.4.14: SCAN functions the same as for two characters for", &
                individual_tests)
    end function test_scan

    pure function checkScanStrings(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = &
                assertEquals( &
                    scan(char(strings%first), char(strings%second)), &
                    scan(strings%first, strings%second), &
                    char('scan("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    scan(char(strings%first), char(strings%second), .false.), &
                    scan(strings%first, strings%second, .false.), &
                    char('scan("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    scan(char(strings%first), char(strings%second), .true.), &
                    scan(strings%first, strings%second, .true.), &
                    char('scan("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkScanStrings

    pure function checkScanStringAndCharacter(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = &
                assertEquals( &
                    scan(char(strings%first), char(strings%second)), &
                    scan(strings%first, char(strings%second)), &
                    char('scan("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    scan(char(strings%first), char(strings%second), .false.), &
                    scan(strings%first, char(strings%second), .false.), &
                    char('scan("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    scan(char(strings%first), char(strings%second), .true.), &
                    scan(strings%first, char(strings%second), .true.), &
                    char('scan("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkScanStringAndCharacter

    pure function checkScanCharacterAndString(strings) result(result_)
        class(Input_t), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPairInput_t)
            result_ = &
                assertEquals( &
                    scan(char(strings%first), char(strings%second)), &
                    scan(char(strings%first), strings%second), &
                    char('scan("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    scan(char(strings%first), char(strings%second), .false.), &
                    scan(char(strings%first), strings%second, .false.), &
                    char('scan("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    scan(char(strings%first), char(strings%second), .true.), &
                    scan(char(strings%first), strings%second, .true.), &
                    char('scan("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPairInput_t")
        end select
    end function checkScanCharacterAndString
end module scan_test
