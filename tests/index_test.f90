module index_test
    implicit none
    private

    public :: test_index
contains
    function test_index() result(tests)
        use custom_generator, only: ASCII_STRING_PAIR_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "two strings", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkIndexStrings)
        individual_tests(2) = it( &
                "a string and a character", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkIndexStringAndCharacter)
        individual_tests(3) = it( &
                "a character and a string", &
                ASCII_STRING_PAIR_GENERATOR, &
                checkIndexCharacterAndString)
        tests = describe( &
                "Sec. 3.4.6: INDEX functions the same as for two characters for", &
                individual_tests)
    end function test_index

    pure function checkIndexStrings(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(//), char, index
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = &
                assertEquals( &
                    index(char(strings%first), char(strings%second)), &
                    index(strings%first, strings%second), &
                    char('index("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    index(char(strings%first), char(strings%second), .false.), &
                    index(strings%first, strings%second, .false.), &
                    char('index("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    index(char(strings%first), char(strings%second), .true.), &
                    index(strings%first, strings%second, .true.), &
                    char('index("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkIndexStrings

    pure function checkIndexStringAndCharacter(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(//), char, index
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = &
                assertEquals( &
                    index(char(strings%first), char(strings%second)), &
                    index(strings%first, char(strings%second)), &
                    char('index("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    index(char(strings%first), char(strings%second), .false.), &
                    index(strings%first, char(strings%second), .false.), &
                    char('index("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    index(char(strings%first), char(strings%second), .true.), &
                    index(strings%first, char(strings%second), .true.), &
                    char('index("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkIndexStringAndCharacter

    pure function checkIndexCharacterAndString(strings) result(result_)
        use custom_generator, only: StringPair_t
        use ISO_VARYING_STRING, only: operator(//), char, index
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: strings
        type(Result_t) :: result_

        select type (strings)
        type is (StringPair_t)
            result_ = &
                assertEquals( &
                    index(char(strings%first), char(strings%second)), &
                    index(char(strings%first), strings%second), &
                    char('index("' // strings%first // '", "' // strings%second // '")')) &
                .and.assertEquals( &
                    index(char(strings%first), char(strings%second), .false.), &
                    index(char(strings%first), strings%second, .false.), &
                    char('index("' // strings%first // '", "' // strings%second // '", .false.)')) &
                .and.assertEquals( &
                    index(char(strings%first), char(strings%second), .true.), &
                    index(char(strings%first), strings%second, .true.), &
                    char('index("' // strings%first // '", "' // strings%second // '", .true.)'))
        class default
            result_ = fail("Expected to get a StringPair_t")
        end select
    end function checkIndexCharacterAndString
end module index_test
