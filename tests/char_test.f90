module char_test
    implicit none
    private

    public :: test_char
contains
    function test_char() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(5)

        individual_tests(1) = it( &
                "converts a varying string to a character with the same length", &
                ASCII_STRING_GENERATOR, &
                checkCharWithoutLength)
        individual_tests(2) = it( &
                "converts a varying string to a shorter character", &
                checkCharWithShorterLength)
        individual_tests(3) = it( &
                "converts a varying string to a longer character", &
                checkCharWithLongerLength)
        individual_tests(4) = it( &
                "gives a zero length character for length = 0", &
                checkCharWithZeroLength)
        individual_tests(5) = it( &
                "gives a zero length character for negative length", &
                checkCharWithNegativeLength)
        tests = describe("Sec. 3.4.3: CHAR", individual_tests)
    end function test_char

    pure function checkCharWithoutLength(example) result(result_)
        use ISO_VARYING_STRING, only: char, var_str
        use Vegetables_m, only: Result_t, assertEquals, fail

        class(*), intent(in) :: example
        type(Result_t) :: result_

        select type (example)
        type is (character(len=*))
            result_ = assertEquals( &
                    example, &
                    char(var_str(example)), &
                    "If length is absent, the result is a copy of the" &
                    // " characters in the argument string")
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkCharWithoutLength

    pure function checkCharWithShorterLength() result(result_)
        use ISO_VARYING_STRING, only: char, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "EXAM", &
                char(var_str("EXAMPLE"), 4), &
                "If string is longer than length, result is truncated on the right.")
    end function checkCharWithShorterLength

    pure function checkCharWithLongerLength() result(result_)
        use ISO_VARYING_STRING, only: char, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                "EXAMPLE   ", &
                char(var_str("EXAMPLE"), 10), &
                "If string is shorter than length, the result is padded on the" &
                // " right with blanks.")
    end function checkCharWithLongerLength

    pure function checkCharWithZeroLength() result(result_)
        use ISO_VARYING_STRING, only: char, var_str
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        result_ = assertEmpty( &
                char(var_str("EXAMPLE"), 0), &
                "If length is less than one, the result is of zero length.")
    end function checkCharWithZeroLength

    pure function checkCharWithNegativeLength() result(result_)
        use ISO_VARYING_STRING, only: char, var_str
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        result_ = assertEmpty( &
                char(var_str("EXAMPLE"), -1), &
                "If length is less than one, the result is of zero length.")
    end function checkCharWithNegativeLength
end module char_test
