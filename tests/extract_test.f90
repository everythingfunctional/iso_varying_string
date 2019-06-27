module extract_test
    implicit none
    private

    public :: test_extract_character, test_extract_string
contains
    function test_extract_character() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string between positions start and finish, inclusive.", &
                checkExtractCharacter)
        individual_tests(2) = it( &
                "If start is absent, the value one is used for start.", &
                checkExtractCharacterWithoutStart)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start.", &
                checkExtractCharacterWithStartLTOne)
        individual_tests(4) = it( &
                "If finish is absent, the value LEN(string) is used for finish.", &
                checkExtractCharacterWithoutFinish)
        individual_tests(5) = it( &
                "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                checkExtractCharacterWithFinishGTLenString)
        individual_tests(6) = it( &
                "If finish is less than start, the result is a zero-length string.", &
                checkExtractCharacterZeroLength)
        tests = describe("Sec. 3.7.1 EXTRACT character", individual_tests)
    end function test_extract_character

    function test_extract_string() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string between positions start and finish, inclusive.", &
                checkExtractString)
        individual_tests(2) = it( &
                "If start is absent, the value one is used for start.", &
                checkExtractStringWithoutStart)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start.", &
                checkExtractStringWithStartLTOne)
        individual_tests(4) = it( &
                "If finish is absent, the value LEN(string) is used for finish.", &
                checkExtractStringWithoutFinish)
        individual_tests(5) = it( &
                "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                checkExtractStringWithFinishGTLenString)
        individual_tests(6) = it( &
                "If finish is less than start, the result is a zero-length string.", &
                checkExtractStringZeroLength)
        tests = describe("Sec. 3.7.1 EXTRACT string", individual_tests)
    end function test_extract_string

    pure function checkExtractCharacter() result(result_)
        use ISO_VARYING_STRING, only: char, extract
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(2:4), char(extract(example, 2, 4)))
    end function checkExtractCharacter

    pure function checkExtractCharacterWithoutStart() result(result_)
        use ISO_VARYING_STRING, only: char, extract
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(1:4), char(extract(example, finish = 4)))
    end function checkExtractCharacterWithoutStart

    pure function checkExtractCharacterWithStartLTOne() result(result_)
        use ISO_VARYING_STRING, only: char, extract
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(1:4), char(extract(example, -1, 4)))
    end function checkExtractCharacterWithStartLTOne

    pure function checkExtractCharacterWithoutFinish() result(result_)
        use ISO_VARYING_STRING, only: char, extract
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(2:), char(extract(example, 2)))
    end function checkExtractCharacterWithoutFinish

    pure function checkExtractCharacterWithFinishGTLenString() result(result_)
        use ISO_VARYING_STRING, only: char, extract
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(2:), char(extract(example, 2, len(example) + 1)))
    end function checkExtractCharacterWithFinishGTLenString

    pure function checkExtractCharacterZeroLength() result(result_)
        use ISO_VARYING_STRING, only: char, extract
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEmpty(char(extract(example, 10, -2)))
    end function checkExtractCharacterZeroLength

    pure function checkExtractString() result(result_)
        use ISO_VARYING_STRING, only: char, extract, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(2:4), char(extract(var_str(example), 2, 4)))
    end function checkExtractString

    pure function checkExtractStringWithoutStart() result(result_)
        use ISO_VARYING_STRING, only: char, extract, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(1:4), char(extract(var_str(example), finish = 4)))
    end function checkExtractStringWithoutStart

    pure function checkExtractStringWithStartLTOne() result(result_)
        use ISO_VARYING_STRING, only: char, extract, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(1:4), char(extract(var_str(example), -1, 4)))
    end function checkExtractStringWithStartLTOne

    pure function checkExtractStringWithoutFinish() result(result_)
        use ISO_VARYING_STRING, only: char, extract, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(2:), char(extract(var_str(example), 2)))
    end function checkExtractStringWithoutFinish

    pure function checkExtractStringWithFinishGTLenString() result(result_)
        use ISO_VARYING_STRING, only: char, extract, var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEquals(example(2:), char(extract(var_str(example), 2, len(example) + 1)))
    end function checkExtractStringWithFinishGTLenString

    pure function checkExtractStringZeroLength() result(result_)
        use ISO_VARYING_STRING, only: char, extract, var_str
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assertEmpty(char(extract(var_str(example), 10, -2)))
    end function checkExtractStringZeroLength
end module extract_test
