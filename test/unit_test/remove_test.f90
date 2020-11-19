module remove_test
    use ISO_VARYING_STRING, only: char, remove, var_str
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_remove_character, test_remove_string
contains
    function test_remove_character() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string between positions start and finish, inclusive.", &
                checkRemoveCharacter)
        individual_tests(2) = it( &
                "If start is absent, the value one is used for start.", &
                checkRemoveCharacterWithoutStart)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start.", &
                checkRemoveCharacterWithStartLTOne)
        individual_tests(4) = it( &
                "If finish is absent, the value LEN(string) is used for finish.", &
                checkRemoveCharacterWithoutFinish)
        individual_tests(5) = it( &
                "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                checkRemoveCharacterWithFinishGTLenString)
        individual_tests(6) = it( &
                "If finish is less than start, the characters of string are delivered unchanged.", &
                checkRemoveCharacterZeroLength)
        tests = describe("Sec. 3.7.3 REMOVE character", individual_tests)
    end function test_remove_character

    function test_remove_string() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "The result value is a copy of the characters of the argument" &
                // " string between positions start and finish, inclusive.", &
                checkRemoveString)
        individual_tests(2) = it( &
                "If start is absent, the value one is used for start.", &
                checkRemoveStringWithoutStart)
        individual_tests(3) = it( &
                "If start is less than one, the value one is used for start.", &
                checkRemoveStringWithStartLTOne)
        individual_tests(4) = it( &
                "If finish is absent, the value LEN(string) is used for finish.", &
                checkRemoveStringWithoutFinish)
        individual_tests(5) = it( &
                "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                checkRemoveStringWithFinishGTLenString)
        individual_tests(6) = it( &
                "If finish is less than start, the characters of string are delivered unchanged.", &
                checkRemoveStringZeroLength)
        tests = describe("Sec. 3.7.3 REMOVE string", individual_tests)
    end function test_remove_string

    pure function checkRemoveCharacter() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("EPLE", char(remove("EXAMPLE", 2, 4)))
    end function checkRemoveCharacter

    pure function checkRemoveCharacterWithoutStart() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("PLE", char(remove("EXAMPLE", finish = 4)))
    end function checkRemoveCharacterWithoutStart

    pure function checkRemoveCharacterWithStartLTOne() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("PLE", char(remove("EXAMPLE", -1, 4)))
    end function checkRemoveCharacterWithStartLTOne

    pure function checkRemoveCharacterWithoutFinish() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("E", char(remove("EXAMPLE", 2)))
    end function checkRemoveCharacterWithoutFinish

    pure function checkRemoveCharacterWithFinishGTLenString() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("E", char(remove("EXAMPLE", 2, 8)))
    end function checkRemoveCharacterWithFinishGTLenString

    pure function checkRemoveCharacterZeroLength() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("EXAMPLE", char(remove("EXAMPLE", 10, -2)))
    end function checkRemoveCharacterZeroLength

    pure function checkRemoveString() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("EPLE", char(remove(var_str("EXAMPLE"), 2, 4)))
    end function checkRemoveString

    pure function checkRemoveStringWithoutStart() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("PLE", char(remove(var_str("EXAMPLE"), finish = 4)))
    end function checkRemoveStringWithoutStart

    pure function checkRemoveStringWithStartLTOne() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("PLE", char(remove(var_str("EXAMPLE"), -1, 4)))
    end function checkRemoveStringWithStartLTOne

    pure function checkRemoveStringWithoutFinish() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("E", char(remove(var_str("EXAMPLE"), 2)))
    end function checkRemoveStringWithoutFinish

    pure function checkRemoveStringWithFinishGTLenString() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("E", char(remove(var_str("EXAMPLE"), 2, 8)))
    end function checkRemoveStringWithFinishGTLenString

    pure function checkRemoveStringZeroLength() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("EXAMPLE", char(remove(var_str("EXAMPLE"), 10, -2)))
    end function checkRemoveStringZeroLength
end module remove_test