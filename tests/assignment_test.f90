module assignment_test
    implicit none
    private

    public :: test_assignment
contains
    function test_assignment() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = it( &
                "can assign a character to a string", &
                ASCII_STRING_GENERATOR, &
                checkAssignCharacterToString)
        individual_tests(2) = it( &
                "can assign a string to a string", &
                ASCII_STRING_GENERATOR, &
                checkAssignStringToString)
        individual_tests(3) = it( &
                "can assign a string to a shorter character", &
                checkAssignToShorterCharacter)
        individual_tests(4) = it( &
                "can assign a string to a longer character", &
                checkAssignToLongerCharacter)
        tests = describe("assignment (Sec. 3.3.1)", individual_tests)
    end function test_assignment

    pure function checkAssignCharacterToString(string) result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)
        use Vegetables_m, only: Result_t, fail, succeed

        class(*), intent(in) :: string
        type(Result_t) :: result_

        type(VARYING_STRING) :: assigned

        select type (string)
        type is (character(len=*))
            assigned = string
            result_ = succeed( &
                    "Where the variable is of type VARYING_STRING, the length" &
                    // " of the variable becomes that of the expression")
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkAssignCharacterToString

    pure function checkAssignStringToString(string) result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)
        use Vegetables_m, only: Result_t, fail, succeed

        class(*), intent(in) :: string
        type(Result_t) :: result_

        type(VARYING_STRING) :: first
        type(VARYING_STRING) :: second

        select type (string)
        type is (character(len=*))
            first = string
            second = first
            result_ = succeed( &
                    "Where the variable is of type VARYING_STRING, the length" &
                    // " of the variable becomes that of the expression")
        class default
            result_ = fail("Expected to get a character")
        end select
    end function checkAssignStringToString

    pure function checkAssignToShorterCharacter() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        character(len=4) :: assigned

        string = "EXAMPLE"
        assigned = string
        result_ = assertEquals( &
                "EXAM", &
                assigned, &
                "if the expression string is longer than the declared length of" &
                // " the character variable, only the left-most characters are assigned.")
    end function checkAssignToShorterCharacter

    pure function checkAssignToLongerCharacter() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: string
        character(len=10) :: assigned

        string = "EXAMPLE"
        assigned = string
        result_ = assertEquals( &
                "EXAMPLE   ", &
                assigned, &
                "If the character variable is longer than that of the string" &
                // " expression, it is padded on the right with blanks.")
    end function checkAssigntoLongerCharacter
end module assignment_test
