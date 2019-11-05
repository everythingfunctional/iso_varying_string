module assignment_test
    implicit none
    private

    public :: test_assignment
contains
    function test_assignment() result(tests)
        use ISO_VARYING_STRING ! To make the compiler happy
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
        tests = describe("Sec. 3.3.1: assignment", individual_tests)
    end function test_assignment

    function checkAssignCharacterToString(string) result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=), char
        use Vegetables_m, only: &
                Input_t, Result_t, StringInput_t, assertEquals, fail

        class(Input_t), intent(in) :: string
        type(Result_t) :: result_

        type(VARYING_STRING) :: assigned

        select type (string)
        type is (StringInput_t)
            assigned = char(string%value_)
            result_ = assertEquals( &
                    string%value_, &
                    assigned, &
                    "Where the variable is of type VARYING_STRING, the length" &
                    // " of the variable becomes that of the expression")
        class default
            result_ = fail("Expected to get a StringInput_t")
        end select
    end function checkAssignCharacterToString

    function checkAssignStringToString(string) result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use Vegetables_m, only: &
                Input_t, Result_t, StringInput_t, assertEquals, fail, succeed

        class(Input_t), intent(in) :: string
        type(Result_t) :: result_

        type(VARYING_STRING) :: assigned

        select type (string)
        type is (StringInput_t)
            assigned = string%value_
            result_ = assertEquals( &
                    string%value_, &
                    assigned, &
                    "Where the variable is of type VARYING_STRING, the length" &
                    // " of the variable becomes that of the expression")
        class default
            result_ = fail("Expected to get a StringInput_t")
        end select
    end function checkAssignStringToString

    function checkAssignToShorterCharacter() result(result_)
        use ISO_VARYING_STRING, only: assignment(=), var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=4) :: assigned

        assigned = var_str("EXAMPLE")
        result_ = assertEquals( &
                "EXAM", &
                assigned, &
                "if the expression string is longer than the declared length of" &
                // " the character variable, only the left-most characters are assigned.")
    end function checkAssignToShorterCharacter

    function checkAssignToLongerCharacter() result(result_)
        use ISO_VARYING_STRING, only: assignment(=), var_str
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=10) :: assigned

        assigned = var_str("EXAMPLE")
        result_ = assertEquals( &
                "EXAMPLE   ", &
                assigned, &
                "If the character variable is longer than that of the string" &
                // " expression, it is padded on the right with blanks.")
    end function checkAssigntoLongerCharacter
end module assignment_test
