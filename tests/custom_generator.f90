module custom_generator
    use ISO_VARYING_STRING, only: VARYING_STRING
    use Vegetables_m, only: Generator_t, Input_t

    implicit none
    private

    type, public, extends(Input_t) :: CharacterInput_t
        character(len=1) :: value_
    end type CharacterInput_t

    type, public, extends(Input_t) :: StringAndIntegerInput_t
        type(VARYING_STRING) :: string
        integer :: integer_
    end type StringAndIntegerInput_t

    type, public, extends(Input_t) :: StringPairInput_t
        type(VARYING_STRING) :: first
        type(VARYING_STRING) :: second
    end type StringPairInput_t

    type, public, extends(Generator_t) :: AsciiCharacterGenerator_t
    contains
        private
        procedure, public :: generate => generateCharacter
        procedure, public, nopass :: shrink => shrinkCharacter
    end type AsciiCharacterGenerator_t

    type, public, extends(Generator_t) :: AsciiStringAndIntegerGenerator_t
    contains
        private
        procedure, public :: generate => generateStringAndInteger
        procedure, public, nopass :: shrink => shrinkStringAndInteger
    end type AsciiStringAndIntegerGenerator_t

    type, public, extends(Generator_t) :: AsciiStringPairGenerator_t
    contains
        private
        procedure, public :: generate => generateStringPair
        procedure, public, nopass :: shrink => shrinkStringPair
    end type AsciiStringPairGenerator_t

    type(AsciiCharacterGenerator_t), public, parameter :: &
            ASCII_CHARACTER_GENERATOR = AsciiCharacterGenerator_t()
    type(AsciiStringAndIntegerGenerator_t), public, parameter :: &
            ASCII_STRING_AND_INTEGER_GENERATOR = AsciiStringAndIntegerGenerator_t()
    type(AsciiStringPairGenerator_t), public, parameter ::  &
            ASCII_STRING_PAIR_GENERATOR = AsciiStringPairGenerator_t()
contains
    function generateCharacter(self) result(generated_value)
        use Vegetables_m, only: Generated_t, Generated, getRandomAsciiCharacter

        class(AsciiCharacterGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        type(CharacterInput_t)  :: the_input

        associate(a => self)
        end associate

        the_input%value_ = getRandomAsciiCharacter()

        generated_value = Generated(the_input)
    end function generateCharacter

    function generateStringAndInteger(self) result(generated_value)
        use ISO_VARYING_STRING, only: assignment(=)
        use Vegetables_m, only: &
                Generated_t, Generated, getRandomIntegerWithRange, getRandomAsciiString

        class(AsciiStringAndIntegerGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        type(StringAndIntegerInput_t) :: pair

        associate(a => self)
        end associate

        pair%string = getRandomAsciiString()
        pair%integer_ = getRandomIntegerWithRange(0, 10)
        generated_value = Generated(pair)
    end function generateStringAndInteger

    function generateStringPair(self) result(generated_value)
        use ISO_VARYING_STRING, only: assignment(=)
        use Vegetables_m, only: Generated_t, Generated, getRandomAsciiString

        class(AsciiStringPairGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        type(StringPairInput_t) :: pair

        associate(a => self)
        end associate

        pair%first = getRandomAsciiString()
        pair%second = getRandomAsciiString()
        generated_value = Generated(pair)
    end function generateStringPair

    function shrinkCharacter(input) result(shrunk)
        use Vegetables_m, only: Input_t, ShrinkResult_t, SimplestValue

        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        shrunk = SimplestValue(input)
    end function shrinkCharacter

    function shrinkStringAndInteger(input) result(shrunk)
        use ISO_VARYING_STRING, only: assignment(=), char, len
        use Vegetables_m, only: &
                Input_t, &
                ShrinkResult_t, &
                ShrunkValue, &
                SimplestValue

        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        type(StringAndIntegerInput_t) :: shrunk_value

        select type (input)
        type is (StringAndIntegerInput_t)
            if (input%integer_ == 0) then
                if (len(input%string) <= 1) then
                    shrunk_value%integer_ = 0
                    shrunk_value%string = ""
                    shrunk = SimplestValue(shrunk_value)
                else
                    shrunk_value%integer_ = 0
                    shrunk_value%string = char(input%string, len(input%string) - 1)
                    shrunk = ShrunkValue(shrunk_value)
                end if
            else
                if (len(input%string) <= 1) then
                    shrunk_value%integer_ = input%integer_ / 2
                    shrunk_value%string = ""
                    shrunk = ShrunkValue(shrunk_value)
                else
                    shrunk_value%integer_ = input%integer_ / 2
                    shrunk_value%string = char(input%string, len(input%string) - 1)
                    shrunk = ShrunkValue(shrunk_value)
                end if
            end if
        end select
    end function shrinkStringAndInteger

    function shrinkStringPair(input) result(shrunk)
        use ISO_VARYING_STRING, only: assignment(=), char, len
        use Vegetables_m, only: &
                Input_t, &
                ShrinkResult_t, &
                ShrunkValue, &
                SimplestValue

        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        type(StringPairInput_t) :: shrunk_value

        select type (input)
        type is (StringPairInput_t)
            if (len(input%first) <= 1) then
                if (len(input%second) <= 1) then
                    shrunk_value%first = ""
                    shrunk_value%second = ""
                    shrunk = SimplestValue(shrunk_value)
                else
                    shrunk_value%first = ""
                    shrunk_value%second = char(input%second, len(input%second) - 1)
                    shrunk = ShrunkValue(shrunk_value)
                end if
            else
                if (len(input%second) <= 1) then
                    shrunk_value%first = char(input%first, len(input%first) - 1)
                    shrunk_value%second = ""
                    shrunk = ShrunkValue(shrunk_value)
                else
                    shrunk_value%first = char(input%first, len(input%first) - 1)
                    shrunk_value%second = char(input%second, len(input%second) - 1)
                    shrunk = ShrunkValue(shrunk_value)
                end if
            end if
        end select
    end function shrinkStringPair
end module custom_generator
