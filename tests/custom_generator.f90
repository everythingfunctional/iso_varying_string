module custom_generator
    use ISO_VARYING_STRING, only: VARYING_STRING
    use Vegetables_m, only: Generator_t

    implicit none
    private

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

    type, public :: StringAndInteger_t
        type(VARYING_STRING) :: string
        integer :: integer_
    end type StringAndInteger_t

    type, public :: StringPair_t
        type(VARYING_STRING) :: first
        type(VARYING_STRING) :: second
    end type StringPair_t

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

        associate(a => self)
        end associate

        generated_value = Generated(getRandomAsciiCharacter())
    end function generateCharacter

    function generateStringAndInteger(self) result(generated_value)
        use ISO_VARYING_STRING, only: assignment(=)
        use Vegetables_m, only: &
                Generated_t, Generated, getRandomIntegerWithRange, getRandomAsciiString

        class(AsciiStringAndIntegerGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        type(StringAndInteger_t) :: pair

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

        type(StringPair_t) :: pair

        associate(a => self)
        end associate

        pair%first = getRandomAsciiString()
        pair%second = getRandomAsciiString()
        generated_value = Generated(pair)
    end function generateStringPair

    pure function shrinkCharacter(value_) result(shrunk)
        use Vegetables_m, only: ShrinkResult_t, SimplestValue

        class(*), intent(in) :: value_
        class(ShrinkResult_t), allocatable :: shrunk

        select type (value_)
        type is (character(len=*))
            allocate(shrunk, source = SimplestValue(value_))
        end select
    end function shrinkCharacter

    pure function shrinkStringAndInteger(value_) result(shrunk)
        use ISO_VARYING_STRING, only: assignment(=), char, len
        use Vegetables_m, only: &
                ShrinkResult_t, &
                ShrunkValue, &
                SimplestValue

        class(*), intent(in) :: value_
        class(ShrinkResult_t), allocatable :: shrunk

        type(StringAndInteger_t) :: shrunk_value

        select type (value_)
        type is (StringAndInteger_t)
            if (value_%integer_ == 0) then
                if (len(value_%string) <= 1) then
                    shrunk_value%integer_ = 0
                    shrunk_value%string = ""
                    allocate(shrunk, source = SimplestValue(shrunk_value))
                else
                    shrunk_value%integer_ = 0
                    shrunk_value%string = char(value_%string, len(value_%string) - 1)
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                end if
            else
                if (len(value_%string) <= 1) then
                    shrunk_value%integer_ = value_%integer_ / 2
                    shrunk_value%string = ""
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                else
                    shrunk_value%integer_ = value_%integer_ / 2
                    shrunk_value%string = char(value_%string, len(value_%string) - 1)
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                end if
            end if
        end select
    end function shrinkStringAndInteger

    pure function shrinkStringPair(value_) result(shrunk)
        use ISO_VARYING_STRING, only: assignment(=), char, len
        use Vegetables_m, only: &
                ShrinkResult_t, &
                ShrunkValue, &
                SimplestValue

        class(*), intent(in) :: value_
        class(ShrinkResult_t), allocatable :: shrunk

        type(StringPair_t) :: shrunk_value

        select type (value_)
        type is (StringPair_t)
            if (len(value_%first) <= 1) then
                if (len(value_%second) <= 1) then
                    shrunk_value%first = ""
                    shrunk_value%second = ""
                    allocate(shrunk, source = SimplestValue(shrunk_value))
                else
                    shrunk_value%first = ""
                    shrunk_value%second = char(value_%second, len(value_%second) - 1)
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                end if
            else
                if (len(value_%second) <= 1) then
                    shrunk_value%first = char(value_%first, len(value_%first) - 1)
                    shrunk_value%second = ""
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                else
                    shrunk_value%first = char(value_%first, len(value_%first) - 1)
                    shrunk_value%second = char(value_%second, len(value_%second) - 1)
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                end if
            end if
        end select
    end function shrinkStringPair
end module custom_generator
