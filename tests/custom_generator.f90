module custom_generator
    use ISO_VARYING_STRING, only: VARYING_STRING
    use Vegetables_m, only: Generator_t

    implicit none
    private

    type, public, extends(Generator_t) :: AsciiStringPairGenerator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type AsciiStringPairGenerator_t

    type, public :: StringPair_t
        type(VARYING_STRING) :: first
        type(VARYING_STRING) :: second
    end type StringPair_t

    type(AsciiStringPairGenerator_t), public, parameter ::  &
            ASCII_STRING_PAIR_GENERATOR = AsciiStringPairGenerator_t()
contains
    function generate(self) result(generated_value)
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
    end function generate

    pure function shrink(value_) result(shrunk)
        use ISO_VARYING_STRING, only: assignment(=), char
        use Vegetables_m, only: &
                ShrinkResult_t, &
                ShrunkValue, &
                SimplestValue

        class(*), intent(in) :: value_
        class(ShrinkResult_t), allocatable :: shrunk

        type(StringPair_t) :: shrunk_value

        select type (value_)
        type is (StringPair_t)
            if (len(char(value_%first)) <= 1) then
                if (len(char(value_%second)) <= 1) then
                    shrunk_value%first = ""
                    shrunk_value%second = ""
                    allocate(shrunk, source = SimplestValue(shrunk_value))
                else
                    shrunk_value%first = ""
                    shrunk_value%second = char(value_%second, len(char(value_%second)) - 1)
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                end if
            else
                if (len(char(value_%second)) <= 1) then
                    shrunk_value%first = char(value_%first, len(char(value_%first)) - 1)
                    shrunk_value%second = ""
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                else
                    shrunk_value%first = char(value_%first, len(char(value_%first)) - 1)
                    shrunk_value%second = char(value_%second, len(char(value_%second)) - 1)
                    allocate(shrunk, source = ShrunkValue(shrunk_value))
                end if
            end if
        end select
    end function shrink
end module custom_generator
