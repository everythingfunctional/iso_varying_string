module custom_generator
    use iso_varying_string, only: varying_string
    use vegetables, only: generator_t, input_t

    implicit none
    private
    public :: &
            character_input_t, &
            string_and_integer_input_t, &
            string_pair_input_t, &
            ascii_character_generator_t, &
            ascii_string_and_integer_generator_t, &
            ascii_string_pair_generator_t, &
            ASCII_CHARACTER_GENERATOR, &
            ASCII_STRING_AND_INTEGER_GENERATOR, &
            ASCII_STRING_PAIR_GENERATOR

    type, extends(input_t) :: character_input_t
        character(len=1) :: value_
    end type

    type, extends(input_t) :: string_and_integer_input_t
        type(varying_string) :: string
        integer :: integer_
    end type

    type, extends(input_t) :: string_pair_input_t
        type(varying_string) :: first
        type(varying_string) :: second
    end type

    type, extends(generator_t) :: ascii_character_generator_t
    contains
        private
        procedure, public :: generate => generate_character
        procedure, public, nopass :: shrink => shrink_character
    end type

    type, extends(generator_t) :: ascii_string_and_integer_generator_t
    contains
        private
        procedure, public :: generate => generate_string_and_integer
        procedure, public, nopass :: shrink => shrink_string_and_integer
    end type

    type, extends(generator_t) :: ascii_string_pair_generator_t
    contains
        private
        procedure, public :: generate => generate_string_pair
        procedure, public, nopass :: shrink => shrink_string_pair
    end type

    type(ascii_character_generator_t), parameter :: &
            ASCII_CHARACTER_GENERATOR = ascii_character_generator_t()
    type(ascii_string_and_integer_generator_t), parameter :: &
            ASCII_STRING_AND_INTEGER_GENERATOR = ascii_string_and_integer_generator_t()
    type(ascii_string_pair_generator_t), parameter ::  &
            ASCII_STRING_PAIR_GENERATOR = ascii_string_pair_generator_t()
contains
    function generate_character(self) result(generated_value)
        use vegetables, only: generated_t, generated, get_random_ascii_character

        class(ascii_character_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        type(character_input_t)  :: the_input

        associate(a => self)
        end associate

        the_input%value_ = get_random_ascii_character()

        generated_value = generated(the_input)
    end function

    function generate_string_and_integer(self) result(generated_value)
        use vegetables, only: &
                generated_t, &
                generated, &
                get_random_integer_with_range, &
                get_random_ascii_string

        class(ascii_string_and_integer_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        type(string_and_integer_input_t) :: pair

        associate(a => self)
        end associate

        pair%string = get_random_ascii_string()
        pair%integer_ = get_random_integer_with_range(0, 10)
        generated_value = generated(pair)
    end function

    function generate_string_pair(self) result(generated_value)
        use vegetables, only: generated_t, generated, get_random_ascii_string

        class(ascii_string_pair_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        type(string_pair_input_t) :: pair

        associate(a => self)
        end associate

        pair%first = get_random_ascii_string()
        pair%second = get_random_ascii_string()
        generated_value = generated(pair)
    end function

    pure function shrink_character(input) result(shrunk)
        use vegetables, only: input_t, shrink_result_t, simplest_value

        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        shrunk = simplest_value(input)
    end function

    pure function shrink_string_and_integer(input) result(shrunk)
        use iso_varying_string, only: assignment(=), char, len
        use vegetables, only: &
                input_t, shrink_result_t, shrunk_value, simplest_value

        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        type(string_and_integer_input_t) :: shrunk_value_

        select type (input)
        type is (string_and_integer_input_t)
            if (input%integer_ == 0) then
                if (len(input%string) <= 1) then
                    shrunk_value_%integer_ = 0
                    shrunk_value_%string = ""
                    shrunk = simplest_value(shrunk_value_)
                else
                    shrunk_value_%integer_ = 0
                    shrunk_value_%string = char(input%string, len(input%string) - 1)
                    shrunk = shrunk_value(shrunk_value_)
                end if
            else
                if (len(input%string) <= 1) then
                    shrunk_value_%integer_ = input%integer_ / 2
                    shrunk_value_%string = ""
                    shrunk = shrunk_value(shrunk_value_)
                else
                    shrunk_value_%integer_ = input%integer_ / 2
                    shrunk_value_%string = char(input%string, len(input%string) - 1)
                    shrunk = shrunk_value(shrunk_value_)
                end if
            end if
        end select
    end function

    pure function shrink_string_pair(input) result(shrunk)
        use iso_varying_string, only: assignment(=), char, len
        use vegetables, only: &
                input_t, shrink_result_t, shrunk_value, simplest_value

        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        type(string_pair_input_t) :: shrunk_value_

        select type (input)
        type is (string_pair_input_t)
            if (len(input%first) <= 1) then
                if (len(input%second) <= 1) then
                    shrunk_value_%first = ""
                    shrunk_value_%second = ""
                    shrunk = simplest_value(shrunk_value_)
                else
                    shrunk_value_%first = ""
                    shrunk_value_%second = char(input%second, len(input%second) - 1)
                    shrunk = shrunk_value(shrunk_value_)
                end if
            else
                if (len(input%second) <= 1) then
                    shrunk_value_%first = char(input%first, len(input%first) - 1)
                    shrunk_value_%second = ""
                    shrunk = shrunk_value(shrunk_value_)
                else
                    shrunk_value_%first = char(input%first, len(input%first) - 1)
                    shrunk_value_%second = char(input%second, len(input%second) - 1)
                    shrunk = shrunk_value(shrunk_value_)
                end if
            end if
        end select
    end function
end module
