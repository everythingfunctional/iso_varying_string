program get_terminator
    use iso_varying_string, only: &
            VARYING_STRING, operator(//), operator(/=), get, put_line

    implicit none

    type(VARYING_STRING) :: string
    type(VARYING_STRING) :: separator

    call get(string, ", ", separator)
    if (string /= "hello") then
        call put_line("expected 'hello' but got '" // string // "'")
        call exit(1)
    end if
    if (separator /= ",") then
        call put_line("separator should have been ',' but was '" // separator // "'")
        call exit(1)
    end if
    call get(string, ", ", separator)
    if (string /= "get") then
        call put_line("expected 'get' but got '" // string // "'")
        call exit(1)
    end if
    if (separator /= " ") then
        call put_line("separator should have been ' ' but was '" // separator // "'")
        call exit(1)
    end if
    call get(string, ", ", separator)
    if (string /= "terminator") then
        call put_line("expected 'terminator' but got '" // string // "'")
        call exit(1)
    end if
    if (separator /= "") then
        call put_line("separator should have been empty, but was '" // separator // "'")
        call exit(1)
    end if
end program get_terminator