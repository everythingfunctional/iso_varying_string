program simple_get
    use iso_varying_string, only: &
            VARYING_STRING, operator(//), operator(/=), get, put_line

    implicit none

    type(VARYING_STRING) :: string

    call get(string)
    if (string /= "hello simple_get") then
        call put_line("expected 'hello simple_get' but got '" // string // "'")
        call exit(1)
    end if
end program simple_get
