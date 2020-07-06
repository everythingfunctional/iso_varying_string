program get_maxlen
    use iso_varying_string, only: &
            VARYING_STRING, operator(//), operator(/=), get, put_line

    implicit none

    type(VARYING_STRING) :: hello
    type(VARYING_STRING) :: remaining

    call get(hello, 5)
    if (hello /= "hello") then
        call put_line("expected 'hello' but got '" // hello // "'")
        call exit(1)
    end if
    call get(remaining)
    if (remaining /= " get_maxlen") then
        call put_line("expected ' get_maxlen' but got '" // remaining // "'")
        call exit(1)
    end if
end program get_maxlen
