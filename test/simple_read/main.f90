program simple_read
    use iso_varying_string, only: varying_string, operator(/=), operator(//)

    implicit none

    type(varying_string) :: string

    read(*, *) string
    if (string /= "hello simple_read") then
        write(*, *) "expected 'hello simple_read' but got '" // string // "'"
        error stop
    end if
    read(*, *) string
    if (string /= "next_line") then
        write(*, *) "expected 'next_line' but got '" // string // "'"
        error stop
    end if
end program