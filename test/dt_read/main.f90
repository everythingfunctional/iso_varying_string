program dt_read
    use iso_varying_string, only: varying_string, operator(/=), operator(//), len

    implicit none

    type(varying_string) :: string

    read(*, '(DT)') string
    if (string /= "hello1 dt_read") then
        write(*, *) "expected 'hello1 dt_read' but got '" // string // "'"
        error stop
    end if
    read(*, '(DT(6))') string
    if (string /= "hello2") then
        write(*, *) "expected 'hello2' but got '" // string // "'"
        error stop
    end if
    read(*, '(DT(15))') string
    if (string /= "hello3 dt_read" .or. len(string) /= 15) then
        write(*, *) "expected 'hello3 dt_read ' but got '" // string // "'"
        error stop
    end if
end program