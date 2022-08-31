program namelist_read
    use iso_varying_string, only: varying_string, operator(/=), operator(//)

    implicit none

    type(varying_string) :: string
    namelist /input/ string

    read(*, input)
    if (string /= "hello namelist_read") then
        write(*, *) "expected 'hello namelist_read' but got '" // string // "'"
        error stop
    end if
end program