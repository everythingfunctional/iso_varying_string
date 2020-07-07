program round_trip
    use iso_fortran_env, only: iostat_eor, iostat_end
    use iso_varying_string, only: &
            VARYING_STRING, &
            operator(//), &
            operator(/=), &
            get, &
            put, &
            put_line, &
            var_str

    integer :: file_unit
    type(VARYING_STRING) :: string
    type(VARYING_STRING) :: separator
    integer :: stat

    open(newunit = file_unit, status = "SCRATCH")

    call put(file_unit, "hello from simple_put")
    call put_line(file_unit, "")

    call put(file_unit, var_str("hello from string_put"))
    call put_line(file_unit, "")

    call put_line(file_unit, "hello from simple_put_line")

    call put_line(file_unit, var_str("hello from string_put_line"))

    call put_line(file_unit, "hello get_maxlen")

    call put_line(file_unit, "hello,get terminator")


    rewind(file_unit)

    call get(file_unit, string)
    if (string /= "hello from simple_put") then
        call put_line("expected 'hello from simple_put' but got '" // string // "'")
        call exit(1)
    end if

    call get(file_unit, string)
    if (string /= "hello from string_put") then
        call put_line("expected 'hello from string_put' but got '" // string // "'")
        call exit(1)
    end if

    call get(file_unit, string)
    if (string /= "hello from simple_put_line") then
        call put_line("expected 'hello from simple_put_line' but got '" // string // "'")
        call exit(1)
    end if

    call get(file_unit, string)
    if (string /= "hello from string_put_line") then
        call put_line("expected 'hello from string_put_line' but got '" // string // "'")
        call exit(1)
    end if

    call get(file_unit, string, 5)
    if (string /= "hello") then
        call put_line("expected 'hello' but got '" // string // "'")
        call exit(1)
    end if
    call get(file_unit, string)
    if (string /= " get_maxlen") then
        call put_line("expected ' get_maxlen' but got '" // string // "'")
        call exit(1)
    end if

    call get(file_unit, string, ", ", separator)
    if (string /= "hello") then
        call put_line("expected 'hello' but got '" // string // "'")
        call exit(1)
    end if
    if (separator /= ",") then
        call put_line("separator should have been ',' but was '" // separator // "'")
        call exit(1)
    end if
    call get(file_unit, string, ", ", separator)
    if (string /= "get") then
        call put_line("expected 'get' but got '" // string // "'")
        call exit(1)
    end if
    if (separator /= " ") then
        call put_line("separator should have been ' ' but was '" // separator // "'")
        call exit(1)
    end if
    call get(file_unit, string, ", ", separator)
    if (string /= "terminator") then
        call put_line("expected 'terminator' but got '" // string // "'")
        call exit(1)
    end if
    if (separator /= "") then
        call put_line("separator should have been empty, but was '" // separator // "'")
        call exit(1)
    end if

    call get(file_unit, string, iostat = stat)
    if (stat /= iostat_eor) then
        call put_line("didn't get EOR")
        call exit(1)
    end if
    call get(file_unit, string, iostat = stat)
    if (stat /= iostat_end) then
        call put_line("didn't get EOF")
        call exit(1)
    end if

    close(file_unit)
end program round_trip
