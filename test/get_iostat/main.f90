program get_iostat
    use iso_fortran_env, only: iostat_eor, iostat_end
    use iso_varying_string, only: VARYING_STRING, get, put_line

    integer :: stat
    type(VARYING_STRING) :: string

    call get(string, iostat = stat)
    if (stat /= iostat_eor) then
        call put_line("didn't get EOR")
        call exit(1)
    end if
    call get(string, iostat = stat)
    if (stat /= iostat_end) then
        call put_line("didn't get EOF")
        call exit(1)
    end if
end program get_iostat
