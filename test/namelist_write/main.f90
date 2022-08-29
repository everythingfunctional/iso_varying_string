program namelist_write
    use iso_varying_string, only: varying_string, assignment(=)

    implicit none

    type(varying_string) :: string
    namelist /output/ string

    string = "hello from namelist_write"
    write(*, output)
end program