program dt_write
    use iso_varying_string, only: varying_string, assignment(=)

    implicit none

    type(varying_string) :: string

    string = "hello from dt_write"
    print '(DT)', string
    print '(DT(10))', string
    print '(DT(21))', string
end program