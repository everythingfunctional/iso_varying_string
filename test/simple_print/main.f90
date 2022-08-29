program simple_print
    use iso_varying_string, only: varying_string, assignment(=)

    implicit none

    type(varying_string) :: string

    string = "hello from simple_print"
    print *, string
end program