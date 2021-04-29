module co_broadcast_test
    use iso_varying_string, only: varying_string, assignment(=), operator(//)
    use strff, only: to_string
    use vegetables, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_includes, &
            assert_that, &
            describe, &
            it

    implicit none
    private
    public :: test_co_broadcast
contains
    function test_co_broadcast() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "varying_string%broadcast()", &
                [ it("copies a string across images", check_broadcast) &
                , it( &
                        "fails when broadcasting a string that has not been defined", &
                        check_broadcast_undefined) &
                ])
    end function

    function check_broadcast() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EXPECTED = "Hello, coarrays!"
        type(varying_string) :: the_string

        if (this_image() == 1) then
            the_string = EXPECTED
        end if

        call the_string%co_broadcast(1)

        result_ = assert_equals(EXPECTED, the_string)
    end function

    function check_broadcast_undefined() result(result_)
        type(result_t) :: result_

        character(len=128) :: errmsg
        integer :: stat
        type(varying_string) :: the_string

        call the_string%co_broadcast(1, stat, errmsg)

        result_ = &
                assert_that( &
                        stat /= 0, &
                        "stat was:" // to_string(stat), &
                        "should have gotten a non-zero stat") &
                .and. assert_includes("undefined", errmsg)
    end function
end module
