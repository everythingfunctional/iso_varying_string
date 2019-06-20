module placeholder_test
    implicit none
    private

    public :: test_placeholder
contains
    function test_placeholder() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("succeeds", checkSucceed)
        tests = describe("a placeholder", individual_tests)
    end function test_placeholder

    pure function checkSucceed() result(result_)
        use Vegetables_m, only: Result_t, succeed

        type(Result_t) :: result_

        result_ = succeed("Successfully")
    end function checkSucceed
end module placeholder_test
