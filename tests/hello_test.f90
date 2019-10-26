module hello_test
  use fruit
  implicit none

contains

  subroutine test_hello
    call assert_equals(4, 4)
  end

  subroutine test_hello1
    call assert_equals(4, 6)
  end
end
