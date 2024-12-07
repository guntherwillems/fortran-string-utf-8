!> Unit testing for the module string_utf_8
!> substr, indexof, str_equal
program tests
   use string_utf_8
   implicit none

   call test_substr
   call test_indexof
   call test_str_equal
   call test_str_replace
   call test_str_replace_all

contains

   !> Test substr
   !> substr(str, start_index, length)
   !> substr(str, start_index)
   subroutine test_substr
      type(char_utf), allocatable :: s_chars(:)

      s_chars = chars_array("1234567890")

      print *, "Test substr"

      ! substr
      if (chars(substr(s_chars, 1, 3)) /= "123") print *, "Error ", 1, 3
      if (chars(substr(s_chars, 2, 3)) /= "234") print *, "Error ", 2, 3
      if (chars(substr(s_chars, -5, 3)) /= "678") print *, "Error ", -5, 3
      if (chars(substr(s_chars, -5, -3)) /= "456") print *, "Error ", -5, -3
      if (chars(substr(s_chars, 5, -3)) /= "345") print *, "Error ", 5, -3
      if (chars(substr(s_chars, -4, 4)) /= "7890") print *, "Error ", -4, 4
      if (chars(substr(s_chars, 8, 2)) /= "89") print *, "Error ", 8, 2

      ! Past boundaries
      if (chars(substr(s_chars, 11, 2)) /= "") print *, "Error ", 11, 2
      if (chars(substr(s_chars, 11, 1)) /= "") print *, "Error ", 11, 1
      if (chars(substr(s_chars, 10, 2)) /= "0") print *, "Error ", 10, 2
      if (chars(substr(s_chars, 0, 2)) /= "") print *, "Error ", 0, 2 ! Index starts at 1

      ! Past boundaries
      if (chars(substr(s_chars, 100, -100)) /= "") print *, "Error ", 100, -100
      if (chars(substr(s_chars, -100, 100)) /= "") print *, "Error ", -100, 100
      if (chars(substr(s_chars, -100, 1)) /= "") print *, "Error ", -100, 1
      if (chars(substr(s_chars, 5, -100)) /= "12345") print *, "Error ", 5, -100
      if (chars(substr(s_chars, 2, 100)) /= "234567890") print *, "Error ", 2, 100
      if (chars(substr(s_chars, 100, 1)) /= "") print *, "Error ", 100, 1
      if (chars(substr(s_chars, 100, -3)) /= "") print *, "Error ", 100, -3

      if (chars(substr(s_chars, -10, 3)) /= "123") print *, "Error ", -10, 3
      if (chars(substr(s_chars, 1, 3)) /= "123") print *, "Error ", 1, 3 ! Same as previous

      ! Only start_index
      if (chars(substr(s_chars, 1)) /= "1234567890") print *, "Error ", 1
      if (chars(substr(s_chars, 5)) /= "567890") print *, "Error ", 5
      if (chars(substr(s_chars, 10)) /= "0") print *, "Error ", 10
      if (chars(substr(s_chars, -2)) /= "90") print *, "Error ", -2
      if (chars(substr(s_chars, 11)) /= "") print *, "Error ", 11 ! past boundary
      if (chars(substr(s_chars, -100)) /= "") print *, "Error ", -100 ! past boundary

      ! Empty string results
      if (chars(substr(s_chars, 2, 0)) /= "") print *, "Error ", 2, 0
      if (chars(substr(s_chars, 0, 0)) /= "") print *, "Error ", 0, 0
      if (chars(substr(s_chars, -4, 0)) /= "") print *, "Error ", -4, 0

      if (chars(substr(chars_array(""), 3, 2)) /= "") print *, "Error empty string", 3, 2
      if (chars(substr(chars_array(""), -3, 2)) /= "") print *, "Error empty string", -3, 2
      if (chars(substr(chars_array(""), -3, -2)) /= "") print *, "Error empty string", -3, -2

      ! Empty string results past boundaries
      if (chars(substr(chars_array(""), 100, -100)) /= "") print *, "Error empty string", 100, -100
      if (chars(substr(chars_array(""), -100, 100)) /= "") print *, "Error empty string", -100, 100
   end subroutine test_substr

   ! ---------------------------------------------------------------------------

   !> Test indexof
   !> indexof(str, searchstring[, start_index])
   subroutine test_indexof
      type(char_utf), allocatable :: s_chars(:)

      s_chars = chars_array("1234567890")

      print *, "Test indexof"

      ! indexof
      if (indexof(s_chars, chars_array("12"), 1) /= 1) print *, "Error indexof for 12 1"
      if (indexof(s_chars, chars_array("12"), 2) /= -1) print *, "Error indexof for 12 2"
      if (indexof(s_chars, chars_array("12"), 100) /= -1) print *, "Error indexof for 12 100"
      if (indexof(s_chars, chars_array("1"), 1) /= 1) print *, "Error indexof for 1 1"
      if (indexof(s_chars, chars_array("90"), 2) /= 9) print *, "Error indexof for 90 2"
      if (indexof(s_chars, chars_array("90")) /= 9) print *, "Error indexof for 90 2"
      if (indexof(s_chars, chars_array("1234567890"), 1) /= 1) print *, "Error indexof for 1234567890 1"
      if (indexof(s_chars, chars_array("test"), 2) /= -1) print *, "Error indexof for test 2"

      ! Without start_index
      if (indexof(s_chars, chars_array("12")) /= 1) print *, "Error indexof for 12"
      if (indexof(s_chars, chars_array("1")) /= 1) print *, "Error indexof for 1"
      if (indexof(s_chars, chars_array("5")) /= 5) print *, "Error indexof for 5"
      if (indexof(s_chars, chars_array("567")) /= 5) print *, "Error indexof for 567"
      if (indexof(s_chars, chars_array("test")) /= -1) print *, "Error indexof for test"
   end subroutine test_indexof

   ! ---------------------------------------------------------------------------

   !> Test str_equal
   !> str_equal(str1, str2)
   subroutine test_str_equal
      type(char_utf), allocatable :: s_chars(:)

      s_chars = chars_array("1234567890")

      print *, "Test str_equal"

      ! str_equal
      if (.not. str_equal(s_chars, chars_array("1234567890"))) print *, "Error str_equal 1 compare equal strings"
      if (.not. str_equal(chars_array(""), chars_array(""))) print *, "Error str_equal 2 compare empty strings"
      if (str_equal(s_chars, chars_array("1234567899"))) print *, "Error str_equal 3 different strings"
      if (str_equal(s_chars, chars_array("1"))) print *, "Error str_equal 4 different length strings"
   end subroutine test_str_equal

   ! ---------------------------------------------------------------------------

   !> Test str_replace
   !> str_replace(str, str_old, str_new[, start_index])
   subroutine test_str_replace
      type(char_utf), allocatable :: s_chars(:)

      s_chars = chars_array("test 123 éèçà abcdef")

      print *, "Test str_replace"

      if (chars(str_replace(s_chars, chars_array("123 éèçà"), chars_array("***"))) &
      & /= "test *** abcdef") print *, "Error str_replace test 1"

      if (chars(str_replace(s_chars, chars_array("test 123"), chars_array("***"))) &
      & /= "*** éèçà abcdef") print *, "Error str_replace test 2"

      if (chars(str_replace(s_chars, chars_array("éèçà abcdef"), chars_array("***"))) &
      & /= "test 123 ***") print *, "Error str_replace test 3"

      if (chars(str_replace(s_chars, chars_array("éèçà abcdef"), chars_array(""))) &
      & /= "test 123") print *, "Error str_replace test 4"

      if (chars(str_replace(s_chars, chars_array("èç"), chars_array(""))) &
      & /= "test 123 éà abcdef") print *, "Error str_replace test 5"

      if (chars(str_replace(s_chars, chars_array(""), chars_array("***"))) &
      & /= chars(s_chars)) print *, "Error str_replace test 6"

      if (chars(str_replace(s_chars, chars_array("test 123 éèçà abcdef"), chars_array(""))) &
      & /= "") print *, "Error str_replace test 7"

      if (chars(str_replace(chars_array(""), chars_array("123"), chars_array("***"))) &
      & /= "") print *, "Error str_replace test 8"

      if (chars(str_replace(s_chars, chars_array("èç"), chars_array(""), 7)) &
      & /= "test 123 éà abcdef") print *, "Error str_replace test 9"

      if (chars(str_replace(s_chars, chars_array("èç"), chars_array(""), 14)) &
      & /= chars(s_chars)) print *, "Error str_replace test 10"
   end subroutine test_str_replace

   ! ---------------------------------------------------------------------------

   !> Test str_replace_all
   !> str_replace_all(str, str_old, str_new)
   subroutine test_str_replace_all
      type(char_utf), allocatable :: s_chars(:)

      s_chars = chars_array("test 123 éèçà test 123 abcdef")

      print *, "Test str_replace_all"

      if (chars(str_replace_all(s_chars, chars_array("123"), chars_array("***"))) &
      & /= "test *** éèçà test *** abcdef") print *, "Error str_replace_all test 1"

      if (chars(str_replace_all(s_chars, chars_array("test 123"), chars_array("***"))) &
      & /= "*** éèçà *** abcdef") print *, "Error str_replace_all test 2"

      if (chars(str_replace_all(s_chars, chars_array("èç"), chars_array("***"))) &
      & /= "test 123 é***à test 123 abcdef") print *, "Error str_replace_all test 3"

      if (chars(str_replace_all(s_chars, chars_array("test "), chars_array(""))) &
      & /= "123 éèçà 123 abcdef") print *, "Error str_replace_all test 4"

      if (chars(str_replace_all(s_chars, chars_array(" abcdef"), chars_array(""))) &
      & /= "test 123 éèçà test 123") print *, "Error str_replace_all test 5"

      if (chars(str_replace_all(s_chars, chars_array("abcdef"), chars_array("***"))) &
      & /= "test 123 éèçà test 123 ***") print *, "Error str_replace_all test 6"
   end subroutine test_str_replace_all

end program tests
