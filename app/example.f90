!> Example code using module string_utf_8
!> Using chars_array, chars, substr, indexof, str_replace
program test_utf
   use string_utf_8
   implicit none

   ! The string to convert
   character(:), allocatable :: s
   ! Array of UTF-8 characters
   type(char_utf), allocatable :: s_chars(:)
   type(char_utf), allocatable :: sub_chars(:)
   type(char_utf), allocatable :: search_chars(:)
   ! A deferred-length character string
   character(:), allocatable :: byte_chars
   integer :: total_chars, i

   s = "abcéa😎aࠏa test list" ! ࠏ has 3 bytes, e0 a0 8f
   print *, "s          : ", s

   s_chars = chars_array(s) ! Convert to an array of UTF-8 characters
   total_chars = size(s_chars) ! Number of UTF-8 characters
   print *, "Total UTF-8 chars : ", total_chars ! 19

   ! Convert the array back to a character string
   byte_chars = chars(s_chars)
   print *, "chars      : ", byte_chars

   ! Concatenate and print all array characters without an intermediate variable
   print *, "chars      : ", chars(s_chars)

   ! Concatenate and print all array characters with implied-do
   print *, "Implied do : ", (s_chars(i)%char, i = 1, total_chars)

   ! ---------------------------------------------------------------------------

   ! Substring from array of characters
   print *
   print *, "Substring with array sections"
   sub_chars = s_chars(1:4) ! Get the first 4 characters (as array of UTF-8 characters)
   print *, "chars                               : ", chars(sub_chars)
   print *, "4th character                       : ", sub_chars(4)%char

   print *, "substr from character 1 to 4        : ", chars(substr(s_chars, 1, 4)) ! "abcé"
   print *, "array section from character 1 to 4 : ", chars(s_chars(1:4)) ! "abcé"
   print *, "substr from character, last 4       : ", chars(substr(s_chars, -4, 4)) ! "list"
   print *, "substr from character 16            : ", chars(substr(s_chars, 16)) ! "list"

   ! ---------------------------------------------------------------------------

   print *
   print *, "indexof"
   search_chars = chars_array("list")
   print *, "Search for                          : ", chars(search_chars)
   print *, "Result                              : ", indexof(s_chars, search_chars, 1) ! 16

   print *, "Result find 'éa'                    : ", indexof(s_chars, chars_array("éa"), 1) ! 4

   ! ---------------------------------------------------------------------------

   ! Compare 'list' with 'list'
   print *
   print *, "str_equal"
   print *, "Compare '", chars(search_chars), "' with 'list'"
   if (str_equal(search_chars, chars_array("list"))) then
      print *, "The 2 strings are equal"
   else
      print *, "The 2 strings are different"
   end if

   ! ---------------------------------------------------------------------------

   s_chars = chars_array("test string test string été")

   ! str_replace
   print *
   print *, "str_replace"
   print *, chars(str_replace(chars_array("Test 123 abcdef"), chars_array("123 "), chars_array("*****")))
   ! Output: Test *****abcdef
   print *, chars(str_replace(s_chars, chars_array("test"), chars_array("***")))
   ! Outlput: *** string test string été
   print *, chars(str_replace(s_chars, chars_array("test"), chars_array("***"), 8))
   ! Outlput: test string *** string été

   ! ---------------------------------------------------------------------------

   ! str_replace_all
   print *
   print *, "str_replace_all"
   print *, chars(str_replace_all(chars_array("Test123test123abcdef"), chars_array("123"), chars_array("****")))
   ! Output: Test****test****abcdef
   print *, chars(str_replace_all(s_chars, chars_array("test "), chars_array("***")))
   ! Output: ***string ***string été
end program test_utf
