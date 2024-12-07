# UTF-8 character array module

A Fortran module to convert a native character string to an array of UTF-8 characters that behaves as a UTF-8 string.

Procedures:

- chars_array : convert a native Fortran character string to an array of UTF-8 characters
- chars : convert an array of UTF-8 characters to a native Fortran character string
- substr : get a substring of an UTF-8 character array using start index and length (signed values)
- indexof : get the position from one UTF-8 character array into another
- str_equal : check if 2 UTF-8 character arrays have the same content
- str_replace : replace the first occurrence of a string
- str_replace_all : replace all occurrences of a string

UTF-8 is a variable length encoding. The module uses an array where each single or multi-byte UTF-8 character is one array element. Array manipulations can be used on this UTF-8 character array. To take advantage of the highly optimized array manipulations in Fortran.

Multiple code points for one character ("base character" plus "combining marks") are seen as separate characters.

Code files:

    src\string_utf_8.f90  : string_utf_8 module
    app\example.f90       : some examples using the module
    test\tests.f90        : unit testing for the module

The 'string_utf_8.f90' module file can directly be used in a project.

The project structure is ready to compile and run the example code and tests with [fpm](https://fpm.fortran-lang.org/). To run it on Windows with [gfortran](https://gcc.gnu.org/) 2 batch files are provided: run-example.bat and run-test.bat


### chars_array

Syntax: chars_array(chr)

Convert a native Fortran character string to an array of UTF-8 characters.  
Each character in the UTF-8 character array can be accessed with %char: variableName%char

~~~fortran
use string_utf_8
implicit none

character(:), allocatable :: s
type(char_utf), allocatable :: s_chars(:)
type(char_utf), allocatable :: sub_chars(:)

s = "Test éèçà string"

! Convert the character string to an array of UTF-8 characters
s_chars = chars_array(s)

! Get the first 4 characters (array section)
sub_chars = s_chars(1:4)
print *, ">" // chars(sub_chars) // "<" ! Output: >Test string<

! Get the 6th character = "é"
print *, s_chars(6)%char ! Get the 6th UTF-8 character
~~~


Loop each UTF-8 character in the UTF-8 character array

~~~fortran
use string_utf_8
implicit none

type(char_utf), allocatable :: s_chars(:)
integer :: i

s_chars = chars_array("Test éèçà string")

do i = 1, size(s_chars)
   print *, s_chars(i)%char! Print the UTF-8 character at position i
end do
~~~


### chars

Syntax: chars(str)

Convert an array of UTF-8 characters to a native Fortran character string.

~~~fortran
use string_utf_8
implicit none

character(:), allocatable :: s1
character(:), allocatable :: s2
type(char_utf), allocatable :: s_chars(:)

s1 = "Test string"

s_chars = chars_array(s1) ! Convert to array of UTF-8 characters
s2 = chars(s_chars) ! Convert back to character string

print *, ">" // s2 // "<" ! Output: >Test string<
~~~


### substr

Syntax:  
substr(str, start_index, length)  
substr(str, start_index)

Get a substring of a UTF-8 characters array 'str', beginning at character index 'start_index' and take 'length' characters.  
Negative numbers count backwards. 'start_index' backwards from the end of the string, length backwards from 'start_index'.  
If no length is given, take until end of string.  
Index of the first character is 1.  
If start_index exceeds the string boundary limits, return an empty string. (Similar to C++ std::substr() and c# String.Substring.)  
'str' and the return value are UTF-8 character arrays.

Examples:

s = chars_array("1234567890")

substr(s, 1, 3) => "123"  
substr(s, 8, 2) => "89"  
substr(s, -5, 3) => "678"  
substr(s, -5, -3) => "456"  
substr(s, 5, -3) => "345"  
substr(s, -4, 4) => "7890"

substr(s, -10, 3) => "123"  
substr(s, 1, 3) => "123"

Past string boundaries:

substr(s, 11, 2) => ""  
substr(s, 11, 1) => ""  
substr(s, 10, 2) => "0"

substr(s, 100, -100) => ""  
substr(s, -100, 100) => ""  
substr(s, -100, 1) => ""  
substr(s, 5, -100) => "12345"  
substr(s, 2, 100) => "234567890"  
substr(s, 100, 1) => ""  
substr(s, 100, -3) => ""

Only start index:

substr(s_chars, 1) => "1234567890"  
substr(s_chars, 5) => "567890"  
substr(s_chars, 10) => "0"  
substr(s_chars, -2) => "90"

substr(s_chars, 11) => ""  
substr(s_chars, -100) => ""

~~~fortran
use string_utf_8
implicit none

type(char_utf), allocatable :: s_chars(:)
type(char_utf), allocatable :: sub_chars(:)

s_chars = chars_array("Test string")

sub_chars = substr(s_chars, 1, 4)  ! Get the first 4 characters (as array of UTF-8 characters)
print *, ">" // chars(sub_chars) // "<" ! Output: >test<

sub_chars = substr(s_chars, -6, 6) ! Get the last 6 characters (as array of UTF-8 characters)
print *, ">" // chars(sub_chars) // "<" ! Output: >string<
sub_chars = substr(s_chars, -6) ! Same as previous
print *, ">" // chars(sub_chars) // "<" ! Output: >string<
~~~

Using array sections

~~~fortran
use string_utf_8
implicit none

type(char_utf), allocatable :: s_chars(:)
type(char_utf), allocatable :: sub_chars(:)

s_chars = chars_array("Test string")

sub_chars = substr(s_chars, 1, 4)  ! Get the first 4 characters (as array of UTF-8 characters).
print *, ">" // chars(sub_chars) // "<" ! Output: >test<

sub_chars = s_chars(1:4)  ! Same result with array sections.
print *, ">" // chars(sub_chars) // "<" ! Output: >test<

sub_chars = substr(s_chars, 3, 2) ! From position 3, take 2 characters.
print *, ">" // chars(sub_chars) // "<" ! Output: >st<

sub_chars = s_chars(3:4)  ! Same as previous with array sections. From position 3 till position 4.
print *, ">" // chars(sub_chars) // "<" ! Output: >st<

sub_chars = substr(s_chars, 3) ! From position 3, take till end of string.
print *, ">" // chars(sub_chars) // "<" ! Output: >st string<

sub_chars = s_chars(3:)  ! From position 3 till the end. With array sections.
print *, ">" // chars(sub_chars) // "<" ! Output: >st string<

sub_chars = substr(s_chars, -6) ! Take 6 last characters.
print *, ">" // chars(sub_chars) // "<" ! Output: >string<

sub_chars = s_chars(6:11)  ! From position 3 till the end. With array sections.
print *, ">" // chars(sub_chars) // "<" ! Output: >string<
~~~


### indexof

Syntax:  
indexof(str, searchstring, start_index)  
indexof(str, searchstring)

Get the index of the 'searchstring' in the string 'str'. Start checking from index 'start_index'.  
If no start_index is given, start from position 1.  
Return -1 if 'searchstring' is empty or if it is not found.  
'str' and 'searchstring' are UTF-8 character arrays.

~~~fortran
use string_utf_8
implicit none

character(:), allocatable :: s
type(char_utf), allocatable :: s_chars(:)
integer :: pos

s = "Test string éèçà test"

s_chars = chars_array(s) ! Convert to array of UTF-8 characters

pos = indexof(s_chars, chars_array("test"), 6)
print *, pos ! 18

pos = indexof(s_chars, chars_array("test"), 1) ! Case sensitive, same result.
print *, pos ! 18

pos = indexof(chars_array("Test éèçà list"), chars_array("list"), 1)
print *, pos ! 11

pos = indexof(chars_array("Test éèçà list"), chars_array("list"))
print *, pos ! 11
~~~


### str_equal

Syntax: str_equal(str1, str2)

Check if 2 UTF-8 character arrays have the same content.

~~~fortran
use string_utf_8
implicit none

character(:), allocatable :: s
type(char_utf), allocatable :: s_chars(:)

s = "Test string"

s_chars = chars_array(s) ! Convert to array of UTF-8 characters

if (str_equal(s_chars, chars_array("Test string"))) then
  print *, "The 2 strings are equal"
end if
~~~


### str_replace

Syntax:  
str_replace(str, str_old, str_new, start_index)  
str_replace(str, str_old, str_new)

In the string str, replace the first occurrence of str_old with str_new.
Start checking from start_index. If no start_index is given, start searching from position 1.
str, str_old, str_new and the return value are UTF-8 character arrays.

~~~fortran
use string_utf_8
implicit none

type(char_utf), allocatable :: s_chars(:)

s_chars = chars_array("test string test string été")

print *, chars(str_replace(s_chars, chars_array("test"), chars_array("***")))
! Output: *** string test string été

print *, chars(str_replace(s_chars, chars_array("test string"), chars_array("***")))
! Output: *** test string été

print *, chars(str_replace(s_chars, chars_array(" été"), chars_array("")))
! Output: test string test string
~~~



### str_replace_all

Syntax: str_replace_all(str, str_old, str_new)

Replace all occurences of str_old with str_new in UTF-8 character array str.
str, str_old, str_new and the return value are UTF-8 character arrays.

~~~fortran
use string_utf_8
implicit none

type(char_utf), allocatable :: s_chars(:)

s_chars = chars_array("test string test string été")

print *, chars(str_replace_all(s_chars, chars_array("test"), chars_array("***")))
! Output: *** string *** string été

print *, chars(str_replace_all(s_chars, chars_array("test string"), chars_array("***")))
! Output: *** *** été

print *, chars(str_replace_all(s_chars, chars_array("test string "), chars_array("***")))
! Output: ******été

print *, chars(str_replace_all(s_chars, chars_array(" été"), chars_array("")))
! Output: test string test string
~~~
