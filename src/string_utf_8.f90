! Module string_utf_8
! Author: Gunther Willems
! Started 10/2024
! License: MIT

!> Convert a native Fortran character string to an array of UTF-8 characters.
!> The module has manipulation procedures.

!> UTF-8 is a variable length encoding. The module uses an array where each single
!> or multi byte UTF-8 character is one array element.
!> This code assembles each Unicode codepoint that can consist of one to four code units (bytes).

!> Multiple code points for one character ("base character" plus "combining marks")
!> are seen as separate characters.

module string_utf_8
   implicit none
   private

   ! A derived type is used because this code will not work with different character string lengths:
   ! character(:), allocatable :: s1(:)
   ! Another option would be to use the fixed 4 byte ISO 10646 UCS-4 character type.
   ! Not supported by all Fortran compilers!? It could unnecessarily allocate much more memory.

   !> Derived type to represent a UTF-8 character with 1 to 4 bytes.
   type char_utf
      character(:), allocatable :: char
   end type char_utf

   public :: char_utf
   public :: chars_array, chars, substr, indexof, str_equal, calc_start_end, int_to_bit_string

contains

   !> Convert a Fortran character string to an array of UTF-8 characters.
   !> UTF-8 is a variable length encoding. Individual Unicode characters can vary
   !> in byte length, from 1 to 4 bytes.
   !> In bit notation, a character byte starting with 0 is a normal ASCII character.
   !> Bits from one byte starting with
   !>   110 (followed by 1 byte starting with 10) or
   !>   1110 (followed by 2 bytes starting with 10) or
   !>   11110 (followed by 3 bytes starting with 10)
   !> are a multi byte UTF-8 character.
   function chars_array(chr) result(chars_a)
      !> A native Fortran characters chring
      character(*), intent(in) :: chr
      !> An array of UTF-8 characters
      type(char_utf), allocatable :: chars_a(:)

      ! Prefixes (in decimal notation) that start a new UTF-8 character (bytes)
      ! Binary: 0 110 1110 11110
      integer, parameter :: char_start_prefixes(4) = (/ 0, 6, 14, 30 /)

      integer, allocatable :: utf8_prefixes(:) ! UTF-8 prefixes array from the char bytes
      integer :: i, bytes_total
      integer :: ichar       ! integer value of character
      integer :: chars_total ! Total UTF-8 characters (code points)
      integer :: char_pos    ! UTF-8 character position

      chars_total = 0
      char_pos = 0

      bytes_total = len(chr)
      allocate(utf8_prefixes(bytes_total))

      ! Count UTF-8 code points and create array with UTF-8 prefix bits
      do i = 1, bytes_total
         ! Can also be written with iand
         ! write(gchar, "(B8.8)") iand(iachar(s(i:1)), b'11100000')
         ! print *, iand(iachar(s(i:1)), b'11100000'), ' ', gchar

         ichar = iachar(chr(i:i))

         ! ibits counts position from right to left, length counts to the left
         if (ibits(ichar, 7, 1) == 0) then ! 0 ASCII character
            utf8_prefixes(i) = 0
            chars_total = chars_total + 1
         else if (ibits(ichar, 6, 2) == 2) then ! 10, continuation of a multibyte UTF-8 character
            utf8_prefixes(i) = 2
         else if (ibits(ichar, 5, 3) == 6) then ! 110, start of a UTF-8 2 bytes character
            utf8_prefixes(i) = 6
            chars_total = chars_total + 1
         else if (ibits(ichar, 4, 4) == 14) then ! 1110, start of a UTF-8 3 bytes character
            utf8_prefixes(i) = 14
            chars_total = chars_total + 1
         else if (ibits(ichar, 3, 5) == 30) then ! 11110, start of a UTF-8 4 bytes character
            utf8_prefixes(i) = 30
            chars_total = chars_total + 1
         end if
      end do

      ! Generate the UTF-8 characters array
      allocate(chars_a(chars_total))

      do i = 1, bytes_total
         ! Code point if byte starts with (binary): 0 110 1110 11110
         if (findloc(char_start_prefixes, utf8_prefixes(i), 1) /= 0) then
            char_pos = char_pos + 1
            chars_a(char_pos)%char = chr(i:i)
         else
            chars_a(char_pos)%char = chars_a(char_pos)%char // chr(i:i)
         end if
      end do
   end function chars_array

   ! ---------------------------------------------------------------------------

   !> Convert a UTF-8 characters array 'str' to native Fortran character string.
   function chars(str)
      !> Array of UTF-8 characters
      type(char_utf), intent(in) :: str(:)
      !> Returns a native Fortran character string
      character(:), allocatable :: chars

      integer :: total_chars, i

      total_chars = size(str)

      chars = ""
      do i = 1, total_chars
         chars = chars // str(i)%char
      end do
   end function chars

   ! ---------------------------------------------------------------------------

   !> Get a substring of a UTF-8 characters array 'str', beginning at character index 'start_index'
   !> and take 'length' characters. Index of the first character is 1.
   !> Negative numbers count backwards. 'start_index' backwards from the end of the string,
   !> length backwards from 'start_index'. If no length is given, take until end of string from 'start_index'.
   !> If start_index exceeds the string boundary limits, return an empty string.
   !> (Similar to C++ std::substr() and c# String.Substring.)
   !> 'str' and the return value are UTF-8 character arrays.
   function substr(str, start_index, length)
      type(char_utf), intent(in) :: str(:)
      integer, intent(in) :: start_index
      integer, intent(in), optional :: length
      type(char_utf), allocatable :: substr(:)

      integer :: length1
      integer :: pos_start
      integer :: pos_end
      integer :: str_len

      str_len = size(str)

      if (present(length)) then
         length1 = length
      else
         length1 = str_len
      end if

      call calc_start_end(str_len, start_index, length1, pos_start, pos_end)

      substr = str(pos_start:pos_end)
   end function substr

   ! ---------------------------------------------------------------------------

   !> Get the index of the 'searchstring' in the string 'str'. Start checking from index 'start_index'.
   !> Return -1 if 'searchstring' is empty or it is not found.
   !> 'str' and 'searchstring' are UTF-8 character arrays.
   function indexof(str, searchstring, start_index)
      type(char_utf), intent(in) :: str(:)
      type(char_utf), intent(in) :: searchstring(:)
      integer, intent(in) :: start_index
      integer :: indexof

      integer :: match_count ! How many characters match from the search position
      integer :: search_len  ! Lenght search string
      integer :: str_len     ! Length given string
      integer :: next_index  ! Next search index
      integer :: i, j

      str_len = size(str)
      search_len = size(searchstring)
      if (search_len == 0 .or. start_index < 1 .or. start_index > str_len) then
         indexof = -1
         return
      end if

      do i = start_index, size(str)
         ! First character tested with if statement. Faster than starting a do loop.
         if (str(i)%char == searchstring(1)%char) then
            match_count = 1
            do j = 2, search_len
               next_index = i + j -1
               ! gfortran stops if first evaluation before .or. is true
               if (next_index > str_len .or. str(next_index)%char /= searchstring(j)%char) then
                  exit
               end if
               match_count = match_count + 1
            end do
            if (match_count == search_len) then
               indexof = i
               return
            end if
         end if
      end do

      indexof = -1
   end function indexof

   ! ---------------------------------------------------------------------------

   !> Check if the 2 given UTF-8 character arrays are the same. Returns .true. or .false.
   function str_equal(str1, str2)
      type(char_utf), intent(in) :: str1(:)
      type(char_utf), intent(in) :: str2(:)
      logical :: str_equal

      integer length, i

      length = size(str1)
      if (length /= size(str2)) then
         str_equal = .false.
         return
      end if

      do i = 1, length
         if (str1(i)%char /= str2(i)%char) then
            str_equal = .false.
            return
         end if
      end do

      str_equal = .true.
   end function str_equal

   ! ---------------------------------------------------------------------------

   !> Convert an integer value to corresponding bits (binary). Return as a character string.
   !> int_to_bit_string(iachar('a')) => "01100001" (= 97 decimal)
   function int_to_bit_string(int)
      integer, intent(in) :: int
      character(8) :: int_to_bit_string ! For one byte in bit notation

      write(int_to_bit_string, "(B8.8)") int
   end function int_to_bit_string

   ! ---------------------------------------------------------------------------

   !> Theoretically calculate the start and end position within a string with a total of length characters.
   !> Numbers can be negative to count from the end of the string (start_index) and backwards (length).
   !> total_length = total length of the string.
   !> start_index = start index (can be negative).
   !> length = how many characters to count from the start_index (can be negative for backwards).
   !> If start_index exceeds the string boundary limits, return an empty string.
   !> (Similar to C++ std::substr() and c# String.Substring.)
   !> If length is 0, start at position 1 and end equals to 0.
   subroutine calc_start_end(total_length, start_index, length, pos_start, pos_end)
      integer, intent(in) :: total_length
      integer, intent(in) :: start_index
      integer, intent(in) :: length
      integer, intent(out) :: pos_start
      integer, intent(out) :: pos_end

      integer :: start ! start_index limited to boundary.
      integer :: last  ! end index limited to boundary. (end is a reserved word)
      integer :: start_ref ! The reference position from where the length must be added or subtracted.

      if (total_length == 0 .or. length == 0 &
         .or. start_index == 0 &
         .or. start_index < -total_length .or. start_index > total_length) then
         pos_start = 1
         pos_end = 0
         return
      end if

      start_ref = clip_int(start_index, -total_length, total_length)

      ! Negative start, count backwards from the end
      if (start_ref > 0) then
         start = start_ref
      else if (start_ref == 0) then
         start = 1
      else
         start = total_length + start_ref + 1 ! start_index -1 is the last element, ex 10 -1 +1
      end if

      ! Negative length, count backwards from start position
      if (length > 0) then
         last = clip_int(start + length - 1, 1, total_length)
      else
         last = clip_int(start + length + 1, 1, total_length)
      end if

      if (start > last) then
         pos_start = last
         pos_end = start
      else
         pos_start = start
         pos_end = last
      end if
   end subroutine calc_start_end

   ! ---------------------------------------------------------------------------

   !> Dedicate clip (clamp) function for integers.
   !> [Exists in stdlib_math](https://github.com/fortran-lang/stdlib/blob/master/src/stdlib_math.fypp)
   function clip_int(x, xmin, xmax)
      integer, intent(in) :: x
      integer, intent(in) :: xmin
      integer, intent(in) :: xmax
      integer :: clip_int

      clip_int = max(min(x, xmax), xmin)
   end function clip_int

   ! ---------------------------------------------------------------------------

end module string_utf_8
