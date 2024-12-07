# Changelog

## [0.1.0] - 2024-11-09

- Initial release

## [0.2.0] - 2024-11-19

- substr change

To follow the boundary logic of C++ std::substr() and c# String.Substring, if start_index exceeds the string boundary limits, return an empty string.

## [0.2.1] - 2024-12-03

- str_replace, str_replace_all, indexof default start_index

Code stays compatible with version 0.2.0.  
Two new functions are added: str_replace, str_replace_all.  
The start_index in function indexof is optional.
