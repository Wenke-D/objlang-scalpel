open Objlng

type 'a unexpected_error_data = { expected : 'a; actual : 'a }
type type_error_data = typ unexpected_error_data
type argument_length_error_data = int unexpected_error_data
type unknown_field_error_data = { clazz : string; field : string }

exception UnexpectedTypeError of type_error_data
exception ArgumentLengthNotMatchError of argument_length_error_data
exception NotIndexableError of typ
exception ClassNotFoundError of string
exception UnknownFieldError of { clazz : string; field : string }
exception UndefinedVariableError of string
exception UndefinedFunctionError of string

let sf = Format.sprintf

let format_unexpected_error formater data =
  sf "expecting [%s], got [%s]" (formater data.expected) (formater data.actual)

let format_type_error = format_unexpected_error string_of_typ
let format_argument_length_error = format_unexpected_error string_of_int

let format_not_indexable_error t =
  sf "type: %s is not indexable" (string_of_typ t)

let format_not_class_error name = sf "type: %s is not a class" name

let format_unknown_field_error data =
  sf "class [%s] do not have field [%s]" data.clazz data.field
