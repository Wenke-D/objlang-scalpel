open Objlng

(*  *)

type construct =
  | Class of string
  (* class.attribute *)
  | Attribute of string * string
  | Method of string * string
  | Variable of string
  | Function of string

exception UndefinedError of construct

type ('yes, 'no) unmatch_data = { expected : 'yes; actual : 'no }
type type_behavior = HasField of string | HasMethod of string | Indexable
type type_description = Definition of typ | Behavior of type_behavior

exception
  UnexpectedTypeError of (type_description, type_description) unmatch_data

exception MissingArgumentError of (int, int) unmatch_data

let s_format = Format.sprintf

let format_undefined_error c =
  let suffix =
    match c with
    | Class name -> s_format "class [%s] is not defined" name
    | Attribute (class_name, attribute_name) ->
        s_format "class [%s] does not define attribute [%s]" class_name
          attribute_name
    | Method (class_name, method_name) ->
        s_format "class [%s] does not define method [%s]" class_name method_name
    | Variable name -> s_format "variable [%s] is not defined" name
    | Function name -> s_format "function [%s] is not defined" name
  in
  s_format "Undefined error: %s" suffix
