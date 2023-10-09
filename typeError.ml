(*  *)

type construct =
  | Class of string
  (* class.attribute *)
  | Attribute of string * string
  | Method of string * string
  | Variable of string
  | Function of string

exception UndefinedError of construct
(** Error of use before definition  *)

type ('yes, 'no) unmatch_data = { expected : 'yes; actual : 'no }
type type_behavior = HasField of string | HasMethod of string | Indexable
type type_description = Definition of Objlang.typ | Behavior of type_behavior

exception
  UnexpectedTypeError of (type_description, type_description) unmatch_data
(** type not match *)

exception MissingArgumentError of (int, int) unmatch_data
(** argument number not match *)

let s_format = Format.sprintf

let format_undefined_error data =
  let suffix =
    match data with
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

let format_unexpected_type_error data =
  let format_type_behavior tb =
    match tb with
    | HasField g -> s_format "has field %s" g
    | Indexable -> "is indexable"
    | HasMethod m -> s_format "has method %s" m
  in
  let format_type_description td =
    match td with
    | Definition t -> "a type " ^ Objlang.string_of_typ t
    | Behavior b -> "a type which" ^ format_type_behavior b
  in

  let expect = format_type_description data.expected in
  let actual = format_type_description data.actual in
  s_format "[Type error] expecting: %s, but got: %s" expect actual
