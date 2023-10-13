open Color_text

(*  *)

type construct =
  | Class of string
  (* class.attribute *)
  | Attribute of string * string
  | Method of string * string
  | Variable of string
  | Function of string

(** Error of use before definition *)
exception UndefinedError of construct

type ('yes, 'no) unmatch_data = {expected: 'yes; actual: 'no}

type type_behavior = HasField of string | HasMethod of string | Indexable

(** Descriotion of a type *)
type type_description =
  (* Definition of an existing type *)
  | Definition of Objlang.typ
  (* Expecting behavior *)
  | Behavior of type_behavior

(** type not match *)
exception
  UnexpectedTypeError of (type_description, type_description) unmatch_data

(** argument number not match *)
exception ArgumentLengthError of (int, int) unmatch_data

let s_format = Format.sprintf

let format_undefined_error data =
  match data with
  | Class name ->
      s_format "class '%s' is not defined." (info name)
  | Attribute (class_name, attribute_name) ->
      s_format "class '%s' does not have a attribute '%s'." (info class_name)
        (info attribute_name)
  | Method (class_name, method_name) ->
      s_format "class '%s' does not have a method '%s'." (info class_name)
        (info method_name)
  | Variable name ->
      s_format "variable '%s' is not defined." (info name)
  | Function name ->
      s_format "function '%s' is not defined." (info name)


let format_unexpected_type_error data =
  let format_type_behavior tb =
    match tb with
    | HasField f ->
        s_format "has field %s" (info f)
    | Indexable ->
        s_format "is %s" (info "indexable")
    | HasMethod m ->
        s_format "has method %s" (info m)
  in
  let format_type_description td =
    match td with
    | Definition t ->
        "a type " ^ info (Objlang.string_of_typ t)
    | Behavior b ->
        "a type which" ^ format_type_behavior b
  in
  let expect = format_type_description data.expected in
  let actual = format_type_description data.actual in
  s_format "expecting %s, but got %s." expect actual


let plurial word = word ^ "s"

let if_plurial word n = if n <> 1 then plurial word else word

let format_argument_length_error data =
  let argument = if_plurial "argument" in
  s_format "need %s %s, got %s %s."
    (info (string_of_int data.expected))
    (argument data.expected)
    (info (string_of_int data.actual))
    (argument data.actual)
