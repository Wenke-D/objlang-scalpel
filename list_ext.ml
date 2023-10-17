let rec find_index l predicate =
  match l with
  | e :: sub ->
      if predicate e then 0 else 1 + find_index sub predicate
  | [] ->
      raise (Invalid_argument "found nothing in the list")


(** Accumulate int value when iter on a list. Stop the travel when predicate
    gives a true *)
let rec partial_accum l to_int predicate =
  match l with
  | e :: sub ->
      if predicate e then to_int e
      else to_int e + partial_accum sub to_int predicate
  | [] ->
      raise (Invalid_argument "found nothing in the list")


(** Same as [partial_accum] without the true element *)
let rec partial_accum_excl l to_int predicate =
  match l with
  | e :: sub ->
      if predicate e then 0
      else to_int e + partial_accum_excl sub to_int predicate
  | [] ->
      raise (Invalid_argument "found nothing in the list")


let accumulate l to_int = List.fold_left (fun sum e -> sum + to_int e) 0 l
