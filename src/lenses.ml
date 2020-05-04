open TomlTypes

let safe_find key table =
  try
    let value = Table.find (Table.Key.bare_key_of_string key) table in
    Some value
  with Not_found -> None

type ('out, 'input, 'elt) lens = {
  create : 'input -> 'elt ;
  get: 'elt -> 'out option;
  set: 'input -> 'elt -> 'elt;
}

let key k =
  {
    create = (fun new_value ->
        Table.singleton (Table.Key.bare_key_of_string k)
          new_value);
    get = (fun value -> safe_find k value);
    set = (fun new_value value ->
        Table.add (Table.Key.bare_key_of_string k)
          (new_value) value)
  }

let bool = {
  create = (fun new_value -> TBool new_value);
  get = (fun (value:value) ->
      match value with
      | TBool v -> Some v
      | _ -> None);
  set = (fun new_value _ -> TBool new_value)
}

let int = {
  create = (fun new_value -> TInt new_value);
  get = (fun (value:value) ->
      match value with
      | TInt v -> Some v
      | _ -> None);
  set = (fun new_value _ -> TInt new_value)
}

let float = {
  create = (fun new_value -> TFloat new_value);
  get = (fun (value:value) ->
      match value with
      | TFloat v -> Some v
      | _ -> None);
  set = (fun new_value _ -> TFloat new_value)
}

let string = {
  create = (fun new_value -> TString new_value);
  get = (fun (value:value) ->
      match value with
      | TString v -> Some v
      | _ -> None);
  set = (fun new_value _ -> TString new_value)
}

let date = {
  create = (fun new_value -> TDate new_value);
  get = (fun (value:value) ->
      match value with
      | TDate v -> Some v
      | _ -> None);
  set = (fun new_value _ -> TDate new_value)
}

let array = {
  create = (fun new_value -> TArray new_value);
  get = (fun (value:value) ->
      match value with
      | TArray v -> Some v
      | _ -> None);
  set = (fun new_value _ -> TArray new_value)
}

let table = {
  create = (fun new_value -> TTable new_value);
  get = (fun (value:value) ->
      match value with
      | TTable v -> Some v
      | _ -> None);
  set = (fun new_value _ -> TTable new_value)
}

let strings = {
  create = (fun new_value -> NodeString new_value);
  get = (fun (value:array) ->
      match value with
      | NodeString v -> Some v
      | NodeEmpty -> Some []
      | _ -> None);
  set = (fun new_value _ -> NodeString new_value)
}

let bools = {
  create = (fun new_value -> NodeBool new_value);
  get = (fun (value:array) ->
      match value with
      | NodeBool v -> Some v
      | NodeEmpty -> Some []
      | _ -> None);
  set = (fun new_value _ -> NodeBool new_value)
}

let ints = {
  create = (fun new_value -> NodeInt new_value);
  get = (fun (value:array) ->
      match value with
      | NodeInt v -> Some v
      | NodeEmpty -> Some []
      | _ -> None);
  set = (fun new_value _ -> NodeInt new_value)
}

let floats = {
  create = (fun new_value -> NodeFloat new_value);
  get = (fun (value:array) ->
      match value with
      | NodeFloat v -> Some v
      | NodeEmpty -> Some []
      | _ -> None);
  set = (fun new_value _ -> NodeFloat new_value)
}

let dates = {
  create = (fun new_value -> NodeDate new_value);
  get = (fun (value:array) ->
      match value with
      | NodeDate v -> Some v
      | NodeEmpty -> Some []
      | _ -> None);
  set = (fun new_value _ -> NodeDate new_value)
}

let arrays = {
  create = (fun new_value -> NodeArray new_value);
  get = (fun (value:array) ->
      match value with
      | NodeArray v -> Some v
      | NodeEmpty -> Some []
      | _ -> None);
  set = (fun new_value _ -> NodeArray new_value)
}

let tables = {
  create = (fun new_value -> NodeTable new_value);
  get = (fun (value:array) ->
      match value with
      | NodeTable v -> Some v
      | NodeEmpty -> Some []
      | _ -> None);
  set = (fun new_value _ -> NodeTable new_value)
}

let (|-) (f:('a -> 'b option)) (g:'b -> 'c option) (x:'a) =
  match f x with
  | Some r -> g r
  | None -> None

let update (f:('b -> 'b option)) (a:'a) l =
  match l.get a with
  | Some old_value -> begin
      match f old_value with
      | Some new_value -> Some (l.set new_value a)
      | None -> None
    end
  | None -> None

let update_or_create (update:('b -> 'b)) create (a:'a) l  =
  match l.get a with
  | Some old_value -> begin
      let new_value = update old_value in
      l.set new_value a
    end
  | None ->
    let new_value = create in
    l.create new_value

let compose l1 l2 = {
  create = (fun v -> l2.create @@ l1.create v);
  get = l2.get |- l1.get;
  set = (fun v x -> update_or_create (l1.set v) (l1.create v) x l2);
}

let (|--) l1 l2 = compose l2 l1


let field k = key k |-- table

let optional k l =
  let create = function
    | None -> Table.empty
    | Some v -> Table.singleton (Table.Key.bare_key_of_string k) (l.create v)
  in
  let get toml =
    match safe_find k toml with None -> Some None | Some v -> Some (l.get v)
  in
  let set v toml =
    let old_field = safe_find k toml in
    match old_field, v with
    | _, None -> toml
    | None, Some v ->
      Table.singleton (Table.Key.bare_key_of_string k) (l.create v)
    | Some ov, Some v ->
      Table.add (Table.Key.bare_key_of_string k) (l.set v ov) toml
  in
  { create ; get ; set }

(** lifting functions *)

(* We always keep the value on the right *)
let (++) t1 t2 = Table.union (fun _ _ v -> Some v) t1 t2
let (%>) f g x = g (f x)

let lift1_opt f o1 = match o1 with
  | Some v1 -> Some (f v1)
  | _ -> None
let map (type a b) (f : a -> b) l1 =
  let create v = l1.create v
  and get toml = lift1_opt f (l1.get toml)
  and set v toml = l1.set v toml
  in { create ; set ; get }
let comap f l1 =
  let create v = l1.create (f v)
  and get toml = l1.get toml
  and set v toml = l1.set (f v) toml
  in { create ; set ; get }

let (<$>) = map
let (>$<) = comap

let lift2_opt f o1 o2 = match o1, o2 with
  | Some v1, Some v2 -> Some (f v1 v2)
  | _ -> None
let app l1 l2 =
  let create v = l1.create v ++ l2.create v
  and get toml = lift2_opt (@@) (l1.get toml) (l2.get toml)
  and set v = l1.set v %> l2.set v
  in { create ; set ; get }
let (<*>) = app

let get record lens = lens.get record

let set value record lens = lens.set value record

let create value lens = lens.create value
