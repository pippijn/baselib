include String


let without_prefix prefix str =
  if BatString.starts_with str prefix then
    let plen = String.length prefix in
    let nlen = String.length str in
    Some (String.sub str plen (nlen - plen))
  else
    None

let without_prefixes prefixes str =
  List.fold_left (fun result prefix ->
    match without_prefix prefix str with
    | Some _ as result -> result
    | None -> result
  ) None prefixes


let without_suffix suffix str =
  if BatString.ends_with str suffix then
    let slen = String.length suffix in
    let nlen = String.length str in
    Some (String.sub str 0 (nlen - slen))
  else
    None

let without_suffixes suffixes str =
  List.fold_left (fun result suffix ->
    match without_suffix suffix str with
    | Some _ as result -> result
    | None -> result
  ) None suffixes
