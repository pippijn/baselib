let of_char = String.make 1


let fold_left f a s =
  let rec fold n f a s =
    if n = String.length s then
      a
    else
      fold (n + 1) f (f a s.[n]) s
  in
  fold 0 f a s


let fold_right f s b =
  let rec fold n f s b =
    if n = 0 then
      b
    else
      fold (n - 1) f s (f s.[n - 1] b)
  in
  fold (String.length s) f s b


let rec nsplit s sep =
  assert (String.length sep = 1);
  try
    let pos = String.index s sep.[0] in
    String.sub s 0 pos :: nsplit (String.sub s (pos + 1) (String.length s - pos - 1)) sep
  with Not_found ->
    [s]


let of_foldable length fold_left l =
  let res = String.create (length l) in
  ignore (fold_left (fun i c ->
    res.[i] <- c;
    i + 1
  ) 0 l);
  res


let of_list = of_foldable List.length List.fold_left
let of_array = of_foldable Array.length Array.fold_left

let to_list s =
  fold_right CoreList.cons s []


type escape_state =
  | NoEscape
  | Escape
  | HexEscape of int * int

let unescaped str =
  let state, pos =
    fold_left (fun (state, pos) c ->
      match c with
      | '"' | '\'' | '\\' when state == Escape -> str.[pos] <- c; NoEscape, pos + 1
      | 't'  when state == Escape -> str.[pos] <- '\t'; NoEscape, pos + 1
      | 'r'  when state == Escape -> str.[pos] <- '\r'; NoEscape, pos + 1
      | 'n'  when state == Escape -> str.[pos] <- '\n'; NoEscape, pos + 1
      | 'v'  when state == Escape -> str.[pos] <- '\x0c'; NoEscape, pos + 1
      | 'f'  when state == Escape -> str.[pos] <- '\x0b'; NoEscape, pos + 1

      | 'x'  when state == Escape -> HexEscape (0, 2), pos

      | '0' .. '9' as c ->
          begin match state with
          | HexEscape (i, 0) ->
              str.[pos + 0] <- char_of_int i;
              str.[pos + 1] <- c; NoEscape, pos + 2
          | HexEscape (i, l) ->
              let m = int_of_char c - int_of_char '0' in
              HexEscape (i * 16 + m, l - 1), pos
          | _ ->
              str.[pos] <- c; state, pos + 1
          end

      | 'a' .. 'f' as c ->
          begin match state with
          | HexEscape (i, 0) ->
              str.[pos + 0] <- char_of_int i;
              str.[pos + 1] <- c; NoEscape, pos + 2
          | HexEscape (i, l) ->
              let m = int_of_char c - int_of_char 'a' in
              HexEscape (i * 16 + m, l - 1), pos
          | _ -> str.[pos] <- c; state, pos + 1
          end

      | 'A' .. 'F' as c ->
          begin match state with
          | HexEscape (i, 0) ->
              str.[pos + 0] <- char_of_int i;
              str.[pos + 1] <- c; NoEscape, pos + 2
          | HexEscape (i, l) ->
              let m = int_of_char c - int_of_char 'A' in
              HexEscape (i * 16 + m, l - 1), pos
          | _ -> str.[pos] <- c; state, pos + 1
          end

      | c when state == Escape ->
          failwith ("Unknown escape sequence: " ^ Char.escaped c)

      | '\\' -> Escape, pos
      | c -> str.[pos] <- c; NoEscape, pos + 1
    ) (NoEscape, 0) str
  in

  let pos =
    match state with
    | NoEscape -> pos
    | Escape -> failwith "unterminated escape sequence"
    | HexEscape (i, 0) -> str.[pos] <- char_of_int i; pos + 1
    | HexEscape (i, l) -> failwith "unterminated hex escape"
  in

  String.sub str 0 pos
