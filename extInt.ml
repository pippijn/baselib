let rec fold_left f x l h =
  if l == h + 1 then
    x
  else
    fold_left f (f x l) (l + 1) h
