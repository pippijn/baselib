external identity : 'a -> 'a = "%identity"
let (|>) f x = x f
let (@@) f x = f x
let (-|) f g x = f (g x)
let (%) f g x = f (g x)
let (|-) f g x = g (f x)
let (%>) f g x = g (f x)
let flip f x y = f y x
let tap f x = f x; x


let with_dispose dispose f x =
  let r =
    try
      f x
    with e ->
      dispose x;
      raise e
  in
  dispose x;
  r
