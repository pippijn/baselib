module Make(T : Hashtbl.S) = struct
  type 'a t = {
    table : 'a T.t;
    stack : (T.key * 'a) Stack.t;
  }


  let create size = {
    table = T.create size;
    stack = Stack.create ();
  }


  let is_empty stack =
    Stack.is_empty stack.stack


  let push key value stack =
    T.add stack.table key value;
    Stack.push (key, value) stack.stack


  let pop stack =
    let key, value = Stack.pop stack.stack in
    T.remove stack.table key;
    value


  let mem stack key =
    T.mem stack.table key


  let find stack key =
    T.find stack.table key


end
