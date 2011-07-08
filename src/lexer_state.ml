type t = {
  mutable curr_offset : int;
  offset_stack : int Stack.t;
  mutable nl_ignore : int;
}

let create () =
  let stack = Stack.create () in
    Stack.push 0 stack;
    { curr_offset = 0;
      offset_stack = stack;
      nl_ignore = 0 }

let ignore_nl t =
  t.nl_ignore <- succ t.nl_ignore

and aware_nl t =
  t.nl_ignore <- pred t.nl_ignore
