type 'a node = {
  mutable value : 'a option ;
  mutable next : 'a node;
}

type 'a t = {
  head : 'a node Atomic.t;
  tail : 'a node Atomic.t;
}

let is_empty t =
  let tail = Atomic.get t.tail in
  Option.is_none tail.next.value

let push t v =
  let rec n = { next = n; value = Some v } in
  let prev = Atomic.exchange t.head n in
  prev.next <- n

let pop t =
  let tail = Atomic.get t.tail in
  let value = tail.next.value in
  if Option.is_some value then begin
    tail.next.value <- None;
    Atomic.set t.tail tail.next;
  end;
  value

let make () =
  let rec stub = { next = stub; value = None } in
  { head = Atomic.make stub; tail = Atomic.make stub }
