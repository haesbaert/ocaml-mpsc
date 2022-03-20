(*
 * Copyright (c) 2022 Christiano Haesbaert <haesbaert@haesbaert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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
