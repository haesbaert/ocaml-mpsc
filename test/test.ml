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

let total_entries = 100000000
let mpq = Mpsc.make ()

let dump_gc () =
  let s = Gc.stat () in
  Printf.printf "live_words=%-8d live_blocks=%-8d \
                 minor_collections=%-6d major_collections=%-6d\n%!"
    s.live_words s.live_blocks s.minor_collections s.major_collections

let producer a n () =
  for i = a to n - 1 do
    Mpsc.push mpq i
  done

let consumer () =
  let rec loop seen =
    let () = if (seen mod 10000000) = 0 then dump_gc () in
    if total_entries = seen then
      ()
    else
      match Mpsc.pop mpq with
      | None ->
        Domain.cpu_relax ();
        loop seen
      | Some _ ->
        loop (succ seen)
  in
  loop 0
  
let _ =
  (* This is very naive *)
  let half = total_entries / 2 in
  let p1 = Domain.spawn (producer 0 half) in
  let p2 = Domain.spawn (producer half total_entries) in
  let c = Domain.spawn consumer in
  Domain.join p1;
  Domain.join p2;
  Domain.join c


