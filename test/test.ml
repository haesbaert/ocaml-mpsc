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

let producer n () =
  let mydomain = Domain.self () in
  for i = 0 to n - 1 do
    Mpsc.push mpq (mydomain, i)
  done

let consumer p0_domain p1_domain () =
  let rec loop seen p0_last p1_last =
    if (seen mod 10000000) = 0 then dump_gc ();
    if total_entries = seen then
      ()
    else
      match Mpsc.pop mpq with
      | None ->
        Domain.cpu_relax ();
        loop seen p0_last p1_last
      | Some (domain, cur) ->
        if domain = p0_domain then
          let () = assert (cur = (succ p0_last)) in
          loop (succ seen) cur p1_last
        else if domain = p1_domain then
          let () = assert (cur = (succ p1_last)) in
          loop (succ seen) p0_last cur
        else
          failwith "invalid domain"
  in
  loop 0 (-1) (-1)
  
let _ =
  (* This is very naive *)
  let half = total_entries / 2 in
  let p0 = Domain.spawn (producer half) in
  let p1 = Domain.spawn (producer half) in
  let c = Domain.spawn (consumer (Domain.get_id p0) (Domain.get_id p1)) in
  Domain.join p0;
  Domain.join p1;
  Domain.join c


