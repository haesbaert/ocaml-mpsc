let total_entries = 100000000
let mpq = Mpsc.make ()

let producer a n () =
  for i = a to n - 1 do
    Mpsc.push mpq i
  done

let consumer () =
  let rec loop total_seen =
    if total_entries = total_entries then
      ()
    else
      match Mpsc.pop mpq with
      | None ->
        Domain.cpu_relax ();
        loop total_seen
      | Some _ ->
        loop (succ total_seen)
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


