(*

1:
OLD: 3.734000
NEW: 4.812000
OLD: 3.719000
NEW: 4.813000

2:
OLD: 2.812000
NEW: 4.813000
OLD: 2.781000
NEW: 4.797000

3:
OLD: 2.485000
NEW: 4.812000
OLD: 2.469000
NEW: 4.813000

4:
OLD: 2.390000
NEW: 4.797000
OLD: 2.406000
NEW: 4.797000

5:
OLD: 2.468000
NEW: 4.797000
OLD: 2.485000
NEW: 4.796000

6:
OLD: 2.594000
NEW: 4.796000
OLD: 2.594000
NEW: 4.797000

7:
OLD: 2.750000
NEW: 4.875000
OLD: 2.719000
NEW: 4.828000

8:
OLD: 2.859000
NEW: 4.813000
OLD: 2.859000
NEW: 4.813000

*)


open Mp3framehuffman;;

let test_string = String.create 65536;;
for a = 0 to 65535 do
	test_string.[a] <- Char.chr (Random.int 256)
done;;

let do_old ht = (
	let r = (test_string, 0) in
	let rec iter (r_str, r_at) = (
		if r_at > 524256 then (
			r_at
		) else (
			let (_,r) = huffman_decode (r_str, r_at) ht global_ht_bits in
			iter r
		)
	) in
	iter r
);;

let do_new ht = (
	let r = (test_string, 0) in
	let rec iter (r_str, r_at) = (
		if r_at > 524256 then (
			r_at
		) else (
			let (_,r) = huffman_decode_unsafe (r_str, r_at) ht global_ht_bits in
			iter r
		)
	) in
	iter r
);;

let times = 32;;
(*
let old_t1 = Unix.gettimeofday ();;
for a = 1 to 1 do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			let a = (do_old global_ht.(t)) in
			Printf.printf "%d " a;
		)
	done;
done;;
let old_t2 = Unix.gettimeofday ();;
Printf.printf "\n";;
Printf.printf "OLD: %f\n" (old_t2 -. old_t1);;

let new_t1 = Unix.gettimeofday ();;
for a = 1 to 1 do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			let a = (do_new global_ht.(t)) in
			Printf.printf "%d " a;
		)
	done;
done;;
let new_t2 = Unix.gettimeofday ();;
Printf.printf "\n";;
Printf.printf "NEW: %f\n" (new_t2 -. new_t1);;
*)


let old_t1 = Unix.gettimeofday ();;
for a = 1 to times do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			ignore (do_old global_ht.(t))
		)
	done;
done;;
let old_t2 = Unix.gettimeofday ();;
Printf.printf "OLD: %f\n" (old_t2 -. old_t1);;

let new_t1 = Unix.gettimeofday ();;
for a = 1 to times do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			ignore (do_new global_ht.(t))
		)
	done;
done;;
let new_t2 = Unix.gettimeofday ();;
Printf.printf "NEW: %f\n" (new_t2 -. new_t1);;

let old_t1 = Unix.gettimeofday ();;
for a = 1 to times do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			ignore (do_old global_ht.(t))
		)
	done;
done;;
let old_t2 = Unix.gettimeofday ();;
Printf.printf "OLD: %f\n" (old_t2 -. old_t1);;

let new_t1 = Unix.gettimeofday ();;
for a = 1 to times do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			ignore (do_new global_ht.(t))
		)
	done;
done;;
let new_t2 = Unix.gettimeofday ();;
Printf.printf "NEW: %f\n" (new_t2 -. new_t1);;

let old_t1 = Unix.gettimeofday ();;
for a = 1 to times do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			ignore (do_old global_ht.(t))
		)
	done;
done;;
let old_t2 = Unix.gettimeofday ();;
Printf.printf "OLD: %f\n" (old_t2 -. old_t1);;

let new_t1 = Unix.gettimeofday ();;
for a = 1 to times do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			ignore (do_new global_ht.(t))
		)
	done;
done;;
let new_t2 = Unix.gettimeofday ();;
Printf.printf "NEW: %f\n" (new_t2 -. new_t1);;

let old_t1 = Unix.gettimeofday ();;
for a = 1 to times do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			ignore (do_old global_ht.(t))
		)
	done;
done;;
let old_t2 = Unix.gettimeofday ();;
Printf.printf "OLD: %f\n" (old_t2 -. old_t1);;

let new_t1 = Unix.gettimeofday ();;
for a = 1 to times do
	for t = 1 to 31 do
		if t <> 4 && t <> 14 then (
			ignore (do_new global_ht.(t))
		)
	done;
done;;
let new_t2 = Unix.gettimeofday ();;
Printf.printf "NEW: %f\n" (new_t2 -. new_t1);;

