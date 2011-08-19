(*******************************************************************************
	This file is a part of mp3packer.

	mp3packer is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	mp3packer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with mp3packer; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*******************************************************************************)

(*
mp3packer -f -z test\aps.mp3 NUL
Before: 15.328 / 15.343
*)

open Mp3types;;
open Pack;;
open Mp3framehuffman;;

(*
let debug = true;;
let debug_out = false || debug;;
*)
let debug_more = true;;

let tab = " ";;

(****************)
(* GLOBAL STUFF *)
(****************)

let num_quants = 576;;

(*
	The lowest frequency sample of each scalefactor band is given in this table
*)
let global_scalefactors sfreq is_short = match (sfreq, is_short) with
	| (S48000, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 42; 50; 60; 72; 88;106;128;156;190;230;276;330;384;576 |]
	| (S44100, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 44; 52; 62; 74; 90;110;134;162;196;238;288;342;418;576 |]
	| (S32000, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 44; 54; 66; 82;102;126;156;194;240;296;364;448;550;576 |]
	| (S24000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;114;136;162;194;232;278;332;394;464;540;576 |]
	| (S22050, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S16000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S12000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S11025, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| ( S8000, false) -> [| 0;12;24;36;48;60;72;88;108;132;160;192;232;280;336;400;476;566;568;570;572;574;576 |]

	| (S48000, true ) -> [| 0;4; 8;12;16;22;28;38; 50; 64; 80;100;126;192 |]
	| (S44100, true ) -> [| 0;4; 8;12;16;22;30;40; 52; 66; 84;106;136;192 |]
	| (S32000, true ) -> [| 0;4; 8;12;16;22;30;42; 58; 78;104;138;180;192 |]
	| (S24000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;136;180;192 |]
	| (S22050, true ) -> [| 0;4; 8;12;18;24;32;42; 56; 74;100;132;174;192 |]
	| (S16000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| (S12000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| (S11025, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| ( S8000, true ) -> [| 0;8;16;24;36;52;72;96;124;160;162;164;166;192 |]
;;

type block_type_t = Block_type_long | Block_type_short | Block_type_start | Block_type_stop;;

type window_normal_t = {
	normal_table_select1 : int;  (* Which Huffman table to use for each region *)
	normal_table_select2 : int;  (* etc *)
	normal_table_select3 : int;  (* etc *)
	normal_region_0_count : int; (* How many values are in the first region (starting from lowest freq) *)
	normal_region_1_count : int; (* Ditto for the next higher region (region_2_count is implicit because big_values is known beforehand) *)
};;
type window_other_t = {
	other_block_type : block_type_t; (* 01 = 1 long block start window. 10 = 3 short blocks. 11 = 1 long block stop window. 00 = invalid *)
	other_mixed_block : bool;        (* 0 = normal. 1 = lower 2 subbands are long, upper 30 subbands are short (for block_type = 10) *)
	other_table_select1 : int;       (* Which Huffman table to use for the particular region (regions are implicit) *)
	other_table_select2 : int;       (* Ditto (note that there is no region3) *)
	other_sub_block_gain1 : int;     (* Offset from the global gain of each short block, if Block_type = 10. Otherwise not used *)
	other_sub_block_gain2 : int;     (* Same for next short block *)
	other_sub_block_gain3 : int;     (* etc *)
};;

(* gc = granule-channel *)
type gc_window_t = Window_normal of window_normal_t | Window_other of window_other_t;;

type side_gc_t = {
	gc_part2_3_length : int;     (* Length in bits of parts 2 (scalefactors) and 3 (Huffman encoded data) *)
	gc_part2_3_offset : int;     (* The integral of the previous part2_3_lengths *)
	gc_big_values : int;         (* Half the number of frequency bands (starting from lowest freq) whose absolute values are greater than 1 *)
	gc_global_gain : int;        (* Logarithmic encoding of the global gain value *)
		(* xxxxxxxx11111111111111110000000000000000000000  *)
		(*        |               |                     |  *)
		(*   bigvalues*2    bv*2+count1*4              576 *)
		(* Everything in "0" region is 0 *)
		(* Everything in "1" region is -1, 0, or +1 *)
		(* "x" can be anything *)
	gc_scf_compress_index : int; (* An index determining the number of bits to use for the scale factors (For MPEG1) *)
	gc_window : gc_window_t;     (* 0 = granule / channel has only 1 block and a normal window. 1 = something else *)
	gc_pre_flag : bool;          (* High-frequency amplification shortcut; not used for MPEG2/2.5 *)
	gc_sf_scale : int;           (* Which Huffman table to use for scalefactor scaling *)
	gc_count1_table_1 : bool;    (* true if table 1 is used for the Count1 region, false if 0 *)
}

type side_internal_t = {
	side_main_data_begin : int;
	side_scfi : bool array array; (* side_scfi.(channel).(scfi band) 0 = Scale factors are transmitted for each granule. 1 = One scalefactor is for both granules. Note that there are 4 scale factor bands. *)
	side_gc : side_gc_t array array; (* side_gc.(granule).(channel) *)
};;

(*
	The number of bits to use in the (low,high) frequencies of the scalefactors.
	Most of the time, the first number corresponds to the first 11 scalefactors and the last number is for the last 10 scalefactors
	Note that the last scalefactor band (#22) does not have its own scalefactor
*)
let scalefactor_compress_m1 = [|
	(0,0);
	(0,1);
	(0,2);
	(0,3);
	(3,0);
	(1,1);
	(1,2);
	(1,3);
	(2,1);
	(2,2);
	(2,3);
	(3,1);
	(3,2);
	(3,3);
	(4,2);
	(4,3);
|];;

(* This is used with MPEG2, for everything except the right channel in IS frames *)
let scalefactor_compress_m2 = Array.init 512 (fun i ->
	if i < 400 then (
		((i lsr 4 / 5), (i lsr 4 mod 5), ((i land 15) lsr 2), (i land 3))
	) else if i < 500 then (
		let j = i - 400 in
		((j lsr 2 / 5), (j lsr 2 mod 5), (j land 3), (0))
	) else (
		let j = i - 500 in
		((j / 3), (j mod 3), (0), (0))
	)
);;

(* Only for the right channel in IS frames *)
let scalefactor_compress_m2_is = Array.init 512 (fun q ->
	let i = q lsr 1 in
	if i < 180 then (
		((i / 36), (i mod 36 / 6), (i mod 6), 0)
	) else if i < 244 then (
		let j = i - 180 in
		((j land 63) lsr 4, (j land 15) lsr 2, j land 3, 0)
	) else (
		let j = i - 244 in
		(j / 3, j mod 3, 0, 0)
	)
);;

let scalefactor_bands_m2 is gc =
	if is then (
		if gc.gc_scf_compress_index < 360 then (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,15,12,0)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (12,12,12,0)
			| _ ->                                                                                           (7,7,7,0)
		) else if gc.gc_scf_compress_index < 488 then (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,12,9,6)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (12,9,9,6)
			| _ ->                                                                                           (6,6,6,3)
		) else (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,18,9,0)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (15,12,9,0)
			| _ ->                                                                                           (8,8,5,0)
		)
	) else (
		if gc.gc_scf_compress_index < 400 then (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,9,9,9)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (9,9,9,9)
			| _ ->                                                                                           (6,5,5,5)
		) else if gc.gc_scf_compress_index < 500 then (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,9,12,6)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (9,9,12,6)
			| _ ->                                                                                           (6,5,7,3)
		) else (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (15,18,0,0)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (18,18,0,0)
			| _ ->                                                                                           (11,10,0,0)
		)
	)
;;



(************************)
(* Frame internal types *)
(************************)

type m1_frame_data_t = {
	m1_header : header_t; (* Do I need this? *)
	m1_side_info : side_internal_t;
	m1_scalefactors : int array array array; (* m1_scalefactors.(granule).(channel).(scalefactor) *)
	m1_quantizers : int array array array; (* m1_quantizers.(granule).(channel).(quantizer) *)
	m1_starting_f1 : f1_t;
};;

type m2_frame_data_t = {
	m2_header : header_t;
	m2_side_info : side_internal_t;
	m2_data_deleteme : string; (* deleteme because it should be processed sometime *)

	m2_scalefactors : int array array; (* m2_scalefactors.(channel).(scalefactor) *)
	m2_quantizers : int array array; (* m2_quantizers.(channel).(quantizer) *)

	m2_starting_f1 : f1_t;
};;

type frame_data_t = M1_frame_data of m1_frame_data_t | M2_frame_data of m2_frame_data_t;; (* MPEG 1 or 2 *)




(* Frame printing functions *)
let print_side ?(tabs="") s =

(*	let p = Printf.printf in*)

	let p = (
		let h f = Printf.printf "%s%s" tabs f in
		fun a -> Printf.kprintf h a
	) in

	p "Main data begin: %d\n" s.side_main_data_begin;
	p "SCFI each granule:";
	Array.iter (fun n -> Printf.printf " ("; Array.iter (fun q -> Printf.printf "%s" (if q then "#" else ".")) n; Printf.printf ")") s.side_scfi;
	p "\n";
	p "Granule / channel info:\n";
	Array.iteri (fun i granule ->
		p " Granule %d:\n" i;
		Array.iteri (fun j gc ->
			p "  Channel %d:\n" j;
			p "   Part2_3:     %d (offset %d)\n" gc.gc_part2_3_length gc.gc_part2_3_offset;
			p "   big_values:  %d\n" gc.gc_big_values;
			p "   global_gain: %d\n" gc.gc_global_gain;
			p "   scf_comp_i:  %d\n" gc.gc_scf_compress_index;
			(match gc.gc_window with
				| Window_normal w -> (
					p "   Normal window\n";
					p "    Tables:     %d,%d,%d\n" w.normal_table_select1 w.normal_table_select2 w.normal_table_select3;
					p "    Region 0,1: %d,%d\n" w.normal_region_0_count w.normal_region_1_count;
				)
				| Window_other w -> (
					p "   Other window\n";
					p "    Block type:     %s\n" (match w.other_block_type with
						| Block_type_long -> "INVALID!"
						| Block_type_short -> "Short"
						| Block_type_start -> "Start"
						| Block_type_stop -> "Stop"
					);
					p "    Mixed block?    %b\n" w.other_mixed_block;
					p "    Tables:         %d,%d\n" w.other_table_select1 w.other_table_select2;
					p "    Sub block gain: %d,%d,%d\n" w.other_sub_block_gain1 w.other_sub_block_gain2 w.other_sub_block_gain3;
				)
			);
			p "   HF amp:      %b\n" gc.gc_pre_flag;
			p "   SF scale:    %d\n" gc.gc_sf_scale;
			p "   Count1 table %d\n" (if gc.gc_count1_table_1 then 1 else 0);
		) granule;
	) s.side_gc;
;;

(*
## Side info:
# 9/17/32 BYTES = 136/256 bits
# 9: main_data_begin (8 for MPEG2)
# ?: private_bits
# 4: SCFI Band
# 59: Side Information Granule
#  12: part2_3 length (main data for this channel, granule in bits)
#  9: Big values
#  8: Global gain
#  4: Scalefactor compress (9 for MPEG2)
#  1: Window switch flag
#   if 1:
#    2: Block type
#    1: Mix block flag
#    5x2: Table Select [region]
#    3x3: sub_block_gain [window]
#   if 0:
#    5x3: Table select [region]
#    4: Region 0 count
#    3: Region 1 count
#  1: Pre flag (NOT FOR MPEG2)
#  1: Scale factor scale
#  1: Count1 table select

# MPEG1 mono:
# [9 main data] [5 privates] [4 SCFI] [59 Gr0] [59 Gr1]
# (18 - 30) (77 - 89)
# MPEG1 stereo:
# [9 main data] [3 privates] [4 SCFI0] [4 SCFI1] [59 Gr0ch1] [59 Gr0ch2] [59 Gr1ch1] [59 Gr1ch2]
# 20 79 138 197
# MPEG2 mono:
# [8 main data] [1 privates] [63 Gr*]
# (9 - 21)
# MPEG2 stereo:
# [8 main data] [2 privates] [63 Gr*ch1] [63 Gr*ch2]
# (10 - 22) (73 - 85)
*)



(*********************)
(* READ SCALEFACTORS *)
(*********************)
let read_scalefactors_m1 ?(debug=false) ?(tabs="") scfi prev_scf_option gc r =
	let make_initial_array = match (prev_scf_option, scfi) with
		| (None, _) -> (fun x -> Array.make x 0)
		| (Some y, [| false;false;false;false |]) -> (fun x -> Array.make x 0) (* If scfi indicates to not use anything from previous frame, just recreate the array. This helps for short blocks, when the array is the wrong length anyway *)
		| (Some y, _) -> (fun x -> Array.sub y 0 x)
	in
	let (bits1,bits2) = scalefactor_compress_m1.(gc.gc_scf_compress_index) in
	let (num1,num2,scf_out) = match gc.gc_window with
		| Window_normal x -> (
			(* Normal *)
			(11, 10, make_initial_array 21)
		)
		| Window_other x when x.other_block_type <> Block_type_short -> (
			(* Start or stop (actually the same as normal) *)
			(11, 10, make_initial_array 21)
		)
		| Window_other x when x.other_mixed_block -> (
			(* Short, mixed block *)
			(17, 18, make_initial_array 35) (* Mixed blocks seem to have 36 scalefactors, but only 33 scalefactor bands. However, the way the windows are set up, there are a total of 35 scalefactor scales *)
		)
		| Window_other x -> (
			(* Short block *)
			(18, 18, make_initial_array 36) (* Short blocks have 39 scalefactors, but only 36 scalefactor bands *)
		)
	in
	(* Perhaps I should check for short frames somewhere? Or maybe just assume that the file has SCFSI set to false for short frames already *)
	let rec read_stuff r i = (
		let num_bits = if i < num1 then bits1 else bits2 in

		if i >= Array.length scf_out then (
			if debug && debug_more then Printf.printf "!";
			r
		) else if i < 6 then (
			let r = if scfi.(0) then r else (let (a,r) = read_bits r num_bits in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "0"); r) in
			read_stuff r (succ i)
		) else if i < 11 then (
			let r = if scfi.(1) then r else (let (a,r) = read_bits r num_bits in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "1"); r) in
			read_stuff r (succ i)
		) else if i < 16 then (
			let r = if scfi.(2) then r else (let (a,r) = read_bits r num_bits in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "2"); r) in
			read_stuff r (succ i)
		) else if i < 21 then (
			let r = if scfi.(3) then r else (let (a,r) = read_bits r num_bits in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "3"); r) in
			read_stuff r (succ i)
		) else (
			let r = (let (a,r) = read_bits r num_bits in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "+"); r) in
			read_stuff r (succ i)
		)
	) in

	let new_r = read_stuff r 0 in
	if debug && debug_more then Printf.printf "\n";

(*	if debug then Printf.printf "SCF compress (%d,%d)\n" bits1 bits2;*)
	if debug then Printf.printf "%sREAD_SCALEFACTORS_M1: (%d,%d)=%d,%d - [%s ] " tabs bits1 bits2 num1 num2 (Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" scf_out);
	if debug then Array.iter (fun q -> Printf.printf "%s" (if q then "#" else ".")) scfi;
	if debug then Printf.printf "\n";
	(scf_out,new_r)
;;

let read_scalefactors_m2 ?(debug=false) ?(tabs="") is gc r =

	let (bits0, bits1, bits2, bits3) = if is then (
		scalefactor_compress_m2_is.(gc.gc_scf_compress_index)
	) else (
		scalefactor_compress_m2.(gc.gc_scf_compress_index)
	) in
	let (num0, num1, num2, num3) = scalefactor_bands_m2 is gc in

	let sob1 = num0 in
	let sob2 = sob1 + num1 in
	let sob3 = sob2 + num2 in
	let num_total = sob3 + num3 in
	
	let scf_out = Array.make num_total 0 in

	let rec read_stuff r i = (
		if i < sob1 then (
			let r = (let (a,r) = read_bits r bits0 in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "0"); r) in
			read_stuff r (succ i)
		) else if i < sob2 then (
			let r = (let (a,r) = read_bits r bits1 in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "1"); r) in
			read_stuff r (succ i)
		) else if i < sob3 then (
			let r = (let (a,r) = read_bits r bits2 in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "2"); r) in
			read_stuff r (succ i)
		) else if i < num_total then (
			let r = (let (a,r) = read_bits r bits3 in scf_out.(i) <- a; (if debug && debug_more then Printf.printf "3"); r) in
			read_stuff r (succ i)
		) else (
			if debug && debug_more then Printf.printf "!";
			r
		)
	) in

	let new_r = read_stuff r 0 in
	if debug && debug_more then Printf.printf "\n";

	if debug then (
		let str0 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out    0 num0) in
		let str1 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob1 num1) in
		let str2 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob2 num2) in
		let str3 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob3 num3) in
		Printf.printf "%sREAD_SCALEFACTORS_M2: (%d,%d,%d,%d)=%d,%d,%d,%d - [%s ]-[%s ]-[%s ]-[%s ]\n" tabs bits0 bits1 bits2 bits3 num0 num1 num2 num3 str0 str1 str2 str3;
	);

	(scf_out,new_r)
;;

(* It ends when r_at = r_to *)
let read_quantizers_m1 ?(debug=false) ?(tabs="") k gc (r_str,r_at) r_to =
	if debug && debug_more then Printf.printf "Starting on %S at %d, going to %d\n" (String.sub (to_bin r_str) r_at (r_to - r_at)) r_at r_to;

(*let debug = false in*)

	let decoder_error_ref = ref false in
	let out_quants = Array.make num_quants 0 in


	(* Big_values *)
	let rec read_from_table index left ht linbits r_now = (
if debug && debug_more then Printf.printf " Running read_from_table index=%d left=%d linbits=%d\n" index left linbits;
		if left = 0 then (
			(index, r_now)
		) else if left < 0 then (
			(* I don't entirely see how this can be hit, since 1 is subtracted from left during each iteration... *)
			if debug then Printf.printf " OOPS! Decoding bigvalues went %d values overboard (now at %d)\n" ( ~- left) (snd r_now);
			decoder_error_ref := true;
			(index, r_now)
		) else if snd r_now >= r_to then (
			if debug then Printf.printf " OOPS! Decoding bigvalues got too many bits (now at %d)\n" (snd r_now);
			decoder_error_ref := true;
			(index, r_now)
		) else if index + 2 > num_quants then (
			(* The big_values went off the end of the out_quants array! *)
			if debug then Printf.printf " OOPS! Too many bigvalues; truncating here\n";
			decoder_error_ref := true;
			(index, r_now)
		) else (
			let ((abs_x,abs_y), r) = huffman_decode r_now ht global_ht_bits in
			if debug && debug_more then Printf.printf " Found (%d,%d) for %d at %d\n" abs_x abs_y index (snd r_now);

			let (x_add_lin, r) = if abs_x = 15 && linbits > 0 then (
				read_bits r linbits
			) else (0, r) in

			let (x_neg, r) = if abs_x > 0 then (
				read_bits r 1
			) else (0, r) in

			let (y_add_lin, r) = if abs_y = 15 && linbits > 0 then (
				read_bits r linbits
			) else (0, r) in
			let (y_neg, r) = if abs_y > 0 then (
				read_bits r 1
			) else (0, r) in
			let x_mult = if x_neg = 0 then 1 else -1 in
			let y_mult = if y_neg = 0 then 1 else -1 in

			if debug && debug_more then Printf.printf "  Add (%d,%d)\n" x_add_lin y_add_lin;
			if debug && debug_more then Printf.printf "  Multiply by (%d,%d)\n" x_mult y_mult;

(*
			if snd r > r_to then (
				if debug && debug_more then Printf.printf " OOPS! Decoding bigvalues went a little overboard (pos %d > %d)\n" (snd r) r_to;
				(index, r)
			) else*) (
				out_quants.(index) <- (abs_x + x_add_lin) * x_mult;
				out_quants.(index + 1) <- (abs_y + y_add_lin) * y_mult;
				read_from_table (index + 2) (left - 1) ht linbits r
			)

		)
	) in

	let (region0, region1, region2, table0, table1, table2) = (match gc.gc_window with
		| Window_normal w -> (
			let scfend0 = w.normal_region_0_count + 1 in
			let scfend1 = w.normal_region_1_count + 1 + scfend0 in
			let region0 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend0) lsr 1) in
			let region1 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend1) lsr 1) - region0 in
			(region0, region1, gc.gc_big_values - region0 - region1, w.normal_table_select1, w.normal_table_select2, w.normal_table_select3)
		)
		| Window_other  w -> (
			let region0 = if w.other_block_type = Block_type_short && not w.other_mixed_block then (
				(global_scalefactors k.header_samplerate true).(9 / 3) lsr 1 * 3
			) else (
				(global_scalefactors k.header_samplerate false).(8) lsr 1
			) in

			(min region0 gc.gc_big_values, max 0 (gc.gc_big_values - region0), 0, w.other_table_select1, w.other_table_select2, 0)
		)
	) in
	if debug && debug_more then Printf.printf " L = (%d,%d,%d), ht = (%d,%d,%d)\n" region0 region1 region2 table0 table1 table2;

	let (index, r) = read_from_table   0   region0 global_ht.(table0) global_ht_linbits.(table0) (r_str, r_at) in
	let (index, r) = read_from_table index region1 global_ht.(table1) global_ht_linbits.(table1)       r       in
	let (index, r) = read_from_table index region2 global_ht.(table2) global_ht_linbits.(table2)       r       in

	if debug && debug_more then Printf.printf "DONE!\n";

	(* Count1 *)
	let rec read_count1_from_table index ht r = (
		if debug && debug_more then Printf.printf "%d = %d? %B. %d = %d? %B.\n" (snd r) r_to (snd r = r_to) index num_quants (index = num_quants);
		if snd r > r_to || index > num_quants then (
			if debug && debug_more then Printf.printf " OOPS! Decoding count1 went a little overboard (pos %d > %d or length %d > %d)\n" (snd r) r_to index num_quants;
			decoder_error_ref := true;
			(index, r)
		) else if snd r = r_to || index = num_quants then (
			(index, r)
		) else (
			let ((v1,w1,x1,y1),r) = huffman_decode r ht global_ht_bits in

			if debug && debug_more then Printf.printf " Found (%d,%d,%d,%d) for %d at %d (to %d)\n" v1 w1 x1 y1 index (snd r) r_to;

			let (v_neg, r) = if v1 = 0 then (0, r) else (read_bits r 1) in
			let (w_neg, r) = if w1 = 0 then (0, r) else (read_bits r 1) in
			let (x_neg, r) = if x1 = 0 then (0, r) else (read_bits r 1) in
			let (y_neg, r) = if y1 = 0 then (0, r) else (read_bits r 1) in

			if debug then Printf.printf "  %s%s%s%s\n" (if v_neg = 1 then "-" else "+") (if w_neg = 1 then "-" else "+") (if x_neg = 1 then "-" else "+") (if y_neg = 1 then "-" else "+");

			let v = if v_neg = 1 then ~- v1 else v1 in
			let w = if w_neg = 1 then ~- w1 else w1 in
			let x = if x_neg = 1 then ~- x1 else x1 in
			let y = if y_neg = 1 then ~- y1 else y1 in

			if (index + 4) > num_quants && x = 0 && y = 0 then (
				(* If the ones above the max were 0, assume that the encoder just does that *)
				if debug && debug_more then Printf.printf " Num_quants exceeded, but the values are 0 so ignore (length %d > %d)\n" (index + 4) num_quants;
				out_quants.(index + 0) <- v;
				out_quants.(index + 1) <- w;
				(index + 2, r)
			) else if snd r > r_to || (index + 4) > num_quants then (
				(* The stuff above the max frequency is not zero! This indicates badness *)
				if debug && debug_more then Printf.printf " OOPS! Decoding count1 went a little overboard (pos %d > %d or length %d > %d)\n" (snd r) r_to (index + 4) num_quants;

				(* However, it is not considered an error as of 1.16; instead, the quants are set to 0 *)
(*				decoder_error_ref := true;*)

				(index, r)
			) else (
				out_quants.(index + 0) <- v;
				out_quants.(index + 1) <- w;
				out_quants.(index + 2) <- x;
				out_quants.(index + 3) <- y;

				read_count1_from_table (index + 4) ht r
			)
		)
	) in

	let (index, r) = read_count1_from_table index global_ht_count1.(if gc.gc_count1_table_1 then 1 else 0) r in

	if debug then Printf.printf "%sREAD_QUANTIZERS_M1: (%d) - [" tabs index;
	if debug then (Array.iter (fun x -> Printf.printf " %d" x) out_quants; Printf.printf " ]\n");
	(out_quants, !decoder_error_ref)
;;

(***
let read_quantizers_m1_old ?(debug=false) ?(tabs="") k gc (r_str,r_at) r_to =
	if debug && debug_more then Printf.printf "Starting on %S at %d, going to %d\n" (String.sub (to_bin r_str) r_at (r_to - r_at)) r_at r_to;

(*let debug = false in*)

	let decoder_error_ref = ref false in
	let out_quants = Array.make num_quants 0 in


	(* Big_values *)
	let rec read_from_table index left ht linbits r_now = (
if debug && debug_more then Printf.printf " Running read_from_table index=%d left=%d linbits=%d\n" index left linbits;
		if left = 0 then (index, r_now) else if left < 0 then (
			if debug then Printf.printf " OOPS! Decoding bigvalues went %d bytes overboard (now at %d)\n" ( ~- left) (snd r_now);
			decoder_error_ref := true;
			(index, r_now)
		) else (
			let ((abs_x,abs_y), r) = huffman_decode r_now ht global_ht_bits in
			if debug && debug_more then Printf.printf " Found (%d,%d) for %d at %d\n" abs_x abs_y index (snd r_now);

			let (x_add_lin, r) = if abs_x = 15 && linbits > 0 then (
				read_bits r linbits
			) else (0, r) in

			let (x_neg, r) = if abs_x > 0 then (
				read_bits r 1
			) else (0, r) in

			let (y_add_lin, r) = if abs_y = 15 && linbits > 0 then (
				read_bits r linbits
			) else (0, r) in
			let (y_neg, r) = if abs_y > 0 then (
				read_bits r 1
			) else (0, r) in
			let x_mult = if x_neg = 0 then 1 else -1 in
			let y_mult = if y_neg = 0 then 1 else -1 in

			if debug && debug_more then Printf.printf "  Add (%d,%d)\n" x_add_lin y_add_lin;
			if debug && debug_more then Printf.printf "  Multiply by (%d,%d)\n" x_mult y_mult;

			out_quants.(index) <- (abs_x + x_add_lin) * x_mult;
			out_quants.(index + 1) <- (abs_y + y_add_lin) * y_mult;

if debug && debug_more then Printf.printf "1\n";

			read_from_table (index + 2) (left - 1) ht linbits r
		)
	) in

	let (region0, region1, region2, table0, table1, table2) = (match gc.gc_window with
		| Window_normal w -> (
			let scfend0 = w.normal_region_0_count + 1 in
			let scfend1 = w.normal_region_1_count + 1 + scfend0 in
			let region0 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend0) lsr 1) in
			let region1 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend1) lsr 1) - region0 in
			(region0, region1, gc.gc_big_values - region0 - region1, w.normal_table_select1, w.normal_table_select2, w.normal_table_select3)
		)
		| Window_other  w -> (
			let region0 = if w.other_block_type = Block_type_short && not w.other_mixed_block then (
				(global_scalefactors k.header_samplerate true).(9 / 3) lsr 1 * 3
			) else (
				(global_scalefactors k.header_samplerate false).(8) lsr 1
			) in

			(min region0 gc.gc_big_values, max 0 (gc.gc_big_values - region0), 0, w.other_table_select1, w.other_table_select2, 0)
		)
	) in
	if debug && debug_more then Printf.printf " L = (%d,%d,%d), ht = (%d,%d,%d)\n" region0 region1 region2 table0 table1 table2;

	let (index, r) = read_from_table   0   region0 global_ht.(table0) global_ht_linbits.(table0) (r_str, r_at) in
	let (index, r) = read_from_table index region1 global_ht.(table1) global_ht_linbits.(table1)       r       in
	let (index, r) = read_from_table index region2 global_ht.(table2) global_ht_linbits.(table2)       r       in

if debug && debug_more then Printf.printf "DONE!\n";

	(* Count1 *)
	let rec read_count1_from_table index ht r = (
		if debug && debug_more then Printf.printf "%d = %d? %B. %d = %d? %B.\n" (snd r) r_to (snd r = r_to) index num_quants (index = num_quants);
		if snd r > r_to || index > num_quants then (
			if debug && debug_more then Printf.printf " OOPS! Decoding count1 went a little overboard (pos %d > %d or length %d > %d)\n" (snd r) r_to index num_quants;
			decoder_error_ref := true;
			(index, r)
		) else if snd r = r_to || index = num_quants then (
			(index, r)
		) else (
			let ((v1,w1,x1,y1),r) = huffman_decode r ht global_ht_bits in

			if debug && debug_more then Printf.printf " Found (%d,%d,%d,%d) for %d at %d (to %d)\n" v1 w1 x1 y1 index (snd r) r_to;

			let (v_neg, r) = if v1 = 0 then (0, r) else (read_bits r 1) in
			let (w_neg, r) = if w1 = 0 then (0, r) else (read_bits r 1) in
			let (x_neg, r) = if x1 = 0 then (0, r) else (read_bits r 1) in
			let (y_neg, r) = if y1 = 0 then (0, r) else (read_bits r 1) in

			if debug then Printf.printf "  %s%s%s%s\n" (if v_neg = 1 then "-" else "+") (if w_neg = 1 then "-" else "+") (if x_neg = 1 then "-" else "+") (if y_neg = 1 then "-" else "+");

			let v = if v_neg = 1 then ~- v1 else v1 in
			let w = if w_neg = 1 then ~- w1 else w1 in
			let x = if x_neg = 1 then ~- x1 else x1 in
			let y = if y_neg = 1 then ~- y1 else y1 in

			if (index + 4) > num_quants && x = 0 && y = 0 then (
				(* If the ones above the max were 0, assume that the encoder just does that *)
				if debug && debug_more then Printf.printf " Num_quants exceeded, but the values are 0 so ignore (length %d > %d)\n" (index + 4) num_quants;
				out_quants.(index + 0) <- v;
				out_quants.(index + 1) <- w;
				(index + 2, r)
			) else if snd r > r_to || (index + 4) > num_quants then (
				(* The stuff above the max frequency is not zero! This indicates badness *)
				if debug && debug_more then Printf.printf " OOPS! Decoding count1 went a little overboard (pos %d > %d or length %d > %d)\n" (snd r) r_to (index + 4) num_quants;

				(* However, it is not considered an error as of 1.16; instead, the quants are set to 0 *)
(*				decoder_error_ref := true;*)

				(index, r)
			) else (
				out_quants.(index + 0) <- v;
				out_quants.(index + 1) <- w;
				out_quants.(index + 2) <- x;
				out_quants.(index + 3) <- y;

				read_count1_from_table (index + 4) ht r
			)
		)
	) in

	let (index, r) = read_count1_from_table index global_ht_count1.(if gc.gc_count1_table_1 then 1 else 0) r in

	if debug then Printf.printf "%sREAD_QUANTIZERS_M1: (%d) - [" tabs index;
	if debug then (Array.iter (fun x -> Printf.printf " %d" x) out_quants; Printf.printf " ]\n");
	(out_quants, !decoder_error_ref)
;;
***)

(* WORKING *)
(*
let read_quantizers_m2 ?(debug=false) ?(tabs="") k gc (r.str,r_at) r_to =
	if debug && debug_more then Printf.printf "Starting on %S at %d, going to %d\n" (String.sub (to_bin r_str) r_at (r_to - r_at)) r_at r_to;

	let decoder_error_ref = ref false in
	let out_quants = Array.make num_quants 0 in

	(* Big_values *)
	let rec read_from_table index left ht linbits r_now
;;
WORKING;;
*)

(*********************************************************************************)
(* DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME *)
(*********************************************************************************)
let decode_frame ?(debug = false) f =
	let r = (f.f1_side.side_raw, 0) in
	let (side_info, r) = match f.f1_header.header_id with
		| MPEG1 -> (

			let read_gc r part2_3_offset = (
				let (part2_3_length, r) = read_bits r 12 in
				let (big_values, r) = read_bits r 9 in
				let (global_gain, r) = read_bits r 8 in
				let (scf_compress, r) = read_bits r 4 in
				let (window_flag, r) = read_bits r 1 in
				let (window, r) = if window_flag = 0 then (
					let (huff1, r) = read_bits r 5 in
					let (huff2, r) = read_bits r 5 in
					let (huff3, r) = read_bits r 5 in
					let (r0, r) = read_bits r 4 in
					let (r1, r) = read_bits r 3 in
					(Window_normal {
						normal_table_select1 = huff1;
						normal_table_select2 = huff2;
						normal_table_select3 = huff3;
						normal_region_0_count = r0;
						normal_region_1_count = r1;
					}, r)
				) else (
					let (block_type_index, r) = read_bits r 2 in
					let (mixed_block, r) = read_bits r 1 in
					let (huff1, r) = read_bits r 5 in
					let (huff2, r) = read_bits r 5 in
					let (sb_gain1, r) = read_bits r 3 in
					let (sb_gain2, r) = read_bits r 3 in
					let (sb_gain3, r) = read_bits r 3 in
					(Window_other {
						other_block_type = [| Block_type_long; Block_type_start; Block_type_short; Block_type_stop |].(block_type_index);
						other_mixed_block = (mixed_block = 1);
						other_table_select1 = huff1;
						other_table_select2 = huff2;
						other_sub_block_gain1 = sb_gain1;
						other_sub_block_gain2 = sb_gain2;
						other_sub_block_gain3 = sb_gain3;
					}, r)
				) in
				let (pre_flag, r) = read_bits r 1 in
				let (sf_scale, r) = read_bits r 1 in
				let (count1_table, r) = read_bits r 1 in
				({
					gc_part2_3_length = part2_3_length;
					gc_part2_3_offset = part2_3_offset;
					gc_big_values = big_values;
					gc_global_gain = global_gain;
					gc_scf_compress_index = scf_compress;
					gc_window = window;
					gc_pre_flag = (pre_flag = 1);
					gc_sf_scale = sf_scale;
					gc_count1_table_1 = (count1_table = 1);
				}, r, part2_3_offset + part2_3_length)
			) in

			match f.f1_header.header_channel_mode with
			| ChannelMono -> (
				let (main_data, r) = read_bits r 9 in
				let (_, r) = read_bits r 5 in
				let (a, r) = read_bits r 1 in
				let (b, r) = read_bits r 1 in
				let (c, r) = read_bits r 1 in
				let (d, r) = read_bits r 1 in
				let side_scfi = [| [| a = 1; b = 1; c = 1; d = 1 |] |] in
				let (side_gc1, r, new_so_far) = read_gc r 0 in
				let (side_gc2, r, new_so_far) = read_gc r new_so_far in
				({
					side_main_data_begin = main_data;
					side_scfi = side_scfi;
					side_gc = [| [| side_gc1 |]; [| side_gc2 |] |];
				}, r)
			)
			| _ -> (
				let (main_data, r) = read_bits r 9 in
				let (_, r) = read_bits r 3 in
				let (a, r) = read_bits r 1 in
				let (b, r) = read_bits r 1 in
				let (c, r) = read_bits r 1 in
				let (d, r) = read_bits r 1 in
				let (e, r) = read_bits r 1 in
				let (f, r) = read_bits r 1 in
				let (g, r) = read_bits r 1 in
				let (h, r) = read_bits r 1 in
				let side_scfi = [| [| a = 1; b = 1; c = 1; d = 1 |]; [| e = 1; f = 1; g = 1; h = 1 |] |] in
				let (side_gc1, r, new_so_far) = read_gc r 0 in
				let (side_gc2, r, new_so_far) = read_gc r new_so_far in
				let (side_gc3, r, new_so_far) = read_gc r new_so_far in
				let (side_gc4, r, new_so_far) = read_gc r new_so_far in
				({
					side_main_data_begin = main_data;
					side_scfi = side_scfi;
					side_gc = [| [| side_gc1; side_gc2 |]; [| side_gc3; side_gc4 |] |];
				}, r)
			)
		)
		| _ -> (

			let read_gc r part2_3_offset = (
				let (part2_3_length, r) = read_bits r 12 in
				let (big_values, r) = read_bits r 9 in
				let (global_gain, r) = read_bits r 8 in
				let (scf_compress, r) = read_bits r 9 in
				let (window_flag, r) = read_bits r 1 in
				let (window, r) = if window_flag = 0 then (
					let (huff1, r) = read_bits r 5 in
					let (huff2, r) = read_bits r 5 in
					let (huff3, r) = read_bits r 5 in
					let (r0, r) = read_bits r 4 in
					let (r1, r) = read_bits r 3 in
					(Window_normal {
						normal_table_select1 = huff1;
						normal_table_select2 = huff2;
						normal_table_select3 = huff3;
						normal_region_0_count = r0;
						normal_region_1_count = r1;
					}, r)
				) else (
					let (block_type_index, r) = read_bits r 2 in
					let (mixed_block, r) = read_bits r 1 in
					let (huff1, r) = read_bits r 5 in
					let (huff2, r) = read_bits r 5 in
					let (sb_gain1, r) = read_bits r 3 in
					let (sb_gain2, r) = read_bits r 3 in
					let (sb_gain3, r) = read_bits r 3 in
					(Window_other {
						other_block_type = [| Block_type_long; Block_type_start; Block_type_short; Block_type_stop |].(block_type_index);
						other_mixed_block = (mixed_block = 1);
						other_table_select1 = huff1;
						other_table_select2 = huff2;
						other_sub_block_gain1 = sb_gain1;
						other_sub_block_gain2 = sb_gain2;
						other_sub_block_gain3 = sb_gain3;
					}, r)
				) in
				let (sf_scale, r) = read_bits r 1 in
				let (count1_table, r) = read_bits r 1 in
				({
					gc_part2_3_length = part2_3_length;
					gc_part2_3_offset = part2_3_offset;
					gc_big_values = big_values;
					gc_global_gain = global_gain;
					gc_scf_compress_index = scf_compress;
					gc_window = window;
					gc_pre_flag = false; (* No pre flag for MPEG2 *)
					gc_sf_scale = sf_scale;
					gc_count1_table_1 = (count1_table = 1);
				}, r, part2_3_offset + part2_3_length)
			) in

			match f.f1_header.header_channel_mode with
			| ChannelMono -> (
				let (main_data, r) = read_bits r 8 in
				let (_, r) = read_bits r 1 in
				let (side_gc, r, _) = read_gc r 0 in
				({
					side_main_data_begin = main_data;
					side_scfi = [| |];
					side_gc = [| [| side_gc |] |]
				}, r)
			)
			| _ -> (
				let (main_data, r) = read_bits r 8 in
				let (_, r) = read_bits r 2 in
				let (side_gc1, r, new_so_far) = read_gc r 0 in
				let (side_gc2, r, new_so_far) = read_gc r new_so_far in
				({
					side_main_data_begin = main_data;
					side_scfi = [| |];
					side_gc = [| [| side_gc1; side_gc2 |] |]
				}, r)
			) (* Mono / stereo *)
		) (* MPEG 2/2.5 *)
	in (* defines side_info *)

(*	print_side side_info;*)

	let tab2 = tab ^ tab in

(*
	for granule = 0 to Array.length side_info.side_gc - 1 do
		for channel = 0 to Array.length side_info.side_gc.(granule) - 1 do
			if debug then Printf.printf "DOING GR %d, CH %d\n" granule channel;
			decode_granule_channel f side_info granule channel;
		done;
	done;
*)

	let decoded = (
		let k = f.f1_header in
		match (f.f1_header.header_id, f.f1_header.header_channel_mode) with
		| (MPEG1, ChannelMono) -> (
			(* Frame has 1 channel, 2 granules *)
			(* First, get the first scalefactors *)
			if debug then Printf.printf "DECODE FRAME %d (MPEG1, MONO)\n" f.f1_num;

			if debug then (
				Printf.printf "%sSide %S\n" tab2 (to_hex f.f1_side.side_raw);
				Printf.printf "%sData %S\n" tab2 (to_hex f.f1_data);
				print_side ~tabs:tab2 side_info;
			);

			if debug then Printf.printf "%sGr0:\n" tab;
			let (scf0,r0) = read_scalefactors_m1 ~debug:debug ~tabs:tab2 [| false;false;false;false |]  None       side_info.side_gc.(0).(0) (f.f1_data, side_info.side_gc.(0).(0).gc_part2_3_offset) in
			let (q0, error0) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) r0 (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) in

			if debug then Printf.printf "%sGr1:\n" tab;
			let (scf1,r1) = read_scalefactors_m1 ~debug:debug ~tabs:tab2 side_info.side_scfi.(0)       (Some scf0) side_info.side_gc.(1).(0) (f.f1_data, side_info.side_gc.(1).(0).gc_part2_3_offset) in
			let (q1, error1) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(1).(0) r1 (side_info.side_gc.(1).(0).gc_part2_3_length + side_info.side_gc.(1).(0).gc_part2_3_offset) in

			(
				M1_frame_data {
					m1_header = f.f1_header;
					m1_side_info = side_info;
					m1_scalefactors = [| [| scf0  |]; [| scf1 |] |];
					m1_quantizers = [| [| q0 |]; [| q1 |] |];
					m1_starting_f1 = f;
				}
			,
				error0 || error1
			)
		)
		| (MPEG1, _) -> (
			(* Frame has 2 channels, 2 granules *)
			(* First, get the first scalefactors *)
			if debug then Printf.printf "DECODE FRAME %d (MPEG1, STEREO)\n" f.f1_num;

			if debug then (
				Printf.printf "%sSide %S\n" tab (to_hex f.f1_side.side_raw);
				Printf.printf "%sData %S\n" tab (to_hex f.f1_data);
				print_side ~tabs:tab side_info;
			);

(*Printf.fprintf file_in "%S\n" (String.sub (to_bin f.transitData) 0 f.transitBits);*)
(*flush file_out;*)

			if debug then Printf.printf "%sGr0 Ch0:\n" tab;
			let (scf00,r00) = read_scalefactors_m1 ~debug:debug ~tabs:tab2 [| false;false;false;false |]  None        side_info.side_gc.(0).(0) (f.f1_data, side_info.side_gc.(0).(0).gc_part2_3_offset) in
			let (q00,error00) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) r00 (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) in
(*			let (q00,error00) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) r00 (side_info.side_gc.(1).(1).gc_part2_3_length + side_info.side_gc.(1).(1).gc_part2_3_offset) in*)

			if debug then Printf.printf "%sGr0 Ch1:\n" tab;
			let (scf01,r01) = read_scalefactors_m1 ~debug:debug ~tabs:tab2 [| false;false;false;false |]  None        side_info.side_gc.(0).(1) (f.f1_data, side_info.side_gc.(0).(1).gc_part2_3_offset) in
			let (q01,error01) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(1) r01 (side_info.side_gc.(0).(1).gc_part2_3_length + side_info.side_gc.(0).(1).gc_part2_3_offset) in
(*			let (q01,error01) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(1) r01 (side_info.side_gc.(1).(1).gc_part2_3_length + side_info.side_gc.(1).(1).gc_part2_3_offset) in*)

			if debug then Printf.printf "%sGr1 Ch0:\n" tab;
			let (scf10,r10) = read_scalefactors_m1 ~debug:debug ~tabs:tab2 side_info.side_scfi.(0)       (Some scf00) side_info.side_gc.(1).(0) (f.f1_data, side_info.side_gc.(1).(0).gc_part2_3_offset) in
			let (q10,error10) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(1).(0) r10 (side_info.side_gc.(1).(0).gc_part2_3_length + side_info.side_gc.(1).(0).gc_part2_3_offset) in
(*			let (q10,error10) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(1).(0) r10 (side_info.side_gc.(1).(1).gc_part2_3_length + side_info.side_gc.(1).(1).gc_part2_3_offset) in*)

			if debug then Printf.printf "%sGr1 Ch1:\n" tab;
			let (scf11,r11) = read_scalefactors_m1 ~debug:debug ~tabs:tab2 side_info.side_scfi.(1)       (Some scf10) side_info.side_gc.(1).(1) (f.f1_data, side_info.side_gc.(1).(1).gc_part2_3_offset) in
			let (q11,error11) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(1).(1) r11 (side_info.side_gc.(1).(1).gc_part2_3_length + side_info.side_gc.(1).(1).gc_part2_3_offset) in
(*			let (q11,error11) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(1).(1) r11 (side_info.side_gc.(1).(1).gc_part2_3_length + side_info.side_gc.(1).(1).gc_part2_3_offset) in*)

			(
				M1_frame_data {
					m1_header = f.f1_header;
					m1_side_info = side_info;
					m1_scalefactors = [| [| scf00; scf01 |]; [| scf10; scf11 |] |];
					m1_quantizers = [| [| q00; q01 |]; [| q10; q11 |] |];
					m1_starting_f1 = f;
				}
			,
				error00 || error01 || error10 || error11
			)
		)
		| (_, ChannelMono) -> (
			if debug then Printf.printf "DECODE FRAME %d (MPEG2, MONO)\n" f.f1_num;

			if debug then (
				Printf.printf "%sSide %S\n" tab2 (to_hex f.f1_side.side_raw);
				Printf.printf "%sData %S\n" tab2 (to_hex f.f1_data);
				print_side ~tabs:tab2 side_info;
			);

			if debug then Printf.printf "%sGr: (USING MPEG1 QUANTIZER FUNCTION)\n" tab;
			let (scf, r) = read_scalefactors_m2 ~debug:debug ~tabs:tab2 false side_info.side_gc.(0).(0) (f.f1_data, side_info.side_gc.(0).(0).gc_part2_3_offset) in
			let (q, error) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) r (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) in

			(
				M2_frame_data {
					m2_header = f.f1_header;
					m2_side_info = side_info;
					m2_data_deleteme = f.f1_data;
					m2_scalefactors = [| scf |];
					m2_quantizers = [| q |];
					m2_starting_f1 = f;
				}
			,
				error
			)

(*			(M2_frame_data {m2_header = f.f1_header; m2_side_info = side_info; m2_data_deleteme = f.f1_data; m2_starting_f1 = f}, false)*)
		)
		| (_, _) -> (
			if debug then Printf.printf "DECODE FRAME %d (MPEG2, STEREO)\n" f.f1_num;

			if debug then (
				Printf.printf "%sSide %S\n" tab2 (to_hex f.f1_side.side_raw);
				Printf.printf "%sData %S\n" tab2 (to_hex f.f1_data);
				print_side ~tabs:tab2 side_info;
			);

			(* Remember that the IS should only be set on the right channel; the left channel uses the same scalefactors as non-IS GCs *)
			if debug then Printf.printf "%sGr0: (USING MPEG1 QUANTIZER READER)\n" tab;
			let (scf0, r0) = read_scalefactors_m2 ~debug:debug ~tabs:tab2         false         side_info.side_gc.(0).(0) (f.f1_data, side_info.side_gc.(0).(0).gc_part2_3_offset) in
			let (q0, error0) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) r0 (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) in

			if debug then Printf.printf "%sGr1: (USING MPEG1 QUANTIZER READER)\n" tab;
			let (scf1, r1) = read_scalefactors_m2 ~debug:debug ~tabs:tab2 f.f1_header.header_is side_info.side_gc.(0).(1) (f.f1_data, side_info.side_gc.(0).(1).gc_part2_3_offset) in
			let (q1, error1) = read_quantizers_m1 ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(1) r1 (side_info.side_gc.(0).(1).gc_part2_3_length + side_info.side_gc.(0).(1).gc_part2_3_offset) in

			(
				M2_frame_data {
					m2_header = f.f1_header;
					m2_side_info = side_info;
					m2_data_deleteme = f.f1_data;
					m2_scalefactors = [| scf0; scf1 |];
					m2_quantizers = [| q0; q1 |];
					m2_starting_f1 = f;
				}
			,
				error0 || error1
			)


(*			(M2_frame_data {m2_header = f.f1_header; m2_side_info = side_info; m2_data_deleteme = f.f1_data; m2_scalefactors = [||]; m2_quantizers = [||]; m2_starting_f1 = f}, false)*)
		)
	) in

(*	decode_granule_channel f side_info*)

	decoded
;;






(************************************************************************************************************************)
(* REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME *)
(************************************************************************************************************************)
let deleteme_ref = ref true;;
let rehuff_frame ?(debug=false) frame = (

(*	let debug = (if !deleteme_ref then (deleteme_ref := false; true) else false) in*)
(*	let debug = true in*)
	if debug then Printf.printf "\n";

	let rehuff_granule quants gc scf_bands = (match gc.gc_window with
		| Window_other w -> {
			gc_part2_3_length = gc.gc_part2_3_length; (* This isn't used for anything during the re-encoding process, so it's OK to not calculate it here *)
			gc_part2_3_offset = gc.gc_part2_3_offset; (* This isn't actually used for anything... *)
			gc_big_values = gc.gc_big_values; (* This is NOT recalculated during encoding *)
			gc_global_gain = gc.gc_global_gain;
			gc_scf_compress_index = gc.gc_scf_compress_index;
			gc_window = Window_other w;
			gc_pre_flag = gc.gc_pre_flag;
			gc_sf_scale = gc.gc_sf_scale;
			gc_count1_table_1 = gc.gc_count1_table_1;
		}
		| Window_normal w -> (

			let num_scf_bands = Array.length scf_bands - 1 in

			let find_quant_band q = (
				let rec find_quant_band_rec next_band = if next_band > num_scf_bands then (num_scf_bands, q - scf_bands.(num_scf_bands),false) else (
					if q < scf_bands.(next_band) then (
						(pred next_band, q - scf_bands.(pred next_band), q = scf_bands.(next_band) - 1)
					) else (
						find_quant_band_rec (succ next_band)
					)
				) in
				if q < 0 then (-1,-1,true) else find_quant_band_rec 1
			) in

			let last_two_zero = (quants.(Array.length quants - 1) = 0 && quants.(Array.length quants - 2) = 0) in
			let bits_per_band_table = Array.init num_scf_bands (fun b -> Array.init 32 (fun t -> if t = 4 || t = 14 then 100000 else 0)) in
(*			let bits_per_first_range_table = Array.init 16 (fun b -> Array.init 32 (fun t -> if t = 4 || t = 14 then 100000 else 0)) in*) (* .(0) is the number of bits when the first partition ends at the end of band 0, etc. *)
			let zeros_at_beginning_of_band = Array.make num_scf_bands max_int in

			let last_nonzero_quant_ref = ref ~-1 in
			let last_nonzero_band_ref = ref ~-1 in
			let last_big_quant_ref = ref ~-1 in
			let last_big_band_ref = ref ~-1 in
			
			let rec do_band band_now = if band_now < 0 then () else (
				let first_quant_in_band = scf_bands.(band_now) in
				let num_quants_in_band = scf_bands.(succ band_now) - first_quant_in_band in
				let num_15_or_more_in_band_ref = ref 0 in
				let max_quant_in_band_ref = ref 0 in
				let bt = bits_per_band_table.(band_now) in
				
				let rec do_quant quant_add = if quant_add < 0 then () else (
					let quant_total = first_quant_in_band + quant_add in
					let x = abs quants.(quant_total) in
					let y = abs quants.(quant_total + 1) in
					let x15 = min x 15 in
					let y15 = min y 15 in

					if x15 = 15 then incr num_15_or_more_in_band_ref;
					if y15 = 15 then incr num_15_or_more_in_band_ref;

					(
						(* Update the last big/nonzero quants *)
						if x15 > 1 || y15 > 1 then (
							last_nonzero_quant_ref := max (quant_total + 1) !last_nonzero_quant_ref;
							last_big_quant_ref := max (quant_total + 1) !last_big_quant_ref;
						) else if x15 = 1 || y15 = 1 then (
							last_nonzero_quant_ref := max (quant_total + 1) !last_nonzero_quant_ref;
						)
					);

					max_quant_in_band_ref := max !max_quant_in_band_ref (max x y);

(*if !last_nonzero_quant_ref > -1 then ( *)

					(
						(* Add the bits to the bits_per_band_table *)
						let code = (x15 lsl 4) lor y15 in
						for t = 0 to 15 do
							if t <> 4 && t <> 14 then (
								match global_ht_encode.(t).(code) with
								| (0,1) -> bt.(t) <- 100000
								| (b,_) -> bt.(t) <- bt.(t) + b
							)
						done;
						(* Do tables 16 and 24 *)
						(
							let (b,_) = global_ht_encode.(16).(code) in
							bt.(16) <- bt.(16) + b
						);
						(
							let (b,_) = global_ht_encode.(24).(code) in
							bt.(24) <- bt.(24) + b
						);
					);
					
					if x <> 0 || y <> 0 then (
						(* Update the number of zeros at the beginning of the band *)
						zeros_at_beginning_of_band.(band_now) <- quant_add
					);

(* ); *)
					
					do_quant (quant_add - 2)
				) in
				do_quant (num_quants_in_band - 2);

				(
					(* Update tables 16-23 and 24-31, using tables 16 and 24 for the base bits, and num_15_or_more_in_band_ref for linbit calculation *)
					let b16 = bt.(16) in
					let b24 = bt.(24) in
					let n15 = !num_15_or_more_in_band_ref in
					let mq = !max_quant_in_band_ref in
					bt.(16) <- if mq >   16 then 100000 else b16 + n15 * 1;
					bt.(17) <- if mq >   18 then 100000 else b16 + n15 * 2;
					bt.(18) <- if mq >   22 then 100000 else b16 + n15 * 3;
					bt.(19) <- if mq >   30 then 100000 else b16 + n15 * 4;
					bt.(20) <- if mq >   78 then 100000 else b16 + n15 * 6;
					bt.(21) <- if mq >  270 then 100000 else b16 + n15 * 8;
					bt.(22) <- if mq > 1038 then 100000 else b16 + n15 * 10;
					bt.(23) <- if mq > 8206 then 100000 else b16 + n15 * 13;
					bt.(24) <- if mq >   30 then 100000 else b24 + n15 * 4;
					bt.(25) <- if mq >   46 then 100000 else b24 + n15 * 5;
					bt.(26) <- if mq >   78 then 100000 else b24 + n15 * 6;
					bt.(27) <- if mq >  142 then 100000 else b24 + n15 * 7;
					bt.(28) <- if mq >  270 then 100000 else b24 + n15 * 8;
					bt.(29) <- if mq >  526 then 100000 else b24 + n15 * 9;
					bt.(30) <- if mq > 2062 then 100000 else b24 + n15 * 11;
					bt.(31) <- if mq > 8206 then 100000 else b24 + n15 * 13;
					(* Also void tables 13 and 15 if the max quant is greater than 15. These two tables are not thrown out during the quant iteration *)
					if mq > 15 then (
						bt.(13) <- 100000;
						bt.(15) <- 100000;
					)
				);
				
				(* Update the last big/nonzero band *)
				if !last_big_quant_ref >= 0 then (
					last_nonzero_band_ref := max !last_nonzero_band_ref band_now;
					last_big_band_ref := max !last_big_band_ref band_now;
				) else if !last_nonzero_quant_ref >= 0 then (
					last_nonzero_band_ref := max !last_nonzero_band_ref band_now;
				);
				
				do_band (band_now - 1)
			) in
			do_band (pred num_scf_bands);

			let do_ends_ref = ref (
				if true then (
					if last_two_zero then (
						let rec find_end so_far now = if now <= !last_big_band_ref then so_far else (
							if zeros_at_beginning_of_band.(now) >= scf_bands.(now + 1) - scf_bands.(now) then (
								(* The entire band is zero; check to see if the next band has any zeros at the beginning *)
								(* Note that there is always at least 4 quants in a band, therefore the endpoint at two quants before the last zero is always there *)
								if zeros_at_beginning_of_band.(succ now) = 0 then (
									let quants_in_band = scf_bands.(now + 1) - scf_bands.(now) in
									find_end ((now, quants_in_band - 3, false) :: (now, quants_in_band - 1, true) :: so_far) (pred now)
								) else (
									(* Already been added; ignore *)
									find_end so_far (pred now)
								)
							) else if zeros_at_beginning_of_band.(now) > 3 then (
								(* Mark as TWO possible end points - one at the last zero and one two quants before that *)
								find_end ((now, zeros_at_beginning_of_band.(now) - 3, false) :: (now, zeros_at_beginning_of_band.(now) - 1, false) :: so_far) (pred now)
							) else if zeros_at_beginning_of_band.(now) > 1 then (
								find_end ((now, zeros_at_beginning_of_band.(now) - 1, false) :: so_far) (pred now)
							) else (
								(* No zeros! Keep going... *)
								find_end so_far (pred now)
							)
						) in
						find_end [] !last_nonzero_band_ref 
					) else (
						(* Last 2 are NOT 0 *)
						(* This can be optimized... *)
						[]
					)
				) else (
					[]
				)
			) in
			
			if last_two_zero then (
				if !last_big_quant_ref < 0 then (
					(* There are no big values! Oh noes! *)
					do_ends_ref := (-1,-1,true) :: !do_ends_ref;
				) else if !last_big_quant_ref = scf_bands.(num_scf_bands) - 1 then (
					(* It's the last quant *)
					do_ends_ref := (num_scf_bands - 1, scf_bands.(num_scf_bands) - scf_bands.(num_scf_bands - 1) - 1, true) :: !do_ends_ref
				) else (
					(* Normal *)
					do_ends_ref := find_quant_band !last_big_quant_ref :: find_quant_band (!last_big_quant_ref + 2) :: !do_ends_ref
				)
			) else (
				let real_end = !last_big_quant_ref lor 3 in
				if real_end < 0 then (
					(* There are no big values! Oh noes! *)
					do_ends_ref := (-1,-1,true) :: !do_ends_ref;
				) else (
					(* Normal *)
					do_ends_ref := find_quant_band real_end :: !do_ends_ref
				)
			);
			
			if debug then (
				Printf.printf "  Last two are zero?  %B\n" last_two_zero;
				Printf.printf "  Last nonzero quant: %d in band %d\n" !last_nonzero_quant_ref !last_nonzero_band_ref;
				Printf.printf "  Last big quant:     %d in band %d\n" !last_big_quant_ref !last_big_band_ref;
				Printf.printf "  Bits per band table:\n";
				Array.iter (fun band ->
					Printf.printf "  ";
					Array.iter (fun table ->
						if table >= 100000 then (
							Printf.printf "    -"
						) else (
							Printf.printf " %4d" table
						)
					) band;
					Printf.printf "\n";
				) bits_per_band_table;
				Printf.printf "  Zeros at beginning of band:\n";
				Printf.printf "  ";
				Array.iter (fun band -> if band = max_int then Printf.printf " ALL" else Printf.printf " %3d" band) zeros_at_beginning_of_band;
				Printf.printf "\n";
				Printf.printf "  Ends to check:\n";
				Printf.printf "   [";
				List.iter (fun (a,b,c) -> Printf.printf " (%d,%d,%B)" a b c) !do_ends_ref;
				Printf.printf " ]\n";
			);

			(* RANGE CACHE! *)
			let range_cache = Array.init num_scf_bands (fun a -> Array.init (num_scf_bands - a) (fun b -> None)) in
			let cache a b = (
				match range_cache.(a).(b - a) with
				| Some x -> (if debug then Printf.printf "O"; x)
				| None -> (
					if debug then Printf.printf "X";
					let rec check_table smallest_bits smallest_table t = (
						if t > 31 then (
							(smallest_bits, smallest_table)
						) else (
							let rec check_band so_far band_now = (
								if band_now > b then (
									so_far
								) else (
									check_band (so_far + bits_per_band_table.(band_now).(t)) (succ band_now)
								)
							) in
							let current_table_bits = check_band 0 a in
							if current_table_bits = 0 then (
								(* Can't get much smaller than 0 *)
								(t, 0)
							) else if current_table_bits < smallest_bits then (
								check_table current_table_bits t (succ t)
							) else (
								check_table smallest_bits smallest_table (succ t)
							)
						)
					) in
					let answer = check_table max_int ~-1 0 in
					range_cache.(a).(b - a) <- Some answer;
					answer
				)
			) in

			(* Use table 1? *)
			let smallest_part1 q_from q_to = (
				let num_part1 = (q_to - q_from) lor 3 + 1 in
				let table1_bits = num_part1 in
				let table0_bits = (
					let rec iter so_far now = (
						if now > q_to then so_far else (
							if now + 3 < Array.length quants then (
								let (bits,_) = global_ht_encode_count1.(0).(abs quants.(now) lsl 3 lor abs quants.(now + 1) lsl 2 lor abs quants.(now + 2) lsl 1 lor abs quants.(now + 3)) in
								iter (so_far + bits) (now + 4)
							) else if now + 3 = Array.length quants then (
								(* NONE OF THESE SHOULD HAPPEN! *)
								let (bits,_) = global_ht_encode_count1.(0).(abs quants.(now) lsl 3 lor abs quants.(now + 1) lsl 2 lor abs quants.(now + 2) lsl 1) in
								(so_far + bits)
							) else if now + 2 = Array.length quants then (
								let (bits,_) = global_ht_encode_count1.(0).(abs quants.(now) lsl 3 lor abs quants.(now + 1) lsl 2) in
								(so_far + bits)
							) else if now + 1 = Array.length quants then (
								let (bits,_) = global_ht_encode_count1.(0).(abs quants.(now) lsl 3) in
								(so_far + bits)
							) else (
								so_far
							)
						)
					) in
					iter 0 q_from
				) in
				(table1_bits < table0_bits, min table1_bits table0_bits)
			) in

			(* Now check all the ends in !do_ends_ref *)
			let smallest_config = List.fold_left (fun (so_far_bits, so_far_config) (new_end_band, new_end_quant, new_end_full) ->
				if debug then Printf.printf "   (%d,%d,%B):\n" new_end_band new_end_quant new_end_full;
				let (bits,config) = if new_end_band < 0 then (
					(* No big values! *)
					if debug then Printf.printf "    No big values; use only part1\n";
					let (use_table_1, bits) = smallest_part1 0 !last_nonzero_quant_ref in
					(bits, (0,0,0,0,0,0,use_table_1))
				) else (
					(* Some big values *)
					let last_quant = scf_bands.(new_end_band) + new_end_quant in
					let (part1_table_1, part1_bits) = smallest_part1 (succ last_quant) !last_nonzero_quant_ref in
					if debug then Printf.printf "    Part 1: table %d = %d bits\n" (if part1_table_1 then 1 else 0) part1_bits;
					let check_range = (
						if new_end_full then (
							if debug then Printf.printf "    Full band; use normal lookups\n";
							cache
						) else (
							(* Need to recreate the number of bits in the last band *)
							let from_quant = scf_bands.(new_end_band) in
							let last_band_bits = Array.init 32 (fun t -> if t = 4 || t = 14 then 100000 else 0) in
							let num_15_or_more_ref = ref 0 in
							let max_quant_ref = ref 0 in
							let rec do_quant quant_now = if quant_now > last_quant then () else (
								let x = abs quants.(quant_now) in
								let y = abs quants.(quant_now + 1) in
								let x15 = min x 15 in
								let y15 = min y 15 in
								
								if x15 = 15 then incr num_15_or_more_ref;
								if y15 = 15 then incr num_15_or_more_ref;
								
								max_quant_ref := max !max_quant_ref (max x y);
								
								(
									let code = (x15 lsl 4) lor y15 in
									for t = 0 to 15 do
										if t <> 4 && t <> 14 then (
											match global_ht_encode.(t).(code) with
											| (0,1) -> last_band_bits.(t) <- 100000
											| (b,_) -> last_band_bits.(t) <- last_band_bits.(t) + b
										)
									done;
									(* Tables 16 and 24 *)
									(
										let (b,_) = global_ht_encode.(16).(code) in
										last_band_bits.(16) <- last_band_bits.(16) + b
									);
									(
										let (b,_) = global_ht_encode.(24).(code) in
										last_band_bits.(24) <- last_band_bits.(24) + b
									);
								);
								
								do_quant (quant_now + 2)
							) in
							do_quant from_quant;
							(
								(* Update tables 16-23 and 24-31 *)
								let b16 = last_band_bits.(16) in
								let b24 = last_band_bits.(24) in
								let n15 = !num_15_or_more_ref in
								let mq = !max_quant_ref in
								last_band_bits.(16) <- if mq >   16 then 100000 else b16 + n15 * 1;
								last_band_bits.(17) <- if mq >   18 then 100000 else b16 + n15 * 2;
								last_band_bits.(18) <- if mq >   22 then 100000 else b16 + n15 * 3;
								last_band_bits.(19) <- if mq >   30 then 100000 else b16 + n15 * 4;
								last_band_bits.(20) <- if mq >   78 then 100000 else b16 + n15 * 6;
								last_band_bits.(21) <- if mq >  270 then 100000 else b16 + n15 * 8;
								last_band_bits.(22) <- if mq > 1038 then 100000 else b16 + n15 * 10;
								last_band_bits.(23) <- if mq > 8206 then 100000 else b16 + n15 * 13;
								last_band_bits.(24) <- if mq >   30 then 100000 else b24 + n15 * 4;
								last_band_bits.(25) <- if mq >   46 then 100000 else b24 + n15 * 5;
								last_band_bits.(26) <- if mq >   78 then 100000 else b24 + n15 * 6;
								last_band_bits.(27) <- if mq >  142 then 100000 else b24 + n15 * 7;
								last_band_bits.(28) <- if mq >  270 then 100000 else b24 + n15 * 8;
								last_band_bits.(29) <- if mq >  526 then 100000 else b24 + n15 * 9;
								last_band_bits.(30) <- if mq > 2062 then 100000 else b24 + n15 * 11;
								last_band_bits.(31) <- if mq > 8206 then 100000 else b24 + n15 * 13;
								if mq > 15 then (
									last_band_bits.(13) <- 100000;
									last_band_bits.(15) <- 100000;
								)
							);
							
							if debug then (
								Printf.printf "    Not full band. Bits in last band:\n";
								Printf.printf "    ";
								Array.iter (fun table ->
									if table >= 100000 then (
										Printf.printf "    -"
									) else (
										Printf.printf " %4d" table
									)
								) last_band_bits;
								Printf.printf "\n";
							);
							
							fun b_from b_to -> (
								if b_to < new_end_band then (
									(* Might as well add it to the cache *)
									cache b_from b_to
								) else (
									let rec check_table smallest_bits smallest_table t = (
										if t > 31 then (
											(smallest_bits, smallest_table)
										) else (
											let rec check_band so_far band_now = (
												if band_now > b_to then (
													so_far
												) else (
													let new_bits = (if band_now >= new_end_band then last_band_bits.(t) else bits_per_band_table.(band_now).(t)) in
													check_band (so_far + new_bits) (succ band_now)
												)
											) in
											let current_table_bits = check_band 0 b_from in
											if current_table_bits = 0 then (
												(* Can't gen much smaller than 0 *)
												(t, 0)
											) else if current_table_bits < smallest_bits then (
												check_table current_table_bits t (succ t)
											) else (
												check_table smallest_bits smallest_table (succ t)
											)
										)
									) in
									check_table max_int ~-1 0
								)
							)
						)
					) in

(*(try*)
					if new_end_band = 0 then (
						if debug then Printf.printf "    Big values end in band 0; just check the first partition\n";
						let (smallest_bits, smallest_table) = check_range 0 0 in
						(smallest_bits + part1_bits, (0, 0, (last_quant + 1) lsr 1, smallest_table, 0, 0, part1_table_1))
					) else if new_end_band = 1 then (
						if debug then Printf.printf "    Big values end in band 1; check the first two partitions\n";
						let (b0,t0) = check_range 0 0 in
						let (b1,t1) = check_range 1 1 in
						(b0 + b1 + part1_bits, (0, 0, (last_quant + 1) lsr 1, t0, t1, 0, part1_table_1))
					) else (
						if debug then Printf.printf "    Big values end after band 1; iterate normally\n";

						let min_bits_ref = ref max_int in
						let min_reg1_start_ref = ref 0 in
						let min_reg2_start_ref = ref 0 in
						let min_reg0_table_ref = ref 0 in
						let min_reg1_table_ref = ref 0 in
						let min_reg2_table_ref = ref 0 in
						for reg1_start = 1 to min 16 (new_end_band - 1) do
(*							let (b0a, t0a) = table_for_first_range_full_band.(reg1_start - 1) in*)
let (b0, t0) = check_range 0 (reg1_start - 1) in
							if debug then Printf.printf "     %02d=%d\n" t0 b0;
							for reg2_start = (reg1_start + 1) to min (reg1_start + 8) new_end_band do
								let (b1,t1) = check_range reg1_start (pred reg2_start) in
								let (b2,t2) = check_range reg2_start new_end_band in
								if debug then Printf.printf "      %02d,%02d - %02d,%02d = %d\n" reg1_start reg2_start t1 t2 (b0 + b1 + b2);
								if b0 + b1 + b2 < !min_bits_ref then (
									if debug then Printf.printf "       USE!\n";
									min_bits_ref := b0 + b1 + b2;
									min_reg1_start_ref := reg1_start;
									min_reg2_start_ref := reg2_start;
									min_reg0_table_ref := t0;
									min_reg1_table_ref := t1;
									min_reg2_table_ref := t2;
								)
							done;
						done;

						(!min_bits_ref + part1_bits, (!min_reg1_start_ref - 1, !min_reg2_start_ref - !min_reg1_start_ref - 1, (last_quant + 1) lsr 1, !min_reg0_table_ref, !min_reg1_table_ref, !min_reg2_table_ref, part1_table_1))
					)
(*
with
	x -> (Printf.printf "ERROR: %s\n" (Printexc.to_string x); raise x)
)
*)
				) in

				if bits < so_far_bits then (
					(bits, config)
				) else (
					(so_far_bits, so_far_config)
				)
			) (max_int, (0,0,0,0,0,0,false)) !do_ends_ref in (* Smallest_bits, (length1, length2, big_values, table1, table2, table3, part1_table_1) *)

			let (s_bits,(s_l1,s_l2,s_big,s_t1,s_t2,s_t3,p1t1)) = smallest_config in
			if debug then Printf.printf "  (%d,(%d,%d,%d,%d,%d,%d,%B)\n" s_bits s_l1 s_l2 s_big s_t1 s_t2 s_t3 p1t1;

			if debug then (
				Printf.printf "  Last cache:\n";
				Array.iter (fun b1 ->
					Printf.printf "  ";
					Array.iter (fun b2 ->
						match b2 with
						| None -> Printf.printf " (--,----)"
						| Some (a,b) -> Printf.printf " (%02d,%4d)" b a
					) b1;
					Printf.printf "\n";
				) range_cache;
			);

			if true then (
				{
					gc_part2_3_length = gc.gc_part2_3_length;
					gc_part2_3_offset = gc.gc_part2_3_offset;
					gc_big_values = s_big;
					gc_global_gain = gc.gc_global_gain;
					gc_scf_compress_index = gc.gc_scf_compress_index;
					gc_window = Window_normal {normal_table_select1 = s_t1; normal_table_select2 = s_t2; normal_table_select3 = s_t3; normal_region_0_count = s_l1; normal_region_1_count = s_l2};
					gc_pre_flag = gc.gc_pre_flag;
					gc_sf_scale = gc.gc_sf_scale;
					gc_count1_table_1 = p1t1;
				}
			) else (
				{
					gc_part2_3_length = gc.gc_part2_3_length; (* This isn't used for anything during the re-encoding process, so it's OK to not calculate it here *)
					gc_part2_3_offset = gc.gc_part2_3_offset; (* This isn't actually used for anything... *)
					gc_big_values = gc.gc_big_values; (* This is NOT recalculated during encoding *)
					gc_global_gain = gc.gc_global_gain;
					gc_scf_compress_index = gc.gc_scf_compress_index;
					gc_window = Window_normal w;
					gc_pre_flag = gc.gc_pre_flag;
					gc_sf_scale = gc.gc_sf_scale;
					gc_count1_table_1 = gc.gc_count1_table_1;
				}
			)
		)
	) in (* rehuff_granule quants gc scf_quants *)

	match frame with
	| M1_frame_data f -> (
		if debug then Printf.printf "REHUFF_FRAME M1\n";
		let new_granules = Array.mapi (fun gran_i quant_gran ->
			let new_channels = Array.mapi (fun chan_i quants ->
				if debug then Printf.printf " GC %d,%d\n" gran_i chan_i;
				let gc = f.m1_side_info.side_gc.(gran_i).(chan_i) in
				let scf_quants = global_scalefactors f.m1_header.header_samplerate false in
				rehuff_granule quants gc scf_quants
			) quant_gran in
			new_channels
		) f.m1_quantizers in
		M1_frame_data {
			m1_header = f.m1_header;
			m1_side_info = {
				side_main_data_begin = f.m1_side_info.side_main_data_begin;
				side_scfi = f.m1_side_info.side_scfi;
				side_gc = new_granules;
			};
			m1_scalefactors = f.m1_scalefactors;
			m1_quantizers = f.m1_quantizers;
			m1_starting_f1 = f.m1_starting_f1;
		}
	)
	| M2_frame_data f -> (
		if debug then Printf.printf "REHUFF_FRAME M2\n";

		let new_granules = Array.mapi (fun chan_i quants ->
			if debug then Printf.printf " GC %d\n" chan_i;
			let gc = f.m2_side_info.side_gc.(0).(chan_i) in
			let scf_quants = global_scalefactors f.m2_header.header_samplerate false in
			rehuff_granule quants gc scf_quants
		) f.m2_quantizers in
		M2_frame_data {
			m2_header = f.m2_header;
			m2_side_info = {
				side_main_data_begin = f.m2_side_info.side_main_data_begin;
				side_scfi = f.m2_side_info.side_scfi;
				side_gc = [| new_granules |];
			};
			m2_data_deleteme = f.m2_data_deleteme;
			m2_scalefactors = f.m2_scalefactors;
			m2_quantizers = f.m2_quantizers;
			m2_starting_f1 = f.m2_starting_f1;
		}
	)

);;

let rehuff_frame_old ?(debug=false) frame = (

	let rehuff_granule quants gc scf_quants = (
		let rec find_last_big now = (match (now, quants.(now) = 0 || quants.(now) = -1 || quants.(now) = 1) with
			| (0,true ) -> -1 (* Nothing! Yays! *)
			| (_,true ) -> find_last_big (pred now)
			| (_,false) -> now
		) in
		let last_big = (find_last_big (Array.length quants - 1)) lor 1 in
		let rec find_last_nonzero now = (match (now, quants.(now)) with
			| (0,0) -> -1
			| (_,0) -> find_last_nonzero (pred now)
			|   _   -> now
		) in
		let last_nonzero = find_last_nonzero (Array.length quants - 1) in
		let last_big2 = min (min (last_big + 2) (Array.length quants - 1)) last_nonzero in

		if debug then Printf.printf "  Last big:  %d\n" last_big;
		if debug then Printf.printf "  Last big2: %d\n" last_big2;
		if debug then Printf.printf "  Last not0: %d\n" last_nonzero;

		(* But wait! If the last 2 quantizers are nonzero, they need to be encoded, and that means that the number of big_values must be a multiple of 4. *)
		(* If big_values was not a multple of 4, then the encoding of count1 would go off the end of the array, and encoders may not handle it well. *)
		let (last_big, last_big2) = if last_nonzero >= num_quants - 2 then (
			let a = last_big lor 3 in (* "lor 3" pushes the number up to the next highest number which is 3 mod 4... *)
			if debug then Printf.printf "   BUT the last 2 numbers are nonzero, so last_big = last_big2 = %d\n" a;
			(a,a)
		) else (
			(last_big, last_big2)
		) in

		let (new_window, (big_values, count1_table_1)) = (match gc.gc_window with
			| Window_normal w -> ( (* Full freedom of scalefactor band selection *)
				(* From 1 to 16 in the first band thingie *)
				(* From 1 to 8 in the second *)
				if debug then Printf.printf "   Normal window\n";
				let encode_this_max = Array.map abs quants in (* What to encode, minus the signs *)
				let encode_this = Array.map (fun x -> if abs x >= 15 then 15 else abs x) quants in (* What to encode, minus the linbits and signs *)
				let rec find_largest encode_this so_far on stop = (
					(* Finds the largest in a range of an array *)
					if on >= stop
						then (max so_far encode_this.(on))
						else find_largest encode_this (max so_far encode_this.(on)) (succ on) stop
				) in
				let max_per_scf = Array.init 22 (fun i ->
					find_largest encode_this_max 0 scf_quants.(i) (scf_quants.(i + 1) - 1)
				) in
				if debug then (
					Printf.printf "    Largest in scf:[";
					Array.iter (fun x -> Printf.printf " %d" x) max_per_scf;
					Printf.printf " ]\n";
				);

				let find_smallest_encoding scf_from scf_to quant_max = (
					(* Given a range of scalefactors, this will find the Huffman table which results in the smallest number of bitses *)
					if scf_from < 0 || scf_to < 0 || scf_from > scf_to then (0,0) else (
						let quant_from = scf_quants.(scf_from) in
						let quant_to = scf_quants.(scf_to + 1) - 1 in

						(* Find out which Huffman tables to look at *)
						let max_in_range = find_largest max_per_scf 0 scf_from scf_to in
						let (_,tables_to_check) = List.find (fun (a,_) -> a >= max_in_range) huffman_tables_from_lengths in
						if false then (
							Printf.printf "     Largest is %d\n" max_in_range;
							Printf.printf "     Going from %d to %d (quants %d to %d (or %d))\n" scf_from scf_to quant_from quant_to quant_max;
							Printf.printf "     Tables to check: [";
							Array.iter (fun x -> Printf.printf " %d" x) tables_to_check;
							Printf.printf " ]\n";
						);
						(* A function for adding up the total number of bits with a given table, then comparing it to the smallest number of bits so far *)
						let (smallest_table,smallest_bits) = Array.fold_left (fun (smallest_table,smallest_bits) new_table_index ->
							if debug then Printf.printf "      Checking table %d: " new_table_index;
							let ht_encode = global_ht_encode.(new_table_index) in
							let ht_linbits = global_ht_linbits.(new_table_index) in
							let rec accumulate so_far new_index go_to = (
								if new_index > go_to then (
(*									if debug then Printf.printf "done!\n";*)
									so_far
								) else (
									let x = encode_this.(new_index) in
									let y = encode_this.(new_index + 1) in
(*									if debug then Printf.printf " (%d,%d)" x y;*)
(*									let (bits,_) = Hashtbl.find ht_encode (x,y) in*)
									let (bits,_) = ht_encode.((x lsl 4) lor (y land 15)) in
(*									if debug then Printf.printf "o";*)
									let linbits = (if x = 15 then ht_linbits else 0) + (if y = 15 then ht_linbits else 0) in
									accumulate (so_far + bits + linbits) (new_index + 2) go_to
								)
							) in
							let bits = accumulate 0 quant_from (min quant_to quant_max) in
(*							if debug then Printf.printf "+\n";*)
							if debug then Printf.printf " found %d; best was %d @ %d\n" bits smallest_table smallest_bits;
							if bits < smallest_bits then (
								(new_table_index,bits)
							) else (
								(smallest_table,smallest_bits)
							)
						) (-1,max_int) tables_to_check in

(*						if debug then Printf.printf "     Smallest table is %d @ %d\n" smallest_table smallest_bits;*)
						(smallest_table,smallest_bits)
					)
				) in
				(* Same as the function above, but only checks to see which count1 table is best *)
				let table_a = global_ht_encode_count1.(0) in
				let find_smallest_count1_encoding quant_from quant_to = (
					let bits_b = ((quant_to - quant_from + 4) lsr 2) lsl 2 in
					let rec accumulate so_far new_index go_to = (
						if new_index > go_to then (
							so_far
						) else if new_index + 2 = Array.length encode_this then (
							(* Just do the first 2 *)
							let (bits,_) = table_a.(encode_this.(new_index) lsl 3 lor encode_this.(new_index + 1) lsl 2) in
							so_far + bits
						) else (
							let (bits,_) = table_a.(encode_this.(new_index) lsl 3 lor encode_this.(new_index + 1) lsl 2 lor encode_this.(new_index + 2) lsl 1 lor encode_this.(new_index + 3)) in
							accumulate (so_far + bits) (new_index + 4) go_to
						)
					) in
					let bits_a = accumulate 0 quant_from quant_to in
					if bits_b <= bits_a then (
						(* Use table B *)
						(1, bits_b)
					) else (
						(* Use table A *)
						(0, bits_a)
					)
				) in

				(* Find the scalefactor band with the last big value in it *)
				let rec find_last_big_scf_band check_now = (
					if scf_quants.(check_now + 1) > last_big then check_now else find_last_big_scf_band (succ check_now)
				) in
				let last_big_scf = find_last_big_scf_band 0 in
				let rec find_last_big_scf_band2 check_now = (
					if scf_quants.(check_now + 1) > last_big2 then check_now else find_last_big_scf_band2 (succ check_now)
				) in
				let last_big_scf2 = find_last_big_scf_band2 0 in
				if debug then Printf.printf "    Last SCF band to check is %d (or %d)\n" last_big_scf last_big_scf2;

				(* Let's do this better(ly) *)
				(* Region 0 can be from 1 to 16 scf bands long *)
				(* Make an array where array.(n) represents the number of bits taken up by the smallest encoding of scalefactor bands 0 to n *)
				let first_region_bits_by_end_scf = Array.init (max 1 (min 16 (last_big_scf - 1))) (fun i ->
					let (table,bits) = find_smallest_encoding 0 i last_big in
					if debug then Printf.printf "    1: %d bits in bands 0 to %d with table %d\n" bits i table;
					(table,bits)
				) in
				if debug then Printf.printf "\n";
				(* This next variable is if the last big value is in the first scf band, and therefore in the first region *)
				(* It is also different if the last big value is right at the end of an scf band, and therefore needs one more table to worry about *)
				(* This second case could be integrated into the previous table for speed, but it's not common enough to worry about *)
				let first_region_bits_by_end_scf2 = if last_big_scf2 = 0 || last_big_scf <> last_big_scf2 then (
					Array.init (max 1 (min 16 (last_big_scf2 - 1))) (fun i ->
						let (table,bits) = find_smallest_encoding 0 i last_big2 in
						if debug then Printf.printf "    1: %d bits in bands 0 to %d with table %d (BIG2)\n" bits i table;
						(table,bits)
					)
				) else (
					if debug then Printf.printf "    1: last_big has no effect on the first region (BIG2)\n";
					first_region_bits_by_end_scf
				) in
				if debug then Printf.printf "\n";
				let third_region_bits_by_start_scf = Array.init (max 0 (last_big_scf - 1)) (fun i ->
					let (table,bits) = find_smallest_encoding (last_big_scf - i) last_big_scf last_big in
					if debug then Printf.printf "    3: %d bits in bands %d to %d with table %d\n" bits (last_big_scf - i) last_big_scf table;
					(table,bits)
				) in
				if debug then Printf.printf "\n";

				let third_region_bits_by_start_scf2 = if last_big = last_big2 then (
					(* last_big and last_big2 are equal! This may happen if there are no count1 values or if there are no count0 values *)
					if debug then Printf.printf "    3: last_big = last_big2, so using the normal table for BIG2\n";
					third_region_bits_by_start_scf
				) else (
					Array.init (max 0 (last_big_scf2 - 1)) (fun i ->
						let (table,bits) = if i = 0 && last_big_scf <> last_big_scf2 && max_per_scf.(last_big_scf2) = 1 && encode_this.(last_big + 1) = 0 && encode_this.(last_big + 2) = 0 then (
							(* This is a bizarre special case. If the last big value is at the end of a scalefactor band, the next two quants are 0, and there are nonzero quants in the next scf band, *)
							(* then my max_per_scf will return 1 for the scf band after the last big value, even though only 0s need to be encoded *)
							if debug then Printf.printf "    SPECIAL CASE: overriding the max_per_scf on scalefactor %d\n" last_big_scf2;
							(0,0)
						) else (
							find_smallest_encoding (last_big_scf2 - i) last_big_scf2 last_big2
						) in
						if debug then Printf.printf "    3: %d bits in bands %d to %d with table %d (BIG2)\n" bits (last_big_scf2 - i) last_big_scf2 table;
						(table,bits)
					)
				) in
				if debug then Printf.printf "\n";

				let (count1_table1, count1_bits1) = find_smallest_count1_encoding (last_big + 1) last_nonzero in
				let (count1_table2, count1_bits2) = find_smallest_count1_encoding (last_big2 + 1) last_nonzero in
				if debug then Printf.printf "    c1-1: %d bits with table %d (from %d to %d)\n" count1_bits1 count1_table1 (last_big + 1) last_nonzero;
				if debug then Printf.printf "    c1-2: %d bits with table %d (from %d to %d)\n" count1_bits2 count1_table2 (last_big2 + 1) last_nonzero;

				(* Do the test thang *)
				let rec check_combination (smallest_bits, smallest_config) second_start third_start = (
					if debug then Printf.printf "     Testing %d,%d\n" second_start third_start;

					let (first_table2, first_bits2) = first_region_bits_by_end_scf2.(pred second_start) in
					let (second_table1, second_bits1) = find_smallest_encoding second_start (pred third_start) last_big in
					let (second_table2, second_bits2) = if last_big_scf < third_start then (
						(* The last scf is, for some reason, in the second region *)
						(* Therefore, the second region needs to be recalculated for the alternate last big value *)
						(* I think this only happens when the last big value is in the second scalefactor *)
						find_smallest_encoding second_start (pred third_start) last_big2
					) else (
						(* Normally, the two second regions are the same, since the last big value is in the third region *)
						(second_table1, second_bits1)
					) in
					let (third_table2, third_bits2) = if last_big_scf2 - third_start < 0 then (0,0) else third_region_bits_by_start_scf2.(last_big_scf2 - third_start) in
					let (bits2, config2) = (first_bits2 + second_bits2 + third_bits2 + count1_bits2, (second_start - 1, third_start - second_start - 1, first_table2, second_table2, third_table2, true)) in

					(* The ordering of this section is getting pretty twisted... *)

					let (bits1, config1) = if third_start > last_big_scf then (
						(* The third section starts on an scf band which does not have any big_values in it, but it's being calculated because of the addition of two quants to the big values (if that makes sense) *)
						(* Therefore, make sure the config is not chosen *)
						(max_int, (0,0,0,0,0,false))
					) else (
						let (first_table1, first_bits1) = first_region_bits_by_end_scf.(pred second_start) in
						let (third_table1, third_bits1) = third_region_bits_by_start_scf.(last_big_scf - third_start) in (* proBLAM! *)

						if debug then Printf.printf "      1: (%d,%d), (%d,%d), (%d,%d)\n" first_bits1 first_table1 second_bits1 second_table1 third_bits1 third_table1;
						(first_bits1 + second_bits1 + third_bits1 + count1_bits1, (second_start - 1, third_start - second_start - 1, first_table1, second_table1, third_table1, false))
					) in

					if debug then Printf.printf "      2: (%d,%d), (%d,%d), (%d,%d)\n" first_bits2 first_table2 second_bits2 second_table2 third_bits2 third_table2;

					if debug then Printf.printf "       (%d,%d) bits\n" bits1 bits2;

					let (smallest_bits, smallest_config) = if bits1 < bits2 && bits1 < smallest_bits then (
						(bits1, config1)
					) else if bits2 < smallest_bits then (
						(bits2, config2)
					) else (
						(smallest_bits, smallest_config)
					) in

					if third_start >= last_big_scf2 || third_start - second_start >= 8 then (
						(* Third range is only 1 scf band wide, or second scalefactor is the max width *)
						if second_start >= last_big_scf2 - 1 || second_start >= 16 then (
							(* Second range is pretty small too; looks like that's the end! *)
							(smallest_bits, smallest_config)
						) else (
							(* Increment the second band *)
							check_combination (smallest_bits, smallest_config) (second_start + 1) (second_start + 2)
						)
					) else (
						(* Increment the third band *)
						check_combination (smallest_bits, smallest_config) second_start (third_start + 1)
					)
				) in

				let (smallest_bits, (len1, len2, table1, table2, table3, two_more)) = check_combination (max_int, (0,0,0,0,0,false)) 1 2 in

				if debug then Printf.printf "    Best configuration is %d bits at %d,%d with tables %d,%d,%d. Two more? %B\n" smallest_bits len1 len2 table1 table2 table3 two_more;
				(
					Window_normal {
						normal_table_select1 = table1;
						normal_table_select2 = table2;
						normal_table_select3 = table3;
						normal_region_0_count = len1;
						normal_region_1_count = len2;
					}
				,
					if two_more then ((last_big2 + 2) lsr 1, (count1_table2 = 1)) else ((last_big + 2) lsr 1, (count1_table1 = 1))
				)
(*				Window_normal w*)
			)
			| Window_other w -> ( (* Scalefactor bands predefined *)
				if debug then Printf.printf "   Other window\n";
				(Window_other w, (gc.gc_big_values, gc.gc_count1_table_1))
			)
		) in
		{
			gc_part2_3_length = gc.gc_part2_3_length; (* This isn't used for anything during the re-encoding process, so it's OK to not calculate it here *)
			gc_part2_3_offset = gc.gc_part2_3_offset; (* This isn't actually used for anything... *)
			gc_big_values = big_values; (* This is NOT recalculated during encoding *)
			gc_global_gain = gc.gc_global_gain;
			gc_scf_compress_index = gc.gc_scf_compress_index;
			gc_window = new_window;
			gc_pre_flag = gc.gc_pre_flag;
			gc_sf_scale = gc.gc_sf_scale;
			gc_count1_table_1 = count1_table_1;
		}
	) in

	match frame with
	| M1_frame_data f -> (
		if debug then Printf.printf "REHUFF_FRAME M1\n";
		let new_granules = Array.mapi (fun gran_i quant_gran ->
			let new_channels = Array.mapi (fun chan_i quants ->
				if debug then Printf.printf " GC %d,%d\n" gran_i chan_i;
				let gc = f.m1_side_info.side_gc.(gran_i).(chan_i) in
				let scf_quants = global_scalefactors f.m1_header.header_samplerate false in
				rehuff_granule quants gc scf_quants
			) quant_gran in
			new_channels
		) f.m1_quantizers in
		M1_frame_data {
			m1_header = f.m1_header;
			m1_side_info = {
				side_main_data_begin = f.m1_side_info.side_main_data_begin;
				side_scfi = f.m1_side_info.side_scfi;
				side_gc = new_granules;
			};
			m1_scalefactors = f.m1_scalefactors;
			m1_quantizers = f.m1_quantizers;
			m1_starting_f1 = f.m1_starting_f1;
		}
(*
		M1_frame_data f
*)
	)
	| M2_frame_data f -> (
		if debug then Printf.printf "REHUFF_FRAME M2\n";

		let new_granules = Array.mapi (fun chan_i quants ->
			if debug then Printf.printf " GC %d\n" chan_i;
			let gc = f.m2_side_info.side_gc.(0).(chan_i) in
			let scf_quants = global_scalefactors f.m2_header.header_samplerate false in
			rehuff_granule quants gc scf_quants
		) f.m2_quantizers in
		M2_frame_data {
			m2_header = f.m2_header;
			m2_side_info = {
				side_main_data_begin = f.m2_side_info.side_main_data_begin;
				side_scfi = f.m2_side_info.side_scfi;
				side_gc = [| new_granules |];
			};
			m2_data_deleteme = f.m2_data_deleteme;
			m2_scalefactors = f.m2_scalefactors;
			m2_quantizers = f.m2_quantizers;
			m2_starting_f1 = f.m2_starting_f1;
		}
(*
		M2_frame_data f
*)
	)
);;

(*
## Side info:
# 9/17/32 BYTES = 136/256 bits
# 9: main_data_begin (8 for MPEG2)
# ?: private_bits
# 4: SCFI Band
# 59: Side Information Granule
#  12: part2_3 length (main data for this channel, granule in bits)
#  9: Big values
#  8: Global gain
#  4: Scalefactor compress (9 for MPEG2)
#  1: Window switch flag
#   if 1:
#    2: Block type
#    1: Mix block flag
#    5x2: Table Select [region]
#    3x3: sub_block_gain [window]
#   if 0:
#    5x3: Table select [region]
#    4: Region 0 count
#    3: Region 1 count
#  1: Pre flag (NOT FOR MPEG2)
#  1: Scale factor scale
#  1: Count1 table select

# MPEG1 mono:
# [9 main data] [5 privates] [4 SCFI] [59 Gr0] [59 Gr1]
# (18 - 30) (77 - 89)
# MPEG1 stereo:
# [9 main data] [3 privates] [4 SCFI0] [4 SCFI1] [59 Gr0ch1] [59 Gr0ch2] [59 Gr1ch1] [59 Gr1ch2]
# 20 79 138 197
# MPEG2 mono:
# [8 main data] [1 privates] [63 Gr*]
# (9 - 21)
# MPEG2 stereo:
# [8 main data] [2 privates] [63 Gr*ch1] [63 Gr*ch2]
# (10 - 22) (73 - 85)
*)

(**********************************************************************************************)
(* ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME *)
(**********************************************************************************************)
let encode_frame ?(debug=false) d =




	let write_granule_m1 k r scfsi gc scf quants = (
		(* Scalefactors *)
		let (scf_bits1, scf_bits2) = scalefactor_compress_m1.(gc.gc_scf_compress_index) in
		let (num1, num2) = (match gc.gc_window with
			| Window_normal x -> (11,10)
			| Window_other x when x.other_block_type <> Block_type_short -> (11,10)
			| Window_other x when x.other_mixed_block -> (17,18)
			| Window_other x -> (18,18)
		) in
		let rec write_scf r i = (
			let num_bits = if i < num1 then scf_bits1 else scf_bits2 in
			if i >= Array.length scf then (
				if debug && debug_more then Printf.printf "!";
				r
			) else if i < 6 then (
				let r = if scfsi.(0) then r else (if debug && debug_more then Printf.printf "0"; write_bits r num_bits scf.(i)) in
				write_scf r (succ i)
			) else if i < 11 then (
				let r = if scfsi.(1) then r else (if debug && debug_more then Printf.printf "1"; write_bits r num_bits scf.(i)) in
				write_scf r (succ i)
			) else if i < 16 then (
				let r = if scfsi.(2) then r else (if debug && debug_more then Printf.printf "2"; write_bits r num_bits scf.(i)) in
				write_scf r (succ i)
			) else if i < 21 then (
				let r = if scfsi.(3) then r else (if debug && debug_more then Printf.printf "3"; write_bits r num_bits scf.(i)) in
				write_scf r (succ i)
			) else (
				let r = (if debug && debug_more then Printf.printf "+"; write_bits r num_bits scf.(i)) in
				write_scf r (succ i)
			)
		) in
		let r = write_scf r 0 in
		if debug && debug_more then Printf.printf "\n";

		(* Quantizers *)
		let (region0, region1, region2, table0, table1, table2) = (match gc.gc_window with
			| Window_normal w -> (
				let scfend0 = w.normal_region_0_count + 1 in
				let scfend1 = w.normal_region_1_count + 1 + scfend0 in
				let region0 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend0) lsr 1) in
				let region1 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend1) lsr 1) - region0 in
				(region0, region1, gc.gc_big_values - region0 - region1, w.normal_table_select1, w.normal_table_select2, w.normal_table_select3)
			)
			| Window_other w -> (
				let region0 = if w.other_block_type = Block_type_short && not w.other_mixed_block then (
					(global_scalefactors k.header_samplerate true).(9 / 3) lsr 1 * 3
				) else (
					(global_scalefactors k.header_samplerate false).(8) lsr 1
				) in
				(min region0 gc.gc_big_values, max 0 (gc.gc_big_values - region0), 0, w.other_table_select1, w.other_table_select2, 0)
			)
		) in
		let rec write_to_frame index left ht_encode linbits r = (
			if left <= 0 then (index, r) else (
				if debug && debug_more then Printf.printf "Trying to squeeze (%d,%d) from %d to position %d (%d)\n" quants.(index) quants.(index + 1) index (snd r) left;

				let (x, y) = (quants.(index), quants.(index + 1)) in

				let x_base = min (abs x) 15 in
				let y_base = min (abs y) 15 in
				let (x_sign, x_sign_bits) = if x = 0 then (0,0) else if x > 0 then (0,1) else (1,1) in
				let (y_sign, y_sign_bits) = if y = 0 then (0,0) else if y > 0 then (0,1) else (1,1) in
				let (x_lin, x_lin_bits) = if x_base = 15 && linbits > 0 then (abs x - x_base, linbits) else (0,0) in
				let (y_lin, y_lin_bits) = if y_base = 15 && linbits > 0 then (abs y - y_base, linbits) else (0,0) in

				let (bits, value) = ht_encode.((x_base lsl 4) lor (y_base land 15)) in
				if debug && debug_more then Printf.printf " Found Huffman code %d of length %d (%d,%d,%d,%d)\n" value bits x_lin_bits x_sign_bits y_lin_bits y_sign_bits;

				let r = write_bits r bits value in
				let r = write_bits r x_lin_bits x_lin in
				let r = write_bits r x_sign_bits x_sign in
				let r = write_bits r y_lin_bits y_lin in
				let r = write_bits r y_sign_bits y_sign in

				if debug then Printf.printf " Done writing!\n";

				write_to_frame (index + 2) (left - 1) ht_encode linbits r
			)
		) in
		let (index, r) = write_to_frame   0   region0 global_ht_encode.(table0) global_ht_linbits.(table0) r in
		if debug then Printf.printf "Done with first region\n";
		let (index, r) = write_to_frame index region1 global_ht_encode.(table1) global_ht_linbits.(table1) r in
		if debug then Printf.printf "Done with second region\n";
		let (index, r) = write_to_frame index region2 global_ht_encode.(table2) global_ht_linbits.(table2) r in
		if debug then Printf.printf "Done with third region\n";

		let find_last_nonzero_index table = (
			let rec find_last_nonzero_rec table on = (
				if on < 0 then (
					-1
				) else if table.(on) = 0 then (
					if on = 0 then -1 else find_last_nonzero_rec table (pred on)
				) else (
					on
				)
			) in
			find_last_nonzero_rec table (Array.length table - 1)
		) in

		let rec write_count1_to_frame index go_to ht_encode r = (
			if index > go_to then (index, r) else (
				if debug && debug_more then Printf.printf "Trying to squeeze (%d,%d,%d,%d) from %d to position %d of %d going to %d\n" quants.(index) quants.(index + 1) quants.(index + 2) quants.(index + 3) index (snd r) (String.length (fst r) * 8) go_to;

				let (v,w,x,y) = (*if index = (Array.length quants - 2) then (quants.(index), quants.(index + 1), 0, 0) else*) (quants.(index), quants.(index + 1), quants.(index + 2), quants.(index + 3)) in
				let (v_sign, v_sign_bits) = if v = 0 then (0,0) else if v > 0 then (0,1) else (1,1) in
				let (w_sign, w_sign_bits) = if w = 0 then (0,0) else if w > 0 then (0,1) else (1,1) in
				let (x_sign, x_sign_bits) = if x = 0 then (0,0) else if x > 0 then (0,1) else (1,1) in
				let (y_sign, y_sign_bits) = if y = 0 then (0,0) else if y > 0 then (0,1) else (1,1) in

(*				let (bits, value) = Hashtbl.find ht_encode (abs v, abs w, abs x, abs y) in*)
				let (bits, value) = ht_encode.((abs v) lsl 3 lor (abs w) lsl 2 lor (abs x) lsl 1 lor (abs y)) in
				if debug && debug_more then Printf.printf " Found Huffman code %d of length %d\n" value bits;

				let r = write_bits r bits value in
				let r = write_bits r v_sign_bits v_sign in
				let r = write_bits r w_sign_bits w_sign in
				let r = write_bits r x_sign_bits x_sign in
				let r = write_bits r y_sign_bits y_sign in
				write_count1_to_frame (index + 4) go_to ht_encode r
			)
		) in
		let go_to = find_last_nonzero_index quants in
		if debug then Printf.printf "Last nonzero quantizer is %d\n" go_to;
		let (index, r) = write_count1_to_frame index go_to global_ht_encode_count1.(if gc.gc_count1_table_1 then 1 else 0) r in
		if debug then Printf.printf "Wrote count1 to frame\n";
		r
	) in

	let write_granule_m2 k r is gc scf quants = (
		(* Just copy the original layout; I doubt there's much savings to be had here *)
		let (bits0, bits1, bits2, bits3) = if is then (
			scalefactor_compress_m2_is.(gc.gc_scf_compress_index)
		) else (
			scalefactor_compress_m2.(gc.gc_scf_compress_index)
		) in
		let (num0, num1, num2, num3) = scalefactor_bands_m2 is gc in
		if debug then Printf.printf "Writing %d,%d,%d,%d bits to %d,%d,%d,%d bands\n" bits0 bits1 bits2 bits3 num0 num1 num2 num3;
		
		let sob1 = num0 in
		let sob2 = sob1 + num1 in
		let sob3 = sob2 + num2 in
		let num_total = sob3 + num3 in
		
		let rec write_scf r i = (
			if i < sob1 then (
				let r = (if debug && debug_more then Printf.printf "0"; write_bits r bits0 scf.(i)) in
				write_scf r (succ i)
			) else if i < sob2 then (
				let r = (if debug && debug_more then Printf.printf "1"; write_bits r bits1 scf.(i)) in
				write_scf r (succ i)
			) else if i < sob3 then (
				let r = (if debug && debug_more then Printf.printf "2"; write_bits r bits2 scf.(i)) in
				write_scf r (succ i)
			) else if i < num_total then (
				let r = (if debug && debug_more then Printf.printf "3"; write_bits r bits3 scf.(i)) in
				write_scf r (succ i)
			) else (
				if debug && debug_more then Printf.printf "!";
				r
			)
		) in
		let r = write_scf r 0 in
		if debug && debug_more then Printf.printf "\n";
		
		(* Quants *)
		let (region0, region1, region2, table0, table1, table2) = (match gc.gc_window with
			| Window_normal w -> (
				let scfend0 = w.normal_region_0_count + 1 in
				let scfend1 = w.normal_region_1_count + 1 + scfend0 in
				let region0 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend0) lsr 1) in
				let region1 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend1) lsr 1) - region0 in
				(region0, region1, gc.gc_big_values - region0 - region1, w.normal_table_select1, w.normal_table_select2, w.normal_table_select3)
			)
			| Window_other w -> (
				let region0 = if w.other_block_type = Block_type_short && not w.other_mixed_block then (
					(global_scalefactors k.header_samplerate true).(9 / 3) lsr 1 * 3
				) else (
					(global_scalefactors k.header_samplerate false).(8) lsr 1
				) in
				(min region0 gc.gc_big_values, max 0 (gc.gc_big_values - region0), 0, w.other_table_select1, w.other_table_select2, 0)
			)
		) in

		let rec write_to_frame index left ht_encode linbits r = (
			if left <= 0 then (index, r) else (
				if debug && debug_more then Printf.printf "Trying to squeeze (%d,%d) from %d to position %d (%d)\n" quants.(index) quants.(index + 1) index (snd r) left;

				let (x, y) = (quants.(index), quants.(index + 1)) in

				let x_base = min (abs x) 15 in
				let y_base = min (abs y) 15 in
				let (x_sign, x_sign_bits) = if x = 0 then (0,0) else if x > 0 then (0,1) else (1,1) in
				let (y_sign, y_sign_bits) = if y = 0 then (0,0) else if y > 0 then (0,1) else (1,1) in
				let (x_lin, x_lin_bits) = if x_base = 15 && linbits > 0 then (abs x - x_base, linbits) else (0,0) in
				let (y_lin, y_lin_bits) = if y_base = 15 && linbits > 0 then (abs y - y_base, linbits) else (0,0) in

				let (bits, value) = ht_encode.((x_base lsl 4) lor (y_base land 15)) in
				if debug && debug_more then Printf.printf " Found Huffman code %d of length %d (%d,%d,%d,%d)\n" value bits x_lin_bits x_sign_bits y_lin_bits y_sign_bits;

				let r = write_bits r bits value in
				let r = write_bits r x_lin_bits x_lin in
				let r = write_bits r x_sign_bits x_sign in
				let r = write_bits r y_lin_bits y_lin in
				let r = write_bits r y_sign_bits y_sign in

				if debug then Printf.printf " Done writing!\n";

				write_to_frame (index + 2) (left - 1) ht_encode linbits r
			)
		) in
		let (index, r) = write_to_frame   0   region0 global_ht_encode.(table0) global_ht_linbits.(table0) r in
		if debug then Printf.printf "Done with first region\n";
		let (index, r) = write_to_frame index region1 global_ht_encode.(table1) global_ht_linbits.(table1) r in
		if debug then Printf.printf "Done with second region\n";
		let (index, r) = write_to_frame index region2 global_ht_encode.(table2) global_ht_linbits.(table2) r in
		if debug then Printf.printf "Done with third region\n";

		let find_last_nonzero_index table = (
			let rec find_last_nonzero_rec table on = (
				if on < 0 then (
					-1
				) else if table.(on) = 0 then (
					if on = 0 then -1 else find_last_nonzero_rec table (pred on)
				) else (
					on
				)
			) in
			find_last_nonzero_rec table (Array.length table - 1)
		) in

		let rec write_count1_to_frame index go_to ht_encode r = (
			if index > go_to then (index, r) else (
				if debug && debug_more then Printf.printf "Trying to squeeze (%d,%d,%d,%d) from %d to position %d of %d going to %d\n" quants.(index) quants.(index + 1) quants.(index + 2) quants.(index + 3) index (snd r) (String.length (fst r) * 8) go_to;

				let (v,w,x,y) = (*if index = (Array.length quants - 2) then (quants.(index), quants.(index + 1), 0, 0) else*) (quants.(index), quants.(index + 1), quants.(index + 2), quants.(index + 3)) in
				let (v_sign, v_sign_bits) = if v = 0 then (0,0) else if v > 0 then (0,1) else (1,1) in
				let (w_sign, w_sign_bits) = if w = 0 then (0,0) else if w > 0 then (0,1) else (1,1) in
				let (x_sign, x_sign_bits) = if x = 0 then (0,0) else if x > 0 then (0,1) else (1,1) in
				let (y_sign, y_sign_bits) = if y = 0 then (0,0) else if y > 0 then (0,1) else (1,1) in

(*				let (bits, value) = Hashtbl.find ht_encode (abs v, abs w, abs x, abs y) in*)
				let (bits, value) = ht_encode.((abs v) lsl 3 lor (abs w) lsl 2 lor (abs x) lsl 1 lor (abs y)) in
				if debug && debug_more then Printf.printf " Found Huffman code %d of length %d\n" value bits;

				let r = write_bits r bits value in
				let r = write_bits r v_sign_bits v_sign in
				let r = write_bits r w_sign_bits w_sign in
				let r = write_bits r x_sign_bits x_sign in
				let r = write_bits r y_sign_bits y_sign in
				write_count1_to_frame (index + 4) go_to ht_encode r
			)
		) in
		let go_to = find_last_nonzero_index quants in
		if debug then Printf.printf "Last nonzero quantizer is %d\n" go_to;
		let (index, r) = write_count1_to_frame index go_to global_ht_encode_count1.(if gc.gc_count1_table_1 then 1 else 0) r in
		if debug then Printf.printf "Wrote count1 to frame\n";
		r

	) in




	match d with
	| M1_frame_data m when m.m1_header.header_channel_mode = ChannelMono -> (
		(* MONO MPEG1 *)
		let k = m.m1_header in
		
		(*************)
		(* Parts 2,3 *)
		(*************)
		let out_q = String.make 2881 '\x00' in
		let r = (out_q, 0) in
		
		let r0 = snd r in
		if debug then Printf.printf "Doing first granule at %d (had better be 0)\n" r0;
		let r = write_granule_m1 k r [| false;false;false;false |] m.m1_side_info.side_gc.(0).(0) m.m1_scalefactors.(0).(0) m.m1_quantizers.(0).(0) in
		let r1 = snd r in
		if debug then Printf.printf "First granule done; starting second at %d\n" r1;
		let r = write_granule_m1 k r m.m1_side_info.side_scfi.(0)  m.m1_side_info.side_gc.(1).(0) m.m1_scalefactors.(1).(0) m.m1_quantizers.(1).(0) in
		let r2 = snd r in
		if debug then Printf.printf "Second granule done at %d\n" r2;

		let data_raw = String.sub (fst r) 0 ((snd r + 7) lsr 3) in

		let (side_raw, side_bits, side_bytes) = (
			(* String.create is OK here since all bits of the side info are well-defined *)
			let r = String.create 17 in
			let s = m.m1_side_info in
			packBits r  0 9 s.side_main_data_begin;
			packBits r  9 5 0;
			packBits r 14 1 (if s.side_scfi.(0).(0) then 1 else 0);
			packBits r 15 1 (if s.side_scfi.(0).(1) then 1 else 0);
			packBits r 16 1 (if s.side_scfi.(0).(2) then 1 else 0);
			packBits r 17 1 (if s.side_scfi.(0).(3) then 1 else 0);
			let pack_gc gc bits o = (
				packBits r (o +  0) 12 bits;
				packBits r (o + 12)  9 gc.gc_big_values;
				packBits r (o + 21)  8 gc.gc_global_gain;
				packBits r (o + 29)  4 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						packBits r (o + 33) 1 0;
						packBits r (o + 34) 5 w.normal_table_select1;
						packBits r (o + 39) 5 w.normal_table_select2;
						packBits r (o + 44) 5 w.normal_table_select3;
						packBits r (o + 49) 4 w.normal_region_0_count;
						packBits r (o + 53) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						packBits r (o + 33) 1 1;
						packBits r (o + 34) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						packBits r (o + 36) 1 (if w.other_mixed_block then 1 else 0);
						packBits r (o + 37) 5 w.other_table_select1;
						packBits r (o + 42) 5 w.other_table_select2;
						packBits r (o + 47) 3 w.other_sub_block_gain1;
						packBits r (o + 50) 3 w.other_sub_block_gain2;
						packBits r (o + 53) 3 w.other_sub_block_gain3;
					)
				);
				packBits r (o + 56) 1 (if gc.gc_pre_flag then 1 else 0);
				packBits r (o + 57) 1 gc.gc_sf_scale;
				packBits r (o + 58) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc s.side_gc.(0).(0) (r1 - r0) 18;
			pack_gc s.side_gc.(1).(0) (r2 - r1) 77;
			(r, [| r1 - r0; r2 - r1 |], (r2 - r0 + 7) asr 3)
		) in

		if debug then Printf.printf "Done packing the side info\n";

		{
			f1_num = m.m1_starting_f1.f1_num;
			f1_header = m.m1_starting_f1.f1_header;
			f1_side = {
				side_raw = side_raw;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = data_raw;
			f1_pad_exact = None;
		}
	)
	| M1_frame_data m -> (
		(* STEREO MPEG1 *)
		let k = m.m1_header in

		(*************)
		(* Parts 2,3 *)
		(*************)
		let out_q = String.make 2881 '\x00' in (* This is the size of a padded 640kbps frame @ 32KHz (this will cover freeformat nicely) *)
		let r = (out_q, 0) in

		let r0 = snd r in (* had better be 0 *)
		if debug then Printf.printf "Doing first granule at %d (had better be 0)\n" r0;
		let r = write_granule_m1 k r [| false;false;false;false |] m.m1_side_info.side_gc.(0).(0) m.m1_scalefactors.(0).(0) m.m1_quantizers.(0).(0) in
		let r1 = snd r in
		if debug then Printf.printf "First granule done; starting second at %d\n" r1;
		let r = write_granule_m1 k r [| false;false;false;false |] m.m1_side_info.side_gc.(0).(1) m.m1_scalefactors.(0).(1) m.m1_quantizers.(0).(1) in
		let r2 = snd r in
		if debug then Printf.printf "Second granule done; starting third at %d\n" r2;
		let r = write_granule_m1 k r m.m1_side_info.side_scfi.(0)  m.m1_side_info.side_gc.(1).(0) m.m1_scalefactors.(1).(0) m.m1_quantizers.(1).(0) in
		let r3 = snd r in
		if debug then Printf.printf "Third granule done; starting fourth at %d\n" r3;
		let r = write_granule_m1 k r m.m1_side_info.side_scfi.(1)  m.m1_side_info.side_gc.(1).(1) m.m1_scalefactors.(1).(1) m.m1_quantizers.(1).(1) in
		let r4 = snd r in
		if debug then Printf.printf "Fourth granule done at %d\n" r4;

		let data_raw = String.sub (fst r) 0 ((snd r + 7) lsr 3) in

		(************************)
		(* Encode the side info *)
		(************************)
		let (side_raw, side_bits, side_bytes) = (
			let r = String.create 32 in
			let s = m.m1_side_info in
			packBits r  0 9 s.side_main_data_begin;
			packBits r  9 3 0;
			packBits r 12 1 (if s.side_scfi.(0).(0) then 1 else 0);
			packBits r 13 1 (if s.side_scfi.(0).(1) then 1 else 0);
			packBits r 14 1 (if s.side_scfi.(0).(2) then 1 else 0);
			packBits r 15 1 (if s.side_scfi.(0).(3) then 1 else 0);
			packBits r 16 1 (if s.side_scfi.(1).(0) then 1 else 0);
			packBits r 17 1 (if s.side_scfi.(1).(1) then 1 else 0);
			packBits r 18 1 (if s.side_scfi.(1).(2) then 1 else 0);
			packBits r 19 1 (if s.side_scfi.(1).(3) then 1 else 0);
			let pack_gc gc bits o = (
				packBits r (o +  0) 12 bits;
				packBits r (o + 12)  9 gc.gc_big_values;
				packBits r (o + 21)  8 gc.gc_global_gain;
				packBits r (o + 29)  4 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						packBits r (o + 33) 1 0;
						packBits r (o + 34) 5 w.normal_table_select1;
						packBits r (o + 39) 5 w.normal_table_select2;
						packBits r (o + 44) 5 w.normal_table_select3;
						packBits r (o + 49) 4 w.normal_region_0_count;
						packBits r (o + 53) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						packBits r (o + 33) 1 1;
						packBits r (o + 34) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						packBits r (o + 36) 1 (if w.other_mixed_block then 1 else 0);
						packBits r (o + 37) 5 w.other_table_select1;
						packBits r (o + 42) 5 w.other_table_select2;
						packBits r (o + 47) 3 w.other_sub_block_gain1;
						packBits r (o + 50) 3 w.other_sub_block_gain2;
						packBits r (o + 53) 3 w.other_sub_block_gain3;
					)
				);
				packBits r (o + 56) 1 (if gc.gc_pre_flag then 1 else 0);
				packBits r (o + 57) 1 gc.gc_sf_scale;
				packBits r (o + 58) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc s.side_gc.(0).(0) (r1 - r0)  20;
			pack_gc s.side_gc.(0).(1) (r2 - r1)  79;
			pack_gc s.side_gc.(1).(0) (r3 - r2) 138;
			pack_gc s.side_gc.(1).(1) (r4 - r3) 197;
			(r, [| r1 - r0; r2 - r1; r3 - r2; r4 - r3 |], (r4 - r0 + 7) asr 3)
		) in

		if debug then Printf.printf "Done packing the side info\n";

(*
		{
			transitFrame = m.m1_starting_transit.transitFrame;
			transitHeader = m.m1_starting_transit.transitHeader;
			transitSide = side_raw;
			transitData = data_raw;
			transitDataLength = String.length data_raw;
			transitBits = r4;
			transitPad = 0;
		}
*)
		{
			f1_num = m.m1_starting_f1.f1_num;
			f1_header = m.m1_starting_f1.f1_header;
			f1_side = {
				side_raw = side_raw;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = data_raw;
			f1_pad_exact = None;
		}
	)
	| M2_frame_data m when m.m2_header.header_channel_mode = ChannelMono -> (
		
		let k = m.m2_header in

		let out_q = String.make 5761 '\x00' in (* Length of a freeformat 640kbps frame @ 8khz (in case anybody wants to do that...) *)
		let r = (out_q, 0) in

		let r0 = snd r in
		if debug then Printf.printf "Doing first (and only) granule at %d (had better be 0)\n" r0;
		let r = write_granule_m2 k r false m.m2_side_info.side_gc.(0).(0) m.m2_scalefactors.(0) m.m2_quantizers.(0) in
		let r1 = snd r in
		if debug then Printf.printf "Granule done at %d\n" r1;
		
		let data_raw = String.sub (fst r) 0 ((snd r + 7) lsr 3) in
		
		let (side_raw, side_bits, side_bytes) = (
			let r = String.create 9 in
			let s = m.m2_side_info in
			packBits r  0 8 s.side_main_data_begin;
			packBits r  8 2 0;
			let pack_gc gc bits o = (
				packBits r (o +  0) 12 bits;
				packBits r (o + 12)  9 gc.gc_big_values;
				packBits r (o + 21)  8 gc.gc_global_gain;
				packBits r (o + 29)  9 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						packBits r (o + 38) 1 0;
						packBits r (o + 39) 5 w.normal_table_select1;
						packBits r (o + 44) 5 w.normal_table_select2;
						packBits r (o + 49) 5 w.normal_table_select3;
						packBits r (o + 54) 4 w.normal_region_0_count;
						packBits r (o + 58) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						packBits r (o + 38) 1 1;
						packBits r (o + 39) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						packBits r (o + 41) 1 (if w.other_mixed_block then 1 else 0);
						packBits r (o + 42) 5 w.other_table_select1;
						packBits r (o + 47) 5 w.other_table_select2;
						packBits r (o + 52) 3 w.other_sub_block_gain1;
						packBits r (o + 55) 3 w.other_sub_block_gain2;
						packBits r (o + 58) 3 w.other_sub_block_gain3;
					)
				);
				packBits r (o + 61) 1 gc.gc_sf_scale;
				packBits r (o + 62) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc s.side_gc.(0).(0) (r1 - r0) 9;
			(r, [| r1 - r0 |], (r1 - r0 + 7) asr 3)
			
		) in
		
		if debug then Printf.printf "Done packing the side info\n";
		
		{
			f1_num = m.m2_starting_f1.f1_num;
			f1_header = m.m2_starting_f1.f1_header;
			f1_side = {
				side_raw = side_raw;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = data_raw;
			f1_pad_exact = None;
		}
	)
	| M2_frame_data m -> (
(*		m.m2_starting_f1*)
		let k = m.m2_header in

		let out_q = String.make 5761 '\x00' in (* Length of a freeformat 640kbps frame @ 8khz (in case anybody wants to do that...) *)
		let r = (out_q, 0) in
		
		let r0 = snd r in
		if debug then Printf.printf "Doing first granule at %d (had better be 0)\n" r0;
		let r = write_granule_m2 k r         false         m.m2_side_info.side_gc.(0).(0) m.m2_scalefactors.(0) m.m2_quantizers.(0) in
		let r1 = snd r in
		if debug then Printf.printf "First granule done; starting second at %d\n" r1;
		let r = write_granule_m2 k r m.m2_header.header_is m.m2_side_info.side_gc.(0).(1) m.m2_scalefactors.(1) m.m2_quantizers.(1) in
		let r2 = snd r in
		if debug then Printf.printf "Second granule done at %d\n" r2;
		
		let data_raw = String.sub (fst r) 0 ((snd r + 7) lsr 3) in
		
		let (side_raw, side_bits, side_bytes) = (
			let r = String.create 17 in
			let s = m.m2_side_info in
			packBits r  0 8 s.side_main_data_begin;
			packBits r  8 3 0;
			let pack_gc gc bits o = (
				packBits r (o +  0) 12 bits;
				packBits r (o + 12)  9 gc.gc_big_values;
				packBits r (o + 21)  8 gc.gc_global_gain;
				packBits r (o + 29)  9 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						packBits r (o + 38) 1 0;
						packBits r (o + 39) 5 w.normal_table_select1;
						packBits r (o + 44) 5 w.normal_table_select2;
						packBits r (o + 49) 5 w.normal_table_select3;
						packBits r (o + 54) 4 w.normal_region_0_count;
						packBits r (o + 58) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						packBits r (o + 38) 1 1;
						packBits r (o + 39) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						packBits r (o + 41) 1 (if w.other_mixed_block then 1 else 0);
						packBits r (o + 42) 5 w.other_table_select1;
						packBits r (o + 47) 5 w.other_table_select2;
						packBits r (o + 52) 3 w.other_sub_block_gain1;
						packBits r (o + 55) 3 w.other_sub_block_gain2;
						packBits r (o + 58) 3 w.other_sub_block_gain3;
					)
				);
				packBits r (o + 61) 1 gc.gc_sf_scale;
				packBits r (o + 62) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc s.side_gc.(0).(0) (r1 - r0) 10;
			pack_gc s.side_gc.(0).(1) (r2 - r1) 73;
			(r, [| r1 - r0; r2 - r1 |], (r2 - r0 + 7) asr 3)
			
		) in
		
		if debug then Printf.printf "Done packing the side info\n";


		{
			f1_num = m.m2_starting_f1.f1_num;
			f1_header = m.m2_starting_f1.f1_header;
			f1_side = {
				side_raw = side_raw;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = data_raw;
			f1_pad_exact = None;
		}
	)
;;

let recompress_frame ?(debug = false) f =

(*
	Printf.printf "%d\n" f.f1_num;
	Printf.printf " Input frame:  \"%s\"\n" (to_hex f.f1_data);
*)
	let (decoded, decoder_error) = decode_frame ~debug:debug f in

	if debug then Printf.printf "Decoder error? %B\n" decoder_error;

	let rehuffed = if true then rehuff_frame ~debug:debug decoded else decoded in

	let encoded = encode_frame ~debug:debug rehuffed in
(*
	Printf.printf " Output frame: \"%s\"\n" (to_hex encoded.f1_data);
*)
	(encoded, decoder_error)
;;
