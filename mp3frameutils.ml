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



let get_seq = if true then Ptr.Ref.get_seq_fast else Ptr.Ref.get_seq;;

let get_huffman_c = false;;



(***********)
(* C STUBS *)
(***********)

external find_best_config       : Ptr.t -> Ptr.t -> Ptr.t -> Ptr.t -> bool -> (int * int * int * int * int * int * int * bool) = "mfu_find_best_config";;
external find_best_config_sse41 : Ptr.t -> Ptr.t -> Ptr.t -> Ptr.t -> bool -> (int * int * int * int * int * int * int * bool) = "mfu_find_best_config_sse41";;

let first_tick_ref = ref 0;;
let last_tick_ref = ref 0;;

let update_ref () = last_tick_ref := counter ();;
let acc_ref here =
	let a = counter () in
	here := !here - !last_tick_ref + a;
	last_tick_ref := a;
;;
let get_total () = !last_tick_ref - !first_tick_ref;;


let print_stuff_ticks_ref = ref 0;;
let outside_ticks_ref = ref 0;;
let before_c_ticks_ref = ref 0;;
let in_c_ticks_ref = ref 0;;
let after_c_ticks_ref = ref 0;;
let before_decode_ticks_ref = ref 0;;
let decode_scf_ticks_ref = ref 0;;
let before_decode_quant_ticks_ref = ref 0;;
let decode_big_quant_ticks_ref = ref 0;;
let decode_part1_quant_ticks_ref = ref 0;;
let decode_quant_ticks_ref = ref 0;;
let decode_ticks_ref = ref 0;;
let encode_ticks_ref = ref 0;;


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
let global_scalefactors_ptr =
	let get_this a b =
		let copy_array = global_scalefactors a b in
		let ptr = Ptr.make (2 * Array.length copy_array) 0 in
		Array.iteri (fun i q -> Ptr.put_16_of_int ptr (2 * i) q) copy_array;
		ptr
	in
	let long48000  = get_this S48000 false in
	let long44100  = get_this S44100 false in
	let long32000  = get_this S32000 false in
	let long24000  = get_this S24000 false in
	let long22050  = get_this S22050 false in
	let long16000  = get_this S16000 false in
	let long12000  = get_this S12000 false in
	let long11025  = get_this S11025 false in
	let long8000   = get_this  S8000 false in
	let short48000 = get_this S48000 true  in
	let short44100 = get_this S44100 true  in
	let short32000 = get_this S32000 true  in
	let short24000 = get_this S24000 true  in
	let short22050 = get_this S22050 true  in
	let short16000 = get_this S16000 true  in
	let short12000 = get_this S12000 true  in
	let short11025 = get_this S11025 true  in
	let short8000  = get_this  S8000 true  in
	fun sfreq is_short -> match (sfreq, is_short) with
	| (S48000, false) -> long48000
	| (S44100, false) -> long44100
	| (S32000, false) -> long32000
	| (S24000, false) -> long24000
	| (S22050, false) -> long22050
	| (S16000, false) -> long16000
	| (S12000, false) -> long12000
	| (S11025, false) -> long11025
	| ( S8000, false) -> long8000

	| (S48000, true ) -> short48000
	| (S44100, true ) -> short44100
	| (S32000, true ) -> short32000
	| (S24000, true ) -> short24000
	| (S22050, true ) -> short22050
	| (S16000, true ) -> short16000
	| (S12000, true ) -> short12000
	| (S11025, true ) -> short11025
	| ( S8000, true ) -> short8000
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
	m1_quantizer_ptrs : Ptr.t array array;
	m1_starting_f1 : f1_t;
};;

type m2_frame_data_t = {
	m2_header : header_t;
	m2_side_info : side_internal_t;
	m2_scalefactors : int array array; (* m2_scalefactors.(channel).(scalefactor) *)
	m2_quantizers : int array array; (* m2_quantizers.(channel).(quantizer) *)
	m2_quantizer_ptrs : Ptr.t array;
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
Side info:
 9/17/32 BYTES = 136/256 bits
 9: main_data_begin (8 for MPEG2)
 ?: private_bits
 4: SCFI Band
 59: Side Information Granule
  12: part2_3 length (main data for this channel, granule in bits)
  9: Big values
  8: Global gain
  4: Scalefactor compress (9 for MPEG2)
  1: Window switch flag
   if 1:
    2: Block type
    1: Mix block flag
    5x2: Table Select [region]
    3x3: sub_block_gain [window]
   if 0:
    5x3: Table select [region]
    4: Region 0 count
    3: Region 1 count
  1: Pre flag (NOT FOR MPEG2)
  1: Scale factor scale
  1: Count1 table select

 MPEG1 mono:
 [9 main data] [5 privates] [4 SCFI] [59 Gr0] [59 Gr1]
 (18 - 30) (77 - 89)
 MPEG1 stereo:
 [9 main data] [3 privates] [4 SCFI0] [4 SCFI1] [59 Gr0ch1] [59 Gr0ch2] [59 Gr1ch1] [59 Gr1ch2]
 20 79 138 197
 MPEG2 mono:
 [8 main data] [1 privates] [63 Gr*]
 (9 - 21)
 MPEG2 stereo:
 [8 main data] [2 privates] [63 Gr*ch1] [63 Gr*ch2]
 (10 - 22) (73 - 85)
*)



(*********************)
(* READ SCALEFACTORS *)
(*********************)
let read_scalefactors_m1 ?(debug=false) ?(tabs="") scfi prev_scf_option gc s =
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
	let rec read_stuff i = (
		let num_bits = if i < num1 then bits1 else bits2 in

		if debug && debug_more then Printf.printf "reading %d bits from %d (len %d bytes)\n" num_bits s.Ptr.Ref.seq_at s.Ptr.Ref.seq_ref.Ptr.Ref.lentot;
		if debug && debug_more then Printf.printf "seq fast is %08X, next bytes is %d\n" s.Ptr.Ref.seq_get_fast_int s.Ptr.Ref.seq_get_fast_next_byte;

		if i >= Array.length scf_out then (
			if debug && debug_more then Printf.printf "!";
		) else if i < 6 then (
			if not scfi.(0) then (scf_out.(i) <- get_seq s num_bits; if debug && debug_more then Printf.printf "0: %d\n" scf_out.(i));
			read_stuff (succ i)
		) else if i < 11 then (
			if not scfi.(1) then (scf_out.(i) <- get_seq s num_bits; if debug && debug_more then Printf.printf "1: %d\n" scf_out.(i));
			read_stuff (succ i)
		) else if i < 16 then (
			if not scfi.(2) then (scf_out.(i) <- get_seq s num_bits; if debug && debug_more then Printf.printf "2: %d\n" scf_out.(i));
			read_stuff (succ i)
		) else if i < 21 then (
			if not scfi.(3) then (scf_out.(i) <- get_seq s num_bits; if debug && debug_more then Printf.printf "3: %d\n" scf_out.(i));
			read_stuff (succ i)
		) else (
			scf_out.(i) <- get_seq s num_bits; (if debug && debug_more then Printf.printf "+");
			read_stuff (succ i)
		)
	) in

	read_stuff 0;
(*	read_stuff_s s 0;*)
	if debug && debug_more then Printf.printf "\n";


(*	if debug then Printf.printf "SCF compress (%d,%d)\n" bits1 bits2;*)
	if debug then Printf.printf "%sREAD_SCALEFACTORS_M1: (%d,%d)=%d,%d - [%s ] " tabs bits1 bits2 num1 num2 (Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" scf_out);
	if debug then Array.iter (fun q -> Printf.printf "%s" (if q then "#" else ".")) scfi;
	if debug then Printf.printf "\n";
	scf_out
;;

let read_scalefactors_m2 ?(debug=false) ?(tabs="") is gc s =

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
(*
	let rec read_stuff i = (
		if i < sob1 then (
			scf_out.(i) <- get_seq s bits0; if debug && debug_more then Printf.printf "0";
			read_stuff (succ i)
		) else if i < sob2 then (
			scf_out.(i) <- get_seq s bits1; if debug && debug_more then Printf.printf "1";
			read_stuff (succ i)
		) else if i < sob3 then (
			scf_out.(i) <- get_seq s bits2; if debug && debug_more then Printf.printf "2";
			read_stuff (succ i)
		) else if i < num_total then (
			scf_out.(i) <- get_seq s bits3; if debug && debug_more then Printf.printf "3";
			read_stuff (succ i)
		) else (
			if debug && debug_more then Printf.printf "!";
		)
	) in

	read_stuff 0;
*)
	for i = 0 to sob1 - 1 do
		scf_out.(i) <- get_seq s bits0;
(*		if debug && debug_more then Printf.printf "0";*)
	done;
	for i = sob1 to sob2 - 1 do
		scf_out.(i) <- get_seq s bits1;
(*		if debug && debug_more then Printf.printf "1";*)
	done;
	for i = sob2 to sob3 - 1 do
		scf_out.(i) <- get_seq s bits2;
(*		if debug && debug_more then Printf.printf "2";*)
	done;
	for i = sob3 to num_total - 1 do
		scf_out.(i) <- get_seq s bits3;
(*		if debug && debug_more then Printf.printf "3";*)
	done;
(*	if debug && debug_more then Printf.printf "!\n";*)

	if debug then (
		let str0 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out    0 num0) in
		let str1 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob1 num1) in
		let str2 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob2 num2) in
		let str3 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob3 num3) in
		Printf.printf "%sREAD_SCALEFACTORS_M2: (%d,%d,%d,%d)=%d,%d,%d,%d - [%s ]-[%s ]-[%s ]-[%s ]\n" tabs bits0 bits1 bits2 bits3 num0 num1 num2 num3 str0 str1 str2 str3;
	);

	scf_out
;;

(* It ends when r_at = r_to *)
let read_quantizers ?(debug=false) ?(tabs="") k gc in_ptr r_at r_to recompress_freq_overflow_warn_ref =
	let s = Ptr.Ref.new_seq (Ptr.Ref.of_ptr in_ptr) in
	Ptr.Ref.set_seq s r_at;
	let gs = get_seq s in
(*let debug = false in*)

	if debug && debug_more then Printf.printf "Starting on %S at %d, going to %d\n" (String.sub (to_bin (Ptr.Ref.to_string s.Ptr.Ref.seq_ref)) s.Ptr.Ref.seq_at (r_to - s.Ptr.Ref.seq_at)) s.Ptr.Ref.seq_at r_to;

	let decoder_error_ref = ref false in
	let out_quants = Array.make num_quants 0 in
	let out_quants_16_ptr = Ptr.make (num_quants * 2) 16 in
(*
	let in_ptr = Ptr.make (Ptr.Ref.length s.Ptr.Ref.seq_ref + 5) 16 in
	Ptr.Ref.blit_to_ptr s.Ptr.Ref.seq_ref 0 in_ptr 0 (Ptr.Ref.length s.Ptr.Ref.seq_ref);
*)
	acc_ref before_decode_quant_ticks_ref;

	let rec read_from_table index left hti linbits = if true then (
		if debug then Printf.printf "Index = %d, left = %d\n" index left;
		let (new_ptr_loc, new_index) = Mp3framehuffman.decode_big_quants in_ptr s.Ptr.Ref.seq_at r_to out_quants_16_ptr index (index + 2 * left) hti decoder_error_ref in
		if debug then Printf.printf "Got new index %d\n" new_index;
		Ptr.Ref.set_seq s new_ptr_loc;
		for transfer = index to new_index - 1 do
			out_quants.(transfer) <- Ptr.get_int_of_16 out_quants_16_ptr (transfer * 2);
(*			if debug then Printf.printf " Q%d" out_quants.(transfer);*)
			if debug && transfer land 1 = 1 then (
				Printf.printf "Running read_from_table index=%d left=00 linbits=%d\n" (transfer - 1) Mp3framehuffman.global_ht_linbits.(hti);
				Printf.printf " Found (%d,%d) for 00 ending with 00\n" (min 15 (abs out_quants.(transfer - 1))) (min 15 (abs out_quants.(transfer)));
				Printf.printf "  Add (%d,%d)\n" ((abs out_quants.(transfer - 1)) - (min 15 (abs out_quants.(transfer - 1)))) ((abs out_quants.(transfer)) - (min 15 (abs out_quants.(transfer))));
				Printf.printf "  Sign (%s,%s)\n" (if out_quants.(transfer - 1) >= 0 then "+" else "-") (if out_quants.(transfer) >= 0 then "+" else "-");
			)
		done;
		if debug then Printf.printf "\n";
		new_index
(*
		if new_index > index + 2 * left then (
			if debug then Printf.printf " OOPS! Decoding bigvalues went %d values overboard (now at %d)\n" (new_index - index - 2 * left) new_ptr_loc;
			decoder_error_ref := true;
			index
		) else if s.Ptr.Ref.seq_at > r_to then (
			if debug then Printf.printf " OOPS! Decoding bigvalues got too many bits (now at %d)\n" s.Ptr.Ref.seq_at;
			decoder_error_ref := true;
			index
		) else (
			new_index
		)
*)
	) else (

	(* Big_values *)
(*	let rec read_from_table index left hti linbits = ( *)
		if debug && debug_more then Printf.printf " Running read_from_table index=%d left=%d linbits=%d\n" index left linbits;
		if left <= 0 then (
			index
(*
		) else if left < 0 then (
			(* I don't entirely see how this can be hit, since 1 is subtracted from left during each iteration... *)
			if debug then Printf.printf " OOPS! Decoding bigvalues went %d values overboard (now at %d)\n" ( ~- left) s.Ptr.Ref.seq_at;
			decoder_error_ref := true;
			index
*)
		) else if s.Ptr.Ref.seq_at > r_to then (
			(* This used to be s.Ptr.Ref.seq_at >= r_to, but I can't figure out why I would have the = part in there *)
			(* If table 0 is used it would fail since it uses no bits but outputs valid big values *)
			if debug then Printf.printf " OOPS! Decoding bigvalues got too many bits (now at %d)\n" s.Ptr.Ref.seq_at;
			decoder_error_ref := true;
			index
		) else if index + 2 > num_quants then (
			(* The big_values went off the end of the out_quants array! *)
			if debug then Printf.printf " OOPS! Too many bigvalues; truncating here\n";
			decoder_error_ref := true;
			index
		) else (
(*
			let (abs_x,abs_y) = huffman_decode_ptrref s ht global_ht_bits in
*)
			Ptr.Ref.seq_fill s 19;
			let current_int = s.Ptr.Ref.seq_get_fast_int in
			let last_bit = s.Ptr.Ref.seq_get_fast_next_byte lsl 3 in
			let current_shift = last_bit - s.Ptr.Ref.seq_at - 1 in
			let got_huff_val = Mp3framehuffman.get_huffman_big hti current_int current_shift in
			let bits_used = got_huff_val lsr 8 in
			let abs_x = (got_huff_val lsr 4) land 15 in
			let abs_y = got_huff_val land 15 in
			Ptr.Ref.seq_add s bits_used;

			let x_add_lin = if abs_x = 15 && linbits > 0 then gs linbits else 0 in
			let x_neg = if abs_x > 0 then gs 1 else 0 in

			let y_add_lin = if abs_y = 15 && linbits > 0 then gs linbits else 0 in
			let y_neg = if abs_y > 0 then gs 1 else 0 in
(*
			let x_mult = if x_neg = 0 then 1 else -1 in
			let y_mult = if y_neg = 0 then 1 else -1 in
*)
			if debug && debug_more then (
				Printf.printf " Found (%d,%d) for %d ending with %d\n" abs_x abs_y index s.Ptr.Ref.seq_at;
				Printf.printf "  Add (%d,%d)\n" x_add_lin y_add_lin;
				Printf.printf "  Sign (%s,%s)\n" (if x_neg = 0 then "+" else "-") (if y_neg = 0 then "+" else "-");
			);

			out_quants.(index + 0) <- (abs_x + x_add_lin) * (1 - 2 * x_neg);
			out_quants.(index + 1) <- (abs_y + y_add_lin) * (1 - 2 * y_neg);
			Ptr.put_16_of_int out_quants_16_ptr (2 * (index + 0)) out_quants.(index + 0);
			Ptr.put_16_of_int out_quants_16_ptr (2 * (index + 1)) out_quants.(index + 1);
			read_from_table (index + 2) (left - 1) hti linbits

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

	let index = read_from_table   0   region0 table0 global_ht_linbits.(table0) in
	let index = read_from_table index region1 table1 global_ht_linbits.(table1) in
	let index = read_from_table index region2 table2 global_ht_linbits.(table2) in

	if index <> 2 * gc.gc_big_values then (
		decoder_error_ref := true;
		if debug && debug_more then Printf.printf "No room for any more bigvalues %d != %d\n" index (2 * gc.gc_big_values);
	);

	if debug && debug_more then Printf.printf "DONE!\n";

	acc_ref decode_big_quant_ticks_ref;

	(* Count1 *)
	let read_count1_from_table index table_1 =
		if debug then Printf.printf "Count1 index = %d\n" index;
		let (new_ptr_loc, new_index) = Mp3framehuffman.decode_count1_quants in_ptr s.Ptr.Ref.seq_at r_to out_quants_16_ptr index num_quants table_1 decoder_error_ref in
		if debug then Printf.printf "Got new index %d, and new loc %d\n" new_index new_ptr_loc;
		Ptr.Ref.set_seq s new_ptr_loc;
		for transfer = index to (min new_index (Array.length out_quants)) - 1 do
			out_quants.(transfer) <- Ptr.get_int_of_16 out_quants_16_ptr (transfer * 2);
			if debug && debug_more then (
				if (transfer - index) mod 4 = 0 then (
					Printf.printf " Found %d at %d\n" out_quants.(transfer) transfer
				) else if (transfer - index) mod 4 = 3 then (
					Printf.printf "       %d (input to bit 00)\n" out_quants.(transfer);
				) else (
					Printf.printf "       %d\n" out_quants.(transfer);
				)
			)
		done;
		new_index
	in
	let rec old_read_count1_from_table index hti = (
		if debug && debug_more then Printf.printf "%d = %d? %B. %d = %d? %B.\n" s.Ptr.Ref.seq_at r_to (s.Ptr.Ref.seq_at = r_to) index num_quants (index = num_quants);
		if s.Ptr.Ref.seq_at > r_to || index > num_quants then (
			if debug && debug_more then Printf.printf " OOPS! Decoding count1 went a little overboard (pos %d > %d or length %d > %d)\n" s.Ptr.Ref.seq_at r_to index num_quants;
			decoder_error_ref := true;
			index
		) else if s.Ptr.Ref.seq_at = r_to || index = num_quants then (
			index
		) else (
(*
			let (v1,w1,x1,y1) = huffman_decode_ptrref s ht global_ht_count1_bits in
*)
			Ptr.Ref.seq_fill s 6;
			let current_int = s.Ptr.Ref.seq_get_fast_int in
			let last_bit = s.Ptr.Ref.seq_get_fast_next_byte lsl 3 in
			let current_shift = last_bit - s.Ptr.Ref.seq_at - 1 in
			let got_huff_val = Mp3framehuffman.get_huffman_part1 hti current_int current_shift in
			let bits_used = got_huff_val lsr 8 in
			Ptr.Ref.seq_add s bits_used;
			let v1 = (got_huff_val lsr 3) land 1 in
			let w1 = (got_huff_val lsr 2) land 1 in
			let x1 = (got_huff_val lsr 1) land 1 in
			let y1 = (got_huff_val      ) land 1 in

			let v_neg = if v1 = 0 then 0 else gs 1 in
			let w_neg = if w1 = 0 then 0 else gs 1 in
			let x_neg = if x1 = 0 then 0 else gs 1 in
			let y_neg = if y1 = 0 then 0 else gs 1 in
(*
			let v = if v_neg = 1 then ~- v1 else v1 in
			let w = if w_neg = 1 then ~- w1 else w1 in
			let x = if x_neg = 1 then ~- x1 else x1 in
			let y = if y_neg = 1 then ~- y1 else y1 in
*)
			let v = v1 * (1 - 2 * v_neg) in
			let w = w1 * (1 - 2 * w_neg) in
			let x = x1 * (1 - 2 * x_neg) in
			let y = y1 * (1 - 2 * y_neg) in

			if debug && debug_more then (
				Printf.printf " Found %d at %d\n" v index;
				Printf.printf "       %d\n" w;
				Printf.printf "       %d\n" x;
				Printf.printf "       %d (input to bit %d)\n" y s.Ptr.Ref.seq_at;
(*
				Printf.printf " Found (%d,%d,%d,%d) for %d ending with %d (to %d)\n" v1 w1 x1 y1 index s.Ptr.Ref.seq_at r_to;
				Printf.printf "  %s%s%s%s\n" (if v_neg = 1 then "-" else "+") (if w_neg = 1 then "-" else "+") (if x_neg = 1 then "-" else "+") (if y_neg = 1 then "-" else "+");
*)
			);

			if s.Ptr.Ref.seq_at > r_to then (
				(* The file has been read too far *)
				if debug && debug_more then Printf.printf " OOPS! Decoding count1 went a little overboard (pos %d > %d)\n" s.Ptr.Ref.seq_at r_to;

				(* This should be an error *)
				decoder_error_ref := true;

				index
			) else if (index + 4) <= num_quants then (
				out_quants.(index + 0) <- v;
				out_quants.(index + 1) <- w;
				out_quants.(index + 2) <- x;
				out_quants.(index + 3) <- y;
				Ptr.put_16_of_int out_quants_16_ptr (2 * (index + 0)) out_quants.(index + 0);
				Ptr.put_16_of_int out_quants_16_ptr (2 * (index + 1)) out_quants.(index + 1);
				Ptr.put_16_of_int out_quants_16_ptr (2 * (index + 2)) out_quants.(index + 2);
				Ptr.put_16_of_int out_quants_16_ptr (2 * (index + 3)) out_quants.(index + 3);

				old_read_count1_from_table (index + 4) hti
			) else if x = 0 && y = 0 then (
				(* If the frequencies above the max were 0, assume that the encoder just does that *)
				if debug && debug_more then (
					Printf.printf " Num_quants exceeded, but the values are 0 so ignore (length %d > %d)\n" (index + 4) num_quants
				) else if !recompress_freq_overflow_warn_ref then (
					Printf.printf "\rWARNING: too many frequencies; files may be decoded differently by some players\n";
					recompress_freq_overflow_warn_ref := false;
				);
				out_quants.(index + 0) <- v;
				out_quants.(index + 1) <- w;
				Ptr.put_16_of_int out_quants_16_ptr (2 * (index + 0)) out_quants.(index + 0);
				Ptr.put_16_of_int out_quants_16_ptr (2 * (index + 1)) out_quants.(index + 1);
				index + 2
			) else (
				(* The stuff above the max frequency is not zero! This indicates badness *)
				if debug && debug_more then (
					Printf.printf " OOPS! Decoding count1 went a little overboard (length %d > %d)\n" (index + 4) num_quants
				) else if !recompress_freq_overflow_warn_ref then (
					Printf.printf "\rWARNING: too many frequencies; files may be decoded differently by some players\n";
					recompress_freq_overflow_warn_ref := false;
				);

				(* However, it is not considered an error as of 1.16; instead, the quants are set to 0 *)
(*				decoder_error_ref := true;*)

				index
			)
		)
	) in

	acc_ref decode_part1_quant_ticks_ref;

	let index = if true then read_count1_from_table index gc.gc_count1_table_1 else old_read_count1_from_table index (if gc.gc_count1_table_1 then 1 else 0) in
	for i = index to num_quants - 1 do
		Ptr.put_16_of_int out_quants_16_ptr (2 * i) 0;
	done;

	if s.Ptr.Ref.seq_at > r_to then (
		if debug && debug_more then Printf.printf " OOPS! Decoding count1 went a little overboard (pos %d > %d)\n" s.Ptr.Ref.seq_at r_to;
		decoder_error_ref := true;
	);

	if debug then Printf.printf "%sREAD_QUANTIZERS_M1: (%d) - [" tabs index;
	if debug then (Array.iter (fun x -> Printf.printf " %d" x) out_quants; Printf.printf " ]\n");
	(out_quants, out_quants_16_ptr, !decoder_error_ref)
;;



(************************************************************************************************************************)
(* REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME *)
(************************************************************************************************************************)
let rehuff_granule debug quant_ptr process_function gc scf_bands_ptr = match gc.gc_window with
	| Window_other w -> gc
	| Window_normal w -> (
(*
		if false then (
			Printf.printf "EDITING QUANTS!\n";
			for i = 0 to 1 do
				quants.(i * 2 + 0) <- 18;
				quants.(i * 2 + 1) <- 2;
			done;
			for i = 2 to 3 do
				quants.(i * 2 + 0) <- 2049;
				quants.(i * 2 + 1) <- 1;
			done;
			for i = 0 to -1 do
				quants.(i * 4 + 0) <- 0;
				quants.(i * 4 + 1) <- 0;
				quants.(i * 4 + 2) <- 1;
				quants.(i * 4 + 3) <- 0;
			done;
		);
*)
		let (s_l1,s_l2,s_big,s_count1num,s_t1,s_t2,s_t3,p1t1) = (

			if debug then Printf.printf "%!";
			acc_ref before_c_ticks_ref;
(*			Printf.printf "FIND BEST CONFIG\n%!";*)
			let (s_l1,s_l2,s_big,s_count1num,s_t1,s_t2,s_t3,p1t1) = process_function
				quant_bits_ptr16
				quant_bits_count1_char_ptr
				scf_bands_ptr
				quant_ptr
				debug
			in
(*			Printf.printf "DONE BEST CONFIG\n%!";*)
			acc_ref in_c_ticks_ref;

			(s_l1,s_l2,s_big,s_count1num,s_t1,s_t2,s_t3,p1t1)
		) in

(*		let (s_l1,s_l2,s_big,s_t1,s_t2,s_t3,p1t1) = smallest_config in*)
		if debug then (
			Printf.printf "  NEW: (%d,%d,%d,%d,%d,%d,%B)\n" s_l1 s_l2 s_big s_t1 s_t2 s_t3 p1t1;
			Printf.printf "       tables %d,%d,%d, length %db,%db,%dq\n" s_t1 s_t2 s_t3 (s_l1 + 1) (s_l2 + 1) (2 * s_big);
			Printf.printf "\n%!";
		);
(*		Printf.printf "Best config: tables %d,%d,%d, length %db,%db,%dq\n" s_t1 s_t2 s_t3 (s_l1 + 1) (s_l2 + 1) (2 * s_big);*)

		{gc with
			gc_big_values = s_big;
			gc_window = Window_normal {normal_table_select1 = s_t1; normal_table_select2 = s_t2; normal_table_select3 = s_t3; normal_region_0_count = s_l1; normal_region_1_count = s_l2};
			gc_count1_table_1 = p1t1;
		}
	)
;; (* rehuff_granule quants gc scf_quants *)


(******************)
(* REHUFF THREADS *)
(******************)
type ('a,'b) rehuff_thread_t = {
	rehuff_thread : Thread.t;
	rehuff_thread_id : os_thread_id;
	rehuff_thread_mutex : Mutex.t;
	rehuff_thread_process_cond : Condition.t;
	mutable rehuff_thread_process : 'a option;
	rehuff_thread_done_cond : Condition.t;
	mutable rehuff_thread_done : 'b option;
};;

let rec rehuff_thread_guts send_thread_channel =
	(* This thread has to make the thread info in order to get the thread_id *)
	let t = {
		rehuff_thread = Thread.self ();
		rehuff_thread_id = get_os_thread_self_id ();
		rehuff_thread_mutex = Mutex.create ();
		rehuff_thread_process_cond = Condition.create ();
		rehuff_thread_process = None;
		rehuff_thread_done_cond = Condition.create ();
		rehuff_thread_done = None;
	} in
	Event.sync (Event.send send_thread_channel t);
	let rec loop () =

		Mutex.lock t.rehuff_thread_mutex;
		let rec get () = match t.rehuff_thread_process with
			| None -> (Condition.wait t.rehuff_thread_process_cond t.rehuff_thread_mutex; get ())
			| Some a -> a
		in
		let (debug, header, ptr, at_bit, to_bit, side_gc, scf_bands_ptr, process_function) = get () in
		t.rehuff_thread_process <- None;
		Mutex.unlock t.rehuff_thread_mutex;

		let ret = try
			let (q,qp,error) = read_quantizers ~debug:debug header side_gc ptr at_bit to_bit (ref false) in
			let gc = rehuff_granule debug qp process_function side_gc scf_bands_ptr in
			Normal (q,qp,error,gc)
		with
			| e -> (
				if debug then Printf.printf "rehuff_thread_guts failed with %s\n%!" (Printexc.to_string e);
				Error e
			)
		in

		Mutex.lock t.rehuff_thread_mutex;
		t.rehuff_thread_done <- Some ret;
		Condition.signal t.rehuff_thread_done_cond;
		Mutex.unlock t.rehuff_thread_mutex;

		loop ()
	in
	loop ()
;;

let make_rehuff_thread () =
	let chan = Event.new_channel () in
	ignore @@ Thread.create rehuff_thread_guts chan;
	let id = Event.sync (Event.receive chan) in
	id
;;

let rehuff_thread_array_ref = ref [||];;

let expand_rehuff_array target_size =
	if Array.length !rehuff_thread_array_ref < target_size then (
		let new_array = Array.init target_size (fun i -> if i < Array.length !rehuff_thread_array_ref then !rehuff_thread_array_ref.(i) else make_rehuff_thread ()) in
		rehuff_thread_array_ref := new_array
	)
;;

let decode_granule_async debug header ptr at_bit to_bit side_gc scf_bands_ptr process_function gran_num =
	expand_rehuff_array (gran_num + 1);
	let t = !rehuff_thread_array_ref.(gran_num) in

	Mutex.lock t.rehuff_thread_mutex;
	t.rehuff_thread_done <- None;
	t.rehuff_thread_process <- Some (debug, header, ptr, at_bit, to_bit, side_gc, scf_bands_ptr, process_function);
	Condition.signal t.rehuff_thread_process_cond;
	Mutex.unlock t.rehuff_thread_mutex;

	gran_num
;;

let sync_decode gran_num =
	if gran_num >= Array.length !rehuff_thread_array_ref then failwith "can't get granule back from worker thread";
	let t = !rehuff_thread_array_ref.(gran_num) in
	if thread_is_alive t.rehuff_thread_id then (
		Mutex.lock t.rehuff_thread_mutex;
		let rec get () =
			match t.rehuff_thread_done with
			| None -> (Condition.wait t.rehuff_thread_done_cond t.rehuff_thread_mutex; get ())
			| Some a -> a
		in
		let a = get () in
		Mutex.unlock t.rehuff_thread_mutex;
		match a with
		| Normal x -> x
		| Error e -> raise e
	) else (
		failwith "worker thread died";
	)
;;

(********
type 'a async_send_t =
	| Async_send of 'a(*bool * Ptr.t * side_gc_t * Ptr.t (* (debug,quant_ptr,side_gc,scf_bands) *)*)
	| Async_ping
;;

type rehuff_thread_t = {
	rehuff_thread : Thread.t;
	rehuff_channel_process : (bool * Ptr.t * side_gc_t * Ptr.t) async_send_t Event.channel;
	rehuff_channel_done : side_gc_t Event.channel;
};;

let rehuff_thread_guts thread_num recv_channel send_channel =
	let rec loop () =
(*		let (debug, quant_ptr, side_gc, scf_bands_ptr) = *)
		match Event.sync (Event.receive recv_channel) with
		| Async_ping -> loop () (* Go again *)
		| Async_send (debug, quant_ptr, side_gc, scf_bands_ptr) -> (
(*			Thread.delay 0.2;*)
(*			if debug then Printf.printf "%s%!" (Printf.sprintf "<%d>\n" thread_num);*)
			let new_gc = rehuff_granule false quant_ptr side_gc scf_bands_ptr in
			Event.sync (Event.send send_channel new_gc);
(*			if debug then Printf.printf "%s%!" (Printf.sprintf "{%d}\n" thread_num);*)
			loop ()
		)
	in
	loop ()
;;
let make_rehuff_thread i =
	let send = Event.new_channel () in
	let recv = Event.new_channel () in
	let t = Thread.create (rehuff_thread_guts i recv) send in
	{
		rehuff_thread = t;
		rehuff_channel_process = recv;
		rehuff_channel_done = send;
	}
;;

let rehuff_thread_array_ref = ref [||];;
let next_rehuff_thread_ref = ref 0;;
let async_sync = false;;
if async_sync then (
	Printf.printf "\n";
	Printf.printf "###########################################\n";
	Printf.printf "#### ASYNC THREADS WORK SYNCHRONOUSLY! ####\n";
	Printf.printf "###########################################\n";
);;
let rehuff_granule_async debug qp gc scf_bands =
	if Array.length !rehuff_thread_array_ref = 0 then (
		(* We'll never need any more than 4 threads, since that's the max number of granules per frame *)
		rehuff_thread_array_ref := Array.init 4 make_rehuff_thread
	);
	while !next_rehuff_thread_ref >= Array.length !rehuff_thread_array_ref do
		next_rehuff_thread_ref := !next_rehuff_thread_ref - Array.length !rehuff_thread_array_ref;
	done;
	let do_thread = !next_rehuff_thread_ref in
	let channel_process = !rehuff_thread_array_ref.(do_thread).rehuff_channel_process in
	let channel_done = !rehuff_thread_array_ref.(do_thread).rehuff_channel_done in
	if debug then Printf.printf "checking thread %d to see if it's still alive\n" do_thread;
	(* If the previous frame processed everything properly, the thread will be waiting on rehuff_channel_process *)
	(* However, if the main thread failed while the processing thread was still going, the processing thread will be waiting on rehuff_channel_done *)
	(* This will force the thread to stop waiting on anything, making it wait on rehuff_channel_process next *)
	Event.sync @@ Event.choose [
		Event.send channel_process Async_ping;
		Event.wrap (Event.receive channel_done) ignore
	];
	if debug then Printf.printf "sending GC to thread %d\n%!" do_thread;
	Event.sync @@ Event.send channel_process (Async_send (debug, qp, gc, scf_bands));
	if debug then Printf.printf "sent GC to thread %d\n%!" do_thread;
	let ret = if async_sync then (
		let put_back = Event.sync @@ Event.receive channel_done in
		Event.always put_back
	) else (
		Event.receive channel_done
	) in
	incr next_rehuff_thread_ref;
	ret
;;

type decode_thread_t = {
	decode_thread : Thread.t;
	decode_channel_process : (bool * header_t * Ptr.t * int * int * side_gc_t * Ptr.t) async_send_t Event.channel;
	decode_channel_done : ((int array * Ptr.t * bool * side_gc_t) option) Event.channel;
};;
let decode_thread_guts thread_num recv_channel send_channel =
	let rec loop () =
		match Event.sync (Event.receive recv_channel) with
		| Async_ping -> loop ()
		| Async_send (debug, header, ptr, at_bit, to_bit, side_gc, scf_bands_ptr) -> (
			let ret = try
				let (q,qp,error) = read_quantizers ~debug:debug header side_gc ptr at_bit to_bit (ref false) in
				let gc = rehuff_granule debug qp side_gc scf_bands_ptr in
				Some (q,qp,error,gc)
			with
				| e -> (
					if debug then Printf.printf "decode_thread_guts failed with %s\n%!" (Printexc.to_string e);
					None
				)
			in
			Event.sync (Event.send send_channel ret);
			loop ()
		)
	in
	loop ()
;;
let make_decode_thread i =
	let send = Event.new_channel () in
	let recv = Event.new_channel () in
	let t = Thread.create (decode_thread_guts i recv) send in
	{
		decode_thread = t;
		decode_channel_process = recv;
		decode_channel_done = send;
	}
;;

let decode_thread_array_ref = ref [||];;
let next_decode_thread_ref = ref 0;;
let decode_granule_async debug header ptr at_bit to_bit side_gc scf_bands_ptr gran_num =
	if Array.length !decode_thread_array_ref = 0 then (
		decode_thread_array_ref := Array.init 4 make_decode_thread
	);
	while !next_decode_thread_ref >= Array.length !decode_thread_array_ref do
		next_decode_thread_ref := !next_decode_thread_ref - Array.length !decode_thread_array_ref;
	done;
	let do_thread = !next_decode_thread_ref in
	let decode = !decode_thread_array_ref.(do_thread) in
	let channel_process = decode.decode_channel_process in
	let channel_done = decode.decode_channel_done in
	if debug then Printf.printf "checking decode thread %d to see if it's still alive\n" do_thread;
	Event.sync @@ Event.choose [
		Event.send channel_process Async_ping;
		Event.wrap (Event.receive channel_done) ignore
	];
	if debug then Printf.printf "sending input data to thread %d\n%!" do_thread;
	Event.sync @@ Event.send channel_process (Async_send (debug, header, ptr, at_bit, to_bit, side_gc, scf_bands_ptr));
	if debug then Printf.printf "sent input data to thread %d\n%!" do_thread;
	let ret = if async_sync then (
		let put_back = Event.sync @@ Event.receive channel_done in
		Event.always put_back
	) else (
		Event.receive channel_done
	) in
	incr next_decode_thread_ref;
	ret
;;
(* The decode functions can raise exceptions, which are caught and turned into None in the channel *)
(* This function turns it back into an exception *)
let sync_decode event =
	match Event.sync event with
	| Some x -> x
	| None -> raise (Failure "decode granule")
;;

(*
let rehuff_frame ?(debug=false) frame = (

	if debug then Printf.printf "\n";

	match frame with
	| M1_frame_data f -> (
		if debug then Printf.printf "REHUFF_FRAME M1\n";
(*		let scf_bands = global_scalefactors f.m1_header.header_samplerate false in*)
		let scf_bands_ptr = global_scalefactors_ptr f.m1_header.header_samplerate false in
		let new_granules = Array.mapi (fun gran_i quant_gran ->
			let new_channels = Array.mapi (fun chan_i quant_ptr ->
				if debug then Printf.printf " GC %d,%d\n" gran_i chan_i;
				let gc = f.m1_side_info.side_gc.(gran_i).(chan_i) in
				rehuff_granule debug quant_ptr gc scf_bands_ptr
			) quant_gran in
			new_channels
		) f.m1_quantizer_ptrs in
		M1_frame_data {f with
			m1_side_info = {f.m1_side_info with side_gc = new_granules}
		}
	)
	| M2_frame_data f -> (
		if debug then Printf.printf "REHUFF_FRAME M2\n";

(*		let scf_bands = global_scalefactors f.m2_header.header_samplerate false in*)
		let scf_bands_ptr = global_scalefactors_ptr f.m2_header.header_samplerate false in
		let new_granules = Array.mapi (fun chan_i quant_ptr ->
			if debug then Printf.printf " GC %d\n" chan_i;
			let gc = f.m2_side_info.side_gc.(0).(chan_i) in
			rehuff_granule debug quant_ptr gc scf_bands_ptr
		) f.m2_quantizer_ptrs in
		M2_frame_data {f with
			m2_side_info = {f.m2_side_info with side_gc = [| new_granules |]}
		}
	)

);;
*)
********)




(*********************************************************************************)
(* DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME *)
(*********************************************************************************)
let decode_frame ?(debug = false) f process_function recompress_freq_overflow_warn_ref =
	let s = Ptr.Ref.new_seq f.f1_side.side_raw in
	let gs = get_seq s in
	let side_info = match f.f1_header.header_id with
		| MPEG1 -> (

			let read_gc part2_3_offset = (
				let part2_3_length = gs 12 in
				let big_values = gs 9 in
				let global_gain = gs 8 in
				let scf_compress = gs 4 in
				let window_flag = gs 1 in
				let window = if window_flag = 0 then (
					let huff1 = gs 5 in
					let huff2 = gs 5 in
					let huff3 = gs 5 in
					let r0 = gs 4 in
					let r1 = gs 3 in
					Window_normal {
						normal_table_select1 = huff1;
						normal_table_select2 = huff2;
						normal_table_select3 = huff3;
						normal_region_0_count = r0;
						normal_region_1_count = r1;
					}
				) else (
					let block_type_index = gs 2 in
					let mixed_block = gs 1 in
					let huff1 = gs 5 in
					let huff2 = gs 5 in
					let sb_gain1 = gs 3 in
					let sb_gain2 = gs 3 in
					let sb_gain3 = gs 3 in
					Window_other {
						other_block_type = [| Block_type_long; Block_type_start; Block_type_short; Block_type_stop |].(block_type_index);
						other_mixed_block = (mixed_block = 1);
						other_table_select1 = huff1;
						other_table_select2 = huff2;
						other_sub_block_gain1 = sb_gain1;
						other_sub_block_gain2 = sb_gain2;
						other_sub_block_gain3 = sb_gain3;
					}
				) in
				let pre_flag = gs 1 in
				let sf_scale = gs 1 in
				let count1_table = gs 1 in
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
				}, part2_3_offset + part2_3_length)
			) in

			match f.f1_header.header_channel_mode with
			| ChannelMono -> (
				let main_data = gs 9 in
				let _ = gs 5 in
				let a = gs 1 in
				let b = gs 1 in
				let c = gs 1 in
				let d = gs 1 in
				let side_scfi = [| [| a = 1; b = 1; c = 1; d = 1 |] |] in
				let (side_gc1, new_so_far) = read_gc 0 in
				let (side_gc2, new_so_far) = read_gc new_so_far in
				{
					side_main_data_begin = main_data;
					side_scfi = side_scfi;
					side_gc = [| [| side_gc1 |]; [| side_gc2 |] |];
				}
			)
			| _ -> (
				let main_data = gs 9 in
				let _ = gs 3 in
				let a = gs 1 in
				let b = gs 1 in
				let c = gs 1 in
				let d = gs 1 in
				let e = gs 1 in
				let f = gs 1 in
				let g = gs 1 in
				let h = gs 1 in
				let side_scfi = [| [| a = 1; b = 1; c = 1; d = 1 |]; [| e = 1; f = 1; g = 1; h = 1 |] |] in
				let (side_gc1, new_so_far) = read_gc 0 in
				let (side_gc2, new_so_far) = read_gc new_so_far in
				let (side_gc3, new_so_far) = read_gc new_so_far in
				let (side_gc4, new_so_far) = read_gc new_so_far in
				{
					side_main_data_begin = main_data;
					side_scfi = side_scfi;
					side_gc = [| [| side_gc1; side_gc2 |]; [| side_gc3; side_gc4 |] |];
				}
			)
		)
		| _ -> (

			let read_gc part2_3_offset = (
				let part2_3_length = gs 12 in
				let big_values = gs 9 in
				let global_gain = gs 8 in
				let scf_compress = gs 9 in
				let window_flag = gs 1 in
				let window = if window_flag = 0 then (
					let huff1 = gs 5 in
					let huff2 = gs 5 in
					let huff3 = gs 5 in
					let r0 = gs 4 in
					let r1 = gs 3 in
					Window_normal {
						normal_table_select1 = huff1;
						normal_table_select2 = huff2;
						normal_table_select3 = huff3;
						normal_region_0_count = r0;
						normal_region_1_count = r1;
					}
				) else (
					let block_type_index = gs 2 in
					let mixed_block = gs 1 in
					let huff1 = gs 5 in
					let huff2 = gs 5 in
					let sb_gain1 = gs 3 in
					let sb_gain2 = gs 3 in
					let sb_gain3 = gs 3 in
					Window_other {
						other_block_type = [| Block_type_long; Block_type_start; Block_type_short; Block_type_stop |].(block_type_index);
						other_mixed_block = (mixed_block = 1);
						other_table_select1 = huff1;
						other_table_select2 = huff2;
						other_sub_block_gain1 = sb_gain1;
						other_sub_block_gain2 = sb_gain2;
						other_sub_block_gain3 = sb_gain3;
					}
				) in
				let sf_scale = gs 1 in
				let count1_table = gs 1 in
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
				}, part2_3_offset + part2_3_length)
			) in

			match f.f1_header.header_channel_mode with
			| ChannelMono -> (
				let main_data = gs 8 in
				let _ = gs 1 in
				let (side_gc, _) = read_gc 0 in
				{
					side_main_data_begin = main_data;
					side_scfi = [| |];
					side_gc = [| [| side_gc |] |]
				}
			)
			| _ -> (
				let main_data = gs 8 in
				let _ = gs 2 in
				let (side_gc1, new_so_far) = read_gc 0 in
				let (side_gc2, new_so_far) = read_gc new_so_far in
				{
					side_main_data_begin = main_data;
					side_scfi = [| |];
					side_gc = [| [| side_gc1; side_gc2 |] |]
				}
			) (* Mono / stereo *)
		) (* MPEG 2/2.5 *)
	in (* defines side_info *)

(*	print_side side_info;*)

	let tab2 = tab ^ tab in

	let decoded = (
		let k = f.f1_header in
		(* This is needed for the new async quant readers *)
		let data_ptr = Ptr.Ref.to_ptr f.f1_data in
		let data_ptr_ref = Ptr.Ref.of_ptr data_ptr in

		match (f.f1_header.header_id, f.f1_header.header_channel_mode) with
		| (MPEG1, ChannelMono) -> (
			(* Frame has 1 channel, 2 granules *)
			(* First, get the first scalefactors *)
			if debug then Printf.printf "DECODE FRAME %d (MPEG1, MONO)\n" f.f1_num;

			if debug then (
				Printf.printf "%sSide %S\n" tab2 (Ptr.Ref.to_HEX f.f1_side.side_raw);
				Printf.printf "%sData %S\n" tab2 (Ptr.Ref.to_HEX f.f1_data);
				print_side ~tabs:tab2 side_info;
			);

			if debug then Printf.printf "%sGr0:\n" tab;
(*
			let s = {
				(Ptr.Ref.new_seq f.f1_data) with Ptr.Ref.seq_at = side_info.side_gc.(0).(0).gc_part2_3_offset
(*
				Ptr.Ref.seq_ref = f.f1_data;
				Ptr.Ref.seq_at = side_info.side_gc.(0).(0).gc_part2_3_offset;
				Ptr.Ref.seq_get_fast_int = 0;
				Ptr.Ref.seq_get_fast_next_byte = 0;
*)
			} in
*)
			let s = Ptr.Ref.new_seq f.f1_data in
			Ptr.Ref.set_seq s side_info.side_gc.(0).(0).gc_part2_3_offset;

			(* The rehuff does nothing for non-normal windows, so just get the scf bands for non-short blocks *)
			let scf_bands_ptr = global_scalefactors_ptr f.f1_header.header_samplerate false in

			acc_ref before_decode_ticks_ref;
			let scf0 = read_scalefactors_m1 ~debug:debug ~tabs:tab2 [| false;false;false;false |]  None       side_info.side_gc.(0).(0) s in
			acc_ref decode_scf_ticks_ref;
			let get_decode_0 = decode_granule_async debug k data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) side_info.side_gc.(0).(0) scf_bands_ptr process_function 0 in
(*			let (q0, qp0, error0) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) recompress_freq_overflow_warn_ref in*)
			acc_ref decode_quant_ticks_ref;
(*			let gc0 = rehuff_granule debug qp0 process_function side_info.side_gc.(0).(0) scf_bands_ptr in*)
(*			let get_gc0 = rehuff_granule_async debug qp0 process_function side_info.side_gc.(0).(0) scf_bands_ptr in*)

			(*s.Ptr.Ref.seq_at <- side_info.side_gc.(1).(0).gc_part2_3_offset;*)
			Ptr.Ref.set_seq s side_info.side_gc.(1).(0).gc_part2_3_offset;
			if debug then Printf.printf "%sGr1:\n" tab;
			let scf1 = read_scalefactors_m1 ~debug:debug ~tabs:tab2 side_info.side_scfi.(0)       (Some scf0) side_info.side_gc.(1).(0) s in
			acc_ref decode_scf_ticks_ref;
			let get_decode_1 = decode_granule_async debug k data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(1).(0).gc_part2_3_length + side_info.side_gc.(1).(0).gc_part2_3_offset) side_info.side_gc.(1).(0) scf_bands_ptr process_function 1 in
(*			let (q1, qp1, error1) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(1).(0) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(1).(0).gc_part2_3_length + side_info.side_gc.(1).(0).gc_part2_3_offset) recompress_freq_overflow_warn_ref in*)
			acc_ref decode_quant_ticks_ref;
(*			let gc1 = rehuff_granule debug qp1 side_info.side_gc.(1).(0) scf_bands_ptr in*)
(*			let get_gc1 = rehuff_granule_async debug qp1 side_info.side_gc.(1).(0) scf_bands_ptr in*)

			let (q0, qp0, error0, gc0) = sync_decode get_decode_0 in
			let (q1, qp1, error1, gc1) = sync_decode get_decode_1 in

			(
				M1_frame_data {
					m1_header = f.f1_header;
					m1_side_info = {side_info with side_gc = [| [| (*Event.sync get_*)gc0 |]; [| (*Event.sync get_*)gc1 |] |]};
					m1_scalefactors = [| [| scf0  |]; [| scf1 |] |];
					m1_quantizers = [| [| q0 |]; [| q1 |] |];
					m1_quantizer_ptrs = [| [| qp0 |]; [| qp1 |] |];
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
				Printf.printf "%sSide %S\n" tab (Ptr.Ref.to_HEX f.f1_side.side_raw);
				Printf.printf "%sData %S\n" tab (Ptr.Ref.to_HEX f.f1_data);
				print_side ~tabs:tab side_info;
			);

			if debug then Printf.printf "%sGr0 Ch0:\n%!" tab;
			let s = Ptr.Ref.new_seq data_ptr_ref in
			Ptr.Ref.set_seq s side_info.side_gc.(0).(0).gc_part2_3_offset;

			(* things for inline rehuff *)
			(* The rehuff does nothing for non-normal windows, so just get the scf bands for non-short blocks *)
			let scf_bands_ptr = global_scalefactors_ptr f.f1_header.header_samplerate false in


			acc_ref before_decode_ticks_ref;
			let scf00 = read_scalefactors_m1 ~debug:debug ~tabs:tab2 [| false;false;false;false |]  None        side_info.side_gc.(0).(0) s in
			acc_ref decode_scf_ticks_ref;
			let get_decode_00 = (*(q00, qp00, error00, gc00) = sync_decode @@*) decode_granule_async debug k data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) side_info.side_gc.(0).(0) scf_bands_ptr process_function 0 in
(*			let (q00,qp00,error00) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) recompress_freq_overflow_warn_ref 1 in*)
			(* Test the async quantizers *)
(*
				if q00 <> q00_test then (
				Printf.printf "ERROR: q's don't match up on frame %d:\n" f.f1_num;
				for i = 0 to Array.length q00 - 1 do
					Printf.printf "%8d%8d\n" q00.(i) q00_test.(i)
				done
			);
*)
			acc_ref decode_quant_ticks_ref;
			(* Now try to rehuff *)
(*			let gc00 = rehuff_granule debug qp00 side_info.side_gc.(0).(0) scf_bands_ptr in*)
(*			let get_gc00 = rehuff_granule_async debug qp00 side_info.side_gc.(0).(0) scf_bands_ptr in*)

			if debug then Printf.printf "%sGr0 Ch1:\n%!" tab;
			Ptr.Ref.set_seq s side_info.side_gc.(0).(1).gc_part2_3_offset;
			let scf01 = read_scalefactors_m1 ~debug:debug ~tabs:tab2 [| false;false;false;false |]  None        side_info.side_gc.(0).(1) s in
			acc_ref decode_scf_ticks_ref;
			let get_decode_01 = (*(q01, qp01, error01, gc01) = sync_decode @@*) decode_granule_async debug k data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(1).gc_part2_3_length + side_info.side_gc.(0).(1).gc_part2_3_offset) side_info.side_gc.(0).(1) scf_bands_ptr process_function 1 in
(*			let (q01,qp01,error01) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(1) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(1).gc_part2_3_length + side_info.side_gc.(0).(1).gc_part2_3_offset) recompress_freq_overflow_warn_ref in*)
			acc_ref decode_quant_ticks_ref;
(*			let gc01 = rehuff_granule debug qp01 side_info.side_gc.(0).(1) scf_bands_ptr in*)
(*			let get_gc01 = rehuff_granule_async debug qp01 side_info.side_gc.(0).(1) scf_bands_ptr in*)

			if debug then Printf.printf "%sGr1 Ch0:\n%!" tab;
			Ptr.Ref.set_seq s side_info.side_gc.(1).(0).gc_part2_3_offset;
			let scf10 = read_scalefactors_m1 ~debug:debug ~tabs:tab2 side_info.side_scfi.(0)       (Some scf00) side_info.side_gc.(1).(0) s in
			acc_ref decode_scf_ticks_ref;
			let get_decode_10 = (*(q10, qp10, error10, gc10) = sync_decode @@*) decode_granule_async debug k data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(1).(0).gc_part2_3_length + side_info.side_gc.(1).(0).gc_part2_3_offset) side_info.side_gc.(1).(0) scf_bands_ptr process_function 2 in
(*			let (q10,qp10,error10) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(1).(0) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(1).(0).gc_part2_3_length + side_info.side_gc.(1).(0).gc_part2_3_offset) recompress_freq_overflow_warn_ref in*)
			acc_ref decode_quant_ticks_ref;
(*			let gc10 = rehuff_granule debug qp10 side_info.side_gc.(1).(0) scf_bands_ptr in*)
(*			let get_gc10 = rehuff_granule_async debug qp10 side_info.side_gc.(1).(0) scf_bands_ptr in*)

			if debug then Printf.printf "%sGr1 Ch1:\n%!" tab;
			Ptr.Ref.set_seq s side_info.side_gc.(1).(1).gc_part2_3_offset;
			let scf11 = read_scalefactors_m1 ~debug:debug ~tabs:tab2 side_info.side_scfi.(1)       (Some scf10) side_info.side_gc.(1).(1) s in
			acc_ref decode_scf_ticks_ref;
			let get_decode_11 = (*(q11, qp11, error11, gc11) = sync_decode @@*) decode_granule_async debug k data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(1).(1).gc_part2_3_length + side_info.side_gc.(1).(1).gc_part2_3_offset) side_info.side_gc.(1).(1) scf_bands_ptr process_function 3 in
(*			let (q11,qp11,error11) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(1).(1) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(1).(1).gc_part2_3_length + side_info.side_gc.(1).(1).gc_part2_3_offset) recompress_freq_overflow_warn_ref in*)
			acc_ref decode_quant_ticks_ref;
(*			let gc11 = rehuff_granule debug qp11 side_info.side_gc.(1).(1) scf_bands_ptr in*)
(*			let get_gc11 = rehuff_granule_async debug qp11 side_info.side_gc.(1).(1) scf_bands_ptr in*)

			let (q00, qp00, error00, gc00) = sync_decode get_decode_00 in
			let (q01, qp01, error01, gc01) = sync_decode get_decode_01 in
			let (q10, qp10, error10, gc10) = sync_decode get_decode_10 in
			let (q11, qp11, error11, gc11) = sync_decode get_decode_11 in

			if debug then Printf.printf "Done decoding\n%!";

			(
				M1_frame_data {
					m1_header = f.f1_header;
					m1_side_info = {side_info with side_gc = [| [| (*Event.sync get_*)gc00; (*Event.sync get_*)gc01 |]; [| (*Event.sync get_*)gc10; (*Event.sync get_*)gc11 |] |]};
					m1_scalefactors = [| [| scf00; scf01 |]; [| scf10; scf11 |] |];
					m1_quantizers = [| [| q00; q01 |]; [| q10; q11 |] |];
					m1_quantizer_ptrs = [| [| qp00; qp01 |]; [| qp10; qp11 |] |];
					m1_starting_f1 = f;
				}
			,
				error00 || error01 || error10 || error11
			)
		)
		| (_, ChannelMono) -> (
			if debug then Printf.printf "DECODE FRAME %d (MPEG2, MONO)\n" f.f1_num;

			if debug then (
				Printf.printf "%sSide %S\n" tab2 (Ptr.Ref.to_HEX f.f1_side.side_raw);
				Printf.printf "%sData %S\n" tab2 (Ptr.Ref.to_HEX f.f1_data);
				print_side ~tabs:tab2 side_info;
			);

			if debug then Printf.printf "%sGr: (USING MPEG1 QUANTIZER FUNCTION)\n" tab;
			let s = Ptr.Ref.new_seq f.f1_data in
			Ptr.Ref.set_seq s side_info.side_gc.(0).(0).gc_part2_3_offset;

			let scf_bands_ptr = global_scalefactors_ptr f.f1_header.header_samplerate false in

			acc_ref before_decode_ticks_ref;
			let scf = read_scalefactors_m2 ~debug:debug ~tabs:tab2 false side_info.side_gc.(0).(0) s in
			acc_ref decode_scf_ticks_ref;
			let (q, qp, error) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) recompress_freq_overflow_warn_ref in
			acc_ref decode_quant_ticks_ref;
			let gc = rehuff_granule debug qp process_function side_info.side_gc.(0).(0) scf_bands_ptr in
			(* There really isn't any need to do this async since it is gathered right after, but it fits with the rest of the function *)
(*			let get_gc = rehuff_granule_async debug qp side_info.side_gc.(0).(0) scf_bands_ptr in*)

			(
				M2_frame_data {
					m2_header = f.f1_header;
					m2_side_info = {side_info with side_gc = [| [| (*Event.sync get_*)gc |] |]};
					m2_scalefactors = [| scf |];
					m2_quantizers = [| q |];
					m2_quantizer_ptrs = [| qp |];
					m2_starting_f1 = f;
				}
			,
				error
			)

(*			(M2_frame_data {m2_header = f.f1_header; m2_side_info = side_info; m2_starting_f1 = f}, false)*)
		)
		| (_, _) -> (
			if debug then Printf.printf "DECODE FRAME %d (MPEG2, STEREO)\n" f.f1_num;

			if debug then (
				Printf.printf "%sSide %S\n" tab2 (Ptr.Ref.to_HEX f.f1_side.side_raw);
				Printf.printf "%sData %S\n" tab2 (Ptr.Ref.to_HEX f.f1_data);
				print_side ~tabs:tab2 side_info;
			);

			(* Remember that the IS should only be set on the right channel; the left channel uses the same scalefactors as non-IS GCs *)
			if debug then Printf.printf "%sGr0: (USING MPEG1 QUANTIZER READER)\n" tab;
(*			let s = {(Ptr.Ref.new_seq f.f1_data) with Ptr.Ref.seq_at = side_info.side_gc.(0).(0).gc_part2_3_offset(*Ptr.Ref.seq_ref = f.f1_data; Ptr.Ref.seq_at = side_info.side_gc.(0).(0).gc_part2_3_offset*)} in*)
			let s = Ptr.Ref.new_seq f.f1_data in
			Ptr.Ref.set_seq s side_info.side_gc.(0).(0).gc_part2_3_offset;

			let scf_bands_ptr = global_scalefactors_ptr f.f1_header.header_samplerate false in

			acc_ref before_decode_ticks_ref;
			let scf0 = read_scalefactors_m2 ~debug:debug ~tabs:tab2         false         side_info.side_gc.(0).(0) s in
			acc_ref decode_scf_ticks_ref;
			let get_decode_0 = decode_granule_async debug k data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) side_info.side_gc.(0).(0) scf_bands_ptr process_function 0 in
(*			let (q0, qp0, error0) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(0) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(0).gc_part2_3_length + side_info.side_gc.(0).(0).gc_part2_3_offset) recompress_freq_overflow_warn_ref in*)
			acc_ref decode_quant_ticks_ref;
(*			let gc0 = rehuff_granule debug qp0 side_info.side_gc.(0).(0) scf_bands_ptr in*)
(*			let get_gc0 = rehuff_granule_async debug qp0 side_info.side_gc.(0).(0) scf_bands_ptr in*)

			if debug then Printf.printf "%sGr1: (USING MPEG1 QUANTIZER READER)\n" tab;
			(*s.Ptr.Ref.seq_at <- side_info.side_gc.(0).(1).gc_part2_3_offset;*)
			Ptr.Ref.set_seq s side_info.side_gc.(0).(1).gc_part2_3_offset;
			let scf1 = read_scalefactors_m2 ~debug:debug ~tabs:tab2 f.f1_header.header_is side_info.side_gc.(0).(1) s in
			acc_ref decode_scf_ticks_ref;
			let get_decode_1 = decode_granule_async debug k data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(1).gc_part2_3_length + side_info.side_gc.(0).(1).gc_part2_3_offset) side_info.side_gc.(0).(1) scf_bands_ptr process_function 1 in
(*			let (q1, qp1, error1) = read_quantizers ~debug:debug ~tabs:tab2 k side_info.side_gc.(0).(1) data_ptr s.Ptr.Ref.seq_at (side_info.side_gc.(0).(1).gc_part2_3_length + side_info.side_gc.(0).(1).gc_part2_3_offset) recompress_freq_overflow_warn_ref in*)
			acc_ref decode_quant_ticks_ref;
(*			let gc1 = rehuff_granule debug qp1 side_info.side_gc.(0).(1) scf_bands_ptr in*)
(*			let get_gc1 = rehuff_granule_async debug qp1 side_info.side_gc.(0).(1) scf_bands_ptr in*)

			let (q0, qp0, error0, gc0) = sync_decode get_decode_0 in
			let (q1, qp1, error1, gc1) = sync_decode get_decode_1 in

			(
				M2_frame_data {
					m2_header = f.f1_header;
					m2_side_info = {side_info with side_gc = [| [| (*Event.sync get_*)gc0; (*Event.sync get_*)gc1 |] |]};
					m2_scalefactors = [| scf0; scf1 |];
					m2_quantizers = [| q0; q1 |];
					m2_quantizer_ptrs = [| qp0; qp1 |];
					m2_starting_f1 = f;
				}
			,
				error0 || error1
			)


(*			(M2_frame_data {m2_header = f.f1_header; m2_side_info = side_info; m2_scalefactors = [||]; m2_quantizers = [||]; m2_starting_f1 = f}, false)*)
		)
	) in

	if debug then Printf.printf "Fully decoded and synced with process threads\n";
(*	decode_granule_channel f side_info*)

	decoded
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

(**********************************************************************************************)
(* ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME *)
(**********************************************************************************************)
let encode_frame ?(debug=false) d =

	let write_granule_m1 k s scfsi gc scf quants = (
		(* Scalefactors *)
		let (scf_bits1, scf_bits2) = scalefactor_compress_m1.(gc.gc_scf_compress_index) in
		let (num1, num2) = (match gc.gc_window with
			| Window_normal x -> (11,10)
			| Window_other x when x.other_block_type <> Block_type_short -> (11,10)
			| Window_other x when x.other_mixed_block -> (17,18)
			| Window_other x -> (18,18)
		) in
		let rec write_scf i = (
			let num_bits = if i < num1 then scf_bits1 else scf_bits2 in
			if i >= Array.length scf then (
				if debug && debug_more then Printf.printf "!";
			) else if i < 6 then (
				if scfsi.(0) then () else (if debug && debug_more then Printf.printf "0"; Ptr.put_seq s num_bits scf.(i));
				write_scf (succ i)
			) else if i < 11 then (
				if scfsi.(1) then () else (if debug && debug_more then Printf.printf "1"; Ptr.put_seq s num_bits scf.(i));
				write_scf (succ i)
			) else if i < 16 then (
				if scfsi.(2) then () else (if debug && debug_more then Printf.printf "2"; Ptr.put_seq s num_bits scf.(i));
				write_scf (succ i)
			) else if i < 21 then (
				if scfsi.(3) then () else (if debug && debug_more then Printf.printf "3"; Ptr.put_seq s num_bits scf.(i));
				write_scf (succ i)
			) else (
				if debug && debug_more then Printf.printf "+"; Ptr.put_seq s num_bits scf.(i);
				write_scf (succ i)
			)
		) in
		write_scf 0;
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
		if debug then Printf.printf "Writing (%d,%d,%d) pairs with tables (%d,%d,%d)\n" region0 region1 region2 table0 table1 table2;
		let rec write_to_frame index left ht_encode linbits = (
			if left <= 0 then index else (
				if debug && debug_more then Printf.printf "Trying to squeeze (%d,%d) from %d to position %d (%d)\n" quants.(index) quants.(index + 1) index s.Ptr.seq_at left;

				let (x, y) = (quants.(index), quants.(index + 1)) in

				let x_base = min (abs x) 15 in
				let y_base = min (abs y) 15 in
				let (x_sign, x_sign_bits) = if x = 0 then (0,0) else if x > 0 then (0,1) else (1,1) in
				let (y_sign, y_sign_bits) = if y = 0 then (0,0) else if y > 0 then (0,1) else (1,1) in
				let (x_lin, x_lin_bits) = if x_base = 15 && linbits > 0 then (abs x - x_base, linbits) else (0,0) in
				let (y_lin, y_lin_bits) = if y_base = 15 && linbits > 0 then (abs y - y_base, linbits) else (0,0) in

				let (bits, value) = ht_encode.((x_base lsl 4) lor (y_base land 15)) in
				if debug && debug_more then Printf.printf " Found Huffman code %d of length %d (%d,%d,%d,%d)\n" value bits x_lin_bits x_sign_bits y_lin_bits y_sign_bits;

				Ptr.put_seq s bits value;
				Ptr.put_seq s x_lin_bits x_lin;
				Ptr.put_seq s x_sign_bits x_sign;
				Ptr.put_seq s y_lin_bits y_lin;
				Ptr.put_seq s y_sign_bits y_sign;

				if debug then Printf.printf " Done writing!\n";

				write_to_frame (index + 2) (left - 1) ht_encode linbits
			)
		) in
		let index = write_to_frame   0   region0 global_ht_encode.(table0) global_ht_linbits.(table0) in
		if debug then Printf.printf "Done with first region\n";
		let index = write_to_frame index region1 global_ht_encode.(table1) global_ht_linbits.(table1) in
		if debug then Printf.printf "Done with second region\n";
		let index = write_to_frame index region2 global_ht_encode.(table2) global_ht_linbits.(table2) in
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

		let rec write_count1_to_frame index go_to ht_encode = (
			if index > go_to then index else (
				if debug && debug_more then Printf.printf "Trying to squeeze (%d,%d,%d,%d) from %d to position %d of %d going to %d\n" quants.(index) quants.(index + 1) quants.(index + 2) quants.(index + 3) index s.Ptr.seq_at (Ptr.length s.Ptr.seq_ptr * 8) go_to;

				let (v,w,x,y) = (*if index = (Array.length quants - 2) then (quants.(index), quants.(index + 1), 0, 0) else*) (quants.(index), quants.(index + 1), quants.(index + 2), quants.(index + 3)) in
(*
				let (v_sign, v_sign_bits) = if v = 0 then (0,0) else if v > 0 then (0,1) else (1,1) in
				let (w_sign, w_sign_bits) = if w = 0 then (0,0) else if w > 0 then (0,1) else (1,1) in
				let (x_sign, x_sign_bits) = if x = 0 then (0,0) else if x > 0 then (0,1) else (1,1) in
				let (y_sign, y_sign_bits) = if y = 0 then (0,0) else if y > 0 then (0,1) else (1,1) in
*)
				let v_sign = (1 - v) lsr 1 in
				let w_sign = (1 - w) lsr 1 in
				let x_sign = (1 - x) lsr 1 in
				let y_sign = (1 - y) lsr 1 in

(*				let (bits, value) = Hashtbl.find ht_encode (abs v, abs w, abs x, abs y) in*)
				let (bits, value) = ht_encode.((abs v) lsl 3 lor (abs w) lsl 2 lor (abs x) lsl 1 lor (abs y)) in
				if debug && debug_more then Printf.printf " Found Huffman code %d of length %d\n" value bits;

				Ptr.put_seq s bits value;
				if v <> 0 then Ptr.put_seq s 1 v_sign;
				if w <> 0 then Ptr.put_seq s 1 w_sign;
				if x <> 0 then Ptr.put_seq s 1 x_sign;
				if y <> 0 then Ptr.put_seq s 1 y_sign;

				write_count1_to_frame (index + 4) go_to ht_encode
			)
		) in
		let go_to = find_last_nonzero_index quants in
		if debug then Printf.printf "Last nonzero quantizer is %d\n" go_to;
		let index = write_count1_to_frame index go_to global_ht_encode_count1.(if gc.gc_count1_table_1 then 1 else 0) in
		if debug then Printf.printf "Wrote count1 to frame\n";
	) in

	let write_granule_m2 k s is gc scf quants = (
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
		
		let rec write_scf i = (
			if i < sob1 then (
				if debug && debug_more then Printf.printf "0"; Ptr.put_seq s bits0 scf.(i);
				write_scf (succ i)
			) else if i < sob2 then (
				if debug && debug_more then Printf.printf "1"; Ptr.put_seq s bits1 scf.(i);
				write_scf (succ i)
			) else if i < sob3 then (
				if debug && debug_more then Printf.printf "2"; Ptr.put_seq s bits2 scf.(i);
				write_scf (succ i)
			) else if i < num_total then (
				if debug && debug_more then Printf.printf "3"; Ptr.put_seq s bits3 scf.(i);
				write_scf (succ i)
			) else (
				if debug && debug_more then Printf.printf "!";
			)
		) in
		write_scf 0;
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

		let rec write_to_frame index left ht_encode linbits = (
			if left <= 0 then index else (
				if debug && debug_more then Printf.printf "Trying to squeeze (%d,%d) from %d to position %d (%d)\n" quants.(index) quants.(index + 1) index s.Ptr.seq_at left;

				let (x, y) = (quants.(index), quants.(index + 1)) in

				let x_base = min (abs x) 15 in
				let y_base = min (abs y) 15 in
				let (x_sign, x_sign_bits) = if x = 0 then (0,0) else if x > 0 then (0,1) else (1,1) in
				let (y_sign, y_sign_bits) = if y = 0 then (0,0) else if y > 0 then (0,1) else (1,1) in
				let (x_lin, x_lin_bits) = if x_base = 15 && linbits > 0 then (abs x - x_base, linbits) else (0,0) in
				let (y_lin, y_lin_bits) = if y_base = 15 && linbits > 0 then (abs y - y_base, linbits) else (0,0) in

				let (bits, value) = ht_encode.((x_base lsl 4) lor (y_base land 15)) in
				if debug && debug_more then Printf.printf " Found Huffman code %d of length %d (%d,%d,%d,%d)\n" value bits x_lin_bits x_sign_bits y_lin_bits y_sign_bits;

				Ptr.put_seq s bits value;
				Ptr.put_seq s x_lin_bits x_lin;
				Ptr.put_seq s x_sign_bits x_sign;
				Ptr.put_seq s y_lin_bits y_lin;
				Ptr.put_seq s y_sign_bits y_sign;

				if debug then Printf.printf " Done writing!\n";

				write_to_frame (index + 2) (left - 1) ht_encode linbits
			)
		) in
		let index = write_to_frame   0   region0 global_ht_encode.(table0) global_ht_linbits.(table0) in
		if debug then Printf.printf "Done with first region\n";
		let index = write_to_frame index region1 global_ht_encode.(table1) global_ht_linbits.(table1) in
		if debug then Printf.printf "Done with second region\n";
		let index = write_to_frame index region2 global_ht_encode.(table2) global_ht_linbits.(table2) in
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

		let rec write_count1_to_frame index go_to ht_encode = (
			if index > go_to then index else (
				if debug && debug_more then Printf.printf "Trying to squeeze (%d,%d,%d,%d) from %d to position %d of %d going to %d\n" quants.(index) quants.(index + 1) quants.(index + 2) quants.(index + 3) index s.Ptr.seq_at (Ptr.length s.Ptr.seq_ptr * 8) go_to;

				let (v,w,x,y) = (*if index = (Array.length quants - 2) then (quants.(index), quants.(index + 1), 0, 0) else*) (quants.(index), quants.(index + 1), quants.(index + 2), quants.(index + 3)) in
				let (v_sign, v_sign_bits) = if v = 0 then (0,0) else if v > 0 then (0,1) else (1,1) in
				let (w_sign, w_sign_bits) = if w = 0 then (0,0) else if w > 0 then (0,1) else (1,1) in
				let (x_sign, x_sign_bits) = if x = 0 then (0,0) else if x > 0 then (0,1) else (1,1) in
				let (y_sign, y_sign_bits) = if y = 0 then (0,0) else if y > 0 then (0,1) else (1,1) in

				let (bits, value) = ht_encode.((abs v) lsl 3 lor (abs w) lsl 2 lor (abs x) lsl 1 lor (abs y)) in
				if debug && debug_more then Printf.printf " Found Huffman code %d of length %d\n" value bits;

				Ptr.put_seq s bits value;
				Ptr.put_seq s v_sign_bits v_sign;
				Ptr.put_seq s w_sign_bits w_sign;
				Ptr.put_seq s x_sign_bits x_sign;
				Ptr.put_seq s y_sign_bits y_sign;

				write_count1_to_frame (index + 4) go_to ht_encode
			)
		) in
		let go_to = find_last_nonzero_index quants in
		if debug then Printf.printf "Last nonzero quantizer is %d\n" go_to;
		let index = write_count1_to_frame index go_to global_ht_encode_count1.(if gc.gc_count1_table_1 then 1 else 0) in
		if debug then Printf.printf "Wrote count1 to frame\n";
	) in




	match d with
	| M1_frame_data m when m.m1_header.header_channel_mode = ChannelMono -> (
		(* MONO MPEG1 *)
		let k = m.m1_header in

		(*************)
		(* Parts 2,3 *)
		(*************)
		let out_ptr = Ptr.clearret (Ptr.make 2881 0) in
		let s = Ptr.new_seq out_ptr in

		let r0 = s.Ptr.seq_at in
		if debug then Printf.printf "Doing first granule at %d (had better be 0)\n" r0;
		write_granule_m1 k s [| false;false;false;false |] m.m1_side_info.side_gc.(0).(0) m.m1_scalefactors.(0).(0) m.m1_quantizers.(0).(0);
		let r1 = s.Ptr.seq_at in
		if debug then Printf.printf "First granule done; starting second at %d\n" r1;
		write_granule_m1 k s m.m1_side_info.side_scfi.(0)  m.m1_side_info.side_gc.(1).(0) m.m1_scalefactors.(1).(0) m.m1_quantizers.(1).(0);
		let r2 = s.Ptr.seq_at in
		if debug then Printf.printf "Second granule done at %d\n" r2;

		let (side_raw_ptr, side_bits, side_bytes) =
			let p = Ptr.make 17 0 in
			let s = m.m1_side_info in
			let pb = Ptr.put_bits p in
			pb 0 9 s.side_main_data_begin;
			pb 9 5 0;
			pb 14 1 (if s.side_scfi.(0).(0) then 1 else 0);
			pb 15 1 (if s.side_scfi.(0).(1) then 1 else 0);
			pb 16 1 (if s.side_scfi.(0).(2) then 1 else 0);
			pb 17 1 (if s.side_scfi.(0).(3) then 1 else 0);
			let pack_gc gc bits o = (
				pb (o +  0) 12 bits;
				pb (o + 12)  9 gc.gc_big_values;
				pb (o + 21)  8 gc.gc_global_gain;
				pb (o + 29)  4 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						pb (o + 33) 1 0;
						pb (o + 34) 5 w.normal_table_select1;
						pb (o + 39) 5 w.normal_table_select2;
						pb (o + 44) 5 w.normal_table_select3;
						pb (o + 49) 4 w.normal_region_0_count;
						pb (o + 53) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						pb (o + 33) 1 1;
						pb (o + 34) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						pb (o + 36) 1 (if w.other_mixed_block then 1 else 0);
						pb (o + 37) 5 w.other_table_select1;
						pb (o + 42) 5 w.other_table_select2;
						pb (o + 47) 3 w.other_sub_block_gain1;
						pb (o + 50) 3 w.other_sub_block_gain2;
						pb (o + 53) 3 w.other_sub_block_gain3;
					)
				);
				pb (o + 56) 1 (if gc.gc_pre_flag then 1 else 0);
				pb (o + 57) 1 gc.gc_sf_scale;
				pb (o + 58) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc s.side_gc.(0).(0) (r1 - r0) 18;
			pack_gc s.side_gc.(1).(0) (r2 - r1) 77;
			(p, [| r1 - r0; r2 - r1 |], (r2 - r0 + 7) asr 3)
		in

		if debug then Printf.printf "Done packing the side info\n";

		let (data_ptr, data_bits) = Ptr.finalize_seq s in

		{
			f1_num = m.m1_starting_f1.f1_num;
			f1_header = m.m1_starting_f1.f1_header;
			f1_side = {
				side_raw = Ptr.Ref.of_ptr side_raw_ptr;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = Ptr.Ref.of_subptr data_ptr 0 ((data_bits + 7) lsr 3);
			f1_pad_exact = None;
		}
	)
	| M1_frame_data m -> (
		(* STEREO MPEG1 *)
		let k = m.m1_header in

		(*************)
		(* Parts 2,3 *)
		(*************)
		let out_ptr = Ptr.clearret (Ptr.make 2881 0) in
		let s = Ptr.new_seq out_ptr in

		let r0 = s.Ptr.seq_at in (* had better be 0 *)
		if debug then Printf.printf "Doing first granule at %d (had better be 0)\n" r0;
		write_granule_m1 k s [| false;false;false;false |] m.m1_side_info.side_gc.(0).(0) m.m1_scalefactors.(0).(0) m.m1_quantizers.(0).(0);
		let r1 = s.Ptr.seq_at in
		if debug then Printf.printf "First granule done; starting second at %d\n" r1;
		write_granule_m1 k s [| false;false;false;false |] m.m1_side_info.side_gc.(0).(1) m.m1_scalefactors.(0).(1) m.m1_quantizers.(0).(1);
		let r2 = s.Ptr.seq_at in
		if debug then Printf.printf "Second granule done; starting third at %d\n" r2;
		write_granule_m1 k s m.m1_side_info.side_scfi.(0)  m.m1_side_info.side_gc.(1).(0) m.m1_scalefactors.(1).(0) m.m1_quantizers.(1).(0);
		let r3 = s.Ptr.seq_at in
		if debug then Printf.printf "Third granule done; starting fourth at %d\n" r3;
		write_granule_m1 k s m.m1_side_info.side_scfi.(1)  m.m1_side_info.side_gc.(1).(1) m.m1_scalefactors.(1).(1) m.m1_quantizers.(1).(1);
		let r4 = s.Ptr.seq_at in
		if debug then Printf.printf "Fourth granule done at %d\n" r4;

		(************************)
		(* Encode the side info *)
		(************************)
		let (side_raw_ptr, side_bits, side_bytes) = (
			let p = Ptr.make 32 0 in
			let s = m.m1_side_info in
			let pb = Ptr.put_bits p in
			pb  0 9 s.side_main_data_begin;
			pb  9 3 0;
			pb 12 1 (if s.side_scfi.(0).(0) then 1 else 0);
			pb 13 1 (if s.side_scfi.(0).(1) then 1 else 0);
			pb 14 1 (if s.side_scfi.(0).(2) then 1 else 0);
			pb 15 1 (if s.side_scfi.(0).(3) then 1 else 0);
			pb 16 1 (if s.side_scfi.(1).(0) then 1 else 0);
			pb 17 1 (if s.side_scfi.(1).(1) then 1 else 0);
			pb 18 1 (if s.side_scfi.(1).(2) then 1 else 0);
			pb 19 1 (if s.side_scfi.(1).(3) then 1 else 0);
			let pack_gc gc bits o = (
				pb (o +  0) 12 bits;
				pb (o + 12)  9 gc.gc_big_values;
				pb (o + 21)  8 gc.gc_global_gain;
				pb (o + 29)  4 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						pb (o + 33) 1 0;
						pb (o + 34) 5 w.normal_table_select1;
						pb (o + 39) 5 w.normal_table_select2;
						pb (o + 44) 5 w.normal_table_select3;
						pb (o + 49) 4 w.normal_region_0_count;
						pb (o + 53) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						pb (o + 33) 1 1;
						pb (o + 34) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						pb (o + 36) 1 (if w.other_mixed_block then 1 else 0);
						pb (o + 37) 5 w.other_table_select1;
						pb (o + 42) 5 w.other_table_select2;
						pb (o + 47) 3 w.other_sub_block_gain1;
						pb (o + 50) 3 w.other_sub_block_gain2;
						pb (o + 53) 3 w.other_sub_block_gain3;
					)
				);
				pb (o + 56) 1 (if gc.gc_pre_flag then 1 else 0);
				pb (o + 57) 1 gc.gc_sf_scale;
				pb (o + 58) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc s.side_gc.(0).(0) (r1 - r0)  20;
			pack_gc s.side_gc.(0).(1) (r2 - r1)  79;
			pack_gc s.side_gc.(1).(0) (r3 - r2) 138;
			pack_gc s.side_gc.(1).(1) (r4 - r3) 197;
			(p, [| r1 - r0; r2 - r1; r3 - r2; r4 - r3 |], (r4 - r0 + 7) asr 3)
		) in

		if debug then Printf.printf "Done packing the side info\n";

		let (data_ptr, data_bits) = Ptr.finalize_seq s in

		{
			f1_num = m.m1_starting_f1.f1_num;
			f1_header = m.m1_starting_f1.f1_header;
			f1_side = {
				side_raw = Ptr.Ref.of_ptr side_raw_ptr;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = Ptr.Ref.of_subptr data_ptr 0 ((data_bits + 7) lsr 3);
			f1_pad_exact = None;
		}
	)
	| M2_frame_data m when m.m2_header.header_channel_mode = ChannelMono -> (
		
		let k = m.m2_header in

		let out_ptr = Ptr.clearret (Ptr.make 5761 0) in
		let s = Ptr.new_seq out_ptr in

		let r0 = s.Ptr.seq_at in
		if debug then Printf.printf "Doing first (and only) granule at %d (had better be 0)\n" r0;
		write_granule_m2 k s false m.m2_side_info.side_gc.(0).(0) m.m2_scalefactors.(0) m.m2_quantizers.(0);
		let r1 = s.Ptr.seq_at in
		if debug then Printf.printf "Granule done at %d\n" r1;

		let (side_raw_ptr, side_bits, side_bytes) = (
			let p = Ptr.make 9 0 in
			let s = m.m2_side_info in
			let pb = Ptr.put_bits p in
			pb  0 8 s.side_main_data_begin;
			pb  8 2 0;
			let pack_gc gc bits o = (
				pb (o +  0) 12 bits;
				pb (o + 12)  9 gc.gc_big_values;
				pb (o + 21)  8 gc.gc_global_gain;
				pb (o + 29)  9 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						pb (o + 38) 1 0;
						pb (o + 39) 5 w.normal_table_select1;
						pb (o + 44) 5 w.normal_table_select2;
						pb (o + 49) 5 w.normal_table_select3;
						pb (o + 54) 4 w.normal_region_0_count;
						pb (o + 58) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						pb (o + 38) 1 1;
						pb (o + 39) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						pb (o + 41) 1 (if w.other_mixed_block then 1 else 0);
						pb (o + 42) 5 w.other_table_select1;
						pb (o + 47) 5 w.other_table_select2;
						pb (o + 52) 3 w.other_sub_block_gain1;
						pb (o + 55) 3 w.other_sub_block_gain2;
						pb (o + 58) 3 w.other_sub_block_gain3;
					)
				);
				pb (o + 61) 1 gc.gc_sf_scale;
				pb (o + 62) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc s.side_gc.(0).(0) (r1 - r0) 9;
			(p, [| r1 - r0 |], (r1 - r0 + 7) asr 3)
		) in
		
		if debug then Printf.printf "Done packing the side info\n";

		let (data_ptr, data_bits) = Ptr.finalize_seq s in

		{
			f1_num = m.m2_starting_f1.f1_num;
			f1_header = m.m2_starting_f1.f1_header;
			f1_side = {
				side_raw = Ptr.Ref.of_ptr side_raw_ptr;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = Ptr.Ref.of_subptr data_ptr 0 ((data_bits + 7) lsr 3);
			f1_pad_exact = None;
		}
	)
	| M2_frame_data m -> (
		let k = m.m2_header in

		let out_ptr = Ptr.clearret (Ptr.make 5761 0) in
		let s = Ptr.new_seq out_ptr in

		let r0 = s.Ptr.seq_at in
		if debug then Printf.printf "Doing first granule at %d (had better be 0)\n" r0;
		write_granule_m2 k s         false         m.m2_side_info.side_gc.(0).(0) m.m2_scalefactors.(0) m.m2_quantizers.(0);
		let r1 = s.Ptr.seq_at in
		if debug then Printf.printf "First granule done; starting second at %d\n" r1;
		write_granule_m2 k s m.m2_header.header_is m.m2_side_info.side_gc.(0).(1) m.m2_scalefactors.(1) m.m2_quantizers.(1);
		let r2 = s.Ptr.seq_at in
		if debug then Printf.printf "Second granule done at %d\n" r2;

		let (side_raw_ptr, side_bits, side_bytes) = (
			let p = Ptr.make 17 0 in
			let s = m.m2_side_info in
			let pb = Ptr.put_bits p in
			pb  0 8 s.side_main_data_begin;
			pb  8 3 0;
			let pack_gc gc bits o = (
				pb (o +  0) 12 bits;
				pb (o + 12)  9 gc.gc_big_values;
				pb (o + 21)  8 gc.gc_global_gain;
				pb (o + 29)  9 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						pb (o + 38) 1 0;
						pb (o + 39) 5 w.normal_table_select1;
						pb (o + 44) 5 w.normal_table_select2;
						pb (o + 49) 5 w.normal_table_select3;
						pb (o + 54) 4 w.normal_region_0_count;
						pb (o + 58) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						pb (o + 38) 1 1;
						pb (o + 39) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						pb (o + 41) 1 (if w.other_mixed_block then 1 else 0);
						pb (o + 42) 5 w.other_table_select1;
						pb (o + 47) 5 w.other_table_select2;
						pb (o + 52) 3 w.other_sub_block_gain1;
						pb (o + 55) 3 w.other_sub_block_gain2;
						pb (o + 58) 3 w.other_sub_block_gain3;
					)
				);
				pb (o + 61) 1 gc.gc_sf_scale;
				pb (o + 62) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc s.side_gc.(0).(0) (r1 - r0) 10;
			pack_gc s.side_gc.(0).(1) (r2 - r1) 73;
			(p, [| r1 - r0; r2 - r1 |], (r2 - r0 + 7) asr 3)
			
		) in
		
		if debug then Printf.printf "Done packing the side info\n";

		let (data_ptr, data_bits) = Ptr.finalize_seq s in

		{
			f1_num = m.m2_starting_f1.f1_num;
			f1_header = m.m2_starting_f1.f1_header;
			f1_side = {
				side_raw = Ptr.Ref.of_ptr side_raw_ptr;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = Ptr.Ref.of_subptr data_ptr 0 ((data_bits + 7) lsr 3);
			f1_pad_exact = None;
		}
	)
;;

let recompress_frame ?(debug=false) process_set f recompress_freq_overflow_warn_ref =

	if !first_tick_ref = 0 then (
		let a = counter () in
		first_tick_ref := a;
		last_tick_ref := a;
	) else (
		acc_ref outside_ticks_ref
	);

	let process_function = match process_set with
		| SSE41 -> find_best_config_sse41
		| Set_base -> find_best_config
	in

(*
	Printf.printf "%d\n" f.f1_num;
	Printf.printf " Input frame:  \"%s\"\n" (to_hex f.f1_string);
*)
(*	update_ref ();*)
	let (decoded, decoder_error) = decode_frame ~debug:debug f process_function recompress_freq_overflow_warn_ref in
	acc_ref decode_ticks_ref;

	if debug then Printf.printf "Decoder error? %B\n" decoder_error;

	let rehuffed = (*if false then rehuff_frame ~debug:debug decoded else*) decoded in

	acc_ref after_c_ticks_ref;

	let encoded = encode_frame ~debug:debug rehuffed in
	acc_ref encode_ticks_ref;

	(* I'm pretty sure I was trying to return the input frame in case of an error... *)
	let return_frame = if decoder_error then f else encoded in

(*
	Printf.printf " Output frame: \"%s\"\n" (to_hex encoded.f1_string);
*)
	if false then (
(*		let total = !before_c_ticks_ref + !c_ticks_ref + !after_c_ref + !everything_else_ticks_ref in*)
		(* This will remove the printing time from the calculation of the total time *)
		let total = get_total () - !print_stuff_ticks_ref in
		let to_percent x y = int_of_float ((float_of_int x *. 100.0) /. (float_of_int y)) in
		Printf.printf "B4 decode:  %d (%d%%)\n" !before_decode_ticks_ref (to_percent !before_decode_ticks_ref total);
		Printf.printf "Dec SCF:    %d (%d%%)\n" !decode_scf_ticks_ref (to_percent !decode_scf_ticks_ref total);
		Printf.printf "Before decq:%d (%d%%)\n" !before_decode_quant_ticks_ref (to_percent !before_decode_quant_ticks_ref total);
		Printf.printf "Dec big:    %d (%d%%)\n" !decode_big_quant_ticks_ref (to_percent !decode_big_quant_ticks_ref total);
		Printf.printf "Dec part1:  %d (%d%%)\n" !decode_part1_quant_ticks_ref (to_percent !decode_part1_quant_ticks_ref total);
		Printf.printf "After quant:%d (%d%%)\n" !decode_quant_ticks_ref (to_percent !decode_quant_ticks_ref total);
		Printf.printf "After dec:  %d (%d%%)\n" !decode_ticks_ref (to_percent !decode_ticks_ref total);
		Printf.printf "Before C:   %d (%d%%)\n" !before_c_ticks_ref (to_percent !before_c_ticks_ref total);
		Printf.printf "In C:       %d (%d%%)\n" !in_c_ticks_ref (to_percent !in_c_ticks_ref total);
		Printf.printf "After C:    %d (%d%%)\n" !after_c_ticks_ref (to_percent !after_c_ticks_ref total);
		Printf.printf "Encode:     %d (%d%%)\n" !encode_ticks_ref (to_percent !encode_ticks_ref total);
(*		Printf.printf "Print stuff:%d (%d%%)\n" !print_stuff_ticks_ref (to_percent !print_stuff_ticks_ref total);*)
		Printf.printf "Outside:    %d (%d%%)\n" !outside_ticks_ref (to_percent !outside_ticks_ref total);
		Printf.printf "Total:      %d\n" total;

		Printf.printf "\n";
	);

	acc_ref print_stuff_ticks_ref;
(*	update_ref ();*)

	(return_frame, decoder_error)
;;
