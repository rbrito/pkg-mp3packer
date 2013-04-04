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

(* Unicode argv *)
let argv = match Unicode.argv_opt with
	| Unicode.Normal u -> u
	| Unicode.Error _ -> Sys.argv
;;

type ('a,'b) error_t = Normal of 'a | Error of 'b;;

let (@@) a b = a b;;

let trap_exception a b =
	try
		Normal (a b)
	with
		e -> Error e
;;
let trap_exception_2 a b c =
	try
		Normal (a b c)
	with
		e -> Error e
;;


let to_hex s =
  let result = String.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    String.blit (Printf.sprintf "%02X" (int_of_char s.[i])) 0 result (2*i) 2;
  done;
  result;;

let to_bin =
	let lookup = [| 128;64;32;16;8;4;2;1 |] in
	fun s -> (
		let result = String.create (8 * String.length s) in
		for chr = 0 to String.length s - 1 do
			let code = Char.code s.[chr] in
			for bit = 0 to 7 do
				result.[(chr lsl 3) lor bit] <- if (code land lookup.(bit) = 0) then '0' else '1'
			done
		done;
		result
	)
;;


(* File name functions *)
let strip_multiple_slashes =
	let regexp = Str.regexp "[/\\\\]+" in
	fun str -> Str.global_replace regexp "/" str
;;
let strip_trailing_slash str =
	let str2 = strip_multiple_slashes str in
	if String.length str2 = 0 then (
		str2
	) else if str2.[String.length str2 - 1] = '/' then (
		String.sub str2 0 (String.length str2 - 1)
	) else (
		str2
	)
;;
let append_before_extension str app =
	(try
		let dot = String.rindex str '.' in
		let before = String.sub str 0 dot in
		let after = String.sub str dot (String.length str - dot) in
		before ^ app ^ after
	with
		Not_found -> str ^ app
	)
;;

(* Other file functions *)
type io_t = IO_File of string | IO_Dir of string;;
let string_of_io = function
	| IO_File x -> Printf.sprintf "File '%s'" x
	| IO_Dir x -> Printf.sprintf "Dir '%s'" x
;;
(*
let find_file x = (try
	Some (Unix.stat x).Unix.st_kind
with
	| Unix.Unix_error _ -> None
);;
*)
let find_file x = (try
	Some (Unicode.stat_utf8 x).Unix.st_kind
with
	| _ -> None
);;




(* Used for printing debug stuff *)
let nul = open_out (if Sys.os_type = "Win32" then "NUL" else "/dev/null");;

exception Loop_end;;

type 'a option3_t = Zero | One of 'a | Many of 'a option3_t array;;
type 'a onemany_t = Single of 'a | Multiple of 'a onemany_t array;;

let rec print_option3_array ?(tab="") option3_array stringify =
	Array.iteri (fun i x ->
		Printf.printf "%s%3d " tab i;
		match x with
		| Zero -> Printf.printf "NONE\n"
		| One y -> Printf.printf "ONE %S\n" (stringify y)
		| Many y -> (Printf.printf "MANY:\n"; print_option3_array ~tab:(tab ^ "  ") y stringify)
	) option3_array
;;


type lameTag_t = {
	lameRevision : int;
	lameVBRMethod : int;
	lameLowpass : int;
	lamePeakAmplitude : float;
	lameRGTrack : int;
	lameRGAlbum : int;
	lameNSPsyTune : bool;
	lameNSSafeJoint : bool;
	lameNoGapPrev : bool;
	lameNoGapNext : bool;
	lameATHType : int;
	lameABRBitrate : int;
	lameDelayStart : int;
	lameDelayEnd : int;
	lameNoiseShaping : int;
	lameStereoMode : int;
	lameUnwise : bool;
	lameSourceFrequency : int;
	lameMP3Gain : int;
	lameSurround : int;
	lamePreset : int;
	lameMusicLength : int;
	lameMusicCRC : int
};;
(*type isLame_t = IsLame of lameTag_t | IsNotLame;;*)
type xingTag_t = {
	xingRawTag : string;
	xingTagType : string;
	xingNumFrames : int option; (* No fair having MP3 files longer than 324 days *)
	xingNumBytes : int option; (* No fair having MP3 files larger than 1GB *)
	xingTOC : int array option;
	xingQuality : int option;
	xingEncoder : string;
	xingLame : lameTag_t option;
};;

type sideInfo_t = {
	sideRaw : string; (* The exact value of the side info *)
	sideDataOffset : int; (* How much of the bit reservoir is used *)
	sideDataBits : int; (* How many bits are in this frame *)
};;
(*
type frameContents_t = {
	frameSide : sideInfo_t;
	frameData : string;
};;
*)
(*type frameType_t = FrameXing of xingTag_t | FrameNormal of frameContents_t;;*)

type channel_t = ChannelStereo | ChannelJoint | ChannelDual | ChannelMono;;
let channel_index = [| ChannelStereo ; ChannelJoint ; ChannelDual ; ChannelMono |];;
let string_of_channel = function
| ChannelStereo -> "ChannelStereo"
| ChannelJoint  -> "ChannelJoint"
| ChannelDual   -> "ChannelDual"
| ChannelMono   -> "ChannelMono"
;;

type mpeg_t = MPEG1 | MPEG2 | MPEG25;;
let mpeg_index = [| MPEG25 ; MPEG25 ; MPEG2 ; MPEG1 |];;
let string_of_mpeg = function
| MPEG1  -> "MPEG1"
| MPEG2  -> "MPEG2"
| MPEG25 -> "MPEG25"
;;

(*type layer_t = Layer1 | Layer2 | Layer3;;*) (* ONLY LAYER 3 IS VALID!!1 *)

type emphasis_t = EmphasisNone | Emphasis5015 | EmphasisInvalid | EmphasisCCITT;;
let emphasis_index = [| EmphasisNone ; Emphasis5015 ; EmphasisInvalid ; EmphasisCCITT |];;
let string_of_emphasis = function
| EmphasisNone    -> "EmphasisNone"
| Emphasis5015    -> "Emphasis5015"
| EmphasisInvalid -> "EmphasisInvalid"
| EmphasisCCITT   -> "EmphasisCCITT"
;;

type samplerate_t = S48000 | S44100 | S32000 | S24000 | S22050 | S16000 | S12000 | S11025 | S8000;;
let int_of_samplerate = function
| S48000 -> 48000
| S44100 -> 44100
| S32000 -> 32000
| S24000 -> 24000
| S22050 -> 22050
| S16000 -> 16000
| S12000 -> 12000
| S11025 -> 11025
|  S8000 ->  8000
;;
let float_of_samplerate = function
| S48000 -> 48000.
| S44100 -> 44100.
| S32000 -> 32000.
| S24000 -> 24000.
| S22050 -> 22050.
| S16000 -> 16000.
| S12000 -> 12000.
| S11025 -> 11025.
|  S8000 ->  8000.
;;

(*
type common_t = { (* Store the things which are common to all the frames in a file *)
	common_id : mpeg_t;
	common_bitrate_index : int array;
	common_samplerate_index : samplerate_t array;
	common_samplerate : samplerate_t;
(*	mutable common_crc : bool;*)
	common_channel_mode : channel_t;
	common_copyright : bool;
	common_original : bool;
	common_emphasis : emphasis_t;
	common_samples_per_frame : int;
	common_side_info_size : int;
	common_bspfk : float; (* Byte seconds per frame kilobit (surprisingly useful) *)
	common_unpadded_frame_length : int -> int; (* Takes a bitrate and returns the total number of bytes in it *)
};;
type frame_t = {
	frameHeader : header_t;
	frameXing : xingTag_t option;
	frameSide : sideInfo_t;
(*	frameSilent : bool;*)
	frameData : string;
};;
*)

(**********************************)
(* NEW FOR MP3READ AS OF 20070326 *)
(**********************************)
let unpadded_frame_length samplerate bitrate =
	if int_of_samplerate samplerate < 32000 then (
		 72000 * bitrate / int_of_samplerate samplerate
	) else (
		144000 * bitrate / int_of_samplerate samplerate
	)
;;

type bitrate_t = {
	bitrate_data : int;
	bitrate_size : int;
	bitrate_num : int;
	bitrate_padding : bool;
	bitrate_index : int;
};;

type header_t = {
(*	header_raw_string   : string;*)
	header_raw          : Ptr.Ref.ref_t;
	header_id           : mpeg_t;
	header_crc          : bool;
	header_bitrate      : int;
	header_samplerate   : samplerate_t;
	header_padding      : bool;
	header_private      : bool;
	header_channel_mode : channel_t;
	header_ms           : bool;
	header_is           : bool;
	header_copyright    : bool;
	header_original     : bool;
	header_emphasis     : emphasis_t;
};;
let frame_length_of_header h = match h.header_id with
	| MPEG1 -> 144000 * h.header_bitrate / int_of_samplerate h.header_samplerate + (if h.header_padding then 1 else 0)
	|   _   ->  72000 * h.header_bitrate / int_of_samplerate h.header_samplerate + (if h.header_padding then 1 else 0)
;;
(* Hooray for byte seconds per frame kilobit! *)
let bspfk_of_samplerate = function
	| S48000 -> 3.
	| S44100 -> 160. /. 49.
	| S32000 -> 9. /. 2.
	| S24000 -> 3.
	| S22050 -> 160. /. 49.
	| S16000 -> 9. /. 2.
	| S12000 -> 6.
	| S11025 -> 320. /. 49.
	| S8000  -> 9.
;;
let bitrate_index_of_id = function
	| MPEG1 -> [| 0;32;40;48;56;64;80;96;112;128;160;192;224;256;320 |]
	|   _   -> [| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |]
;;

type input_frame_t = {
	if_header : header_t;
(*	if_side_string : string;*)
	if_side_raw : Ptr.Ref.ref_t;
(*	if_data_string : string;*)
	if_data_raw : Ptr.Ref.ref_t;
	if_frame_string : string; (* Includes the entire frame (WITH header) *)
	mutable if_xing : xingTag_t option; (* This is mutable so that read_next_frame can change it *)
}

(*
let data_offset_of_if f = match f.if_header.header_id with
	| MPEG1 -> (Char.code f.if_side_string.[0] lsl 1) lor (Char.code f.if_side_string.[1] lsr 7)
	|   _   -> Char.code f.if_side_string.[0]
;;
*)
let data_offset_of_if f = match f.if_header.header_id with
	| MPEG1 -> Ptr.Ref.get_bits f.if_side_raw 0 9
	|   _   -> Ptr.Ref.get_bits f.if_side_raw 0 8
;;

(* New for mp3read: this lets the reader function find out what is needed *)
type 'a req_t = Req_any | Req_equal | Req_matches of 'a list;;
type reqs_t = {
	req_id           : mpeg_t req_t;
	req_crc          : bool req_t;
	req_bitrate      : int req_t;
	req_samplerate   : samplerate_t req_t;
	req_padding      : bool req_t;
	req_private      : bool req_t;
	req_channel_mode : channel_t req_t;
	req_ms           : bool req_t;
	req_is           : bool req_t;
	req_copyright    : bool req_t;
	req_original     : bool req_t;
	req_emphasis     : emphasis_t req_t;
};;

type if_perhaps_t = Fp_eof | Fp_none | Fp_some of input_frame_t;;

type side_t = {
(*	side_raw_string : string;*)
	side_raw : Ptr.Ref.ref_t;
	side_offset : int;
	side_bits : int array;
	side_bytes : int;
};;

type f1_t = {
	f1_num : int;
	f1_header : header_t;
	f1_side : side_t;
(*	f1_string : string;*)
	mutable f1_data : Ptr.Ref.ref_t;
	mutable f1_pad_exact : int option;
};;

type f2_t = {
	f2_num : int;
	f2_header : header_t;
	f2_side : side_t;
	f2_bitrate : bitrate_t;
(*	f2_string : string; (*f2_data*)*)
	f2_data : Ptr.Ref.ref_t;
	f2_pad : int;
	mutable f2_offset : int;
	mutable f2_bytes_left : int;
	mutable f2_flag : bool;
	mutable f2_check_output : bool; (* if the frame may have a gap before it (or before any previous frame, if needed) *)
};;

type f3_t = {
	f3_num : int;
	f3_header_side_raw : Ptr.Ref.ref_t;
(*	f3_output_string : string;*)
	mutable f3_output_data : Ptr.Ref.ref_t;
	f3_bitrate : bitrate_t;
	mutable f3_flag : bool;
};;

(* !NEW 20070326 *)


type process_set_t = SSE41 | Set_base;;


(* NEW FOR THE WORKERS *)
type queue_input_t = {
	q_silent : bool;
	q_debug_in : bool;
	q_debug_queue : bool;
	q_debug_recompress : bool;
	q_min_bitrate : int;
	q_delete_beginning_junk : bool;
	q_delete_end_junk : bool;
	q_padding : string;
	q_recompress : bool;
	q_process_set : process_set_t;
	q_zero_whole_bad_frame : bool;
	q_minimize_bit_reservoir : bool;
};;


type worker_file_t = {
	worker_file_index : int;
	worker_file_input : string;
	worker_file_output: string;
};;
type worker_do_t = 
	| Worker_queue of queue_input_t
	| Worker_do of worker_file_t
	| Worker_finish
;;

type worker_ok_file_t = {
	worker_ok_index : int;
	worker_output_result : (int * int * int);
};;

(* Exceptions are no good to transfer between processes, so make a more concrete type *)
type worker_fail_exception_t =
	| Worker_fail_end_of_file
	| Worker_fail_invalid_argument of string
	| Worker_fail_failure of string
	| Worker_fail_not_found
	| Worker_fail_out_of_memory
	| Worker_fail_stack_overflow
	| Worker_fail_sys_error of string
	| Worker_fail_division_by_zero
	| Worker_fail_other of string (* Should be the result of Printexc.to_string e *)
;;
let string_of_worker_exn = function
	| Worker_fail_end_of_file -> Printexc.to_string End_of_file
	| Worker_fail_invalid_argument s -> Printexc.to_string @@ Invalid_argument s
	| Worker_fail_failure s -> Printexc.to_string @@ Failure s
	| Worker_fail_not_found -> Printexc.to_string Not_found
	| Worker_fail_out_of_memory -> Printexc.to_string Out_of_memory
	| Worker_fail_stack_overflow -> Printexc.to_string Stack_overflow
	| Worker_fail_sys_error s -> Printexc.to_string @@ Sys_error s
	| Worker_fail_division_by_zero -> Printexc.to_string @@ Division_by_zero
	| Worker_fail_other s -> s
;;
let worker_exn_of_exn = function
	| End_of_file -> Worker_fail_end_of_file
	| Invalid_argument s -> Worker_fail_invalid_argument s
	| Failure s -> Worker_fail_failure s
	| Not_found -> Worker_fail_not_found
	| Out_of_memory -> Worker_fail_out_of_memory
	| Stack_overflow -> Worker_fail_stack_overflow
	| Sys_error s -> Worker_fail_sys_error s
	| Division_by_zero -> Worker_fail_division_by_zero
	| e -> Worker_fail_other (Printexc.to_string e)
;;

type worker_fail_t = {
	worker_fail_index : int;
	worker_exception : worker_fail_exception_t;
};;
type worker_ret_t =
	| Worker_ok of worker_ok_file_t
	| Worker_fail of worker_fail_t
	| Worker_skipped of int
	| Worker_done
;;




(******)
(* C! *)
(******)

(* TEMP WINDOWS COUNTER *)
external get_counter_freq : unit -> int = "c_part_counter_freq" "noalloc";;
let counter_freq = get_counter_freq ();;
external counter : unit -> int = "c_part_counter" "noalloc";;


(* This function is not portable, but it won't be used with non-Windows OSes. That's what Unix.nice is for. *)
external nice_c : int -> int = "caml_nice";;

let nice = match Sys.os_type with
	| "Unix" -> Unix.nice
	| _ -> nice_c
;;

let detected_processors = try 
	match Sys.os_type with
	| "Win32" -> (
		let proc = Unix.open_process_in "echo %NUMBER_OF_PROCESSORS%" in
		let s = input_line proc in
		ignore (Unix.close_process_in proc);
		Scanf.sscanf s "%d" (fun x -> x)
	)
	| _ -> (
		let proc = Unix.open_process_in "uname" in
		let n = input_line proc in
		ignore (Unix.close_process_in proc);
		let i = match n with
			| "Linux" -> (
				let proc = Unix.open_process_in "grep -c processor /proc/cpuinfo" in
				let s = input_line proc in
				ignore (Unix.close_process_in proc);
				int_of_string s
			)
			| "Darwin" -> (
				let proc = Unix.open_process_in "sysctl hw.logicalcpu" in
				let s = input_line proc in
				ignore (Unix.close_process_in proc);
				Scanf.sscanf s "%s %d" (fun a b -> b)
			)
			| _ -> (
				(* Other *)
				0
			)
		in
		i
	)
with
	_ -> 0
;;

type capabilities_t = {
	cap_sse : bool;
	cap_sse2 : bool;
	cap_sse3 : bool;
	cap_ssse3 : bool;
	cap_sse41 : bool;
};;
external get_capabilities : unit -> capabilities_t = "get_capabilities";;
let capabilities = get_capabilities ();;
let sse41_ok = capabilities.cap_sse && capabilities.cap_sse2 && capabilities.cap_sse3 && capabilities.cap_ssse3 && capabilities.cap_sse41;;

type os_thread_id;; (* I think this is the same as a Unix.file_descr *)
external get_os_thread_self_id : unit -> os_thread_id = "get_os_thread_self_id";;
external thread_is_alive : os_thread_id -> bool = "thread_is_alive";;

external copy_file_times : Unix.file_descr -> Unix.file_descr -> bool = "copy_file_times";;
let copy_file_times_by_name source target =
	match trap_exception (Unix.openfile source [Unix.O_RDONLY]) 0o600 with
	| Normal h_source -> (
		let ret =
			match trap_exception (Unix.openfile target [Unix.O_WRONLY]) 0o600 with
			| Normal h_target -> (
				let ret =
					if copy_file_times h_source h_target then (
						Normal ()
					) else (
						Error (Failure "copy_file_times failed")
					)
				in
				ignore @@ trap_exception Unix.close h_target;
				ret
			)
			| Error e -> Error e
		in
		ignore @@ trap_exception Unix.close h_source;
		ret
	)
	| Error e -> Error e
;;

(* Weird padding stuff
		Each CBR MP3 seems to have a cycle of padded and unpadded frames which lasts
		49 frames. Some have a cycle that only lasts 7 frames, but this can be
		expanded to 49. The samplerates that are not a multiple of 11025 have no
		padded frames, whereas the other samplerates have these complex patterns
*)
exception Too_many_bytes;;
let padded_frame samplerate bitrate frameno =
	let f = frameno mod 49 in
	match (samplerate, bitrate) with
	| (S44100,  32) -> [| false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true |].(f)
	| (S44100,  40) -> [| false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;true |].(f)
	| (S44100,  48) -> [| false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true |].(f)
	| (S44100,  56) -> [| false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true |].(f)
	| (S44100,  64) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S44100,  80) -> [| false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;true |].(f)
	| (S44100,  96) -> [| false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true |].(f)
	| (S44100, 112) -> [| false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true |].(f)
	| (S44100, 128) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S44100, 160) -> [| false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true |].(f)
	| (S44100, 192) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S44100, 224) -> [| false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true |].(f)
	| (S44100, 256) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S44100, 320) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)

	| (S22050,   8) -> [| false;false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false;true |].(f)
	| (S22050,  16) -> [| false;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true |].(f)
	| (S22050,  24) -> [| false;false;true ;false;false;true ;false;false;true ;false;true ;false;false;true ;false;false;true ;false;false;true ;false;true ;false;false;true ;false;false;true ;false;true ;false;false;true ;false;false;true ;false;false;true ;false;true ;false;false;true ;false;false;true ;false;true |].(f)
	| (S22050,  32) -> [| false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true |].(f)
	| (S22050,  40) -> [| false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;false;true ;true ;false;true ;true |].(f)
	| (S22050,  48) -> [| false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true |].(f)
	| (S22050,  56) -> [| false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true |].(f)
	| (S22050,  64) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S22050,  80) -> [| false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;true |].(f)
	| (S22050,  96) -> [| false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true |].(f)
	| (S22050, 112) -> [| false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true |].(f)
	| (S22050, 128) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S22050, 144) -> [| false;false;false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false;false;true |].(f)
	| (S22050, 160) -> [| false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true |].(f)

	| (S11025,   8) -> [| false;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false;true |].(f)
	| (S11025,  16) -> [| false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true |].(f)
	| (S11025,  24) -> [| false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true ;false;true ;true ;true |].(f)
	| (S11025,  32) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S11025,  40) -> [| false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;false;true ;false;false;false;true ;false;false;false;true |].(f)
	| (S11025,  48) -> [| false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true ;false;true |].(f)
	| (S11025,  56) -> [| false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true ;false;true ;true ;false;true ;true ;true |].(f)
	| (S11025,  64) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S11025,  80) -> [| false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;true ;false;true |].(f)
	| (S11025,  96) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S11025, 112) -> [| false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;true |].(f)
	| (S11025, 128) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)
	| (S11025, 144) -> [| false;false;true ;false;true ;false;false;true ;false;true ;false;false;true ;false;true ;false;false;true ;false;true ;false;false;true ;false;true ;false;true ;false;false;true ;false;true ;false;false;true ;false;true ;false;false;true ;false;true ;false;false;true ;false;true ;false;true |].(f)
	| (S11025, 160) -> [| false;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true ;true ;true |].(f)

	| _ -> false (* All other samplerates, as well as any invalid bitrates *)
;;
