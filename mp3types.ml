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

(* Used for printing debug stuff *)
let nul = open_out (if Sys.os_type = "Win32" then "NUL" else "/dev/null");;

exception Loop_end;;

type 'a option3_t = Zero | One of 'a | Many of 'a option3_t array;;

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
	header_raw          : string;
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
	if_side_raw : string;
	if_data_raw : string;
	if_frame_raw : string; (* Includes the entire frame (WITH header) *)
	mutable if_xing : xingTag_t option; (* This is mutable so that read_next_frame can change it *)
}

let data_offset_of_if f = match f.if_header.header_id with
	| MPEG1 -> (Char.code f.if_side_raw.[0] lsl 1) lor (Char.code f.if_side_raw.[1] lsr 7)
	|   _   -> Char.code f.if_side_raw.[0]
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
	side_raw : string;
	side_offset : int;
	side_bits : int array;
	side_bytes : int;
};;

type f1_t = {
	f1_num : int;
	f1_header : header_t;
	f1_side : side_t;
	f1_data : string;
	mutable f1_pad_exact : int option;
};;

type f2_t = {
	f2_num : int;
	f2_header : header_t;
	f2_side : side_t;
	f2_bitrate : bitrate_t;
	f2_data : string;
	f2_pad : int;
	mutable f2_offset : int;
	mutable f2_bytes_left : int;
	mutable f2_flag : bool;
	mutable f2_check_output : bool; (* if the frame may have a gap before it (or before any previous frame, if needed) *)
};;

type f3_t = {
	f3_num : int;
	f3_header_side_raw : string;
	f3_output_data : string;
	mutable f3_flag : bool;
};;

(*
let side_info_of_if f = (
	let side = f.if_side_raw in
	match (f.if_header.header_id, f.if_header.header_channel_mode) with
	| (MPEG1, ChannelMono) -> (
		let off = unpackBits side 0 9 in
		let g1 = unpackBits side 18 12 in
		let g2 = unpackBits side 77 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2 |];
			side_bytes = (g1 + g2) asr 3;
		}
	)
	| (MPEG1, _) -> (
		let off = unpackBits side 0 9 in
		let g1 = unpackBits side  20 12 in
		let g2 = unpackBits side  79 12 in
		let g3 = unpackBits side 138 12 in
		let g4 = unpackBits side 197 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2; g3; g4 |];
			side_bytes = (g1 + g2 + g3 + g4) asr 3;
		}
	)
	| (_, ChannelMono) -> (
		let off = unpackBits side 0 8 in
		let g1 = unpackBits side  9 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1 |];
			side_bytes = (g1) asr 3;
		}
	)
	| (_, _) -> (
		let off = unpackBits side 0 8 in
		let g1 = unpackBits side 10 12 in
		let g2 = unpackBits side 73 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2 |];
			side_bytes = (g1 + g2) asr 3;
		}
	)
);;
*)


(* !NEW 20070326 *)



(* For the queue *)
(*
type frameInTransit_t = {
	transitFrame : int;
	transitHeader : header_t;
	transitSide : string;
	transitData : string;
	transitDataLength : int;
	transitBits : int;
	mutable transitPad : int; (* How much space the current frame should leave at the end *)
};;

type frameOut_t = {
	outFrame : int;
	outHeader : string;
	outSide : string;
	outData : string;
};;

*)








(******)
(* C! *)
(******)

(* TEMP WINDOWS COUNTER *)
(*external counter : unit -> int = "win_counter";;*)


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
