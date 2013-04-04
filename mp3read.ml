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

open Printf;;
open Mp3types;;
open Pack;;


let header_of_ptrref p r =
	if Ptr.Ref.length r < 4 then None else (
		try
			let b = Ptr.Ref.get_bits r in
			if b 0 11 <> 0x7FF then (p [Str " SYNC'S WRONG"]; raise Not_found);

			let id_index = b 11 2 in
			if id_index = 1 then (p [Str " ID'S WRONG"]; raise Not_found);
			let id = mpeg_index.(id_index) in

			if b 13 2 <> 1 then (p [Str " LAYER'S NOT 3"]; raise Not_found);

			let crc = (b 15 1 = 0) in

			let (bitrate, samplerate) =
				let bitrate_index = b 16 4 in
				let samplerate_index = b 20 2 in
				if bitrate_index = 0 || bitrate_index = 15 then (p [Str " INVALID BITRATE"]; raise Not_found);
				if samplerate_index = 3 then (p [Str " INVALID SAMPLERATE"]; raise Not_found);
				match id with
				| MPEG1  -> ([| 0;32;40;48;56;64;80;96;112;128;160;192;224;256;320 |].(bitrate_index), [| S44100;S48000;S32000 |].(samplerate_index))
				| MPEG2  -> ([| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |].(bitrate_index), [| S22050;S24000;S16000 |].(samplerate_index))
				| MPEG25 -> ([| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |].(bitrate_index), [| S11025;S12000; S8000 |].(samplerate_index))
			in
			let padding = (b 22 1 = 1) in
			let priv = (b 23 1 = 1) in
			let channel_mode = channel_index.(b 24 2) in
			let ms = (b 26 1 = 1) in
			let is = (b 27 1 = 1) in
			let copyright = (b 28 1 = 1) in
			let original = (b 29 1 = 1) in
			let emphasis = emphasis_index.(b 30 2) in
			Some {
				header_raw = r;
				header_id = id;
				header_crc = crc;
				header_bitrate = bitrate;
				header_samplerate = samplerate;
				header_padding = padding;
				header_private = priv;
				header_channel_mode = channel_mode;
				header_ms = ms;
				header_is = is;
				header_copyright = copyright;
				header_original = original;
				header_emphasis = emphasis;
			}
		with
		| Not_found -> None
	)
;;



let side_info_of_header header side =
	let gb = Ptr.Ref.get_bits side in
	match (header.header_id, header.header_channel_mode) with
	| (MPEG1, ChannelMono) -> (
		let off = gb 0 9 in
		let g1 = gb 18 12 in
		let g2 = gb 77 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2 |];
			side_bytes = (g1 + g2 + 7) asr 3;
		}
	)
	| (MPEG1, _) -> (
		let off = gb 0 9 in
		let g1 = gb  20 12 in
		let g2 = gb  79 12 in
		let g3 = gb 138 12 in
		let g4 = gb 197 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2; g3; g4 |];
			side_bytes = (g1 + g2 + g3 + g4 + 7) asr 3;
		}
	)
	| (_, ChannelMono) -> (
		let off = gb 0 8 in
		let g1 = gb  9 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1 |];
			side_bytes = (g1 + 7) asr 3;
		}
	)
	| (_, _) -> (
		let off = gb 0 8 in
		let g1 = gb 10 12 in
		let g2 = gb 73 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2 |];
			side_bytes = (g1 + g2 + 7) asr 3;
		}
	)
;;

(* This function has to be here in order to take advantage of both mp3types and pack *)
let side_info_of_if f = (
	let side = f.if_side_raw in
	let gb = Ptr.Ref.get_bits side in
	match (f.if_header.header_id, f.if_header.header_channel_mode) with
	| (MPEG1, ChannelMono) -> (
		let off = gb 0 9 in
		let g1 = gb 18 12 in
		let g2 = gb 77 12 in
		{
(*			side_raw_string = f.if_side_string;*)
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2 |];
			side_bytes = (g1 + g2 + 7) asr 3;
		}
	)
	| (MPEG1, _) -> (
		let off = gb 0 9 in
		let g1 = gb  20 12 in
		let g2 = gb  79 12 in
		let g3 = gb 138 12 in
		let g4 = gb 197 12 in
		{
(*			side_raw_string = f.if_side_string;*)
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2; g3; g4 |];
			side_bytes = (g1 + g2 + g3 + g4 + 7) asr 3;
		}
	)
	| (_, ChannelMono) -> (
		let off = gb 0 8 in
		let g1 = gb  9 12 in
		{
(*			side_raw_string = f.if_side_string;*)
			side_raw = side;
			side_offset = off;
			side_bits = [| g1 |];
			side_bytes = (g1 + 7) asr 3;
		}
	)
	| (_, _) -> (
		let off = gb 0 8 in
		let g1 = gb 10 12 in
		let g2 = gb 73 12 in
		{
(*			side_raw_string = f.if_side_string;*)
			side_raw = side;
			side_offset = off;
			side_bits = [| g1; g2 |];
			side_bytes = (g1 + g2 + 7) asr 3;
		}
	)
);;

let get_current_crc header side_raw =
	if header.header_crc then (
		let side_bytes = match (header.header_id, header.header_channel_mode) with
			| (MPEG1, ChannelMono) -> 17
			| (MPEG1, _) -> 32
			| (_, ChannelMono) -> 9
			| (_, _) -> 17
		in
		let validate_this = Ptr.Ref.append (Ptr.Ref.sub header.header_raw 2 2) side_raw in
		Crc.mp3_create_ptrref validate_this 0xFFFF
	) else (
		(* This is always an invalid CRC *)
		-1
	)
;;


class virtual virt_mp3read (*?(debug=false)*)(* in_file*) =
	object(o)
		method private virtual p : p_type list -> unit

		val mutable req = {
			req_id           = Req_equal;
			req_crc          = Req_any;
			req_bitrate      = Req_any;
			req_samplerate   = Req_equal;
			req_padding      = Req_any;
			req_private      = Req_any;
			req_channel_mode = Req_any;
			req_ms           = Req_any;
			req_is           = Req_any;
			req_copyright    = Req_any;
			req_original     = Req_any;
			req_emphasis     = Req_any;
		}
		val bitstream = Buffer.create 511
		val mutable frame_number = 0
		val xing_tag = None

		val mutable first_frame_start = max_int
		val mutable last_frame_end = 0
		method first_mp3_byte = first_frame_start
		method last_mp3_byte = last_frame_end
(*
		val handle = open_in_bin in_file
		method seek i = seek_in handle i
		method pos = pos_in handle
		method length = in_channel_length handle
		method read s o l = really_input handle s o l
		method close = close_in handle
*)
		method virtual seek : int -> unit
		method virtual pos : int
		method virtual length : int
		method virtual read : string -> int -> int -> unit
		method virtual read_ptrref : int -> Ptr.Ref.ref_t
		method virtual close : unit
(*		method virtual unix_handle : Unix.file_descr (* There should be a better way to do this *)*)

		(* Take a Ptr.Ref.t and return Some xyz if it's a valid header string, None otherwise *)
		method header_of_ptrref = header_of_ptrref o#p
(*
		method header_of_ptrref r = (
			if Ptr.Ref.length r < 4 then None else (
				try
					let b = Ptr.Ref.get_bits r in
					if b 0 11 <> 0x7FF then (if debug then printf " SYNC'S WRONG\n"; raise Not_found);

					let id_index = b 11 2 in
					if id_index = 1 then (if debug then printf " ID'S WRONG\n"; raise Not_found);
					let id = mpeg_index.(id_index) in

					if b 13 2 <> 1 then (if debug then printf " LAYER'S NOT 3\n"; raise Not_found);

					let crc = (b 15 1 = 0) in

					let (bitrate, samplerate) =
						let bitrate_index = b 16 4 in
						let samplerate_index = b 20 2 in
						if bitrate_index = 0 || bitrate_index = 15 then (if debug then printf " INVALID BITRATE\n"; raise Not_found);
						if samplerate_index = 3 then (if debug then printf " INVALID SAMPLERATE\n"; raise Not_found);
						match id with
						| MPEG1  -> ([| 0;32;40;48;56;64;80;96;112;128;160;192;224;256;320 |].(bitrate_index), [| S44100;S48000;S32000 |].(samplerate_index))
						| MPEG2  -> ([| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |].(bitrate_index), [| S22050;S24000;S16000 |].(samplerate_index))
						| MPEG25 -> ([| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |].(bitrate_index), [| S11025;S12000; S8000 |].(samplerate_index))
					in
					let padding = (b 22 1 = 1) in
					let priv = (b 23 1 = 1) in
					let channel_mode = channel_index.(b 24 2) in
					let ms = (b 26 1 = 1) in
					let is = (b 27 1 = 1) in
					let copyright = (b 28 1 = 1) in
					let original = (b 29 1 = 1) in
					let emphasis = emphasis_index.(b 30 2) in
					Some {
						header_raw = r;
						header_id = id;
						header_crc = crc;
						header_bitrate = bitrate;
						header_samplerate = samplerate;
						header_padding = padding;
						header_private = priv;
						header_channel_mode = channel_mode;
						header_ms = ms;
						header_is = is;
						header_copyright = copyright;
						header_original = original;
						header_emphasis = emphasis;
					}
				with
				| Not_found -> None
			)
		)
*)
		(* Get a frame at the current pos_in, assuming it satisfies the requirements *)
		method get_frame_here reqs = (
			let start_pos = o#pos in
			let return_none () = (o#seek start_pos; Fp_none) in
			o#p [Str "get_frame_here at "; Int start_pos];
			try
(*				let header_string = String.create 4 in*)
				let header_ptrref = (try
					o#read_ptrref 4
				with
					End_of_file -> (o#p [Str "Got EOF reading frame header"]; raise Not_found)
				) in
				o#p [Str " Got bytes "; Ptrref header_ptrref];
				match o#header_of_ptrref header_ptrref with
				| None -> (
					o#p [Str " SYNC ERROR AT "; Int (o#pos - 4)];
					Fp_none
				)
				| Some header -> (
					(* Now match the requirements *)
					let stringify_list str_fun l f = List.iter (fun x -> f (Str " "); f (Str (str_fun x))) l in
					let found_match = match reqs with
						| {req_id           = Req_matches x} when not (List.mem header.header_id x)           -> (o#p [Str " ID ";           Str (string_of_mpeg header.header_id);              Str " is not in ["; Fun (stringify_list string_of_mpeg x);                                  Str " ]"]; false)
						| {req_crc          = Req_matches x} when not (List.mem header.header_crc x)          -> (o#p [Str " CRC ";          Bool header.header_crc;                             Str " is not in ["; Fun (stringify_list string_of_bool x);                                  Str " ]"]; false)
						| {req_bitrate      = Req_matches x} when not (List.mem header.header_bitrate x)      -> (o#p [Str " Bitrate ";      Int header.header_bitrate;                          Str " is not in ["; Fun (stringify_list string_of_int  x);                                  Str " ]"]; false)
						| {req_samplerate   = Req_matches x} when not (List.mem header.header_samplerate x)   -> (o#p [Str " Samplerate ";   Int (int_of_samplerate header.header_samplerate);   Str " is not in ["; Fun (stringify_list (fun n -> string_of_int @@ int_of_samplerate n) x); Str " ]"]; false)
						| {req_padding      = Req_matches x} when not (List.mem header.header_padding x)      -> (o#p [Str " Padding ";      Bool header.header_padding;                         Str " is not in ["; Fun (stringify_list string_of_bool x);                                  Str " ]"]; false)
						| {req_private      = Req_matches x} when not (List.mem header.header_private x)      -> (o#p [Str " Private ";      Bool header.header_private;                         Str " is not in ["; Fun (stringify_list string_of_bool x);                                  Str " ]"]; false)
						| {req_channel_mode = Req_matches x} when not (List.mem header.header_channel_mode x) -> (o#p [Str " Channel mode "; Str (string_of_channel header.header_channel_mode); Str " is not in ["; Fun (stringify_list string_of_channel x);                               Str " ]"]; false)
						| {req_ms           = Req_matches x} when not (List.mem header.header_ms x)           -> (o#p [Str " MS ";           Bool header.header_ms;                              Str " is not in ["; Fun (stringify_list string_of_bool x);                                  Str " ]"]; false)
						| {req_is           = Req_matches x} when not (List.mem header.header_is x)           -> (o#p [Str " IS ";           Bool header.header_is;                              Str " is not in ["; Fun (stringify_list string_of_bool x);                                  Str " ]"]; false)
						| {req_copyright    = Req_matches x} when not (List.mem header.header_copyright x)    -> (o#p [Str " Copyright ";    Bool header.header_copyright;                       Str " is not in ["; Fun (stringify_list string_of_bool x);                                  Str " ]"]; false)
						| {req_original     = Req_matches x} when not (List.mem header.header_original x)     -> (o#p [Str " Original ";     Bool header.header_original;                        Str " is not in ["; Fun (stringify_list string_of_bool x);                                  Str " ]"]; false)
						| {req_emphasis     = Req_matches x} when not (List.mem header.header_emphasis x)     -> (o#p [Str " Emphasis ";     Str (string_of_emphasis header.header_emphasis);    Str " is not in ["; Fun (stringify_list string_of_emphasis x);                              Str " ]"]; false)
						| _ -> true
					in
(*
					let found_match = match reqs with
						| {req_id           = Req_matches x} when not (List.mem header.header_id x)           -> (if debug then printf " ID %s is not in [%s ]\n"           (string_of_mpeg header.header_id)              (List.fold_left (fun s n -> s ^ " " ^ (string_of_mpeg     n)) "" x); false)
						| {req_crc          = Req_matches x} when not (List.mem header.header_crc x)          -> (if debug then printf " CRC %B is not in [%s ]\n"          header.header_crc                              (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x); false)
						| {req_bitrate      = Req_matches x} when not (List.mem header.header_bitrate x)      -> (if debug then printf " Bitrate %d is not in [%s ]\n"      header.header_bitrate                          (List.fold_left (fun s n -> s ^ " " ^ (string_of_int      n)) "" x); false)
						| {req_samplerate   = Req_matches x} when not (List.mem header.header_samplerate x)   -> (if debug then printf " Samplerate %d is not in [%s ]\n"   (int_of_samplerate header.header_samplerate)   (List.fold_left (fun s n -> s ^ " " ^ (string_of_int (int_of_samplerate n))) "" x); false)
						| {req_padding      = Req_matches x} when not (List.mem header.header_padding x)      -> (if debug then printf " Padding %B is not in [%s ]\n"      header.header_padding                          (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x); false)
						| {req_private      = Req_matches x} when not (List.mem header.header_private x)      -> (if debug then printf " Private %B is not in [%s ]\n"      header.header_private                          (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x); false)
						| {req_channel_mode = Req_matches x} when not (List.mem header.header_channel_mode x) -> (if debug then printf " Channel mode %s is not in [%s ]\n" (string_of_channel header.header_channel_mode) (List.fold_left (fun s n -> s ^ " " ^ (string_of_channel  n)) "" x); false)
						| {req_ms           = Req_matches x} when not (List.mem header.header_ms x)           -> (if debug then printf " MS %B is not in [%s ]\n"           header.header_ms                               (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x); false)
						| {req_is           = Req_matches x} when not (List.mem header.header_is x)           -> (if debug then printf " IS %B is not in [%s ]\n"           header.header_is                               (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x); false)
						| {req_copyright    = Req_matches x} when not (List.mem header.header_copyright x)    -> (if debug then printf " Copyright %B is not in [%s ]\n"    header.header_copyright                        (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x); false)
						| {req_original     = Req_matches x} when not (List.mem header.header_original x)     -> (if debug then printf " Original %B is not in [%s ]\n"     header.header_original                         (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x); false)
						| {req_emphasis     = Req_matches x} when not (List.mem header.header_emphasis x)     -> (if debug then printf " Emphasis %s is not in [%s ]\n"     (string_of_emphasis header.header_emphasis)    (List.fold_left (fun s n -> s ^ " " ^ (string_of_emphasis n)) "" x); false)
						| _ -> true
					in
*)
					if found_match then (
						let frame_length = frame_length_of_header header in
						let side_info_size = match (header.header_id, header.header_channel_mode) with
							| (MPEG1, ChannelMono) -> 17
							| (  _  , ChannelMono) ->  9
							| (MPEG1,      _     ) -> 32
							| (  _  ,      _     ) -> 17
						in
						let data_raw_pos = (if header.header_crc then 6 else 4) + side_info_size in

(*
						let frame_string = String.create frame_length in
						String.blit header_string 0 frame_string 0 4; (* Copy the header to the frame string *)
						o#read frame_string 4 (frame_length - 4);
*)
						let after_header = o#read_ptrref (frame_length - 4) in
						let frame = Ptr.Ref.append header_ptrref after_header in
						o#p [Str "Frame ptr ref:"];
						o#p [Str " "; Ptrref frame];
						let frame_string = Ptr.Ref.to_string frame in

						let side_raw = Ptr.Ref.sub frame (if header.header_crc then 6 else 4) side_info_size in

						let crc_ok = if header.header_crc then (
							let target_crc = Ptr.Ref.get_bits frame 32 16 in
							let actual_crc = get_current_crc header side_raw in
							o#p [Str " Frame CRCs: written is "; Hex (4,target_crc); Str ", real is "; Hex (4,actual_crc)];
							target_crc = actual_crc
						) else (
							true
						) in
						if not crc_ok then o#p [Str "  FRAME CRC IS INVALID"];

						o#p [Str " Found requested frame from "; Int start_pos; Str " to "; Int o#pos];

						Fp_some {
							if_raw          = frame;
							if_header       = header;
(*							if_side_string  = Ptr.Ref.to_string (Ptr.Ref.sub frame (if header.header_crc then 6 else 4) side_info_size);*)
							if_side_raw     = side_raw;
(*							if_data_string  = String.sub frame_string data_raw_pos (frame_length - data_raw_pos);*)
							if_data_raw     = Ptr.Ref.sub frame data_raw_pos (frame_length - data_raw_pos);
(*							if_frame_string = frame_string;*)
							if_crc_ok       = crc_ok;
							if_xing         = None;
						}
					) else (
						(* No match found *)
						o#seek start_pos;
						Fp_none
					)
				)
			with
			| End_of_file -> (o#p [Str " HIT THE EOF"]; o#seek start_pos; Fp_none)
			| Not_found -> (o#p [Str " HIT THE EOF when reading header"]; o#seek start_pos; Fp_eof)
		)

		(* This takes some requirements and returns (new_reqs, first_frame, (started_at, found_at) option) option *)
		method resync_here num_frames reqs = (

			let start_pos = o#pos in

			o#p [Str "resync_here at "; Int start_pos];

			let rec find_frame_and_update_reqs more_frames reqs = (
				let frame_perhaps = o#get_frame_here reqs in
				match frame_perhaps with
				| Fp_some f -> (
					let new_pos = o#pos in
					let new_reqs = {
						req_id           = (match reqs.req_id           with Req_equal -> Req_matches [f.if_header.header_id          ] | x -> x);
						req_crc          = (match reqs.req_crc          with Req_equal -> Req_matches [f.if_header.header_crc         ] | x -> x);
						req_bitrate      = (match reqs.req_bitrate      with Req_equal -> Req_matches [f.if_header.header_bitrate     ] | x -> x);
						req_samplerate   = (match reqs.req_samplerate   with Req_equal -> Req_matches [f.if_header.header_samplerate  ] | x -> x);
						req_padding      = (match reqs.req_padding      with Req_equal -> Req_matches [f.if_header.header_padding     ] | x -> x);
						req_private      = (match reqs.req_private      with Req_equal -> Req_matches [f.if_header.header_private     ] | x -> x);
						req_channel_mode = (
							match reqs.req_channel_mode with (* Consider both stereo modes to be equal *)
							| Req_equal when f.if_header.header_channel_mode = ChannelStereo || f.if_header.header_channel_mode = ChannelJoint -> Req_matches [ChannelStereo;ChannelJoint]
							| Req_equal -> Req_matches [f.if_header.header_channel_mode]
							| x -> x
						);
						req_ms           = (match reqs.req_ms           with Req_equal -> Req_matches [f.if_header.header_ms          ] | x -> x);
						req_is           = (match reqs.req_is           with Req_equal -> Req_matches [f.if_header.header_is          ] | x -> x);
						req_copyright    = (match reqs.req_copyright    with Req_equal -> Req_matches [f.if_header.header_copyright   ] | x -> x);
						req_original     = (match reqs.req_original     with Req_equal -> Req_matches [f.if_header.header_original    ] | x -> x);
						req_emphasis     = (match reqs.req_emphasis     with Req_equal -> Req_matches [f.if_header.header_emphasis    ] | x -> x);
					} in
					o#p [Str " resync_here found frame "; Int (num_frames - more_frames); Str " here"];

					if more_frames > 1 then (
						(* Get more frames *)
						match find_frame_and_update_reqs (pred more_frames) new_reqs with
						| None -> (o#seek start_pos; None) (* Oops. Not enough frames found *)
						| Some (really_new_reqs, _) -> (o#seek new_pos; Some (really_new_reqs, f))
					) else (
						(* Just return this frame *)
						Some (new_reqs, f)
					)
				)
				| _ -> (o#seek start_pos; None) (* Uhh... right *)
			) in

			let rec find_sync_rec inky = (
				o#seek (start_pos + inky);
				match find_frame_and_update_reqs num_frames reqs with
				| None -> (
					if start_pos + inky + 4 >= o#length then (
						raise End_of_file;
					) else (
						find_sync_rec (succ inky)
					)
				)
				| Some (r,f) when inky = 0 -> (r, f, (start_pos, start_pos))
				| Some (r,f)               -> (r, f, (start_pos, start_pos + inky))
			) in

			(try
				let (r,f,(wanted,found)) = find_sync_rec 0 in
				if wanted = found then (
					o#p [Str "Found "; Int num_frames; Str " frames starting from "; Int wanted];
				) else (
					o#p [Str "Found "; Int num_frames; Str " frames starting from "; Int found; Str " (resync needed from "; Int wanted; Str ")"]
				);
				Some (r,f,(wanted,found))
			with
				End_of_file -> None
			)
		)

		(* Grab the next frame. Will return (new_req, frame, (pos_expected, pos_found) option) *)
		val mutable val_force_resync = true (* If this is true, force a full resync. Important for syncing at the beginning *)
		method find_next_frame ?(num_frames=3) ?(force_resync=val_force_resync) ?(lame_search=false) req = (
			let frame_info_perhaps = (
				if force_resync then (
					(* Need to do a full resync *)
					val_force_resync <- false;
					o#resync_here num_frames req
				) else (
					let pos_now = o#pos in
					match o#get_frame_here req with
					| Fp_none -> (
						(* Oops. Tried to find a frame, but it didn't exist. Sync error *)
						o#seek pos_now;
						o#resync_here num_frames req
					)
					| Fp_eof -> None
					| Fp_some x -> Some (req, x, (pos_now, pos_now))
				)
			) in
			match frame_info_perhaps with
			| None -> raise End_of_file
			| Some (req2, f, resync_needed) -> (
(*
if debug then (
	let h = f.if_header in
	printf " \"%s\"\n" (Ptr.Ref.to_HEX h.header_raw);
	printf "  ID: %s\n" (string_of_mpeg h.header_id);
	printf "  CRC? %B\n" h.header_crc;
	printf "  Bitrate: %d\n" h.header_bitrate;
	printf "  Samplerate: %d\n" (int_of_samplerate h.header_samplerate);
	printf "  Padding? %B\n" h.header_padding;
	printf "  Private? %B\n" h.header_private;
	printf "  Channel mode: %s\n" (string_of_channel h.header_channel_mode);
	printf "  MS? %B\n" h.header_ms;
	printf "  IS? %B\n" h.header_is;
	printf "  Copyright? %B\n" h.header_copyright;
	printf "  Original? %B\n" h.header_original;
	printf "  Emphasis: %s\n" (string_of_emphasis h.header_emphasis);
	printf " Side:  \"%s\"\n" (Ptr.Ref.to_HEX f.if_side_raw);
	printf " Data:  \"%s\"\n" (Ptr.Ref.to_HEX f.if_data_raw);
	printf " Frame: \"%s\"\n" (Ptr.Ref.to_HEX f.if_raw);
);
*)
				(* Update the frame bounds *)
				first_frame_start <- min first_frame_start (snd resync_needed);
				last_frame_end <- max last_frame_end ((snd resync_needed) + Ptr.Ref.length f.if_raw - 1);

				if lame_search then (
					(* Convert the frame into an XING frame, if possible *)
					o#p [Str "Searching for XING/LAME tag"];
					(try
(*
						let rec count_zeros_string frame_raw now num = (
							if now >= String.length frame_raw || frame_raw.[now] <> '\x00'
								then num
								else count_zeros_string frame_raw (succ now) (succ num)
						) in
*)
						let rec count_zeros frame_raw now num =
							if now >= Ptr.Ref.length frame_raw || Ptr.Ref.ref_get_byte frame_raw now <> 0 then (
								num
							) else (
								count_zeros frame_raw (succ now) (succ num)
							)
						in
						let num_zeros = count_zeros f.if_raw 6 0 in
						if num_zeros < 7 || num_zeros > 32 then (o#p [Str " XING search found "; Int num_zeros; Str " zeros (not between 7 and 32)"]; raise Not_found);
						if num_zeros + 10 > Ptr.Ref.length f.if_raw then (o#p [Str " XING search found "; Int (Ptr.Ref.length f.if_raw); Str " bytes in frame; not long enough for XING"]; raise Not_found); (* Frame is too short to have an XING tag *)
(*						let tag_type = String.sub f.if_frame_string (num_zeros + 6) 4 in*)
						let tag_type = Ptr.Ref.to_string (Ptr.Ref.sub f.if_raw (num_zeros + 6) 4) in
						if tag_type <> "Xing" && tag_type <> "Info" then (o#p [Str " XING search found unknown tag type "; StrS tag_type]; raise Not_found);

						(* TODO: make this less dependent on strings *)
						let tag_guts = Ptr.Ref.to_string (Ptr.Ref.sub f.if_raw (num_zeros + 10) (Ptr.Ref.length f.if_raw - num_zeros - 10)) in
(*						let tag_guts = String.sub f.if_frame_string (num_zeros + 10) (Ptr.Ref.length f.if_raw - num_zeros - 10) in*)

						let tag_pos_ref = ref 4 in (* Where we are in the tag. Pretend that the flags have already been read (0 is right AFTER "Xing" or "Info") *)

						if String.sub tag_guts 0 3 <> "\x00\x00\x00" then (o#p [Str " XING search flags not padded with zeros"]; raise Not_found);

						(* Read XING flags *)
						let (flag_frames, flag_bytes, flag_toc, flag_quality) = (
							let flags = Char.code tag_guts.[3] in
							if flags > 15 then (o#p [Str " XING search found flags > 15"]; raise Not_found);
							let flag_frames = (
								if flags land 1 = 1 then (
									if !tag_pos_ref + 4 > String.length tag_guts then (o#p [Str " XING search finds frame flag, but not enough room for them"]; raise Not_found);
									let p = unpackN tag_guts !tag_pos_ref in
									tag_pos_ref := !tag_pos_ref + 4;
									Some p
								) else (None)
							) in
							let flag_bytes = (
								if flags land 2 = 2 then (
									if !tag_pos_ref + 4 > String.length tag_guts then (o#p [Str " XING search finds byte flag, but not enough room for them"]; raise Not_found);
									let p = unpackN tag_guts !tag_pos_ref in
									tag_pos_ref := !tag_pos_ref + 4;
									Some p
								) else (None)
							) in
							let flag_toc = (
								if flags land 4 = 4 then (
									if !tag_pos_ref + 100 > String.length tag_guts then (o#p [Str " XING search finds TOC, but not enough room for it"]; raise Not_found);
									let p = Array.init 100 (fun i -> Char.code tag_guts.[!tag_pos_ref + i]) in
									tag_pos_ref := !tag_pos_ref + 100;
									Some p
								) else (None)
							) in
							let flag_quality = (
								if flags land 8 = 8 then (
									if !tag_pos_ref + 4 > String.length tag_guts then (o#p [Str " XING search finds quality flag, but not enough room for it"]; raise Not_found);
									let p = unpackN tag_guts !tag_pos_ref in
									tag_pos_ref := !tag_pos_ref + 4;
									Some p
								) else (None)
							) in
							(flag_frames, flag_bytes, flag_toc, flag_quality)
						) in
						(* At this point, the tag is definitely XING *)

						let from_name = !tag_pos_ref in (* Used for the CRC calculation *)

						(* Get the optional encoder and see if it's a LAME tag *)
						let (lame_perhaps, encoder) = (
							if !tag_pos_ref + 20 > String.length tag_guts then (
								(* Oops. Not enough room for the encoder *)
								o#p [Str " XING search has not enough room for encoder; not LAME but still XING"];
								(None, "")
							) else (
								(* There's enough room for the encoder *)
								let encoder_20 = String.sub tag_guts !tag_pos_ref 20 in

								(* Is there enough room for LAME? *)
								if !tag_pos_ref + 36 > String.length tag_guts then (
									o#p [Str " XING search sees not enough room for LAME; returning XING"];
									(None, encoder_20)
								) else (
									let lame_part = String.sub tag_guts !tag_pos_ref 36 in (* This starts from the beginning of the encoder string and goes to the tag CRC *)
									let crc_ok = (
										if lame_part.[20] = '\x00' && String.sub lame_part 24 12 = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" && String.sub lame_part 21 3 <> "\x00\x00\x00" then (
											o#p [Str " XING search found LAME frame with only offset info"];
											true
										) else (
											let tag_crc = unpackn lame_part 34 in
(*											let real_crc = Crc.lame_create (String.sub f.if_frame_string 0 (num_zeros + 10 + from_name + 34)) 0 in*)
											let real_crc = Crc.lame_create_ptrref (Ptr.Ref.sub f.if_raw 0 (num_zeros + 10 + from_name + 34)) 0 in
											o#p [Str " Written CRC is "; Hex (4,tag_crc); Str ", real CRC is "; Hex (4,real_crc)];
											tag_crc = real_crc
										)
									) in
									if crc_ok then (
										(* LAME! *)
										(Some {
											lameRevision        = unpackBits lame_part  72  4;
											lameVBRMethod       = unpackBits lame_part  76  4;
											lameLowpass         = unpackBits lame_part  80  8;
											lamePeakAmplitude   = Int32.float_of_bits (unpackN32 lame_part 11); (* Goes from bit 88 to 119 *)
											lameRGTrack         = unpackBits lame_part 120 16;
											lameRGAlbum         = unpackBits lame_part 136 16;
											lameNoGapPrev       = unpackBits lame_part 152  1 <> 0;
											lameNoGapNext       = unpackBits lame_part 153  1 <> 0;
											lameNSSafeJoint     = unpackBits lame_part 154  1 <> 0;
											lameNSPsyTune       = unpackBits lame_part 155  1 <> 0;
											lameATHType         = unpackBits lame_part 156  4;
											lameABRBitrate      = unpackBits lame_part 160  8;
											lameDelayStart      = unpackBits lame_part 168 12;
											lameDelayEnd        = unpackBits lame_part 180 12;
											lameSourceFrequency = unpackBits lame_part 192  2;
											lameUnwise          = unpackBits lame_part 194  1 <> 0;
											lameStereoMode      = unpackBits lame_part 195  3;
											lameNoiseShaping    = unpackBits lame_part 198  2;
											lameMP3Gain         = unpackBits lame_part 200  8;
											lameSurround        = unpackBits lame_part 210  3; (* Two bits unused here *)
											lamePreset          = unpackBits lame_part 213 11;
											lameMusicLength     = unpackN lame_part 28;
											lameMusicCRC        = unpackBits lame_part 256 16;
										},
											String.sub encoder_20 0 9
										)
									) else (
										(* Not LAME... *)
										(None, encoder_20)
									)
								)
							)
						) in (* (lame_perhaps, encoder) *)

						let xing = {
							xingRawTag = tag_type ^ tag_guts;
							xingTagType = tag_type;
							xingNumFrames = flag_frames;
							xingNumBytes = flag_bytes;
							xingTOC = flag_toc;
							xingQuality = flag_quality;
							xingEncoder = encoder;
							xingLame = lame_perhaps;
						} in

(*
						if debug then (
							printf "XING FOUND:\n";
							printf " Raw tag:    %S\n" xing.xingRawTag;
							printf " Tag type:   %s\n" xing.xingTagType;
							printf " Num frames: %s\n" (match xing.xingNumFrames with None -> "NONE" | Some x -> string_of_int x);
							printf " Num bytes:  %s\n" (match xing.xingNumBytes with None -> "NONE" | Some x -> string_of_int x);
							printf " TOC:        %s\n" (match xing.xingTOC with None -> "NONE" | Some x -> "[" ^ (Array.fold_left (fun so_far gnu -> so_far ^ " " ^ string_of_int gnu) "" x) ^ " ]");
							printf " Quality:    %s\n" (match xing.xingQuality with None -> "NONE" | Some x -> string_of_int x);
							printf " Encoder:    %S\n" xing.xingEncoder;
							match xing.xingLame with
							| None -> printf " NOT LAME\n";
							| Some l -> (
								printf " LAME:\n";
								printf "  Revision:       %d\n" l.lameRevision;
								printf "  VBR method:     %d\n" l.lameVBRMethod;
								printf "  Lowpass:        %d\n" l.lameLowpass;
								printf "  Peak amplitude: %f\n" l.lamePeakAmplitude;
								printf "  RG track:       %d\n" l.lameRGTrack;
								printf "  RG album:       %d\n" l.lameRGAlbum;
								printf "  NS psy tune:    %B\n" l.lameNSPsyTune;
								printf "  NS safe joint:  %B\n" l.lameNSSafeJoint;
								printf "  No gap prev:    %B\n" l.lameNoGapPrev;
								printf "  No gap next:    %B\n" l.lameNoGapNext;
								printf "  ATH type:       %d\n" l.lameATHType;
								printf "  ABR bitrate:    %d\n" l.lameABRBitrate;
								printf "  Delay start:    %d\n" l.lameDelayStart;
								printf "  Delay end:      %d\n" l.lameDelayEnd;
								printf "  Noise shaping:  %d\n" l.lameNoiseShaping;
								printf "  Stereo mode:    %d\n" l.lameStereoMode;
								printf "  Unwise:         %B\n" l.lameUnwise;
								printf "  Source freq:    %d\n" l.lameSourceFrequency;
								printf "  MP3 gain:       %d\n" l.lameMP3Gain;
								printf "  Surround:       %d\n" l.lameSurround;
								printf "  Preset:         %d\n" l.lamePreset;
								printf "  Music length:   %d\n" l.lameMusicLength;
								printf "  Music CRC:      %d\n" l.lameMusicCRC;
							)
						);
*)
						f.if_xing <- Some xing;
						(req2, f, resync_needed)
					with
						Not_found -> (req2, f, resync_needed)
					)
				) else ( (* Not searching for XING *)
					(* Don't bother with XING *)
					(req2, f, resync_needed)
				) (* Search for XING *)
			) (* Frame has been found *)
		)

	end
;;

(*
class mp3read_new ?debug in_file =
	object
		inherit virt_mp3read ?debug:debug(* in_file*)

		val handle = open_in_bin in_file
		method seek i = seek_in handle i
		method pos = pos_in handle
		method length = in_channel_length handle
		method read s o l = really_input handle s o l
		method close = close_in handle

	end
;;
*)

class mp3read_unix print_fun in_file =
	object(o)
		inherit virt_mp3read (*?debug:debug*)
		method private p = print_fun

		val handle = Unicode.openfile_utf8 in_file [Unix.O_RDONLY] 0o600
		method seek i = ignore (Unix.lseek handle i Unix.SEEK_SET)
		method pos = Unix.lseek handle 0 Unix.SEEK_CUR
		method length = (
			let now = o#pos in
			let posend = Unix.lseek handle 0 Unix.SEEK_END in
			o#seek now;
			posend
		)
		method read s r l = (
			if l = 0 then () else (
				let got = Unix.read handle s r l in
				if got = 0 then (
					raise End_of_file
				) else (
					o#read s (r + got) (l - got)
				)
			)
		)
		method read_ptrref l = (
			let s = String.create l in
			o#read s 0 l;
			Ptr.Ref.of_string s
		)
		method close = Unix.close handle
	end
;;


class mp3read_ptr print_fun in_file =
	object(o)
		inherit virt_mp3read (*?debug:debug*)
		method private p = print_fun

		val handle = Unicode.openfile_utf8 in_file [Unix.O_RDONLY] 0o600
		val mutable ptr = Ptr.make 0 0
		val mutable pos = 0
		val mutable len = 0

		method seek i = pos <- i
		method pos = pos
		method length = len
		method read s r l = (
			if l = 0 then (
				()
			) else if pos < 0 || l < 0 then (
				invalid_arg (Printf.sprintf "mp3read_ptr#read %d %d from %d (File length %d)" r l pos len)
			) else if pos + l > len then (
				raise End_of_file
			) else (
				Ptr.blit_to_string ptr pos s r l;
				pos <- pos + l;
			)
		)
		method close = (
			Ptr.unmap ptr;
			ptr <- Ptr.make 0 0;
			len <- 0;
			Unix.close handle;
		)

		method read_ptrref l = (
			if pos + l > len then (
				pos <- len;
				raise End_of_file
			) else (
				let ret = Ptr.Ref.of_subptr ptr pos l in
				pos <- pos + l;
				ret
			)
		)

		initializer (
			ptr <- Ptr.map_handle handle 0 0 Ptr.Map_read_only;
			len <- Unix.lseek handle 0 Unix.SEEK_END;
		)
	end
;;


class mp3read_ptr_only print_fun ptr len =
	object(o)
		inherit virt_mp3read (*?debug:debug*)
		method private p = print_fun

		val mutable pos = 0

		method seek i = pos <- i
		method pos = pos
		method length = len
		method read s r l = (
			if l = 0 then (
				()
			) else if pos < 0 || l < 0 then (
				invalid_arg (Printf.sprintf "mp3read_ptr#read %d %d from %d (File length %d)" r l pos len)
			) else if pos + l > len then (
				raise End_of_file
			) else (
				Ptr.blit_to_string ptr pos s r l;
				pos <- pos + l;
			)
		)
		method close = ()

		method read_ptrref l = (
			if pos + l > len then (
				pos <- len;
				raise End_of_file
			) else (
				let ret = Ptr.Ref.of_subptr ptr pos l in
				pos <- pos + l;
				ret
			)
		)

	end
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

(*
	If Global_gain is 0, the resultant file is very quiet (~-225dB)

*)
