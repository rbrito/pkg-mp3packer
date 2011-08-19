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


(* This function has to be here in order to take advantage of both mp3types and pack *)
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
			side_bytes = (g1 + g2 + 7) asr 3;
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
			side_bytes = (g1 + g2 + g3 + g4 + 7) asr 3;
		}
	)
	| (_, ChannelMono) -> (
		let off = unpackBits side 0 8 in
		let g1 = unpackBits side  9 12 in
		{
			side_raw = side;
			side_offset = off;
			side_bits = [| g1 |];
			side_bytes = (g1 + 7) asr 3;
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
			side_bytes = (g1 + g2 + 7) asr 3;
		}
	)
);;


class mp3read_new ?(debug=false) in_file =
	object(o)
		val handle = open_in_bin in_file

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

		method seek i = seek_in handle i
		method pos = pos_in handle
		method length = in_channel_length handle

		(* Take a string and return Some xyz if it's a valid header string, None otherwise *)
		method header_of_string s = (
			if String.length s < 4 then None else (
				let a = Char.code s.[0] in
				let b = Char.code s.[1] in
				let c = Char.code s.[2] in
				let d = Char.code s.[3] in
				try
					if a <> 255 || b < 224 then (if debug then printf " SYNC'S WRONG\n"; raise Not_found);

					let id = mpeg_index.(b lsr 3 land 3) in
					if b lsr 3 land 3 = 1 then (if debug then printf " ID'S WRONG\n"; raise Not_found);

					if b lsr 1 land 3 <> 1 then (if debug then printf " LAYER'S NOT 3\n"; raise Not_found);

					let crc = (b land 1 = 0) in (* CRC is present if this is a ONE! *)
					let (bitrate, samplerate) = (
						let bitrate_index = c lsr 4 land 15 in
						let samplerate_index = c lsr 2 land 3 in
						if bitrate_index = 0 || bitrate_index = 15 then (if debug then printf " INVALID BITRATE\n"; raise Not_found);
						if samplerate_index = 3 then (if debug then printf " INVALID SAMPLERATE\n"; raise Not_found);
						match id with
						| MPEG1  -> ([| 0;32;40;48;56;64;80;96;112;128;160;192;224;256;320 |].(bitrate_index), [| S44100;S48000;S32000 |].(samplerate_index))
						| MPEG2  -> ([| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |].(bitrate_index), [| S22050;S24000;S16000 |].(samplerate_index))
						| MPEG25 -> ([| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |].(bitrate_index), [| S11025;S12000; S8000 |].(samplerate_index))
					) in
					let padding = (c land 2 = 2) in
					let priv = (c land 1 = 1) in (* The word "private" is reserved in OCaml... *)
					let channel_mode = channel_index.(d lsr 6 land 3) in
					let ms = (d land 32 = 32) in
					let is = (d land 16 = 16) in
					let copyright = (d land 8 = 8) in
					let original = (d land 4 = 4) in
					let emphasis = emphasis_index.(d land 3) in
					Some {
						header_raw = String.sub s 0 4;
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

		(* Get a frame at the current pos_in, assuming it satisfies the requirements *)
		method get_frame_here reqs = (
			let start_pos = pos_in handle in
			let return_none () = (seek_in handle start_pos; Fp_none) in
			if debug then printf "get_frame_here at %d\n" start_pos;
			try
				let header_string = String.create 4 in
				(try
					really_input handle header_string 0 4
				with
					(* This is the only time when it should return Fp_eof, since there's no chance a frame can fit in < 4 frames *)
					End_of_file -> (if debug then printf "HHHHHHHHHHHHHHHHHHHHHHUUUUUUUUUUUUUUUUUUUUHHHHHHHHHHHHHHHHHHHHH?\n"; raise Not_found)
				);
				if debug then printf " Got bytes %s\n" (to_hex header_string);
				match o#header_of_string header_string with
				| None -> (
					if debug then printf " SYNC ERROR AT %d\n" (pos_in handle - 4);
					Fp_none
				)
				| Some header -> (
					(* Now match the requirements *)
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
					if found_match then (
						let frame_length = frame_length_of_header header in
						let side_info_size = match (header.header_id, header.header_channel_mode) with
							| (MPEG1, ChannelMono) -> 17
							| (  _  , ChannelMono) -> 9
							| (MPEG1,      _     ) -> 32
							| (  _  ,      _     ) -> 17
						in
						let data_raw_pos = (if header.header_crc then 6 else 4) + side_info_size in
						let frame_raw = String.create frame_length in
						String.blit header_string 0 frame_raw 0 4; (* Copy the header to the frame string *)
						really_input handle frame_raw 4 (frame_length - 4);

						if debug then printf " Found requested frame from %d to %d\n" start_pos (pos_in handle);

						Fp_some {
							if_header    = header;
							if_side_raw  = String.sub frame_raw (if header.header_crc then 6 else 4) side_info_size;
							if_data_raw  = String.sub frame_raw data_raw_pos (frame_length - data_raw_pos);
							if_frame_raw = frame_raw;
							if_xing      = None;
						}
					) else (
						(* No match found *)
						seek_in handle start_pos;
						Fp_none
					)
				)
			with
			| End_of_file -> (if debug then printf " HIT THE EOF\n"; seek_in handle start_pos; Fp_none)
			| Not_found -> (if debug then printf " HIT THE EOF when reading header\n"; seek_in handle start_pos; Fp_eof)
		)

		(* This takes some requirements and returns (new_reqs, first_frame, (started_at, found_at) option) option *)
		method resync_here num_frames reqs = (

			let start_pos = pos_in handle in

			if debug then printf "resync_here at %d\n" start_pos;

			let rec find_frame_and_update_reqs more_frames reqs = (
				let frame_perhaps = o#get_frame_here reqs in
				match frame_perhaps with
				| Fp_some f -> (
					let new_pos = pos_in handle in
					let new_reqs = {
						req_id           = (match reqs.req_id           with Req_equal -> Req_matches [f.if_header.header_id          ] | x -> x);
						req_crc          = (match reqs.req_crc          with Req_equal -> Req_matches [f.if_header.header_crc         ] | x -> x);
						req_bitrate      = (match reqs.req_bitrate      with Req_equal -> Req_matches [f.if_header.header_bitrate     ] | x -> x);
						req_samplerate   = (match reqs.req_samplerate   with Req_equal -> Req_matches [f.if_header.header_samplerate  ] | x -> x);
						req_padding      = (match reqs.req_padding      with Req_equal -> Req_matches [f.if_header.header_padding     ] | x -> x);
						req_private      = (match reqs.req_private      with Req_equal -> Req_matches [f.if_header.header_private     ] | x -> x);
						req_channel_mode = (match reqs.req_channel_mode with Req_equal -> Req_matches [f.if_header.header_channel_mode] | x -> x);
						req_ms           = (match reqs.req_ms           with Req_equal -> Req_matches [f.if_header.header_ms          ] | x -> x);
						req_is           = (match reqs.req_is           with Req_equal -> Req_matches [f.if_header.header_is          ] | x -> x);
						req_copyright    = (match reqs.req_copyright    with Req_equal -> Req_matches [f.if_header.header_copyright   ] | x -> x);
						req_original     = (match reqs.req_original     with Req_equal -> Req_matches [f.if_header.header_original    ] | x -> x);
						req_emphasis     = (match reqs.req_emphasis     with Req_equal -> Req_matches [f.if_header.header_emphasis    ] | x -> x);
					} in
					if debug then printf " resync_here found frame %d here\n" (num_frames - more_frames);

					if more_frames > 1 then (
						(* Get more frames *)
						match find_frame_and_update_reqs (pred more_frames) new_reqs with
						| None -> (seek_in handle start_pos; None) (* Oops. Not enough frames found *)
						| Some (really_new_reqs, _) -> (seek_in handle new_pos; Some (really_new_reqs, f))
					) else (
						(* Just return this frame *)
						Some (new_reqs, f)
					)
				)
				| _ -> (seek_in handle start_pos; None) (* Uhh... right *)
			) in

			let rec find_sync_rec inky = (
				seek_in handle (start_pos + inky);
				match find_frame_and_update_reqs num_frames reqs with
				| None -> (
					if start_pos + inky + 4 >= in_channel_length handle then (
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
				if debug then (
					if wanted = found then (
						printf "Found %d frames starting from %d\n" num_frames wanted
					) else (
						printf "Found %d frames starting from %d (resync needed from %d)\n" num_frames found wanted
					)
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
					let pos_now = pos_in handle in
					match o#get_frame_here req with
					| Fp_none -> (
						(* Oops. Tried to find a frame, but it didn't exist. Sync error *)
						o#resync_here num_frames req
					)
					| Fp_eof -> None
					| Fp_some x -> Some (req, x, (pos_now, pos_now))
				)
			) in
			match frame_info_perhaps with
			| None -> raise End_of_file
			| Some (req2, f, resync_needed) -> (

if debug then (
	let h = f.if_header in
	printf " \"%s\"\n" (to_hex h.header_raw);
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
	printf " Side:  \"%s\"\n" (to_hex f.if_side_raw);
	printf " Data:  \"%s\"\n" (to_hex f.if_data_raw);
	printf " Frame: \"%s\"\n" (to_hex f.if_frame_raw);
);

				(* Update the frame bounds *)
				first_frame_start <- min first_frame_start (snd resync_needed);
				last_frame_end <- max last_frame_end ((snd resync_needed) + String.length f.if_frame_raw - 1);

				if lame_search then (
					(* Convert the frame into an XING frame, if possible *)
					if debug then printf "Searching for XING/LAME tag\n";
					(try
						let rec count_zeros frame_raw now num = (
							if now >= String.length frame_raw || frame_raw.[now] <> '\x00'
								then num
								else count_zeros frame_raw (succ now) (succ num)
						) in
						let num_zeros = count_zeros f.if_frame_raw 6 0 in
						if num_zeros < 7 || num_zeros > 32 then (if debug then printf " XING search found %d zeros (not between 7 and 32)\n" num_zeros; raise Not_found);
						if num_zeros + 10 > String.length f.if_frame_raw then (if debug then printf " XING search found %d bytes in frame; not long enough for XING\n" (String.length f.if_frame_raw); raise Not_found); (* Frame is too short to have an XING tag *)
						let tag_type = String.sub f.if_frame_raw (num_zeros + 6) 4 in
						if tag_type <> "Xing" && tag_type <> "Info" then (if debug then printf " XING search found unknown tag type %S\n" tag_type; raise Not_found);

						let tag_guts = String.sub f.if_frame_raw (num_zeros + 10) (String.length f.if_frame_raw - num_zeros - 10) in

						let tag_pos_ref = ref 4 in (* Where we are in the tag. Pretend that the flags have already been read (0 is right AFTER "Xing" or "Info") *)

						if String.sub tag_guts 0 3 <> "\x00\x00\x00" then (if debug then printf " XING search flags not padded with zeros\n"; raise Not_found);

						(* Read XING flags *)
						let (flag_frames, flag_bytes, flag_toc, flag_quality) = (
							let flags = Char.code tag_guts.[3] in
							if flags > 15 then (if debug then printf " XING search found flags > 15\n"; raise Not_found);
							let flag_frames = (
								if flags land 1 = 1 then (
									if !tag_pos_ref + 4 > String.length tag_guts then (if debug then printf " XING search finds frame flag, but not enough room for them\n"; raise Not_found);
									let p = unpackN tag_guts !tag_pos_ref in
									tag_pos_ref := !tag_pos_ref + 4;
									Some p
								) else (None)
							) in
							let flag_bytes = (
								if flags land 2 = 2 then (
									if !tag_pos_ref + 4 > String.length tag_guts then (if debug then printf " XING search finds byte flag, but not enough room for them\n"; raise Not_found);
									let p = unpackN tag_guts !tag_pos_ref in
									tag_pos_ref := !tag_pos_ref + 4;
									Some p
								) else (None)
							) in
							let flag_toc = (
								if flags land 4 = 4 then (
									if !tag_pos_ref + 100 > String.length tag_guts then (if debug then printf " XING search finds TOC, but not enough room for it\n"; raise Not_found);
									let p = Array.init 100 (fun i -> Char.code tag_guts.[!tag_pos_ref + i]) in
									tag_pos_ref := !tag_pos_ref + 100;
									Some p
								) else (None)
							) in
							let flag_quality = (
								if flags land 8 = 8 then (
									if !tag_pos_ref + 4 > String.length tag_guts then (if debug then printf " XING search finds quality flag, but not enough room for it\n"; raise Not_found);
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
								if debug then printf " XING search has not enough room for encoder; not LAME but still XING\n";
								(None, "")
							) else (
								(* There's enough room for the encoder *)
								let encoder_20 = String.sub tag_guts !tag_pos_ref 20 in

								(* Is there enough room for LAME? *)
								if !tag_pos_ref + 36 > String.length tag_guts then (
									if debug then printf " XING search sees not enough room for LAME; returning XING\n";
									(None, encoder_20)
								) else (
									let lame_part = String.sub tag_guts !tag_pos_ref 36 in (* This starts from the beginning of the encoder string and goes to the tag CRC *)
									let crc_ok = (
										if lame_part.[20] = '\x00' && String.sub lame_part 24 12 = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" && String.sub lame_part 21 3 <> "\x00\x00\x00" then (
											if debug then printf " XING search found LAME frame with only offset info\n";
											true
										) else (
											let tag_crc = unpackn lame_part 34 in
											let real_crc = Crc.create (String.sub f.if_frame_raw 0 (num_zeros + 10 + from_name + 34)) 0 in
											if debug then printf " Written CRC is %04X, real CRC is %04X\n" tag_crc real_crc;
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

		method close = close_in handle
	end
;;
(***********************************************************************

class mp3read_INCOMPLETE ?(debug=false) in_file =
	object o
		val handle = open_in_bin in_file
		val current_header_reqs = req_nothing ()

		(* This will return Fp_some x if frame x matches the requirements *)
		method find_frame_here reqs = (
			let pos_start = LargeFile.pos_in handle in
			try
				let header = String.create 4 in
				really_input handle header 0 4;
				let a = Char.code header.[0] in
				let b = Char.code header.[1] in
				let c = Char.code header.[2] in
				let d = Char.code header.[3] in
				if a <> 255 || b < 224 then (if debug then Printf.printf " SYNC'S BROKEN\n"; LargeFile.seek_in handle pos_start; Fp_none) else (
					if debug then Printf.printf " Sync's right\n";
					try
						if b lsr 3 land 3 = 1 then (if debug then Printf.printf " OOPS! Invalid id 1\n"; raise Not_found);
						let frame_id = mpeg_index.(b lsr 3 land 3) in
						let (samples_per_frame, bitrate_index, samplerate_index) = (match frame_id with
							| (MPEG1 ) -> (1152, [| 0;32;40;48;56;64;80;96;112;128;160;192;224;256;320 |], [| S44100;S48000;S32000 |])
							| (MPEG2 ) -> ( 576, [| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |], [| S22050;S24000;S16000 |])
							| (MPEG25) -> ( 576, [| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |], [| S11025;S12000; S8000 |])
						) in
						if debug then Printf.printf " Frame ID's right (MPEG%s)\n" (match frame_id with MPEG1 -> "1" | MPEG2 -> "2" | _ -> "25");

						if b lsr 1 land 3 <> 1 then (
							if debug then Printf.printf " OOPS! %s\n" [| "RESERVED layer"; "Layer 3?"; "Layer 2"; "Layer 1" |].(b lsr 1 land 3);
							raise Not_found; (* Layer 1 and 2 are no good *)
						) else if debug then (
							Printf.printf " Layer 3\n";
						);

						let frame_crc = (b land 1 = 0) in (* CRC is present if this is a 1! *)
						if debug then Printf.printf " CRC present? %B\n" frame_crc;

						let frame_bitrate = bitrate_index.(c lsr 4 land 15) in
						if frame_bitrate = 0 then (if debug then Printf.printf " OOPS! Invalid frame bitrate\n"; raise Not_found);
						if debug then Printf.printf " Frame bitrate: %d\n" frame_bitrate;

						let frame_samplerate = samplerate_index.(c lsr 2 land 3) in

						if debug then Printf.printf " Frame samplerate: %d\n" (int_of_samplerate frame_samplerate);

						let frame_padding = (c land 2 = 2) in
						let frame_private = (c land 1 = 1) in
						if debug then Printf.printf " Padded? %B\n Private? %B\n" frame_padding frame_private;

						let frame_channel_mode = channel_index.(d lsr 6 land 3) in
						if debug then Printf.printf " %s\n" (match frame_channel_mode with ChannelStereo -> "ChannelStereo" | ChannelJoint -> "ChannelJoint" | ChannelDual -> "ChannelDual" | ChannelMono -> "ChannelMono");

						let frame_ms = (d land 32 = 32) in
						let frame_is = (d land 16 = 16) in
						if debug then Printf.printf " MS? %B\n IS? %B\n" frame_ms frame_is;

						let frame_copyright = (d land 8 = 8) in
						let frame_original = (d land 4 = 4) in
						let frame_emphasis = emphasis_index.(d land 3) in
						if debug then (
							Printf.printf " Copyright? %B\n" frame_copyright;
							Printf.printf " Original?  %B\n" frame_original;
							Printf.printf " Emphasis = %s\n" [|"EmphasisNone";"Emphasis5015";"EmphasisInvalid";"EmphasisCCITT"|].(d land 3);
						);
						if frame_emphasis = EmphasisInvalid then (if debug then Printf.printf " OOPS! Invalid emphasis\n"; raise Not_found);

						if debug then Printf.printf " Frame crc? %B\n" frame_crc;

type header_t = {
	common_side_info_size : int;
	common_bspfk : float; (* Byte seconds per frame kilobit (surprisingly useful) *)
	common_unpadded_frame_length : int -> int; (* Takes a bitrate and returns the total number of bytes in it *)
};;
						let header = {
							header_id = frame_id;
							header_crc = frame_crc;
							header_bitrate = frame_bitrate;
							header_samplerate = frame_samplerate;
							header_padding = frame_padding;
							header_private = frame_private;
							header_channel_mode = frame_channel_mode;
							header_ms = frame_ms;
							header_is = frame_is;
							header_copyright = frame_copyright;
							header_original = frame_original;
							header_emphasis = frame_emphasis;
							header_frame_length = (match frame_samplerate with
								| S48000 | S44100 | S32000 -> 144000 * frame_bitrate / int_of_samplerate frame_samplerate + (if frame_padding then 1 else 0)
								| _ -> 72000 * frame_bitrate / int_of_samplerate frame_samplerate + (if frame_padding then 1 else 0)
							);
							header_samples_per_frame = samples_per_frame;
							header_side_info_size = (match (frame_id, frame_channel_mode) with
								| (MPEG1, ChannelMono) -> 17
								| (  _  , ChannelMono) -> 9
								| (MPEG1,      _     ) -> 32
								| (  _  ,      _     ) -> 17
							);




(*
	req_id = None;
	req_crc = None;
	req_samplerate = None;
	req_padding = None;
	req_private = None;
	req_channel_mode = None;
	req_copyright = None;
	req_original = None;
	req_emphasis = None;
	req_lame = None;
*)
						(* Big requirements-matching statement *)
						match (reqs.req_id, reqs.reqs_crc, reqs.req_samplerate, reqs.req_padding, reqs.req_private, reqs.req_channel_mode, reqs.req_copyright, reqs.req_original, reqs.req_emphasis) with
						| (Some x, _, _, _, _, _, _, _, _) when x <> frame_id
						| (_, Some x, _, _, _, _, _, _, _) when x <> frame_crc
						| (_, _, Some x, _, _, _, _, _, _) when x <> frame_samplerate
						| (_, _, _, Some x, _, _, _, _, _) when x <> frame_padding
						| (_, _, _, _, Some x, _, _, _, _) when x <> frame_private
						| (_, _, _, _, _, Some x, _, _, _) when x <> frame_channel_mode
						| (_, _, _, _, _, _, Some x, _, _) when x <> frame_copyright
						| (_, _, _, _, _, _, _, Some x, _) when x <> frame_original
						| (_, _, _, _, _, _, _, _, Some x) when x <> frame_emphasis -> (
							if debug then Printf.printf "  Frame does not match up with required attributes!\n";
							raise Not_found
						)
						| _ -> (
							let frame_length = match frame_samplerate with
								| S48000 | S44100 | S32000 -> 144000 * frame_bitrate / int_of_samplerate frame_samplerate + (if frame_padding then 1 else 0)
								| _ -> 72000 * frame_bitrate / int_of_samplerate frame_samplerate + (if frame_padding then 1 else 0)
							in
							let raw_frame = String.create frame_length in
							LargeFile.seek_in handle pos_start;
							really_input handle raw_frame frame_length;
							let side_info_size = (match (frame_id, frame_channel_mode) with
								| (MPEG1, ChannelMono) -> 17
								| (  _  , ChannelMono) -> 9
								| (MPEG1,      _     ) -> 32
								| (  _  ,      _     ) -> 17
							) in

							(* Make a LAME parser and a non-LAME parser *)
							let parse_non_lame () = (

							) in

					with
						Not_found -> (if debug then Printf.printf " INVALID FRAME\n"; LargeFile.seek_in handle pos_start; Fp_none)
				)
			with
			| End_of_file -> (LargeFile.seek_in handle pos_start; Fp_eof)
		)

class mp3read_old ?(debug = false) ?(check_frames_for_silence = false) (*?(ignore_lame_crc = true)*) inFile =
	let vastness = 32768 in (* Min buffer size *)
	let too_vast = 2 * vastness in
(*
	let printDebug x = match debug with
		| true  -> Printf.printf x
		| false -> Printf.fprintf nul x
	in
*)
	object(o)
		val handle = open_in_bin inFile
		val buffer = Buffer.create vastness
		val mutable bufferOffset = 0 (* Which byte the start of the buffer represents in the file *)
		val mutable bufferPos = 0 (* Like Perl's pos() function; keeps track of where we are in the buffer *)
		val bitstream = Buffer.create 511
		val mutable frameNumber = 0
		val xingtag : (xingTag_t option) = None

		val mutable valid = false
		val mutable only_checked_xing = true (* Needed because sometimes the XING frame doesn't have CRC but the rest of the frames do *)
		val mutable k = {
			common_id = MPEG1;
			common_bitrate_index = [| |];
			common_samplerate_index = [| |];
			common_samplerate = S44100;
(*			common_crc = false;*)
			common_channel_mode = ChannelStereo;
			common_copyright = false;
			common_original = false;
			common_emphasis = EmphasisNone;
			common_samples_per_frame = 1152;
			common_side_info_size = 0;
			common_bspfk = 0.0;
			common_unpadded_frame_length = (fun x -> 0);
		}

		val mutable first_frame_start = max_int
		val mutable last_frame_end = 0

		val mutable xing_found = false

		(****************************************************)
		(* Removes the buffer up to the bufferPos character *)
		(****************************************************)
		method clean_buffer = (
			if bufferPos > 0 then (
				if debug then Printf.printf "clean_buffer %d bytes\n" bufferPos;
				let tempString = Buffer.sub buffer bufferPos (Buffer.length buffer - bufferPos) in
				Buffer.clear buffer;
				Buffer.add_string buffer tempString;
				bufferOffset <- bufferOffset + bufferPos;
				bufferPos <- 0;
			)
		)

		(************************************************)
		(* Fills the buffer with another vastness bytes *)
		(************************************************)
		method fill_buffer = (
			let bytesLeft = (in_channel_length handle) - bufferOffset - (Buffer.length buffer) in
			if debug then Printf.printf "fill_buffer with %d bytes (%d bytes left)\n" (min bytesLeft vastness) bytesLeft;
			if bytesLeft = 0 then raise End_of_file; (* No more to fill! *)
			Buffer.add_channel buffer handle (min bytesLeft vastness);
			if bufferPos > too_vast then o#clean_buffer;
		)

		(***************************************************************************)
		(* Ensures that there are at least num bytes after the bufferPos character *)
		(***************************************************************************)
		method ensure_bytes num = (
			if debug then Printf.printf "ensure_bytes %d\n" num;
			(* Will exit when the specified amount has been reached, or an End_of_file is generated *)
			while bufferPos + num > Buffer.length buffer do
				if debug then Printf.printf " Fill buffer!\n";
				o#fill_buffer;
			done
		)

		(************************)
		(* Increments bufferPos *)
		(************************)
		method succ_pos = (
			if debug then Printf.printf "succ_pos to %d\n" (succ bufferPos);
			o#ensure_bytes 1;
			bufferPos <- succ bufferPos
		)

		(*******************************************************************************)
		(* Returns the header if the specified string is the beginning of an MP3 frame *)
		(*******************************************************************************)
		method  is_valid_header ?(offset = 0) str = (
			if String.length str < 4 + offset || offset < 0 then None else (
				if debug then Printf.printf "is_valid_header on %S\n" (to_hex (String.sub str 0 4));
				if offset <> 0 then if debug then Printf.printf " at offset %d" offset;
				if debug then Printf.printf " Enough letters\n";
				let a = Char.code str.[0 + offset] in
				let b = Char.code str.[1 + offset] in
				let c = Char.code str.[2 + offset] in
				let d = Char.code str.[3 + offset] in
				if a <> 255 || b < 224 then (if debug then Printf.printf " SYNC'S BROKEN\n"; None) else (
					(* First byte must be a 0xFF, second must be 0b111????? *)
					if debug then Printf.printf " Sync's right\n";

					try (
						let frame_id = mpeg_index.(b lsr 3 land 3) in
						if b lsr 3 land 3 = 1 then raise Not_found;
						let (samples_per_frame, bitrate_index, samplerate_index) = (match (valid, frame_id) with
							| (true, x) when x = k.common_id -> (k.common_samples_per_frame, k.common_bitrate_index, k.common_samplerate_index)
							| (true, x) -> (if debug then Printf.printf " OOPS! Invalid MPEG ID\n"; raise Not_found)
							| (false, MPEG1 ) -> (1152, [| 0;32;40;48;56;64;80;96;112;128;160;192;224;256;320 |], [| S44100;S48000;S32000 |])
							| (false, MPEG2 ) -> ( 576, [| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |], [| S22050;S24000;S16000 |])
							| (false, MPEG25) -> ( 576, [| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |], [| S11025;S12000; S8000 |])
						) in
						if debug then Printf.printf " Frame ID's right (MPEG%s)\n" (match frame_id with MPEG1 -> "1" | MPEG2 -> "2" | _ -> "25");

						if b lsr 1 land 3 <> 1 then (
							if debug then Printf.printf " OOPS! %s\n" [| "RESERVED layer"; "Layer 3?"; "Layer 2"; "Layer 1" |].(b lsr 1 land 3);
							raise Not_found; (* Layer 1 and 2 are no good *)
						) else if debug then (
							Printf.printf " Layer 3\n";
						);

						let frame_crc = (b land 1 = 0) in (* CRC is present if this is a 1! *)
						if debug then Printf.printf " CRC present? %B\n" frame_crc;

						let frame_bitrate = bitrate_index.(c lsr 4 land 15) in
						if frame_bitrate = 0 then (if debug then Printf.printf " OOPS! Invalid frame bitrate\n"; raise Not_found);
						if debug then Printf.printf " Frame bitrate: %d\n" frame_bitrate;

						let frame_samplerate = samplerate_index.(c lsr 2 land 3) in

						if debug then Printf.printf " Frame samplerate: %d\n" (int_of_samplerate frame_samplerate);

						let frame_padding = (c land 2 = 2) in
						let frame_private = (c land 1 = 1) in
						if debug then Printf.printf " Padded? %B\n Private? %B\n" frame_padding frame_private;

						let frame_channel_mode = channel_index.(d lsr 6 land 3) in
						if debug then Printf.printf " %s\n" (match frame_channel_mode with ChannelStereo -> "ChannelStereo" | ChannelJoint -> "ChannelJoint" | ChannelDual -> "ChannelDual" | ChannelMono -> "ChannelMono");

						let frame_ms = (d land 32 = 32) in
						let frame_is = (d land 16 = 16) in
						if debug then Printf.printf " MS? %B\n IS? %B\n" frame_ms frame_is;

						let frame_copyright = (d land 8 = 8) in
						let frame_original = (d land 4 = 4) in
						let frame_emphasis = emphasis_index.(d land 3) in
						if debug then (
							Printf.printf " Copyright? %B\n" frame_copyright;
							Printf.printf " Original?  %B\n" frame_original;
							Printf.printf " Emphasis = %s\n" [|"EmphasisNone";"Emphasis5015";"EmphasisInvalid";"EmphasisCCITT"|].(d land 3);
						);
						if frame_emphasis = EmphasisInvalid then (if debug then Printf.printf " OOPS! Invalid emphasis\n"; raise Not_found);

						if debug then Printf.printf "  Valid?     %B\n" valid;
						if debug then Printf.printf "  Only XING? %B\n" only_checked_xing;
						if debug then Printf.printf "  Frame crc? %B\n" frame_crc;

						if valid && not only_checked_xing then (
							(* The file data's valid, so check against it *)
							if (
								frame_samplerate <> k.common_samplerate ||
								frame_channel_mode <> k.common_channel_mode ||
								frame_copyright <> k.common_copyright ||
(*								frame_original <> k.common_original ||*)
								frame_emphasis <> k.common_emphasis
							) then (
								raise Not_found
							);
						) else (
							(* Rewrite k *)
							valid <- true;
							k <- {
								common_id = frame_id;
								common_bitrate_index = bitrate_index;
								common_samplerate_index = samplerate_index;
								common_samplerate = frame_samplerate;
								common_channel_mode = frame_channel_mode;
								common_copyright = frame_copyright;
								common_original = frame_original;
								common_emphasis = frame_emphasis;
								common_samples_per_frame = samples_per_frame;
								common_side_info_size = (match (frame_id, frame_channel_mode) with
									| (MPEG1, ChannelMono) -> 17
									| (  _  , ChannelMono) -> 9
									| (MPEG1,      _     ) -> 32
									| (  _  ,      _     ) -> 17
								);
								common_bspfk = float_of_int (samples_per_frame * 125) /. float_of_samplerate frame_samplerate;
								common_unpadded_frame_length = (if int_of_samplerate frame_samplerate < 32000
									then (fun x -> 72000 * x / int_of_samplerate frame_samplerate)
									else (fun x -> 144000 * x / int_of_samplerate frame_samplerate)
								);
							};
						);
(*
						if valid then (
							if (
(*								frame_crc <> k.common_crc ||*)
								frame_samplerate <> k.common_samplerate ||
								frame_channel_mode <> k.common_channel_mode ||
								frame_copyright <> k.common_copyright ||
								frame_original <> k.common_original ||
								frame_emphasis <> k.common_emphasis
							) then (
								raise Not_found
							);
(*
							if only_checked_xing then (
								(* Update the CRC flag with the actual one if the previous one was based on an XING tag *)
								if debug then Printf.printf " Setting the actual CRC to '%B'\n" frame_crc;
								k.common_crc <- frame_crc;
								only_checked_xing <- false;
							) else if k.common_crc <> frame_crc then (
								if debug then Printf.printf " OOPS. Common CRC is %b, current CRC is %b; invalid frame\n" k.common_crc frame_crc;
								raise Not_found
							) (* else everybody's happy *)
*)
						) else (
							valid <- true;

							k <- {
								common_id = frame_id;
								common_bitrate_index = bitrate_index;
								common_samplerate_index = samplerate_index;
								common_samplerate = frame_samplerate;
(*								common_crc = frame_crc;*)
								common_channel_mode = frame_channel_mode;
								common_copyright = frame_copyright;
								common_original = frame_original;
								common_emphasis = frame_emphasis;
								common_samples_per_frame = samples_per_frame;
								common_side_info_size = (match (frame_id, frame_channel_mode) with
									| (MPEG1, ChannelMono) -> 17
									| (  _  , ChannelMono) -> 9
									| (MPEG1,      _     ) -> 32
									| (  _  ,      _     ) -> 17
								);
								common_bspfk = float_of_int (samples_per_frame * 125) /. float_of_samplerate frame_samplerate;
								common_unpadded_frame_length = (if int_of_samplerate frame_samplerate < 32000
									then (fun x -> 72000 * x / int_of_samplerate frame_samplerate)
									else (fun x -> 144000 * x / int_of_samplerate frame_samplerate)
								);
							};

						);
*)
						(* Now find out the frame size for this frame *)
						let frame_size = k.common_unpadded_frame_length frame_bitrate + (if frame_padding then 1 else 0) in

						Some {
							headerCommon = k;
							headerCRC = frame_crc; (* Back by popular demand *)
							headerBitrate = frame_bitrate;
							headerPadding = frame_padding;
							headerPrivate = frame_private;
							headerMS = frame_ms;
							headerIS = frame_is;
							headerFrameLength = frame_size;
						};
					) with
					| _ -> (if debug then Printf.printf " SOMETHING WENT WRONG\n"; None) (* If anything went wrong, there's no header here *)
				)
			)
		)

		(*******************************************************************)
		(* Does the is_valid_header for the current location in the buffer *)
		(*******************************************************************)
		method check_buffer_for_header = (
			if debug then Printf.printf "check_buffer_for_header at %d\n" bufferPos;
			o#ensure_bytes 4;
			o#is_valid_header (Buffer.sub buffer bufferPos 4)
		)

		(******************************************************************************************)
		(* Similar to check_buffer_for_header, but returns the entire frame and updates bufferPos *)
		(******************************************************************************************)
		method check_buffer_for_frame = (
			if debug then Printf.printf "check_buffer_for_frame at %d (%d)\n" bufferPos (bufferPos + bufferOffset);
			let this_frame_start = bufferPos + bufferOffset in
			match o#check_buffer_for_header with
			| None -> (
				bufferPos <- succ bufferPos;
				None (* No header found, therefore no frame found *)
			)
			| Some head -> (
				o#ensure_bytes head.headerFrameLength; (* Make sure there's enough room for the frame *)
				let rawFrame = Buffer.sub buffer bufferPos head.headerFrameLength in
				bufferPos <- bufferPos + head.headerFrameLength;
				if debug then Printf.printf " Found a valid header\n";

				(* Update the frame extents *)
				first_frame_start <- min first_frame_start this_frame_start;
				last_frame_end <- bufferPos + bufferOffset - 1;
				if debug then Printf.printf "  [%d %d]\n" first_frame_start last_frame_end;

				let side = String.sub rawFrame (if head.headerCRC then 6 else 4) k.common_side_info_size in
				let rest = String.sub rawFrame (if head.headerCRC then (6 + k.common_side_info_size) else (4 + k.common_side_info_size)) (String.length rawFrame - k.common_side_info_size - (if head.headerCRC then 6 else 4)) in
				if debug then Printf.printf " Side info %S\n" side;
				if debug then Printf.printf " Frame     %S\n" rest;

				(* Find out the position and length of the frame's data *)
				let (offset, bits) = match (k.common_id, k.common_channel_mode) with
					| (MPEG1, ChannelMono) -> (
						let off = unpackBits side 0 9 in
						if debug then Printf.printf "  Offset %d\n" off;
						let g1 = unpackBits side 18 12 in
						let g2 = unpackBits side 77 12 in
						if debug then Printf.printf "  Lengths %d,%d\n" g1 g2;
						(off, g1 + g2)
					)
					| (MPEG1, _) -> ( (* Stereo MPEG1, by far the most common *)

(* DELETEME *)
(*
(
	let x = 21 in
	let y =  8 in
	packBits side ( 20 + x) y 0;
	packBits side ( 79 + x) y 0;
	packBits side (138 + x) y 0;
	packBits side (197 + x) y 0;
);
(
	let x = 33 in
	let y = 23 in
	packBits side ( 20 + x) y 0;
	packBits side ( 79 + x) y 0;
	packBits side (138 + x) y 0;
	packBits side (197 + x) y 0;
);
*)
(* DELETEME *)

						let off = unpackBits side 0 9 in
						if debug then Printf.printf "  Offset %d\n" off;
						let g1 = unpackBits side  20 12 in
						let g2 = unpackBits side  79 12 in
						let g3 = unpackBits side 138 12 in
						let g4 = unpackBits side 197 12 in
						if debug then Printf.printf "  Lengths %d,%d,%d,%d\n" g1 g2 g3 g4;
						(off, g1 + g2 + g3 + g4)
					)
					| (_, ChannelMono) -> (
						let off = unpackBits side 0 8 in
						if debug then Printf.printf "  Offset %d\n" off;
						let g1 = unpackBits side  9 12 in
						if debug then Printf.printf "  Lengths %d\n" g1;
						(off, g1)
					)
					| (_, _) -> (
						let off = unpackBits side 0 8 in
						if debug then Printf.printf "  Offset %d\n" off;
						let g1 = unpackBits side 10 12 in
						let g2 = unpackBits side 73 12 in
						if debug then Printf.printf "  Lengths %d,%d\n" g1 g2;
						(off, g1 + g2)
					)
				in

				(* Find an XING tag *)
				try (
					if xing_found then raise Not_found; (* There shouldn't be two XING frames in a file *)
					let rec count_zeros rawFrame now num = (
						if now >= String.length rawFrame || rawFrame.[now] <> '\x00'
							then num
							else count_zeros rawFrame (succ now) (succ num)
					) in
					let num_zeros = count_zeros rawFrame 6 0 in
					if num_zeros < 7 || num_zeros > 32 then raise Not_found;
					if num_zeros + 10 > String.length rawFrame then raise Not_found; (* The frame is not large enough to have an XING tag in it *)
					let tag_type = String.sub rawFrame (num_zeros + 6) 4 in
					if tag_type <> "Xing" && tag_type <> "Info" then raise Not_found;
					if debug then Printf.printf " Looks like an %S frame\n" tag_type;

					let tag_guts = String.sub rawFrame (num_zeros + 10) (String.length rawFrame - num_zeros - 10) in

					(* Read the XING tag *)
					if String.length tag_guts < 4 then raise Not_found;
					if String.sub tag_guts 0 3 <> "\x00\x00\x00" then raise Not_found; (* The 3 MSB of the flag field are always 0 *)

					(* XING flags *)
					let flags = Char.code tag_guts.[3] in
					if flags > 15 then raise Not_found; (* There are only 4 flags defined *)
					let tag_pos_ref = ref 4 in (* Where we are in the tag *)
					let flag_frames_ref = ref None in (* If the number of frames is stored, this will be "ref Some #" *)
					let flag_bytes_ref = ref None in
					let flag_toc_ref = ref None in
					let flag_quality_ref = ref None in
					if flags land 1 = 1 then (
						if !tag_pos_ref + 4 > String.length tag_guts then (if debug then Printf.printf "  String too short for number of frames. Not XING\n"; raise Not_found);
						if debug then Printf.printf "  Found %d frames\n" (unpackN tag_guts !tag_pos_ref);
						flag_frames_ref := Some (unpackN tag_guts !tag_pos_ref);
						tag_pos_ref := !tag_pos_ref + 4;
					);
					if flags land 2 = 2 then (
						if !tag_pos_ref + 4 > String.length tag_guts then (if debug then Printf.printf "  String too short for number of bytes. Not XING\n"; raise Not_found);
						if debug then Printf.printf "  Found %d bytes\n" (unpackN tag_guts !tag_pos_ref);
						flag_bytes_ref := Some (unpackN tag_guts !tag_pos_ref);
						tag_pos_ref := !tag_pos_ref + 4;
					);
					if flags land 4 = 4 then (
						if !tag_pos_ref + 100 > String.length tag_guts then (if debug then Printf.printf "  String too short for TOC. Not XING\n"; raise Not_found);
						if debug then Printf.printf "  Found TOC\n";
						flag_toc_ref := Some (Array.init 100 (fun i -> Char.code tag_guts.[!tag_pos_ref + i]));
						tag_pos_ref := !tag_pos_ref + 100;
					);
					if flags land 8 = 8 then (
						if !tag_pos_ref + 4 > String.length tag_guts then (if debug then Printf.printf "  String too short for quality. Not XING\n"; raise Not_found);
						if debug then Printf.printf "  Found quality %d\n" (unpackN tag_guts !tag_pos_ref);
						flag_quality_ref := Some (unpackN tag_guts !tag_pos_ref);
						tag_pos_ref := !tag_pos_ref + 4;
					);

					let from_name = !tag_pos_ref in (* Used for the CRC calculation *)

					let (lame_part, encoder_20) = if !tag_pos_ref + 20 > String.length tag_guts then (
						if debug then Printf.printf "  Not enough bytes for the encoder string; can't be LAME tag (but is still XING!)\n";
						("", "")
					) else (
						let encoder_20 = String.sub tag_guts !tag_pos_ref 20 in
						if debug then Printf.printf "  Encoder %S\n" encoder_20;
						tag_pos_ref := !tag_pos_ref + 20;

						(* Check to see if it's a LAME tag *)
						let lame_part = String.sub tag_guts !tag_pos_ref (String.length tag_guts - !tag_pos_ref) in
						let lame_zeros = count_zeros lame_part 0 0 in
						if debug then Printf.printf "  %d zeros at beginning of LAME part\n" lame_zeros;

						(* Check the Lame CRC *BEFORE* parsing the info; if the CRC doesn't match up then assume it's regular XING *)
						(* EXCEPT if ignore_lame_crc is selected *)
						(* (num_zeros is the position of the "Xing" or "Info" tags counting from byte 6) *)
						let crc_ok = (
							if String.length lame_part < 16 then (
								(* The frame can't be LAME, because there is not enough room for it *)
								if debug then Printf.printf "  Frame can't be LAME: not enough bytes\n";
								false
							) else if lame_part.[0] = '\x00' && String.sub lame_part 4 12 = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" && String.sub lame_part 1 3 <> "\x00\x00\x00" then (
								if debug then Printf.printf "  Looks like a LAME frame with only offset info\n";
								true
(*
							) else if ignore_lame_crc then (
								if debug then Printf.printf "  LAME frame forced!\n";
								true
*)
							) else (
								let tag_crc = unpackn lame_part 14 in
								if debug then Printf.printf "  Tag CRC: %d\n" tag_crc;
								let crc_string = String.sub rawFrame 0 (num_zeros + 10 + from_name + 34) in
								let crc = Crc.create crc_string 0 in
								if debug then Printf.printf "   ACTUAL tag CRC: %d\n" crc;
								crc = tag_crc
							)
						) in
						if crc_ok && lame_zeros < String.length lame_part && String.length lame_part >= 16 then (
							(* LAME! *)
							(lame_part, encoder_20)
						) else (
							(* Not LAME, but has an encoder *)
							("", encoder_20)
						)
					) in

					if lame_part <> "" then (
						(* LAME part of the tag is non-zero, and there are at least 16 bytes left, which implies that there is LAME data present *)
						if debug then Printf.printf "   That means it's LAME!\n";

						(* Re-read the 20-byte encoder string, as some of it is used for LAME thingies *)
						let encoder_9       = String.sub encoder_20 0 9 in
						let rg_max_int      =  unpackN32 encoder_20 11 in
						let rg_track        =  unpackn encoder_20 15 in
						let rg_album        =  unpackn encoder_20 17 in
						let lame_flags      =  unpackC encoder_20 19 in
						let abr_bitrate     =  unpackC lame_part 0 in
						let padding1        =  unpackC lame_part 1 in
						let padding2        =  unpackC lame_part 2 in
						let padding3        =  unpackC lame_part 3 in
						let lame_misc       =  unpackC lame_part 4 in
						let mp3_gain        =  unpackC lame_part 5 in
						let preset_bytes    =  unpackn lame_part 6 in
						let music_length    =  unpackN lame_part 8 in
						let music_crc       =  unpackn lame_part 12 in
						let tag_crc         =  unpackn lame_part 14 in

						if debug then (
							Printf.printf "   Encoder's actually %S\n" encoder_9;
							Printf.printf "   Tag revision %d\n" ((unpackC encoder_20 9) lsr 4);
							Printf.printf "   VBR method %d\n" ((unpackC encoder_20 9) land 0x0F);
							Printf.printf "   Lowpass %d\n" (unpackC encoder_20 10);
							Printf.printf "   RG Max:   %ld = %g\n" rg_max_int (Int32.float_of_bits rg_max_int);
							Printf.printf "   RG Track: %d\n" rg_track;
							Printf.printf "   RG Album: %d\n" rg_album;
							Printf.printf "   LAME lame_NSpsy      : %B\n" (lame_flags land  16 <> 0);
							Printf.printf "   LAME lame_NSsj       : %B\n" (lame_flags land  32 <> 0);
							Printf.printf "   LAME lame_nogap_next : %B\n" (lame_flags land  64 <> 0);
							Printf.printf "   LAME lame_nogap_prev : %B\n" (lame_flags land 128 <> 0);
							Printf.printf "   ATH type %d\n" (lame_flags land 15);
							Printf.printf "   ABR bitrate %d\n" abr_bitrate;
							Printf.printf "   %d off the front; %d off the end\n" ((padding1 lsl 4) lor (padding2 lsr 4)) (((padding2 land 15) lsl 8) lor padding3);
							Printf.printf "   Noise %d, Stereo %d, Unwise %B, source freq %d\n" (lame_misc land 3) (lame_misc lsr 2 land 7) ((lame_misc land 32 <> 0)) (lame_misc lsr 6);
							Printf.printf "   MP3 Gain: %d\n" mp3_gain;
							Printf.printf "   Surround %d, preset %d\n" (preset_bytes lsr 11 land 7) (preset_bytes land 2047);
							Printf.printf "   Music takes up %d bytes\n" music_length;
							Printf.printf "   Music CRC: %d\n" music_crc;
							Printf.printf "   Tag CRC: %d\n" tag_crc;
						);

						(
							Some {
								frameHeader = head;
								frameXing = Some {
									xingRawTag = tag_type ^ tag_guts;
									xingTagType = tag_type;
									xingNumFrames = !flag_frames_ref;
									xingNumBytes = !flag_bytes_ref;
									xingTOC = !flag_toc_ref;
									xingQuality = !flag_quality_ref;
									xingEncoder = encoder_9;
									xingLame = Some {
										lameRevision        = ((unpackC encoder_20 9) lsr 4);
										lameVBRMethod       = ((unpackC encoder_20 9) land 0x0F);
										lameLowpass         = (unpackC encoder_20 10);
										lamePeakAmplitude   = Int32.float_of_bits rg_max_int;
										lameRGTrack         = rg_track;
										lameRGAlbum         = rg_album;
										lameNSPsyTune       = (lame_flags land  16 <> 0);
										lameNSSafeJoint     = (lame_flags land  32 <> 0);
										lameNoGapPrev       = (lame_flags land 128 <> 0);
										lameNoGapNext       = (lame_flags land  64 <> 0);
										lameATHType         = lame_flags land 15;
										lameABRBitrate      = abr_bitrate;
										lameDelayStart      = (padding1 lsl 4) lor (padding2 lsr 4);
										lameDelayEnd        = ((padding2 land 15) lsl 8) lor padding3;
										lameNoiseShaping    = lame_misc land 3;
										lameStereoMode      = lame_misc lsr 2 land 7;
										lameUnwise          = (lame_misc land 32 <> 0);
										lameSourceFrequency = lame_misc lsr 6;
										lameMP3Gain         = mp3_gain;
										lameSurround        = preset_bytes lsr 11 land 7;
										lamePreset          = preset_bytes land 2047;
										lameMusicLength     = music_length;
										lameMusicCRC        = music_crc;
									};
								};
								frameSide = {
									sideRaw = side;
									sideDataOffset = offset;
									sideDataBits = bits;
								};
								frameData = rest;
							}
						)
					) else (
						if debug then Printf.printf "   Not LAME enough\n";
						(
							Some {
								frameHeader = head;
								frameXing = Some {
									xingRawTag = tag_type ^ tag_guts;
									xingTagType = tag_type;
									xingNumFrames = !flag_frames_ref;
									xingNumBytes = !flag_bytes_ref;
									xingTOC = !flag_toc_ref;
									xingQuality = !flag_quality_ref;
									xingEncoder = encoder_20;
									xingLame = None;
								};
								frameSide = {
									sideRaw = side;
									sideDataOffset = offset;
									sideDataBits = bits;
								};
								frameData = rest;
							}
						)
					);
				) with
				| Not_found -> (
					(* Not an XING frame *)
					if debug then Printf.printf " Not an XING frame\n";

					(* Check for VBRI header??? *)
					let is_vbri = (try
						(* Start checking from 6 due to possibility of CRC *)
						if only_checked_xing && String.length rawFrame >= 62 && String.sub rawFrame 6 36 = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000VBRI\000\001" then (
							(* Maybe! *)
							let table_size = unpackn rawFrame 54 in
							let entry_bytes = unpackn rawFrame 58 in
							if String.length rawFrame >= 62 + table_size * entry_bytes then (
								(* Check the bytes after the VBRI header *)
								let after_bytes = (String.sub rawFrame (62 + table_size * entry_bytes) (String.length rawFrame - 62 - table_size * entry_bytes)) in
								let zero_bytes = String.make (String.length after_bytes) '\x00' in
								if after_bytes = zero_bytes then (
									if debug then Printf.printf " VBRI FRAME! Throw out\n";
									true
								) else (
									if debug then Printf.printf " Not a VBRI frame either (bytes after frame are not null)\n";
									false
								)
							) else (
								if debug then Printf.printf " Not a VBRI frame either (not enough room to fit offset table)\n";
								false
							)
						) else (
							(* "VBRI" not found or weird version *)
							false
						)
					with
						x -> (
							if debug then Printf.printf " Not a VBRI frame either (croaked with %s)\n" (Printexc.to_string x);
							false
						)
					) in

					if is_vbri then (
						(* Ignore the VBRI header *)
						None
					) else (
						only_checked_xing <- false; (* Frame is not an XING tag *)
						Some {
							frameHeader = head;
							frameXing = None;
							frameSide = {
								sideRaw = side;
								sideDataOffset = offset;
								sideDataBits = bits;
							};
							frameData = rest;
						}
					)
				)
			)
		)

		method find_next_frame = (
			let rec guts sync_error = (
				(* A recursive function which calls check_buffer_for_frame until a frame has been found *)
				let current_location = bufferPos + bufferOffset in
				if debug then Printf.printf "find_next_frame at %d\n" current_location;
				match o#check_buffer_for_frame with
				| None -> guts true
				| Some x -> (sync_error, x)
			) in
			guts false
		)

		method close = close_in handle

		method first_mp3_byte = first_frame_start
		method last_mp3_byte = last_frame_end
		method length = in_channel_length handle
		method pos = bufferOffset + bufferPos

		method print_buffer = (
			if debug then Printf.printf "Buffer:\n%S\n" (to_hex (Buffer.contents buffer))
		)
	end;;
**********************************************************************)


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
