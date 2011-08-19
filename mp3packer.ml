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

open Mp3read;;
open Mp3queue;;
open Mp3info;;
(*
let t1 = Unix.gettimeofday ();;
*)
let version = "1.20-195";;

let padding = Printf.sprintf "mp3packer%s\n" version;;

let usage_head = Printf.sprintf "\nMP3 Packer version %s\nCopyright 2006-2008 Reed \"Omion\" Wilson\nThis program is covered under the GNU GPL.\nSee gpl.txt or mp3packer.html for more information\nNumber of processors detected: %d\n" version Mp3types.detected_processors;;

(*****************)
(* PARSE OPTIONS *)
(*****************)
type keep_ok_t = Keep_ok_output | Keep_ok_both;;
type keep_notok_t = Keep_notok_input | Keep_notok_output | Keep_notok_both;;

let min_bitrate_ref = ref 0;;
let delete_begin_ref = ref false;;
let delete_end_ref = ref false;;
(*
let delete_input_ref = ref false;;
let delete_bad_output_ref = ref false;;
*)
let keep_ok_ref = ref Keep_ok_both;;
let keep_notok_ref = ref Keep_notok_both;;
let force_overwrite_ref = ref false;;
let only_info_ref = ref false;;
let only_info_bitrate_ref = ref false;;
let debug_in_ref = ref false;;
let debug_out_ref = ref false;;
let debug_recompress_ref = ref false;;
let append_ref = ref "-vbr";;
let do_files_with_appended_string_ref = ref false;;
let rename_input_ref = ref false;;
let recompress_ref = ref false;;
let zero_whole_bad_frame_ref = ref false;;
let minimize_bit_reservoir_option_ref = ref None;;

let niceness_ref = ref 10;;

type io_t = IO_File of string | IO_Dir of string;;
let string_of_io = function
	| IO_File x -> Printf.sprintf "File '%s'" x
	| IO_Dir x -> Printf.sprintf "Dir '%s'" x
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



let in_name_ref = ref None;;
let out_name_ref = ref None;;

let debug_parse = function
	| "in" ->   (debug_in_ref := true)
	| "out" ->  (debug_out_ref := true)
	| "huff" -> (debug_recompress_ref := true)
	| "all" ->  (debug_in_ref := true;  debug_out_ref := true; debug_recompress_ref := true)
	| _ ->      (debug_in_ref := false; debug_out_ref := false; debug_recompress_ref := false)
;;

let keep_ok_parse = function
(*	| "input" | "in" -> keep_ok_ref := Keep_ok_input*)
	| "output" | "out" -> keep_ok_ref := Keep_ok_output
	| _ -> keep_ok_ref := Keep_ok_both
;;
let keep_notok_parse = function
	| "input" | "in" -> keep_notok_ref := Keep_notok_input
	| "output" | "out" -> keep_notok_ref := Keep_notok_output
	| _ -> keep_notok_ref := Keep_notok_both
;;

let args = Arg.align [
	("-b", Arg.Int (fun x -> min_bitrate_ref := max 0 x), "# Minimum bitrate allowed for output. Defaults to 0");
	("-t", Arg.Set delete_begin_ref,                       " Strip non-mp3 data at the beginning (mainly ID3v2 tags)");
	("-s", Arg.Set delete_end_ref,                         " Strip non-mp3 data at the end (mainly tags)");
	("-z", Arg.Set recompress_ref,                         " Recompress frames to find optimal settings (takes time)");
	("-a", Arg.Set_string append_ref,              "\"-vbr\" Changes the string to append to the filename in certain cases");
	("-A", Arg.Set do_files_with_appended_string_ref,      " Don't skip files in directory which already have the -a string");
	("-u", Arg.Set rename_input_ref,                       " Updates the input file and renames the orig to the output name");
	("-w", Arg.Set zero_whole_bad_frame_ref,               " Silences a whole frame on buffer error. Otherwise skips granules");
	("-r", Arg.Unit (fun () -> minimize_bit_reservoir_option_ref := Some true),  " Minimizes bit reservoir all the time");
	("-R", Arg.Unit (fun () -> minimize_bit_reservoir_option_ref := Some false), " Maximizes bit reservoir all the time");
	("--keep-ok", Arg.String keep_ok_parse,           "\"x\" What files to keep when no errors occur. x = (out | both)");
	("--keep-bad", Arg.String keep_notok_parse,       "\"x\" What files to keep when errors occur. x = (in | out | both)");
	("-f", Arg.Set force_overwrite_ref,                    " Force overwriting of output files");
	("-i", Arg.Set only_info_ref,                          " Print info and exit (no processing)");
	("--ib", Arg.Set only_info_bitrate_ref,                " Print only the min CBR bitrate (similar to -i)");
	("--nice",  Arg.Set_int niceness_ref,                "10 Priority of the encoding. 0 = normal, 19 = idle");
	("--debug", Arg.String debug_parse,               "\"x\" Print a bunch of garbage while processing. x = (in | out | all)")
];;

let find_file x = (try
	Some (Unix.stat x).Unix.st_kind
with
	| Unix.Unix_error _ -> None
);;

Arg.parse args (fun w ->
	let x = strip_trailing_slash w in
	match (!in_name_ref, !out_name_ref, find_file x) with
	| (None, None, None) -> (failwith (Printf.sprintf "'%s' does not exist" x))
	| (Some (IO_File _), None, None) -> out_name_ref := Some (IO_File x)
	| (Some (IO_File _), None, Some Unix.S_REG) -> out_name_ref := Some (IO_File x) (* Output file already exists *)
	| (Some (IO_File _), None, Some Unix.S_DIR) -> out_name_ref := Some (IO_Dir x) (* Use the same name, but in a different directory *)
	| (Some (IO_Dir _), None, None) -> out_name_ref := Some (IO_Dir x) (* Make up a directory *)
	| (Some (IO_Dir _), None, Some Unix.S_REG) -> out_name_ref := Some (IO_File x) (* Directory -> file; not allowed, but catch later *)
	| (Some (IO_Dir _), None, Some Unix.S_DIR) -> out_name_ref := Some (IO_Dir x) (* One dir to another *)
	| (Some _, Some _, _) -> Printf.printf "WARNING: too many arguments; '%s' ignored\n" x
	| (None, _, None) -> Printf.printf "WARNING: '%s' does not exist; ignoring\n" x
	| (None, _, Some Unix.S_REG) -> in_name_ref := Some (IO_File x)
	| (None, _, Some Unix.S_DIR) -> in_name_ref := Some (IO_Dir x)
	| (_, _, Some _) -> (failwith (Printf.sprintf "'%s' is not a file or directory" x))
) (usage_head);;

(* Set the priority *)
ignore (Mp3types.nice !niceness_ref);;


(*
	This will minimize the bitrate reservoir when a minimum bitrate is specified,
	which is exactly when it is the most likely to help and should take the least amount of memory.
	If a minimum bitrate is not specified then the reservoir is maximized, just like before.
*)
let minimize_bit_reservoir = (match !minimize_bit_reservoir_option_ref with
	| None -> !min_bitrate_ref > 0
	| Some x -> x
);;


let print_errors = function
	| (0,0) -> ()
	| (0,1) -> Printf.printf "\nWARNING: There was 1 sync error\n"
	| (0,s) -> Printf.printf "\nWARNING: There were %d sync errors\n" s
	| (1,0) -> Printf.printf "\nWARNING: There was 1 buffer error\n"
	| (1,1) -> Printf.printf "\nWARNING: There was 1 buffer error and 1 sync error\n"
	| (1,s) -> Printf.printf "\nWARNING: There was 1 buffer error and %d sync errors\n" s
	| (b,0) -> Printf.printf "\nWARNING: There were %d buffer errors\n" b
	| (b,1) -> Printf.printf "\nWARNING: There was 1 sync error and %d buffer errors\n" b
	| (b,s) -> Printf.printf "\nWARNING: There were %d buffer errors and %d sync errors\n" b s
;;


(* Now see what the user wants *)
let do_base = if !only_info_ref || !only_info_bitrate_ref then (
	(* In order to generalize the do_base function, just ignore the second (output) string if user only wants info *)
	fun a b -> (
		let errors = do_info ~only_bitrate:!only_info_bitrate_ref ~debug_in:(!debug_in_ref) ~debug_info:(!debug_out_ref) a in
		print_errors errors;
		()
(*		errors*)
	)
) else if !rename_input_ref then (
	(* Rename the input file, and do_queue backwards to update the original filename *)
	fun a b -> (
		if Sys.file_exists b then Sys.remove b;
		Sys.rename a b;
		let errors = (try
			do_queue ~debug_in:(!debug_in_ref) ~debug_queue:(!debug_out_ref) ~min_bitrate:(!min_bitrate_ref) ~delete_beginning_junk:(!delete_begin_ref) ~delete_end_junk:(!delete_end_ref) ~padding:padding ~recompress:(!recompress_ref) ~debug_recompress:(!debug_recompress_ref) ~zero_whole_bad_frame:(!zero_whole_bad_frame_ref) ~minimize_bit_reservoir:minimize_bit_reservoir b a
		with
			(* The only time an End_of_file is reached is when no valid frames were found. Therefore, it's sort of a sync error... *)
			End_of_file -> (Printf.printf "\nWARNING: No valid MP3 headers found\n"; (0,1))
		) in

		(match (errors, !keep_ok_ref, !keep_notok_ref) with
			| ((0,0), Keep_ok_output, _) -> (Printf.printf "Repacking successful; deleting backup\n"; Sys.remove b)
			| (_, _, Keep_notok_output) -> (Printf.printf "Repacking not successful, but deleting backup anyway\n"; Sys.remove b)
			| (_, _, Keep_notok_input) -> (Printf.printf "Repacking NOT successful; deleting output file\n"; Sys.remove a)
			| _ -> () (* Keep everything *)
		);
(*
		if !delete_input_ref && errors = None then (
			Printf.printf "Repacking successful; deleting backup\n" b;
			Sys.remove b;
		) else if !delete_bad_output_ref && errors <> None then (
			Printf.printf "Repacking NOT successful; deleting output file\n" a;
			Sys.remove a;
		);
*)
		print_errors errors;

		()
(*		errors*)
	)
) else (
	(* Regular *)
	fun a b -> (
		let errors = (try
			do_queue ~debug_in:(!debug_in_ref) ~debug_queue:(!debug_out_ref) ~min_bitrate:(!min_bitrate_ref) ~delete_beginning_junk:(!delete_begin_ref) ~delete_end_junk:(!delete_end_ref) ~padding:padding ~recompress:(!recompress_ref) ~debug_recompress:(!debug_recompress_ref) ~zero_whole_bad_frame:(!zero_whole_bad_frame_ref) ~minimize_bit_reservoir:minimize_bit_reservoir a b
		with
			End_of_file -> (Printf.printf "\nWARNING: No valid MP3 headers found\n"; (0,1))
		) in

		(match (errors, !keep_ok_ref, !keep_notok_ref) with
			| ((0,0), Keep_ok_output, _) -> (Printf.printf "Repacking successful; deleting input file\n"; Sys.remove a)
			| (_, _, Keep_notok_output) -> (Printf.printf "Repacking not successful, but deleting input anyway\n"; Sys.remove a)
			| (_, _, Keep_notok_input) -> (Printf.printf "Repacking NOT successful; deleting output file\n"; Sys.remove b)
			| _ -> () (* Keep everything *)
		);

		print_errors errors;


(*
		if !delete_input_ref && errors = None then (
			Printf.printf "Repacking successful; deleting input file\n" a;
			Sys.remove a
		) else if !delete_bad_output_ref && errors <> None then (
			Printf.printf "Repacking NOT successful; deleting output file\n" b;
			Sys.remove b;
		);
*)
(*		errors*)
		()
	)
);;


let do_a_file (*?(force_overwrite = !force_overwrite_ref)*) fin1 fout1 =
	let fin = strip_multiple_slashes fin1 in
	let fout = strip_multiple_slashes fout1 in
	if !only_info_bitrate_ref then (
		(* Print nothing *)
	) else if !only_info_ref then (
		Printf.printf "\n*** '%s'\n" fin
	) else if !rename_input_ref then (
		Printf.printf "\n*** '%s' (backup to '%s')\n" fin fout
	) else (
		Printf.printf "\n*** '%s' -> '%s'\n" fin fout
	);
	(match (find_file fin, find_file fout, !force_overwrite_ref) with
		| (None, _, _) -> (Printf.printf " WARNING: file '%s' does not exist; ignoring\n" fin; ())
		| (Some Unix.S_REG, None, _) -> do_base fin fout
		| (Some Unix.S_REG, Some Unix.S_REG, false) when not !only_info_ref -> (
			Printf.printf "Do you really want to overwrite '%s'? (y/n)\n" fout;
			let answer = read_line () in
			if String.length answer > 0 && (answer.[0] = 'y' || answer.[0] = 'Y') then (
				do_base fin fout
			) else (
				Printf.printf "Skipping file...\n";
				()
			)
		)
		| (Some Unix.S_REG, Some Unix.S_REG, _) -> do_base fin fout
		| _ -> (Printf.printf "WARNING: Invalid mapping from '%s' to '%s'\n" fin fout; ())
	)
;;


(* Let's add full-directory support here *)
let do_a_dir append_extension din1 dout1 =
	let din = strip_multiple_slashes din1 in
	let dout = strip_multiple_slashes dout1 in
	let contents_in = Sys.readdir din in
	Array.iter (fun fin ->
		if Filename.check_suffix fin ".mp3" then (
			(* The file has the right extension *)
			if dout1 = "" && append_extension = "" then (
				(* An invalid output directory has been passed; don't set the file. This is the case when the -i switch was used *)
				Printf.printf "snatoheu";
				let errors = do_a_file (Filename.concat din fin) "" in
				()
			) else if append_extension = "" || !do_files_with_appended_string_ref || not (Filename.check_suffix fin (append_extension ^ ".mp3")) then (
				(* The file doesn't have the append_expression already appended to it, or that doesn't matter *)
				let errors = do_a_file (Filename.concat din fin) (Filename.concat dout (append_before_extension fin append_extension)) in
				()
			) else (
				Printf.printf "SKIPPING FILE '%s'\n" (Filename.concat din fin);
			)
		)
	) contents_in;
	()
;;


(*
  IN  OUT Info 
	mp3 mp3 N | Simple file -> file
	mp3 DIR N | Put the file in the output dir
	mp3  .  N | assume the output file has whatever "-a" is set to
	DIR mp3 N | NO!
	DIR DIR N | Do all the files in the input dir and stick them in the output dir
	DIR  .  N | Append "-a" to the input dir's name, and output the files there
	 .  mp3 N | NO!
	 .  DIR N | NO!
	 .   .  N | Display usage
	mp3 ??? Y | Display info about the file
	DIR ??? Y | Display info about all files in the dir
	 .  ??? Y | Display usage
*)

if !debug_in_ref || !debug_out_ref then (
	match (!in_name_ref, !out_name_ref, !only_info_ref) with
	| (Some (IO_File x), Some (IO_File y), false) -> Printf.printf "mp3 mp3 N\n"
	| (Some (IO_File x), Some (IO_Dir  y), false) -> Printf.printf "mp3 DIR N\n"
	| (Some (IO_File x), None            , false) -> Printf.printf "mp3  .  N\n"
	| (Some (IO_Dir  x), Some (IO_File y), false) -> Printf.printf "DIR mp3 N\n"
	| (Some (IO_Dir  x), Some (IO_Dir  y), false) -> Printf.printf "DIR DIR N\n"
	| (Some (IO_Dir  x), None            , false) -> Printf.printf "DIR  .  N\n"
	| (None            , Some (IO_File y), false) -> Printf.printf " .  mp3 N\n"
	| (None            , Some (IO_Dir  y), false) -> Printf.printf " .  DIR N\n"
	| (None            , None            , false) -> Printf.printf " .   .  N\n"
	| (Some (IO_File x),         _       , true ) -> Printf.printf "mp3 ??? Y\n"
	| (Some (IO_Dir  x),         _       , true ) -> Printf.printf "DIR ??? Y\n"
	| (None            ,         _       , true ) -> Printf.printf " .  ??? Y\n"
);;

(match (!in_name_ref, !out_name_ref, !only_info_ref) with
	| (Some (IO_File x), Some (IO_File y), false) -> ignore (do_a_file x y)
	| (Some (IO_File x), Some (IO_Dir  y), false) -> ignore (do_a_file x (Filename.concat (y) (Filename.basename x)))
	| (Some (IO_File x), None            , false) -> ignore (do_a_file x (append_before_extension x !append_ref))
	| (Some (IO_Dir  x), Some (IO_File y), false) -> (failwith "Can't output a directory to a file")
	| (Some (IO_Dir  x), Some (IO_Dir  y), false) -> (do_a_dir "" x y)
	| (Some (IO_Dir  x), None            , false) -> (do_a_dir !append_ref x x)
	| (None            , Some (IO_File y), false) -> (failwith "This REALLY shouldn't happen... (1)")
	| (None            , Some (IO_Dir  y), false) -> (failwith "This REALLY shouldn't happen... (2)")
	| (None            , None            , false) -> (Arg.usage args ("ERROR: No input given\n" ^ usage_head))
	| (Some (IO_File x),         _       , true ) -> ignore (do_a_file x "")
	| (Some (IO_Dir  x),         _       , true ) -> (do_a_dir "" x "")
	| (None            ,         _       , true ) -> (Arg.usage args ("ERROR: No input given\n" ^ usage_head))
(*	| (Some (IO_File x), _, true) -> do_a_file x "" (* Only info *)*)
);;

(*
let t2 = Unix.gettimeofday ();;
Printf.printf "TIME: %f\n" (t2 -. t1);;
*)

