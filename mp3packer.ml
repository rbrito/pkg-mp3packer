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

open Mp3types;;


(*Unicode.set_utf8_output ();;*)

(* XXX XXX XXX *)
(*
match copy_file_times_by_name "out.txt" "out_check.txt" with
| Normal () -> Printf.printf "OK\n"
| Error e -> raise e
;;
exit 477;;
*)
(* XXX XXX XXX *)




(*let t1 = Unix.gettimeofday ();;*)

let version = "2.00-248";;

let padding = Printf.sprintf "mp3packer%s\n" version;;

let features_ref = ref [];;
if Sys.word_size > 32 then features_ref := Printf.sprintf "%d-bit" Sys.word_size :: !features_ref;;
if sse41_ok then features_ref := "SSE4.1" :: !features_ref;;
let rec make_features_string = function
	| a :: b :: c -> a ^ ", " ^ make_features_string (b :: c)
	| a :: [] -> a
	| [] -> ""
;;
let features_string = make_features_string !features_ref;;

let usage_head = Printf.sprintf "\nMP3 Packer version %s%s\nCopyright 2006-2012 Reed \"Omion\" Wilson\nThis program is covered under the GNU GPL.\nSee gpl.txt or mp3packer.html for more information\nNumber of processors detected: %d\n" version (if features_string = "" then "" else " (" ^ features_string ^ ")") Mp3types.detected_processors;;

(*****************)
(* PARSE OPTIONS *)
(*****************)

(*
(*Printf.printf ">>1:%s:11111111111111111111111111111111<<\n\n" (match Unicode.utf16_of_utf8 argv.(1) with;;*)
Printf.printf "%!";;
Unicode.silly_print (match Unicode.utf16_of_utf8 argv.(1) with | Unicode.Normal x -> x | _ -> "");;
Unicode.silly_print argv.(1);;
Unicode.silly_print argv.(1);;
Printf.printf "%!";;
let _ = read_line ();;
Unicode.silly_print argv.(1);;
Unicode.silly_print argv.(1);;
Unicode.silly_print argv.(1);;
Unicode.silly_print argv.(1);;
let _ = read_line ();;
Printf.printf ">>1:%s:1<<\n\n" argv.(1);;
Printf.printf ">>2:%s:%s:2<<\n\n" argv.(1) argv.(1);;
Printf.printf ">>3:%s:%s:%s:3<<\n\n" argv.(1) argv.(1) argv.(1);;
Printf.printf ">>4:%s:%s:%s:%s:4<<\n\n" argv.(1) argv.(1) argv.(1) argv.(1);;
exit 4567;;
*)


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
let copy_time_ref = ref false;;
let process_set_ref = ref (if sse41_ok then SSE41 else Set_base);;

let niceness_ref = ref 10;;

let worker_ref = ref None;;






let in_name_ref = ref None;;
let out_name_ref = ref None;;

let debug_parse = function
	| "in" ->   (debug_in_ref := true)
	| "out" ->  (debug_out_ref := true)
	| "huff" -> (debug_recompress_ref := true)
	| "all" ->  (debug_in_ref := true;  debug_out_ref := true; debug_recompress_ref := true)
	| _ ->      (debug_in_ref := false; debug_out_ref := false; debug_recompress_ref := false)
;;

let keep_ok_parse v = match String.lowercase v with
(*	| "input" | "in" -> keep_ok_ref := Keep_ok_input*)
	| "output" | "out" -> keep_ok_ref := Keep_ok_output
	| _ -> keep_ok_ref := Keep_ok_both
;;
let keep_notok_parse v = match String.lowercase v with
	| "input" | "in" -> keep_notok_ref := Keep_notok_input
	| "output" | "out" -> keep_notok_ref := Keep_notok_output
	| _ -> keep_notok_ref := Keep_notok_both
;;

let process_parse v = match String.lowercase v with
	| "sse41" when sse41_ok -> process_set_ref := SSE41
	| "sse41" -> Printf.printf "WARNING: SSE4.1 unsupported\n"
	| "base" -> process_set_ref := Set_base
	| _ -> ()
;;

let args = Arg.align [
	("-b", Arg.Int (fun x -> min_bitrate_ref := max 0 x),  "# Minimum bitrate allowed for output. Defaults to 0");
	("-t", Arg.Set delete_begin_ref,                        " Strip non-MP3 data at the beginning (mainly ID3v2 tags)");
	("-s", Arg.Set delete_end_ref,                          " Strip non-MP3 data at the end (mainly tags)");
	("-z", Arg.Set recompress_ref,                          " Recompress frames to find optimal settings (takes time)");
	("-a", Arg.Set_string append_ref,               "\"-vbr\" Changes the string to append to the filename in certain cases");
	("-A", Arg.Set do_files_with_appended_string_ref,       " Don't skip files in directory which already have the -a string");
	("-u", Arg.Set rename_input_ref,                        " Updates the input file and renames the orig to the output name");
	("-w", Arg.Set zero_whole_bad_frame_ref,                " Skips frames on buffer error. Otherwise skips granules");
	("-r", Arg.Unit (fun () -> minimize_bit_reservoir_option_ref := Some true),  " Minimizes bit reservoir all the time");
	("-R", Arg.Unit (fun () -> minimize_bit_reservoir_option_ref := Some false), " Maximizes bit reservoir all the time");
	("--keep-ok", Arg.String keep_ok_parse,             "both What files to keep when no errors occur. [out|both]");
	("--keep-bad", Arg.String keep_notok_parse,         "both What files to keep when errors occur. [in|out|both]");
	("--copy-time", Arg.Set copy_time_ref,                  " Preserve the original files' creation/modification times");
	("-f", Arg.Set force_overwrite_ref,                     " Force overwriting of output files");
	("--process", Arg.String process_parse,Printf.sprintf "%s Which instruction set to use with -z [base|sse41]" (if sse41_ok then "sse41" else "base"));
	("-i", Arg.Set only_info_ref,                           " Print info and exit (no processing)");
	("--ib", Arg.Set only_info_bitrate_ref,                 " Print only the min CBR bitrate (similar to -i)");
	("--nice",  Arg.Set_int niceness_ref,                 "10 Priority of the encoding. 0 = normal, 19 = idle");
	("--debug", Arg.String debug_parse,                 "none Print a bunch of garbage while processing. [in|out|huff|all]");
	("--worker", Arg.Int (fun x -> worker_ref := Some x),  "_ Internal use only -- do not use")
];;



try
	Arg.parse_argv argv args (fun w ->
		let x = strip_trailing_slash w in
		match (!in_name_ref, !out_name_ref, find_file x) with
(*		| (None, None, None) -> (failwith (Printf.sprintf "'%s' does not exist" x))*)
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
	) usage_head
with
	| Arg.Bad msg -> (
		Printf.eprintf "%s" msg;
		exit 2
	)
	| Arg.Help msg -> (
		Printf.printf "%s" msg;
		exit 0;
	)
;;

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

(*
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
*)
let print_errors = function
	| (0,0,0) -> ()
	| (b,s,r) -> (
		let total = b + s + r in
		if total = 1 then (
			Printf.printf "\n  WARNING: an error was encountered\n";
		) else (
			Printf.printf "\n  WARNING: errors were encountered\n";
		);
		let max_len = String.length (string_of_int (max (max b s) r)) in
		(match b with
			| 0 -> ()
			| 1 -> Printf.printf "    %*d buffer error\n" max_len 1
			| _ -> Printf.printf "    %*d buffer errors\n" max_len b
		);
		(match s with
			| 0 -> ()
			| 1 -> Printf.printf "    %*d sync error\n" max_len 1
			| _ -> Printf.printf "    %*d sync errors\n" max_len s
		);
		(match r with
			| 0 -> ()
			| 1 -> Printf.printf "    %*d recompression error\n" max_len 1
			| _ -> Printf.printf "    %*d recompression errors\n" max_len r
		);
	)
;;


let queue_state = {
	q_silent = (!worker_ref <> None);
	q_debug_in = !debug_in_ref;
	q_debug_queue = !debug_out_ref;
	q_debug_recompress = !debug_recompress_ref;
	q_min_bitrate = !min_bitrate_ref;
	q_delete_beginning_junk = !delete_begin_ref;
	q_delete_end_junk = !delete_end_ref;
	q_padding = padding;
	q_recompress = !recompress_ref;
	q_process_set = !process_set_ref;
	q_zero_whole_bad_frame = !zero_whole_bad_frame_ref;
	q_minimize_bit_reservoir = minimize_bit_reservoir;
};;



(* Now see what the user wants *)
let do_base = if !only_info_ref || !only_info_bitrate_ref then (
	(* In order to generalize the do_base function, just ignore the second (output) string if user only wants info *)
	fun a b -> (
		let t1 = Unix.gettimeofday () in
		(try
			let errors = do_info ~only_bitrate:!only_info_bitrate_ref ~debug_in:(!debug_in_ref) ~debug_info:(!debug_out_ref) a in
			if not !only_info_bitrate_ref then (
				print_errors errors;
			);
		with
			_ -> ()
		);
		let t2 = Unix.gettimeofday () in
(*		Printf.printf "That took %f seconds\n" (t2 -. t1);*)
		()
(*		errors*)
	)
) else match !worker_ref with
| Some worker_num -> (
	(* This is being run by another mp3packer; make sure everything is silent *)
	(* rename_input_ref should NOT be used with this since Linux will just overwrite the other file *)
	Multiproc.worker worker_num !process_set_ref true
)
| _ when !rename_input_ref -> (
	(* Rename the input file, and do_queue backwards to update the original filename *)
	fun a b -> (
		if Unicode.file_exists_utf8 b then Unicode.remove_utf8 b;
		ignore @@ Unicode.rename_utf8 a b;
		let r_obj = new Mp3read.mp3read_ptr ~debug:queue_state.q_debug_in b in
		let w_obj = new Mp3write.mp3write_unix_ptrref_buf ~flags:[Unix.O_TRUNC] a in
		let errors = (try
			do_queue queue_state (*new Mp3read.mp3read_unix ~debug:queue_state.q_debug_in b*)r_obj w_obj
		with
			(* The only time an End_of_file is reached is when no valid frames were found. Therefore, it's sort of a sync error... *)
			End_of_file -> (
				r_obj#close;
				w_obj#close;
				Printf.printf "\nWARNING: No valid MP3 headers found\n"; (0,1,0)
			)
		) in

		(* Maybe I should add some sort of warning if this fails? *)
		if !copy_time_ref then (
			ignore @@ copy_file_times_by_name b a
		);

		(match (errors, !keep_ok_ref, !keep_notok_ref) with
			| ((0,0,0), Keep_ok_output, _) -> (Printf.printf "Repacking successful; deleting backup\n"; Unicode.remove_utf8 b)
			| (_, _, Keep_notok_output) -> (Printf.printf "Repacking not successful, but deleting backup anyway\n"; Unicode.remove_utf8 b)
			| (_, _, Keep_notok_input) -> (Printf.printf "Repacking NOT successful; deleting output file\n"; Unicode.remove_utf8 a)
			| _ -> () (* Keep everything *)
		);
		print_errors errors;

		()
(*		errors*)
	)
)
| _ -> (
	(* Regular *)
	fun a b -> (
		let r_obj = new Mp3read.mp3read_ptr ~debug:queue_state.q_debug_in a in
		let w_obj = new Mp3write.mp3write_unix_ptrref_buf ~flags:[Unix.O_TRUNC] b in
		let errors = (try
			do_queue queue_state (*new Mp3read.mp3read_unix ~debug:queue_state.q_debug_in a*)r_obj w_obj
		with
			End_of_file -> (
				r_obj#close;
				w_obj#close;
				Printf.printf "\nWARNING: No valid MP3 headers found\n"; (0,1,0)
			)
		) in

		if !copy_time_ref then (
			ignore @@ copy_file_times_by_name a b
		);

		(match (errors, !keep_ok_ref, !keep_notok_ref) with
			| ((0,0,0), Keep_ok_output, _) -> (Printf.printf "Repacking successful; deleting input file\n"; Unicode.remove_utf8 a)
			| (_, _, Keep_notok_output) -> (Printf.printf "Repacking not successful, but deleting input anyway\n"; Unicode.remove_utf8 a)
			| (_, _, Keep_notok_input) -> (Printf.printf "Repacking NOT successful; deleting output file\n"; Unicode.remove_utf8 b)
			| _ -> () (* Keep everything *)
		);

		print_errors errors;


		()
	)
);;


let do_a_file (*?(force_overwrite = !force_overwrite_ref)*) fin1 fout1 =
	let fin = strip_multiple_slashes fin1 in
	let fout = strip_multiple_slashes fout1 in
	if !only_info_bitrate_ref then (
		(* Print nothing *)
	) else if !only_info_ref then (
		Printf.printf "\n*** '"; Unicode.silly_print fin; Printf.printf "'\n";
	) else if !rename_input_ref then (
		Printf.printf "\n*** '"; Unicode.silly_print fin; Printf.printf "' (backup to '"; Unicode.silly_print fout; Printf.printf "')\n";
	) else (
		Printf.printf "\n*** '"; Unicode.silly_print fin; Printf.printf "' -> '"; Unicode.silly_print fout; Printf.printf "'\n";
	);
	(match (find_file fin, find_file fout, !force_overwrite_ref) with
		| (None, _, _) -> (Printf.printf " WARNING: file '"; Unicode.silly_print fin; Printf.printf "' does not exist; ignoring\n"; ())
		| (Some Unix.S_REG, None, _) -> do_base fin fout
		| (Some Unix.S_REG, Some Unix.S_REG, false) when not !only_info_ref -> (
			Printf.printf "Do you really want to overwrite '"; Unicode.silly_print fout; Printf.printf "'? (y/n)\n";
			let answer = read_line () in
			if String.length answer > 0 && (answer.[0] = 'y' || answer.[0] = 'Y') then (
				do_base fin fout
			) else (
				Printf.printf "Skipping file...\n";
				()
			)
		)
		| (Some Unix.S_REG, Some Unix.S_REG, _) -> do_base fin fout
		| _ -> (Printf.printf "WARNING: Invalid mapping from '"; Unicode.silly_print fin; Printf.printf "' to '"; Unicode.silly_print fout; Printf.printf "'\n"; ())
	)
;;



(* Let's add full-directory support here *)
let do_a_dir append_extension din1 dout1 =
	let din = strip_multiple_slashes din1 in
	let dout = strip_multiple_slashes dout1 in
	let contents_in = Unicode.readdir_utf8 din in
	Array.iter (fun fin ->
		if Filename.check_suffix fin ".mp3" then (
			(* The file has the right extension *)
			if dout1 = "" && append_extension = "" then (
				(* An invalid output directory has been passed; don't set the file. This is the case when the -i switch was used *)
				let errors = do_a_file (Filename.concat din fin) "" in
				()
			) else if append_extension = "" || !do_files_with_appended_string_ref || not (Filename.check_suffix fin (append_extension ^ ".mp3")) then (
				(* The file doesn't have the append_expression already appended to it, or that doesn't matter *)
				let errors = do_a_file (Filename.concat din fin) (Filename.concat dout (append_before_extension fin append_extension)) in
				()
			) else (
				Printf.printf "SKIPPING FILE '"; Unicode.silly_print (Filename.concat din fin); Printf.printf "'\n";
			)
		)
	) contents_in;
	()
;;







(******************)
(* MULTI-THREADED *)
(******************)

(*
let do_a_file_worker fin1 fout1 =
	(try
		let fin = strip_multiple_slashes fin1 in
		let fout = strip_multiple_slashes fout1 in
		(* Only do the forward version -- the file system probably has the best mutex system in place, and the reverse version wouldn't work well *)
		let worker_ret = (try
			let worker_errors = do_queue queue_state (new Mp3read.mp3read_unix ~debug:queue_state.q_debug_in fin) (new Mp3write.mp3write_unix ~flags:[Unix.O_EXCL] fout) in
			Worker_ok {
				worker_ok_input_name = fin ^ "\x1A" ^ fin;
				worker_output_result = worker_errors;
			}
		with
			| Unix.Unix_error (Unix.EEXIST, "open", _) -> raise Not_found
			| End_of_file -> Worker_ok {worker_ok_input_name = fin; worker_output_result = (0,1,0)}
			| e -> Worker_fail {worker_fail_input_name = fin; worker_exception = e}
		) in

		(* Don't remove any files from here; let the controller do that *)
		Printf.printf "nthoeutnhoenuht\n";
		Printf.printf "\x1A";
		Printf.printf "\x0A";
		Printf.printf "\x0D";
		Printf.printf "nthoeutnhoenuht\n";
		let ret = Marshal.to_string worker_ret [Marshal.No_sharing] in
		Printf.printf "%S\n" ret;
		flush stdout;
	with
		_ -> ()
	);
	()
;;
*)








(************)
(* PROCESS! *)
(************)


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



(*prepare_dir "TEST" "OUT" (fun x y -> Printf.printf "FUN\n  \"%s\"\n  \"%s\"\n" x y);;*)


(***
if !worker_ref then (
	set_binary_mode_in stdin true;
	set_binary_mode_out stdout true;
	Multiproc.worker_loop None
) else if true then (
	(* Controller; make a few workers *)
	Printf.printf "%S\n" Sys.argv.(0);
	let (to_all, get_something, get_available, close_all) = Multiproc.controller_setup () in
	to_all (Worker_queue queue_state);
(*
	to_all (Worker_do ("TEST\\APE.mp3","OUT\\APE.mp3"));
*)
(*	Printf.printf "Sent queue\n%!";*)

	let filename_index_table = Hashtbl.create 10 in
	let get_names_of_index i = match trap_exception_2 Hashtbl.find filename_index_table i with
		| Normal x -> x
		| Error e -> (Printf.sprintf ">Unknown input file %d<" i, Printf.sprintf ">Unknown output file %d<" i)
	in

	let file_i_ref = ref 0 in

	let rec try_get () =
		match get_available () with
		| Some x -> (
			(match x with
				| Worker_ok {worker_ok_index = i; worker_output_result = (a,b,c)} -> (
					let (in_name, out_name) = get_names_of_index i in
					Printf.printf "\n*** '%s' -> '%s'" in_name out_name;
					print_errors (a,b,c);
					Printf.printf "\n%!";
(*					Printf.printf "OK (%d,%d,%d) \"%d\"\n%!" a b c i*)
				)
				| Worker_fail {worker_fail_index = i; worker_exception = Worker_fail_end_of_file} -> (
					let (in_name, out_name) = get_names_of_index i in
					Printf.printf "\n*** '%s' -> '%s'" in_name out_name;
					Printf.printf "\nWARNING: No valid MP3 headers found\n%!"
				)
				| Worker_fail {worker_fail_index = i; worker_exception = e} -> (
					let (in_name, out_name) = get_names_of_index i in
					Printf.printf "\n*** '%s' -> '%s'" in_name out_name;
					Printf.printf "\nERROR: Failed with error '%s'\n%!" (string_of_worker_exn e)
				)
				| Worker_skipped i -> ()(*Printf.printf "A worker skipped \"%s\"\n%!" i*)
				| Worker_done -> ()(*Printf.printf "A worker is done\n%!"*)
			);
			try_get ()
		)
		| None -> ()(*Printf.printf "nothing good found\n%!"*)
	in
	let f a b =
		try_get ();
		to_all (Worker_do {worker_file_index = !file_i_ref; worker_file_input = a; worker_file_output = b});
		Hashtbl.add filename_index_table !file_i_ref (a,b);
		try_get ();
(*		Printf.printf "Sent file #%d \"%s\"\n%!" !file_i_ref a;*)
		incr file_i_ref;
	in
(*	Multiproc.prepare_dir !rename_input_ref !force_overwrite_ref "TEST" "OUT" f;*)

	let f_test a b =
		Printf.printf ">>> SENDING '%s' -> '%s'\n" a b
	in
	Multiproc.list_problems !rename_input_ref !force_overwrite_ref "TEST" "OUT" f;

(*
	to_all (Worker_do ("TEST\\APS.mp3","OUT\\APS.mp3"));
	Printf.printf "Sent file 1\n%!";
	to_all (Worker_do ("TEST\\APE.mp3","OUT\\APE.mp3"));
	Printf.printf "Sent file 2\n%!";
*)
	to_all Worker_finish;
	Printf.printf "Sent finish\n%!";

	let rec controller_loop () =
		match get_something () with
		| Some x -> (
			(match x with
				| Worker_ok {worker_ok_index = i; worker_output_result = (a,b,c)} -> Printf.printf "OK (%d,%d,%d) \"%d\"\n%!" a b c i
				| Worker_fail {worker_fail_index = i; worker_exception = e} -> Printf.printf "FAIL \"%s\" \"%d\"\n%!" (string_of_worker_exn e) i
				| Worker_skipped i -> ()(*Printf.printf "A worker skipped \"%s\"\n%!" i*)
				| Worker_done -> Printf.printf "A worker is done\n%!"
			);
			controller_loop ()
		)
		| None -> Printf.printf "ALL DONE!\n%!"
	in
	controller_loop ()

) else (
	()
);;
***)



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
(*
if !worker_ref then (
	set_binary_mode_out stdout true;
	match (!in_name_ref, !out_name_ref) with
	| (Some (IO_File x), Some (IO_File y)) -> do_a_file_worker x y
	| (Some (IO_File x), Some (IO_Dir  y)) -> do_a_file_worker x (Filename.concat y (Filename.basename x))
(*	do_a_file_worker*)
) else*) (

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
	);
);;





(*
let t2 = Unix.gettimeofday ();;
Printf.printf "TIME: %f\n" (t2 -. t1);;
*)


