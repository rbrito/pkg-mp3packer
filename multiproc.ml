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

open Mp3types;;



let rec unix_really_write d s o l =
	if l = 0 then () else (
		let put = Unix.write d s o l in
		if put = 0 then raise End_of_file else unix_really_write d s (o + put) (l - put)
	)
;;
let rec unix_really_read d s o l =
	if l = 0 then () else (
		let got = Unix.read d s o l in
		if got = 0 then raise End_of_file else unix_really_read d s (o + got) (l - got)
	)
;;


(* For marshalling to/from Unixy file descriptors *)
let marshal_to_descr descr x flag_list =
	let str = Marshal.to_string x flag_list in
	unix_really_write descr str 0 (String.length str)
;;


let marshal_from_descr descr =
	let head_str = String.create Marshal.header_size in
	unix_really_read descr head_str 0 Marshal.header_size;
	let data_size = Marshal.data_size head_str 0 in
	let temp_str = String.create (Marshal.header_size + data_size) in
	String.blit head_str 0 temp_str 0 Marshal.header_size;
	unix_really_read descr temp_str Marshal.header_size data_size;
	Marshal.from_string temp_str 0
;;













(* Ensures that the files can be processed without running into file-related problems *)
(* Returns Some (from_file, to_file) if processing can continue, or None if it can't *)
(* This should not be used with only_info *)
let prepare_file rename_input force_overwrite fin1 fout1 =
	let fin = strip_multiple_slashes fin1 in
	let fout = strip_multiple_slashes fout1 in
	if rename_input then (
		(* Delete fout, move fin to fout, then do fout -> fin *)
		match (find_file fin, find_file fout, force_overwrite) with
		| (None, _, _) -> (Printf.printf " WARNING: file '%s' does not exist; ignoring\n" fin; None)
		| (Some Unix.S_REG, None, _) -> (
			match trap_exception_2 Sys.rename fin fout with
			| Normal () -> Some (fout, fin)
			| Error e -> (
				Printf.printf " WARNING: Got error '%s' renaming '%s' to '%s'; ignoring\n" (Printexc.to_string e) fin fout;
				None
			)
		)
		| (Some Unix.S_REG, Some Unix.S_REG, false) -> (
			Printf.printf "Do you really want to overwrite '%s'? (y/n)\n" fout;
			let answer = read_line () in
			if String.length answer > 0 && (answer.[0] = 'y' || answer.[0] = 'Y') then (
				match trap_exception Sys.remove fout with
				| Normal () -> (
					match trap_exception_2 Sys.rename fin fout with
					| Normal () -> Some (fout, fin)
					| Error e -> (
						Printf.printf " WARNING: Got error '%s' renaming '%s' to '%s'; ignoring\n" (Printexc.to_string e) fin fout;
						None
					)
				)
				| Error e -> (
					Printf.printf " WARNING: Got error '%s' deleting '%s'; ignoring\n" (Printexc.to_string e) fout;
					None
				)
			) else (
				None
			)
		)
		| (Some Unix.S_REG, Some Unix.S_REG, true) -> (
			Sys.remove fout;
			Sys.rename fin fout;
			Some (fout, fin)
		)
		| _ -> (Printf.printf "WARNING: Invalid mapping from '%s' to '%s'; ignoring\n" fin fout; None)
	) else (
		(* Normal fin -> fout *)
		match (find_file fin, find_file fout, force_overwrite) with
		| (None, _, _) -> (Printf.printf " WARNING: file '%s' does not exist; ignoring\n" fin; None)
		| (Some Unix.S_REG, None, _) -> Some (fin, fout)
		| (Some Unix.S_REG, Some Unix.S_REG, false) -> (
			Printf.printf "Do you really want to overwrite '%s'? (y/n)\n" fout;
			let answer = read_line () in
			if String.length answer > 0 && (answer.[0] = 'y' || answer.[0] = 'Y') then (
				Sys.remove fout;
				Some (fin, fout)
			) else (
				None
			)
		)
		| (Some Unix.S_REG, Some Unix.S_REG, true) -> (
			Sys.remove fout;
			Some (fin, fout)
		)
		| _ -> (Printf.printf "WARNING: Invalid mapping from '%s' to '%s'; ignoring\n" fin fout; None)
	)
;;

(* Runs "f" on any file pairs which are valid (input has an .mp3 extension and output does not exist) *)
let prepare_dir rename_input force_overwrite din1 dout1 f =
	let din = strip_multiple_slashes din1 in
	let dout = strip_multiple_slashes dout1 in
	let all_contents = Sys.readdir din in
	Array.iter (fun fin_base ->
		if Filename.check_suffix fin_base ".mp3" then (
			(* Smells like an MP3 *)
			let fin = Filename.concat din fin_base in
			let fout = Filename.concat dout fin_base in
			match prepare_file rename_input force_overwrite fin fout with
			| None -> () (* Skip *)
			| Some (file_from, file_to) -> f file_from file_to
		) else (
			Printf.printf "SKIPPING '%s'\n" (Filename.concat din fin_base);
		)
	) all_contents
;;


(* Remake prepare_(file|dir) since they don't have the ability to skip over files that need confirmation *)
(* This function will run "f" on the files that can be processed now, or add them to do_so_far to be handled later *)
let check_problem_file rename_input force_overwrite fin1 fout1 f do_so_far =
	let fin = strip_multiple_slashes fin1 in
	let fout = strip_multiple_slashes fout1 in
	if rename_input then (
		(* Delete fout, move fin to fout, then fout -> fin *)
		match (find_file fin, find_file fout, force_overwrite) with
		| (None, _, _) -> (
			Printf.printf " WARNING: file '%s' does not exist; ignoring\n";
			do_so_far
		)
		| (Some Unix.S_REG, None, _) -> (
			match trap_exception_2 Sys.rename fin fout with
			| Normal () -> (
				f fout fin;
				do_so_far
			)
			| Error e -> (
				Printf.printf " WARNING: Got error '%s' renaming '%s' to '%s'; trying again later\n" (Printexc.to_string e) fin fout;
				(fun () ->
					match trap_exception_2 Sys.rename fin fout with
					| Normal () -> f fout fin
					| Error e -> Printf.printf " WARNING: Got error '%s' renaming '%s' to '%s' again\n" (Printexc.to_string e) fin fout;
				) :: do_so_far
			)
		)
		| (Some Unix.S_REG, Some Unix.S_REG, false) -> (
			(* Tell the user about it later *)
			(fun () ->
				Printf.printf "Do you really want to overwrite '%s'? (y/n)\n" fout;
				let answer = read_line () in
				if String.length answer > 0 && (answer.[0] = 'y' || answer.[0] = 'Y') then (
					match trap_exception Sys.remove fout with
					| Normal () -> (
						match trap_exception_2 Sys.rename fin fout with
						| Normal () -> f fout fin
						| Error e -> Printf.printf " WARNING: Got error '%s' renaming '%s' to '%s'; ignoring\n" (Printexc.to_string e) fin fout
					)
					| Error e -> (
						Printf.printf " WARNING: Got error '%s' deleting '%s'; ignoring\n" (Printexc.to_string e) fout
					)
				)
			) :: do_so_far
		)
		| (Some Unix.S_REG, Some Unix.S_REG, true) -> (
			(* Don't bother telling the user about it later *)
			(match trap_exception Sys.remove fout with
				| Normal () -> (
					match trap_exception_2 Sys.rename fin fout with
					| Normal () -> f fout fin
					| Error e -> Printf.printf " WARNING: Got error '%s' renaming '%s' to '%s'; ignoring\n" (Printexc.to_string e) fin fout
				)
				| Error e -> Printf.printf " WARNING: Got error '%s' deleting '%s'; ignoring\n" (Printexc.to_string e) fout
			);
			do_so_far
		)
		| _ -> (
			Printf.printf " WARNING: Invalid mapping from '%s' to '%s; ignoring\n" fin fout;
			do_so_far
		)
	) else (
		(* Don't do the rename part *)
		match (find_file fin, find_file fout, force_overwrite) with
		| (None, _, _) -> (
			Printf.printf " WARNING: file '%s' does not exist; ignoring\n";
			do_so_far
		)
		| (Some Unix.S_REG, None, _) -> (
			f fin fout;
			do_so_far
		)
		| (Some Unix.S_REG, Some Unix.S_REG, false) -> (
			(* Tell the user about it later *)
			(fun () ->
				Printf.printf "Do you really want to overwrite '%s'? (y/n)\n" fout;
				let answer = read_line () in
				if String.length answer > 0 && (answer.[0] = 'y' || answer.[0] = 'Y') then (
					match trap_exception Sys.remove fout with
					| Normal () -> f fin fout
					| Error e -> Printf.printf " WARNING: Got error '%s' deleting '%s'; ignoring\n" (Printexc.to_string e) fout
				)
			) :: do_so_far
		)
		| (Some Unix.S_REG, Some Unix.S_REG, true) -> (
			(* Try to delete now, and don't bother telling the user about it later *)
			(match trap_exception Sys.remove fout with
				| Normal () -> f fin fout
				| Error e -> Printf.printf " WARNING: Got error '%s' deleting '%s'; ignoring\n" (Printexc.to_string e) fout
			);
			do_so_far
		)
		| _ -> (
			Printf.printf " WARNING: Invalid mapping from '%s' to '%s; ignoring\n" fin fout;
			do_so_far
		)
	)
;;

let list_problems rename_input force_overwrite din1 dout1 f =
	let din = strip_multiple_slashes din1 in
	let dout = strip_multiple_slashes dout1 in
	let all_contents = Sys.readdir din in
	let problem_list = Array.fold_left (fun do_so_far fin_base ->
		if Filename.check_suffix fin_base ".mp3" then (
			let fin = Filename.concat din fin_base in
			let fout = Filename.concat dout fin_base in
			check_problem_file rename_input force_overwrite fin fout f do_so_far
		) else (
			Printf.printf "SKIPPING '%s'\n" fin_base;
			do_so_far
		)
	) [] all_contents in
	List.iter (fun x -> x ()) problem_list;
;;




(* If another mp3packer is running this one, this is the loop *)
(* It will wait for information from stdin, process it, then write it to stdout *)
let rec worker_loop queue_state_opt =
(*	Printf.eprintf "Starting worker\n%!";*)
	match marshal_from_descr Unix.stdin with
	| Worker_queue new_queue_state -> worker_loop (Some {new_queue_state with q_silent = true; q_debug_in = false; q_debug_queue = false; q_debug_recompress = false})
	| Worker_do {worker_file_index = i; worker_file_input = fin; worker_file_output = fout} -> (
		let worker_ret = match queue_state_opt with
			| Some queue_state -> (try
				let in_obj = new Mp3read.mp3read_unix fin in
				let out_obj = new Mp3write.mp3write_unix ~flags:[Unix.O_EXCL] fout in
				let worker_errors = Mp3queue.do_queue queue_state in_obj out_obj in
				Worker_ok {
					worker_ok_index = i(* ^ "\x1A\x0D\x1A\x0A\x1A" ^ fin*);
					worker_output_result = worker_errors;
				}
			with
				| Unix.Unix_error (Unix.EEXIST, "open", _) -> Worker_skipped i
				| e -> Worker_fail {worker_fail_index = i; worker_exception = worker_exn_of_exn e}
			)
			| None -> Worker_fail {worker_fail_index = i; worker_exception = Worker_fail_not_found}
		in
		marshal_to_descr Unix.stdout worker_ret [Marshal.No_sharing];
(*		flush stdout;*)
		worker_loop queue_state_opt
	)
	| Worker_finish -> (
		marshal_to_descr Unix.stdout Worker_done [Marshal.No_sharing];
(*		flush stdout;*)
		()
	)
;;


let rec controller_setup () =
	let num_workers = Mp3types.detected_processors in
(*	let run = Printf.sprintf "\"%s\" --worker" Sys.argv.(0) in*)
(*
	let channel_array = Array.init num_workers (fun _ ->
		let channels = Unix.open_process run in
		set_binary_mode_in (fst channels) true;
		set_binary_mode_out (snd channels) true;
		channels
	) in
*)
	let channel_array = Array.init num_workers (fun _ ->
		let (pipe_stdin_readme, pipe_stdin_writeme) = Unix.pipe () in
		let (pipe_stdout_readme, pipe_stdout_writeme) = Unix.pipe () in
(*		let (pipe_stderr_readme, pipe_stderr_writeme) = Unix.pipe () in*)
		ignore @@ Unix.create_process Sys.argv.(0) [|Sys.argv.(0);"--worker"|] pipe_stdin_readme pipe_stdout_writeme Unix.stderr;
		(pipe_stdout_readme, pipe_stdin_writeme)
	) in
	let still_running = Array.make num_workers true in

	let marshal_to_all (x : worker_do_t) = Array.iteri (fun i (_, write_channel) ->
		if still_running.(i) then (
(*			Marshal.to_channel write_channel x [Marshal.No_sharing];*)
			marshal_to_descr write_channel x [Marshal.No_sharing];
(*			flush write_channel*)
		)
	) channel_array in
(*
	let get_all () =
		let out_array = Array.make num_workers Worker_done in
		Array.iteri (fun i channels ->
			if still_running.(i) then (
(*				out_array.(i) <- Marshal.from_channel (fst channels)*)
				out_array
			)
		) channel_array;
		out_array
	in
*)

	let get_something wait_time () =
		let wait_list_ref = ref [] in
		Array.iteri (fun i (x,_) -> if still_running.(i) then wait_list_ref := x :: !wait_list_ref) channel_array;
		if !wait_list_ref = [] then (
			(* Nobody is running *)
			None
		) else (
			match Unix.select !wait_list_ref [] [] wait_time with
			| (hd :: tl, _, _) -> (
				match marshal_from_descr hd with
				| Worker_done -> (
					(* Great. Now we just need to know which handle is done *)
					Array.mapi (fun i (x,_) -> if hd = x then still_running.(i) <- false) channel_array;
					Some Worker_done
				)
				| x -> Some x
			)
			| ([], _, _) -> None
		)
	in

(*	let close_all () = Array.map (fun channels -> Unix.close_process channels) channel_array in*)
	let close_all () = Array.iter (fun (a,b) -> Unix.close a; Unix.close b) channel_array in
	(marshal_to_all, get_something ~-.1.0, get_something 0.0, close_all)
;;


