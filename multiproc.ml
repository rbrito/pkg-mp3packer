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

(* Just accept pretty much anything *)
let frame_reqs = {
	req_id           = Req_any;
	req_crc          = Req_any;
	req_bitrate      = Req_any;
	req_samplerate   = Req_any;
	req_padding      = Req_any;
	req_private      = Req_any;
	req_channel_mode = Req_any;
	req_ms           = Req_any;
	req_is           = Req_any;
	req_copyright    = Req_any;
	req_original     = Req_any;
	req_emphasis     = Req_any;
};;


let unused_error_codes =    0x3FFFFFFE;;
let error_code_recompress = 0x00000001;;

let max_reasonable_len = 4000;;

(* This is run by each of the worker processes *)

(*
There are 3 possibilities for the input frames:
	A new frame (should have both frame number and length)
	An EOF marker (should have only frame number)
	An exit request (should not really have anything)
*)
(*
If the frame number is -1 then it's an exit request.
If the frame number is positive then it's something else:
	If the length is 0 then it's an EOF marker
	If the length is > 4+9 then it's a frame
*)

(* It accepts a 32-bit frame number, 32-bit length, then a that number of bytes as a frame (no CRC, 0 bit reservoir, no padding after frame data) *)
(* The main process tells this one to stop by using a length of 0 *)
(* This returns a 32-bit mask of the errors encountered, 32-bit frame number, a 32-bit length, then length bytes corresponding to only the frame side info and data *)
(* NOTE: MUST use eprintf since stdout will be piped to the main program *)
let worker worker_num process_set debug =
	if debug then Printf.eprintf "WORKER STARTING!\n%!";
	let temp_in_h = Unix.openfile (Printf.sprintf "worker_%d_in.bin" worker_num) [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
	let keep_printing () =
		let temp_ptr = Ptr.make 4 0 in
		Ptr.put_32_of_int temp_ptr 0 0x21212121;
		Ptr.really_write temp_in_h temp_ptr 0 4;
		while true do
			Ptr.really_read Unix.stdin temp_ptr 0 (Ptr.length temp_ptr);
			Ptr.really_write temp_in_h temp_ptr 0 (Ptr.length temp_ptr);
		done
	in
	try
		let temp_ptr = Ptr.make 4 0 in
		let rec loop () =
(*			Thread.delay @@ Random.float 0.1;*)
			Ptr.put_32_of_int temp_ptr 0 0x23232323;
			Ptr.really_write temp_in_h temp_ptr 0 4;
			Ptr.really_read Unix.stdin temp_ptr 0 4;
			Ptr.really_write temp_in_h temp_ptr 0 4;
			let frame_num = Ptr.get_int_of_32 temp_ptr 0 in
			if frame_num = -1 then (
				(* EXIT! *)
				if debug then Printf.eprintf "Worker exiting\n%!";
				Ptr.put_32_of_int temp_ptr 0 (-1);
				Ptr.really_write Unix.stdout temp_ptr 0 4;
				exit 0
			) else if frame_num < 0 then (
				(* ERROR! *)
				if debug then Printf.eprintf "Worker %d got a badly-numbered frame (%d)\n%!" worker_num frame_num;
				keep_printing ();
				exit 1
			) else (
(*				if debug then Printf.eprintf "Worker got frame number %X\n%!" frame_num;*)
				Ptr.really_read Unix.stdin temp_ptr 0 4;
				Ptr.really_write temp_in_h temp_ptr 0 4;
				let len = Ptr.get_int_of_32 temp_ptr 0 in
				if len = 0 then (
					(* EOF; just send the EOF back *)
					if debug then Printf.eprintf "Worker responding with EOF\n%!";
					Ptr.put_32_of_int temp_ptr 0 frame_num;
					Ptr.really_write Unix.stdout temp_ptr 0 4;
					Ptr.put_32_of_int temp_ptr 0 0;
					Ptr.really_write Unix.stdout temp_ptr 0 4;
					loop ()
				) else if len < 4 + 9 || len > max_reasonable_len then (
					(* OOPS! *)
					if debug then Printf.eprintf "Worker %d got an invalid length (%d)\n%!" worker_num len;
					keep_printing ();
					exit 2
				) else (
					let frame_ptr = Ptr.make max_reasonable_len 0 in (* The frame-reading function requires a full frame to work *)
					Ptr.really_read Unix.stdin frame_ptr 0 len;
					Ptr.really_write temp_in_h frame_ptr 0 len;
					let header_ptrref = Ptr.Ref.of_subptr frame_ptr 0 4 in

					match Mp3read.header_of_ptrref false header_ptrref with
					| None -> (
						if debug then Printf.eprintf "Worker %d got invalid frame header\n%!" worker_num;
						keep_printing ();
						exit 3
					)
					| Some header -> (
						let side_len = match (header.header_id, header.header_channel_mode) with
							| (MPEG1, ChannelMono) -> 17
							| (  _  , ChannelMono) ->  9
							| (MPEG1,      _     ) -> 32
							| (  _  ,      _     ) -> 17
						in
						let side_ptrref = Ptr.Ref.of_subptr frame_ptr 4 side_len in
						let frame_to_compress = {
							f1_num = frame_num;
							f1_header = header;
							f1_side = Mp3read.side_info_of_header header side_ptrref;
							f1_data = Ptr.Ref.of_subptr frame_ptr (4 + side_len) (len - 4 - side_len);
							f1_pad_exact = None;
						} in

						let (q, recompress_error) = (try
							Mp3frameutils.recompress_frame process_set frame_to_compress (ref false)
						with
							_ -> (frame_to_compress, true)
						) in
						let errors = (if recompress_error then error_code_recompress else 0) in

						Ptr.put_32_of_int temp_ptr 0 frame_num;
						Ptr.really_write Unix.stdout temp_ptr 0 4;
						let out_len = Ptr.Ref.length q.f1_header.header_raw + Ptr.Ref.length q.f1_side.side_raw + Ptr.Ref.length q.f1_data in
						Ptr.put_32_of_int temp_ptr 0 out_len;
						Ptr.really_write Unix.stdout temp_ptr 0 4;
						Ptr.put_32_of_int temp_ptr 0 errors;
						Ptr.really_write Unix.stdout temp_ptr 0 4;

						let write_me = Ptr.make out_len 0 in
						Ptr.Ref.blit_to_ptr q.f1_header.header_raw 0 write_me 0 4;
						Ptr.Ref.blit_to_ptr q.f1_side.side_raw 0 write_me 4 (Ptr.Ref.length q.f1_side.side_raw);
						Ptr.Ref.blit_to_ptr q.f1_data 0 write_me (4 + Ptr.Ref.length q.f1_side.side_raw) (Ptr.Ref.length q.f1_data);
						Ptr.really_write Unix.stdout write_me 0 out_len;
(*
						Ptr.Ref.really_write_ref Unix.stdout q.f1_header.header_raw;
						Ptr.Ref.really_write_ref Unix.stdout q.f1_side.side_raw;
						Ptr.Ref.really_write_ref Unix.stdout q.f1_data;
*)
						loop ()
					)
				)
			)
		in
		loop ()
	with
	| e -> (
		if debug then Printf.eprintf "worker returned \"%s\"\n%!" (Printexc.to_string e);
		exit 256
	);
;;


let temp_out_h = Unix.openfile "pipe.bin" [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640;;


(* These functions are for the main program to send and receive stuff from the workers *)
let (send_frame, send_eof, send_exit) =
	let send f1 ?temp_h h =
		let write_if_temp_h = match temp_h with
			| None -> fun a b c -> ()
			| Some h -> fun a b c -> Ptr.really_write h a b c
		in
		let write_ref_if_temp_h = match temp_h with
			| None -> fun x -> ()
			| Some h -> fun x -> Ptr.Ref.really_write_ref h x
		in
		let temp_ptr = Ptr.make 4 0 in
		Ptr.put_32_of_int temp_ptr 0 0x23232323;
		write_if_temp_h temp_ptr 0 4;
		let total_bytes = Ptr.Ref.length f1.f1_header.header_raw + Ptr.Ref.length f1.f1_side.side_raw + Ptr.Ref.length f1.f1_data in
		if Ptr.Ref.length f1.f1_header.header_raw <> 4 then failwith "Header wrong size";
		Ptr.put_32_of_int temp_ptr 0 f1.f1_num;
		write_if_temp_h temp_ptr 0 4;
		Ptr.really_write h temp_ptr 0 4;
		Ptr.put_32_of_int temp_ptr 0 total_bytes;
		write_if_temp_h temp_ptr 0 4;
		Ptr.really_write h temp_ptr 0 4;

		let write_me = Ptr.make total_bytes 0 in
		Ptr.Ref.blit_to_ptr f1.f1_header.header_raw 0 write_me 0 4;
(*		Printf.printf "Header is %s\n" (Ptr.Ref.to_HEX f1.f1_header.header_raw);*)
		Ptr.Ref.blit_to_ptr f1.f1_side.side_raw 0 write_me 4 (Ptr.Ref.length f1.f1_side.side_raw);
		Ptr.Ref.blit_to_ptr f1.f1_data 0 write_me (4 + Ptr.Ref.length f1.f1_side.side_raw) (Ptr.Ref.length f1.f1_data);
(*		Printf.printf "Writing %s\n" (Ptr.to_HEX write_me);*)

(*		Ptr.really_write h write_me 0 total_bytes;*)
(*		write_if_temp_h write_me 0 total_bytes;*)
(*
		for i = 0 to total_bytes - 1 do
			write_if_temp_h write_me i 1;
			Ptr.really_write h write_me i 1;
		done;
*)

		Printf.printf "Writing %016X\n" (Obj.magic f1.f1_header.header_raw);
		write_ref_if_temp_h f1.f1_header.header_raw;
		write_ref_if_temp_h f1.f1_side.side_raw;
		write_ref_if_temp_h f1.f1_data;
		Ptr.Ref.really_write_ref h f1.f1_header.header_raw;
		Ptr.Ref.really_write_ref h f1.f1_side.side_raw;
		Ptr.Ref.really_write_ref h f1.f1_data;

	in
	let eof tot h =
		let temp_ptr = Ptr.make 4 0 in
		Ptr.put_32_of_int temp_ptr 0 tot;
		Ptr.really_write h temp_ptr 0 4;
		Ptr.put_32_of_int temp_ptr 0 0;
		Ptr.really_write h temp_ptr 0 4;
	in
	let final h =
		let temp_ptr = Ptr.make 4 0 in
		Ptr.put_32_of_int temp_ptr 0 (-1);
		Ptr.really_write h temp_ptr 0 4;
	in
	(send, eof, final)
;;


type recv_t =
	| Recv_exit
	| Recv_EOF of int
	| Recv_frame of f1_t * bool(* recompress error *)
;;

let recv_frame =
	let temp_ptr = Ptr.make 4 0 in
	fun h -> (
(*		Gc.compact ();*)
		Ptr.put_32_of_int temp_ptr 0 0x23232323;
		Ptr.really_write temp_out_h temp_ptr 0 4;
		Ptr.really_read h temp_ptr 0 4;
		Ptr.really_write temp_out_h temp_ptr 0 4;
		let frame_num = Ptr.get_int_of_32 temp_ptr 0 in
		if frame_num = -1 then (
			Recv_exit
		) else if frame_num < 0 then (
			(* ERROR! *)
(*			failwith (Printf.sprintf "invalid frame received from worker %s" (Ptr.to_HEX temp_ptr));*)
			exit 22;
		) else (
			Ptr.really_read h temp_ptr 0 4;
			Ptr.really_write temp_out_h temp_ptr 0 4;
			let frame_len = Ptr.get_int_of_32 temp_ptr 0 in
			if frame_len = 0 then (
				Recv_EOF frame_num
			) else if frame_len < 4 + 9 || frame_len > max_reasonable_len then (
(*				failwith (Printf.sprintf "invalid frame length received from worker %s" (Ptr.to_HEX temp_ptr));*)
				exit 33;
			) else (
				Ptr.really_read h temp_ptr 0 4;
				Ptr.really_write temp_out_h temp_ptr 0 4;
				let error_mask = Ptr.get_int_of_32 temp_ptr 0 in
				let recompress_error = (error_mask land error_code_recompress <> 0) in
				let invalid_error_code = (error_mask land unused_error_codes <> 0) in

				if invalid_error_code then (
(*					failwith (Printf.sprintf "invalid error code received from worker %s" (Ptr.to_HEX temp_ptr));*)
					exit 44;
				) else (
					let frame_ptr = Ptr.make frame_len 0 in
					Ptr.really_read h frame_ptr 0 frame_len;
					Ptr.really_write temp_out_h frame_ptr 0 frame_len;
					let header_ptrref = Ptr.Ref.of_subptr frame_ptr 0 4 in

					match Mp3read.header_of_ptrref false header_ptrref with
					| None -> (*failwith "invalid frame header received from worker"*)exit 55
					| Some header -> (
						let side_len = match (header.header_id, header.header_channel_mode) with
							| (MPEG1, ChannelMono) -> 17
							| (  _  , ChannelMono) ->  9
							| (MPEG1,      _     ) -> 32
							| (  _  ,      _     ) -> 17
						in
						let side_ptrref = Ptr.Ref.of_subptr frame_ptr 4 side_len in
						let got_frame = {
							f1_num = frame_num;
							f1_header = header;
							f1_side = Mp3read.side_info_of_header header side_ptrref;
							f1_data = Ptr.Ref.of_subptr frame_ptr (4 + side_len) (frame_len - 4 - side_len);
							f1_pad_exact = None;
						} in
						Recv_frame (got_frame, recompress_error)
					)
				)
			)
		)
	)
;;


class processes num_procs debug =
	object(o)

		val procs = Array.init num_procs (fun i ->
			let (stdin_read,  stdin_write)  = Unix.pipe () in
			let (stdout_read, stdout_write) = Unix.pipe () in
			let proc_id = Unix.create_process argv.(0) [|Sys.argv.(0); "--worker"; string_of_int i; "--nice"; "0"|] stdin_read stdout_write Unix.stderr in
			(proc_id, stdin_write, stdout_read)
		)

		val temp_h = Array.init num_procs (fun i -> Unix.openfile (Printf.sprintf "worker_%d_from_main.bin" i) [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640)

		val write_worker_ref = ref 0
		val read_worker_ref = ref 0

		method next_worker r = (
			let old = !r in
			r := (succ old) mod num_procs;
			old
		)
		method next_write_worker = o#next_worker write_worker_ref
		method next_read_worker = o#next_worker read_worker_ref

		method scatter_frame f = (
			let worker_num = o#next_write_worker in
			let (worker_pid, worker_stdin, worker_stdout) = procs.(worker_num) in
			send_frame f ~temp_h:(temp_h.(worker_num)) worker_stdin;
		)
		method scatter_eof num_frames = (
			let worker_num = o#next_write_worker in
			let (worker_pid, worker_stdin, worker_stdout) = procs.(worker_num) in
			send_eof num_frames worker_stdin;
		)
		method scatter_exit = (
			Array.iter (fun (worker_pid, worker_stdin, worker_stdout) ->
				send_exit worker_stdin
			) procs;
			Array.iter Unix.close temp_h;
		)

		method gather = (
			let worker_num = o#next_read_worker in
			let (worker_pid, worker_stdin, worker_stdout) = procs.(worker_num) in
			recv_frame worker_stdout
		)

	end
;;

(*
type frame_queue_t =
	| Queued_frame of (int * int) (* (frame_num, worker_num) *)
	| Queue_file_end of int (* num_frames *)
;;
let start_procs num_procs debug =
	let ret = Array.init num_procs (fun i ->
		let (stdin_read,  stdin_write)  = Unix.pipe () in
		let (stdout_read, stdout_write) = Unix.pipe () in
		let proc_id = Unix.create_process argv.(0) [|Sys.argv.(0); "--worker"|] stdin_read stdout_write Unix.stderr in
		(proc_id, stdin_write, stdout_read)
	) in

	(* Have to kill off the workers when the user quits *)
	let close_workers () = Array.iter (fun (_,a,b) -> Unix.close a; Unix.close b) ret in
	Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
		close_workers ();
		exit Sys.sigint (* I guess... *)
	));

	let q_mutex = Mutex.create () in
	let q = Queue.create () in

	let next_frame_num_ref = ref 0 in

	(* Now make functions which distribute the frames to the workers *)
	let scatter_frame f =
		let worker_num = (*Random.int num_procs*)f.f1_num mod num_procs in
		let (worker_pid, worker_stdin, worker_stdout) = ret.(worker_num) in
		next_frame_num_ref := (succ f.f1_num);

		send_frame f worker_stdin;
		Mutex.lock q_mutex;
		Queue.add (Queued_frame (f.f1_num, worker_num)) q;
		Mutex.unlock q_mutex;
	in
	let scatter_file_end () =
		let num_frames = !next_frame_num_ref in
		Mutex.lock q_mutex;
		Queue.add (Queue_file_end num_frames) q;
		Mutex.unlock q_mutex;
		next_frame_num_ref := 0;
	in
	let scatter_close () = close_workers () in

	let gather_frame () =
		Mutex.lock q_mutex;
		Queue.take
		Mutex.unlock q_mutex;
*)



