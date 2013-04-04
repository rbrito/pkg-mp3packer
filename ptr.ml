type t;;

(* NEW PTR STYLE *)

external make : int -> int -> t = "ptr_make";;

external length : t -> int = "ptr_length" "noalloc";;
external align : t -> int = "ptr_align" "noalloc";;


(* VirtualAlloc things *)
(* Only for Windows! *)
external get_page_size : unit -> int = "ptr_get_page_size" "noalloc";;
let page_size = get_page_size ();;
external make_virtual_alloc_unsafe : int -> int -> t = "ptr_make_virtual_alloc";;
let make_page x = make_virtual_alloc_unsafe page_size x;;

(********)
(* BLIT *)
(********)

external clear : t -> unit = "ptr_clear" "noalloc";;

(* Good for doing "let x = Ptr.clearret (Ptr.make a b)" *)
let clearret x =
	clear x;
	x
;;

external blit_unsafe : t -> int -> t -> int -> int -> unit = "ptr_blit" "noalloc";;
let blit pfrom fromoff pto tooff len =
	if fromoff < 0 || fromoff + len > length pfrom || tooff < 0 || tooff + len > length pto || len < 0 then (
		invalid_arg "Ptr.blit"
	) else (
		blit_unsafe pfrom fromoff pto tooff len
	)
;;

external blit_from_string_unsafe : string -> int -> t -> int -> int -> unit = "ptr_blit_from_string" "noalloc";;
let blit_from_string s soff p poff len =
	if soff < 0 || soff + len > String.length s || poff < 0 || poff + len > length p || len < 0 then (
		invalid_arg "Ptr.blit_from_string"
	) else (
		blit_from_string_unsafe s soff p poff len
	)
;;

external blit_to_string_unsafe : t -> int -> string -> int -> int -> unit = "ptr_blit_to_string";;
let blit_to_string p poff s soff len =
	if soff < 0 || soff + len > String.length s || poff < 0 || poff + len > length p || len < 0 then (
		invalid_arg "Ptr.blit_to_string"
	) else (
		blit_to_string_unsafe p poff s soff len
	)
;;

let copy pin =
	let pout = make (length pin) (align pin) in
	blit_unsafe pin 0 pout 0 (length pin);
	pout
;;

let sub pin off len =
	if off < 0 || off + len > length pin then (
		invalid_arg "Ptr.sub"
	) else (
		let pout = make len (align pin) in
		blit_unsafe pin off pout 0 len;
		pout
	)
;;

let sub_to_string pin off len =
	if off < 0 || off + len > length pin then (
		invalid_arg "Ptr.sub_to_string"
	) else (
		let sout = String.create len in
		blit_to_string_unsafe pin off sout 0 len;
		sout
	)
;;


(*********************)
(* FROM / TO STRINGS *)
(*********************)

let of_string s =
	let p = make (String.length s) 1 in
	blit_from_string_unsafe s 0 p 0 (String.length s);
	p
;;
let to_string p =
	let s = String.create (length p) in
	blit_to_string_unsafe p 0 s 0 (length p);
	s
;;


(**************)
(* SMALL COPY *)
(**************)

(* PUT *)
let put_this name len f ptr off num =
	if off < 0 || off > length ptr - len then (
		invalid_arg name
	) else (
		f ptr off num
	)
;;

external put_8_of_int_unsafe    : t -> int -> int   -> unit = "ptr_put_8_of_int" "noalloc";;
external put_16_of_int_unsafe   : t -> int -> int   -> unit = "ptr_put_16_of_int" "noalloc";;
external put_32_of_int_unsafe   : t -> int -> int   -> unit = "ptr_put_32_of_int" "noalloc";;
external put_64_of_int_unsafe   : t -> int -> int   -> unit = "ptr_put_64_of_int" "noalloc";;
external put_64_of_int64_unsafe : t -> int -> int64 -> unit = "ptr_put_64_of_int64" "noalloc";;
external put_32_of_float_unsafe : t -> int -> float -> unit = "ptr_put_32_of_float" "noalloc";;
external put_64_of_float_unsafe : t -> int -> float -> unit = "ptr_put_64_of_float" "noalloc";;

let put_8_of_int    = put_this "Ptr.put_8_of_int"    1 put_8_of_int_unsafe;;
let put_16_of_int   = put_this "Ptr.put_16_of_int"   2 put_16_of_int_unsafe;;
let put_32_of_int   = put_this "Ptr.put_32_of_int"   4 put_32_of_int_unsafe;;
let put_64_of_int   = put_this "Ptr.put_64_of_int"   8 put_64_of_int_unsafe;;
let put_64_of_int64 = put_this "Ptr.put_64_of_int64" 8 put_64_of_int64_unsafe;;
let put_32_of_float = put_this "Ptr.put_32_of_float" 4 put_32_of_float_unsafe;;
let put_64_of_float = put_this "Ptr.put_64_of_float" 8 put_64_of_float_unsafe;;
let put_byte = put_8_of_int;;

(* GET *)
let get_this name len f ptr off =
	if off < 0 || off > length ptr - len then (
		invalid_arg name
	) else (
		f ptr off
	)
;;

(* "u" is unsigned *)
external get_int_of_8_unsafe    : t -> int -> int   = "ptr_get_int_of_8" "noalloc";;
external get_int_of_8u_unsafe   : t -> int -> int   = "ptr_get_int_of_8u" "noalloc";;
external get_int_of_16_unsafe   : t -> int -> int   = "ptr_get_int_of_16" "noalloc";;
external get_int_of_16u_unsafe  : t -> int -> int   = "ptr_get_int_of_16u" "noalloc";;
external get_int_of_32_unsafe   : t -> int -> int   = "ptr_get_int_of_32" "noalloc";;
external get_int_of_32u_unsafe  : t -> int -> int   = "ptr_get_int_of_32u" "noalloc";;
external get_int_of_64_unsafe   : t -> int -> int   = "ptr_get_int_of_64" "noalloc";;
external get_int_of_64u_unsafe  : t -> int -> int   = "ptr_get_int_of_64u" "noalloc";;
external get_int64_of_64_unsafe : t -> int -> int64 = "ptr_get_int64_of_64";;
external get_float_of_32_unsafe : t -> int -> float = "ptr_get_float_of_32";;
external get_float_of_64_unsafe : t -> int -> float = "ptr_get_float_of_64";;

let get_int_of_8    = get_this "Ptr.get_int_of_8"    1 get_int_of_8_unsafe;;
let get_int_of_8u   = get_this "Ptr.get_int_of_8u"   1 get_int_of_8u_unsafe;;
let get_int_of_16   = get_this "Ptr.get_int_of_16"   2 get_int_of_16_unsafe;;
let get_int_of_16u  = get_this "Ptr.get_int_of_16u"  2 get_int_of_16u_unsafe;;
let get_int_of_32   = get_this "Ptr.get_int_of_32"   4 get_int_of_32_unsafe;;
let get_int_of_32u  = get_this "Ptr.get_int_of_32u"  4 get_int_of_32u_unsafe;;
let get_int_of_64   = get_this "Ptr.get_int_of_64"   8 get_int_of_64_unsafe;;
let get_int_of_64u  = get_this "Ptr.get_int_of_64u"  8 get_int_of_64u_unsafe;;
let get_int64_of_64 = get_this "Ptr.get_int64_of_64" 8 get_int64_of_64_unsafe;;
let get_float_of_32 = get_this "Ptr.get_float_of_32" 4 get_float_of_32_unsafe;;
let get_float_of_64 = get_this "Ptr.get_float_of_64" 8 get_float_of_64_unsafe;;
let get_byte = get_int_of_8u;;




(********)
(* MMAP *)
(********)
(* Now supports start / length arguments! *)
type map_access_t = Map_cow | Map_read_only | Map_write;;
external map_handle : Unix.file_descr -> int -> int -> map_access_t -> t = "ptr_map_handle";;
external flush_map : t -> bool = "ptr_flush_map";;
external unmap : t -> unit = "ptr_unmap";;


(************************)
(* UNIXY FILE FUNCTIONS *)
(************************)
external read_unsafe : Unix.file_descr -> t -> int -> int -> bool -> int -> int = "ptr_read_bytecode" "ptr_read";;
let read fh ?(pos=(-1)) ptr off len =
	if off < 0 || len < 0 || off + len > length ptr then (
		invalid_arg "Ptr.read"
	) else (
		read_unsafe fh ptr off len false pos
	)
;;
let really_read fh ?(pos=(-1)) ptr off len =
	if off < 0 || len < 0 || off + len > length ptr then (
		invalid_arg "Ptr.really_read"
	) else (
		ignore (read_unsafe fh ptr off len true pos)
	)
;;

external write_unsafe : Unix.file_descr -> t -> int -> int -> bool -> int -> int = "ptr_write_bytecode" "ptr_write";;
let write fh ?(pos=(-1)) ptr off len =
	if off < 0 || len < 0 || off + len > length ptr then (
		invalid_arg "Ptr.write"
	) else (
		write_unsafe fh ptr off len false pos
	)
;;
let really_write fh ?(pos=(-1)) ptr off len =
	if off < 0 || len < 0 || off + len > length ptr then (
		invalid_arg "Ptr.really_write"
	) else (
		ignore (write_unsafe fh ptr off len true pos)
	)
;;


(************)
(* PRINTING *)
(************)
let ptr_to_HEX =
	let x = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F'|] in
	fun p -> (
		let s = String.create (length p * 2) in
		for pi = 0 to length p - 1 do
			let c = get_int_of_8u p pi in
			s.[2 * pi + 0] <- x.(c lsr 4);
			s.[2 * pi + 1] <- x.(c land 0xF);
		done;
		s
	)
;;

