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


class virtual virt_mp3write =
	object

		method virtual output_this : string -> unit
		method virtual seek : int -> unit
		method virtual pos : int
		method virtual close : unit

	end
;;

class mp3write_pervasive out_file =
	object
		inherit virt_mp3write

		val handle = open_out_bin out_file
		method output_this = output_string handle
		method seek = seek_out handle
		method pos = pos_out handle
		method close = close_out handle

	end
;;

class mp3write_unix ?(flags=[Unix.O_EXCL]) out_file =
	object(o)
		inherit virt_mp3write

		val handle = Unix.openfile out_file (Unix.O_WRONLY :: Unix.O_CREAT :: flags) 0o660
		method output s r l = (
			if l = 0 then () else (
				let wrote = Unix.write handle s r l in
				o#output s (r + wrote) (l - wrote)
			)
		)
		method output_this s = o#output s 0 (String.length s)
		method seek i = ignore (Unix.lseek handle i Unix.SEEK_SET)
		method pos = Unix.lseek handle 0 Unix.SEEK_CUR
		method close = Unix.close handle
	end
;;

