#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <caml/unixsupport.h>

#if defined(__WIN32__) || defined(WIN32) || defined(_WIN32)
#define WIN32
#endif

#include <stdio.h>

#if defined(WIN32)
#include <windows.h>
#else
#include <sys/types.h>
#include <sys/time.h>
#endif


CAMLprim value c_part_counter_freq() {
	CAMLparam0();
#if defined(WIN32)
	LARGE_INTEGER out_large;
	QueryPerformanceFrequency(&out_large);
	CAMLreturn(Val_long(out_large.QuadPart));
#else
#ifdef HAS_GETTIMEOFDAY
	CAMLreturn(Val_long(1000000));
#else
	CAMLreturn(Val_long(1));
#endif
#endif
}

CAMLprim value c_part_counter(value nothing) {
	CAMLparam1(nothing);
#if defined(WIN32)
	LARGE_INTEGER out_large;
	QueryPerformanceCounter(&out_large);
	CAMLreturn(Val_long(out_large.QuadPart));
#else
#ifdef HAS_GETTIMEOFDAY
	struct timeval tp;
	if(gettimeofday(&tp, NULL) == -1) {
		CAMLreturn(Val_long(0));
	} else {
		CAMLreturn(Val_long(tp.tv_sec * 1000000 + tp.tv_usec));
	}
#else
	CAMLreturn(Val_long(0));
#endif
#endif
}

CAMLprim value caml_nice(value val_niceness)
{

#if defined(WIN32)

	CAMLparam1(val_niceness);

	int niceness = Int_val(val_niceness);
	DWORD priority_class = NORMAL_PRIORITY_CLASS;
	int thread_priority = THREAD_PRIORITY_NORMAL;
	
	/* Simple mapping from Unixy nice values to Windows priority classes */
	if(niceness <= -7) {
		priority_class = HIGH_PRIORITY_CLASS;
		if(niceness <= -16)
			thread_priority = THREAD_PRIORITY_HIGHEST;
		else if(niceness <= -13)
			thread_priority = THREAD_PRIORITY_ABOVE_NORMAL;
		else if(niceness <= -10)
			thread_priority = THREAD_PRIORITY_NORMAL;
		else
			thread_priority = THREAD_PRIORITY_BELOW_NORMAL;
	} else if(niceness <= -1) {
		priority_class = ABOVE_NORMAL_PRIORITY_CLASS;
		if(niceness <= -4)
			thread_priority = THREAD_PRIORITY_ABOVE_NORMAL;
		else
			thread_priority = THREAD_PRIORITY_NORMAL;
	} else if(niceness <= 0) {
		priority_class = NORMAL_PRIORITY_CLASS;
		thread_priority = THREAD_PRIORITY_NORMAL;
	} else if(niceness <= 12) {
		priority_class = BELOW_NORMAL_PRIORITY_CLASS;
		if(niceness <= 3)
			thread_priority = THREAD_PRIORITY_ABOVE_NORMAL;
		else if(niceness <= 6)
			thread_priority = THREAD_PRIORITY_NORMAL;
		else if(niceness <= 9)
			thread_priority = THREAD_PRIORITY_BELOW_NORMAL;
		else
			thread_priority = THREAD_PRIORITY_LOWEST;
	} else {
		priority_class = IDLE_PRIORITY_CLASS;
		if(niceness <= 15)
			thread_priority = THREAD_PRIORITY_BELOW_NORMAL;
		else if(niceness <= 18)
			thread_priority = THREAD_PRIORITY_LOWEST;
		else
			thread_priority = THREAD_PRIORITY_IDLE;
	}
	
	if(SetPriorityClass(GetCurrentProcess(), priority_class) == 0) {
		CAMLreturn(Val_int(0));
	} else {
		if(SetThreadPriority(GetCurrentThread(), thread_priority) == 0) {
			CAMLreturn(Val_int(0));
		} else {
			CAMLreturn(Val_int(niceness));
		}
	}

#else

	/* Do nothing */
	CAMLparam1(val_niceness);
	CAMLreturn(val_niceness);

#endif

}



CAMLprim value get_capabilities() {
	CAMLparam0();
	int info[4];
	int max_eax;
	CAMLlocal1(out_val);
	out_val = caml_alloc_tuple(5);
#if defined(WIN32)
	__cpuid(info, 0);
	max_eax = info[0];
	if(max_eax >= 1) {
		__cpuid(info, 1);
	} else {
		info[0] = 0;
		info[1] = 0;
		info[2] = 0;
		info[3] = 0;
	}
	Store_field(out_val, 0, Val_bool(info[3] & (1 << 25)));
	Store_field(out_val, 1, Val_bool(info[3] & (1 << 26)));
	Store_field(out_val, 2, Val_bool(info[2] & (1 <<  0)));
	Store_field(out_val, 3, Val_bool(info[2] & (1 <<  9)));
	Store_field(out_val, 4, Val_bool(info[2] & (1 << 19)));
#else
	// Don't use SSE stuff - other OSes may be on any random architecture
	Store_field(out_val, 0, Val_bool(0));
	Store_field(out_val, 1, Val_bool(0));
	Store_field(out_val, 2, Val_bool(0));
	Store_field(out_val, 3, Val_bool(0));
	Store_field(out_val, 4, Val_bool(0));
#endif
	CAMLreturn(out_val);
}



CAMLprim value get_os_thread_self_id() {
	CAMLparam0();
	CAMLlocal1(out_val);
#if defined(WIN32)
	BOOL ret;
	HANDLE dup_handle;
	ret = DuplicateHandle(
		GetCurrentProcess(),
		GetCurrentThread(),
		GetCurrentProcess(),
		&dup_handle,
		0,
		FALSE,
		DUPLICATE_SAME_ACCESS
	);
	if(!ret) {
		caml_failwith("Can't get handle to current thread");
	} else {
//		printf("HANDLE IS %p\n", (int)dup_handle);
		out_val = win_alloc_handle(dup_handle);
	}
#else
	out_val = Val_int(gettid());
#endif
	CAMLreturn(out_val);
}

// Make a function to see if a thread is still alive
// *nix uses pthread_kill(pthread_t thread, 0), Windows uses GetExitCodeThread(HANDLE hThread, &exit_code)
CAMLprim value thread_is_alive(value thread_id_val) {
	CAMLparam1(thread_id_val);
	int still_alive;
#if defined(WIN32)
	HANDLE thread_id = Handle_val(thread_id_val);
	DWORD exit_code;
	if(GetExitCodeThread(thread_id, &exit_code)) {
		still_alive = (exit_code == STILL_ACTIVE);
	} else {
		caml_failwith("Can't get status of thread");
	}
#else
	int thread_id = Int_val(thread_id_val);
#endif
	CAMLreturn(Val_bool(still_alive));
}

// Copy file times
CAMLprim value copy_file_times(value in_h_val, value out_h_val) {
	CAMLparam2(in_h_val, out_h_val);
	int good;
#if defined(WIN32)
	HANDLE in_h = Handle_val(in_h_val);
	HANDLE out_h = Handle_val(out_h_val);
	FILETIME create;
	FILETIME access;
	FILETIME modify;
	good = GetFileTime(in_h, &create, &access, &modify);
	if(good) {
		good = SetFileTime(out_h, &create, &access, &modify);
	}
#else
	int in_h = Int_val(in_h_val);
	int out_h = Int_val(out_h_val);
	struct stat s;
	struct timeval set[2];
	good = !fstat(in_h, &s);
	if(good) {
		set[0].tv_sec = s.st_atime;
		set[0].tv_usec = 0;
		set[1].tv_sec = s.st_mtime;
		set[1].tv_usec = 0;
		good = !futimes(out_h, set);
	}
#endif
	CAMLreturn(Val_bool(good));
}

