#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#if defined(__WIN32__) || defined(WIN32) || defined(_WIN32)
#include <windows.h>
#endif


CAMLprim value caml_nice(value val_niceness)
{

#if defined(__WIN32__) || defined(WIN32) || defined(_WIN32)

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

