#include <stdlib.h>
#include <sys/time.h>
#include <string.h>
#include "efficienC.h"

void ext_chan_undefined(/*@unused@*/ WORD count, /*@unused@*/ BPOOTER address)
{
	/* FIXME: A hook like in ins_not_impl */
}

EXT_CHAN_FUNCTION ext_chan_table[EXT_CHAN_ENTRIES] =
{
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined,
	ext_chan_undefined
};

void exit_runloop()
{
	abort();
}

void tvm_sleep()
{
	/* FIXME! */
}

int AFTER(WORD t1, WORD t2)
{
	/* From the T9000 Transputer Instruction Set Manual, nice and simple */\
	return ((t1 - t2) > 0);
}

WORD get_time(void)
{
	struct timeval t;

	gettimeofday(&t, 0);
	return (WORD)((t.tv_sec * 1000000) + t.tv_usec);
}


/* FIXME: The add_to_queue macro in soccam is in the helpers.scm, though
 * it probably ought to live with the rest of the scheduler stuff in
 * scheduler.scm
 */
/* FIXME: I think this should probably take a POOTER as the first argument, and
 * possibly a BPOOTER as the second */
inline void just_add_to_queue(WORD src_reg)
{
	/* src_reg is effectively the new workspace pointer */

	if(fptr == (POOTER)NOT_PROCESS_P)
	{
		/* If there is nothing on the queue, we will make this process
		 * the only thing on the queue */
		fptr = (POOTER)src_reg;
		bptr = (POOTER)src_reg;
	}
	else
	{
		/* There are other things on the queue, we add this process to the 
		 * back pointer, and add a link to this process at into the previous
		 * processes workspace */
		/* FIXME: soccam does not use the workspace! macro here, it should! */
		WORKSPACE_SET(bptr, WS_NEXT, src_reg);
		bptr = (POOTER)src_reg;
	}
}

inline void add_to_queue(WORD src_reg, WORD iptr_prime)
{
	just_add_to_queue(src_reg);
	
	WORKSPACE_SET(bptr, WS_IPTR, iptr_prime);
}

int dummy_sync = 0;



void print_memory(POOTER start, int num) {
  int *i;
  for (i = start ; i < (start + num) ; i++) {
    printf("[%08x] %08x\n", i, *i);
  }
}

/* FIXME: These should go elsewhere!!! */
#define TRUE 1
#define FALSE 0
inline BPOOTER run_next_on_queue()
{
	//int loops = 0;
	//int now;
	int removed;
	/*
  ! SEQ
	!   tim ? now
	!   removed := FALSE
	!   WHILE (Tptr <> NotProcess.p) AND (NOT (Tptr^[Time] AFTER now))
	!     SEQ
	!       ...  move first process on timer queue to the run queue
	!       removed := TRUE
	!   IF
	!     (Tptr <> NotProcess.p) AND removed
	!       ualarm (Tptr^[Time] MINUS now, 0)
	!     TRUE
	!       SKIP
	*/
	/*
	printf("\\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/\n");
	printf("ENTERED SCHEDULER!\n");
	print_timer_queue();
	print_scheduling_queue();
	*/
sched_start:
#ifndef ENABLE_TIMER
	removed = FALSE;
	if(dummy_sync != 0)
	{
		dummy_sync = 0;
	}
#elif
	now = get_time();
	removed = FALSE;

	while((tptr != (POOTER)NOT_PROCESS_P) && (!(AFTER(WORKSPACE_GET(tptr, WS_TIMEOUT), now))))
	{
		/* Move first process from timer queue to the run queue */
		/*
		! -- Move first process on timer queue to the run queue
		!
		! POINTER temp := Tptr:
		! SEQ
		!   Tptr := Tptr^[TLink]
		!   temp^[TLink] := TimeSet.p           -- i.e. time expired
		!   temp^[Time] := now                  -- ??
		!   IF
		!     temp^[State] = Ready.p
		!       SKIP                            -- used to trap to impossible(1)
		!                                       -- the ALTing process has
		!                                       -- already been put on the
		!                                       -- run queue by something else
		!     TRUE
		!       SEQ
		!         temp^[State] := Ready.p
		!         ...  add on to run queue
		*/
		POOTER temp = tptr;
		tptr = (POOTER)WORKSPACE_GET(tptr, WS_NEXT_T);
		WORKSPACE_SET(temp, WS_NEXT_T, TIME_SET_P);
		WORKSPACE_SET(temp, WS_TIMEOUT, now);
		if(WORKSPACE_GET(temp, WS_ALT_STATE) == READY_P) 
		{
			/* SKIP -- do nothing */
			/*printf("TIMER - SKIP\n");*/
		}
		else
		{
			/*printf("TIMER - ADDED TO QUEUE\n");*/
			WORKSPACE_SET(temp, WS_ALT_STATE, READY_P);
			/* Add on to run queue */
			just_add_to_queue((WORD)temp);
		}
	/*
	print_timer_queue();
	print_scheduling_queue();
	*/
		removed = TRUE;
	}
	/* We update tnext here (which is equivalent to calling ualarm in the occam
	 * code, they seem to use OS services rather than tnext though) */
	if(tptr != (POOTER)NOT_PROCESS_P && removed)
	{
		tnext = WORKSPACE_GET(tptr, WS_TIMEOUT);
	}
#endif

	/* Any processes in the run queue? */
	if(fptr != (POOTER)NOT_PROCESS_P)
	{
		/* yes */
		/*printf("SCHEDULED NEW PROCESS!\n");*/
		wptr = fptr;
	}
	else if(tptr != (POOTER)NOT_PROCESS_P)
	{
#ifndef BUSY_WAIT
		/* It would be nice to get rid of the timer tests at the start of the kernel
		 * and put them here, which I think is ok... so they dont happen everytime
		 * we go into the kernel? Will have to think a bit more about this
		 * perhaps, and check its ok, but it would seem sensible???
		 */
		/* This function sleeps until a time close to when the next process is ready
		 */
		tvm_sleep();
#endif
		goto sched_start;
	}
	else
	{
		/* DEADLOCK */
		exit_runloop(EXIT_DEADLOCK);
	}
	/*printf("LOOPED %d TIMES\n", loops);*/

	/* Update thet run queue by taking new current process off it */
	if(fptr == bptr)
	{
		fptr = (POOTER)NOT_PROCESS_P;
		bptr = (POOTER)NOT_PROCESS_P;
	}
	else
	{
		fptr = (POOTER)WORKSPACE_GET(fptr, WS_NEXT);
	}
	//iptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
	return (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
	/*printf("/\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\\n");*/
}


/* 0x03 - 0xF3 - endp - end process */
BPOOTER endp(WORD areg)
{
	POOTER count_ptr = pooter_plus((POOTER)areg, 1);

	/* Check the process count */
	/* if(((WORD *)areg)[1] == 1) */
	if(read_mem(count_ptr) == 1) 
	{
		/* No more child processes, continue as the parent process */

		/* Set the process count to zero */
		/* ((WORD *)areg)[1] = 0; */
		write_mem(count_ptr, 0);
		/* Get the resume address from the workspace */
		/* iptr = (BYTE *)((WORD *)areg)[0]; */
		// MOVED TO RETURN iptr = (BPOOTER)read_mem((POOTER)areg);
		/* The areg becomes the new wptr */
		wptr = (POOTER)areg;
		return (BPOOTER)read_mem((POOTER)areg);
		/* The entire stack becomes undefined */
		//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));
	}
	else
	{
		/* Terminate current process, and reschedule another from the queue */

		/* Subtract one from the process count */
		/*((WORD *)areg)[1] = ((WORD *)areg)[1] - 1;*/
		write_mem(count_ptr, read_mem(count_ptr) - 1);
		/* The entire stack becomes undefined */
		//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));
		/* Run the next process */
		return run_next_on_queue();
	}
}


/* 0x07 - 0xF7 - in - input message */
/* FIXME: In soccam make sure that in and out are a bit more consittent  in
 * variable naming, and order of things */
inline BPOOTER in(WORD num_bytes, POOTER chan_ptr, BPOOTER write_start, BPOOTER iptr)
{
	/* FIXME: These instructions are interutable in the transputer,
	 * ie a message of size X (words) would take X copies to complete,
	 * and as X could be arbitrarily large, this instruciton can suspend
	 * during the copying.
	 *
	 * In soccam Matt claims that I (clj) claim that this is easy to do,
	 * which I a lie! I think Matt should be given the task of implementing
	 * this functionality as punishment for trying smear my good reputation.
	 */

	/* Makes this code look a bit nicer, and more like soccam */
	/*
	WORD num_bytes      = areg;
	POOTER chan_ptr     = (POOTER)breg;
	BPOOTER write_start = (BPOOTER)creg;
	*/
	POOTER chan_ptr_val = (POOTER) read_mem(chan_ptr);

	if(chan_ptr_val == NOT_PROCESS_P)
	{
		/* FIXME: This could be a function shared between in and out */
		/* ...Put this process('s wptr) into the channel word, */
		write_mem(chan_ptr, (WORD)wptr);
		/* store our state */ 
		WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
		WORKSPACE_SET(wptr, WS_CHAN, (WORD)write_start);
		/* and reschedule */
		return run_next_on_queue();
	} 
	else
	{
		/* FIXME: Optimise this so if we are reading by a multiple of WORD, use
		 * read_mem rather than read_byte */

		/* Where we start reading from */
		/* FIXME: The code in soccam does the following, which is wrong:
		   BPOOTER read_start = (BPOOTER)read_mem(chan_ptr);
		   It should be doing: */
		BPOOTER read_start = (BPOOTER)WORKSPACE_GET((POOTER)chan_ptr_val, WS_CHAN);
		//WORD count = 0;

		/* Copy the data */
  
    /* printf("- in - %d\n", *read_start); */
		memcpy(write_start, read_start, num_bytes);

		/* Add ourselves to the back of the runqueue */
		add_to_queue((WORD)wptr, (WORD)iptr);
		/* Reschedule the process at the other end of the channel */
		wptr = (POOTER)chan_ptr_val;
		// MOVED TO RETURN iptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
		/* Set the channel word to NotProcess.p */
		write_mem(chan_ptr, NOT_PROCESS_P);
		return (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
	}

	//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));
}

/* 0x0B - 0xFB - out - output message */
inline BPOOTER out(WORD num_bytes, POOTER chan_ptr, BPOOTER read_start, BPOOTER iptr)
{
	/* FIXME: These instructions are interutable in the transputer,
	 * ie a message of size X (words) would take X copies to complete,
	 * and as X could be arbitrarily large, this instruciton can suspend
	 * during the copying.
	 *
	 * In soccam Matt claims that I (clj) claim that this is easy to do,
	 * which I a lie! I think Matt should be given the task of implementing
	 * this functionality as punishment for trying smear my good reputation.
	 */

	/* Makes this code look a bit nicer, and more like soccam */
	/* FIXME: But what the heck are those numbers next to these declerations in
	 * soccam????? */
	/*
	WORD num_bytes     = areg;
	POOTER chan_ptr    = (POOTER)breg;
	BPOOTER read_start = (BPOOTER)creg;
	*/
	POOTER chan_ptr_val = (POOTER) read_mem(chan_ptr);


	if(chan_ptr_val == NOT_PROCESS_P)
	{
		/* FIXME: This could be a function shared between in and out */
		/* ...Put this process('s wptr) into the channel word, */
		write_mem(chan_ptr, (WORD)wptr);
		/* store our state */ 
		WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
		WORKSPACE_SET(wptr, WS_CHAN, (WORD)read_start);
		/* and reschedule */
		return run_next_on_queue();
	}
	else //if((read_mem(chan_ptr) != NOT_PROCESS_P))
	{
		int alt_state = WORKSPACE_GET((POOTER)chan_ptr_val, WS_ALT_STATE);

		if((alt_state && 0xFFFFFFFC != MIN_INT ))
		{
			BPOOTER write_start = (BPOOTER)WORKSPACE_GET((POOTER)chan_ptr_val, WS_CHAN);

      /* printf("- outs - %d\n", *read_start); */
			memcpy(write_start, read_start, num_bytes);

			/* Add ourselves to the back of the runqueue */
			add_to_queue((WORD)wptr, (WORD)iptr);
			/* Reschedule the process at the other end of the channel */
			wptr = (POOTER)chan_ptr_val;
			// MOVED TO RETURN iptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
			/* Set the channel word to NotProcess.p */
			write_mem(chan_ptr, NOT_PROCESS_P);
			return (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
		}
		else switch(alt_state)
		{
		  printf("ALTING\n");
		  abort();
		  
			case ENABELING_P:
			{
				WORKSPACE_SET((POOTER)read_mem(chan_ptr), WS_ALT_STATE, DISABELING_P);
				WORKSPACE_SET(chan_ptr, WS_TOP, (WORD)wptr);
				WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
				/* FIXME: This used to be WS_ALT_STATE (which is == WS_CHAN), though
				 * possibly not as semanticalyly clear, a leftover from soccam?
				 */
				WORKSPACE_SET(wptr, WS_ALT_STATE, (WORD)read_start);
				return run_next_on_queue();
			}
			break;
			case WAITING_P:
			{
				POOTER old_chan_word = (POOTER)read_mem(chan_ptr);

				WORKSPACE_SET((POOTER)read_mem(chan_ptr), WS_ALT_STATE, DISABELING_P);
				WORKSPACE_SET(chan_ptr, WS_TOP, (WORD)wptr);
				WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
				WORKSPACE_SET(wptr, WS_CHAN, (WORD)read_start);

				/* The alt process is rescheduled, and this process is descheduled */
				wptr = old_chan_word;
				//MOVED TO RETRUN iptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
				return (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
			}
			break;
			case DISABELING_P:
			{
				WORKSPACE_SET(chan_ptr, WS_TOP, (WORD)wptr);
				WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
				WORKSPACE_SET(wptr, WS_CHAN, (WORD)read_start);
				return run_next_on_queue();
			}
			break;
			default:
			abort();
		}
	}


	//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));
}



