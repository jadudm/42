/* #define USE_REGISTERS */
#define USE_SEQ_DELTA


#define NOT_PROCESS_P 0


#define debug(format, args...) 0;
/* printf(format, ## args) */
/* #define real_printf(format, args...) printf(format, ## args) */


#if defined USE_REGISTERS
# if defined __ppc__
/* PowerPC r24-r27 */
#   define WPTR_REG asm("r24")
#   define WPTR_MOD register
#   define FPTR_REG asm("r25")
#   define FPTR_MOD register
#   define BPTR_REG asm("r26")
#   define BPTR_MOD register
#   warning "Abusing PowerPC registers"
# elif defined __sparc__
/* SPARC g4, g5, g1 */
#   define WPTR_REG asm("g5")
#   define WPTR_MOD register
#   define FPTR_REG asm("g4")
#   define FPTR_MOD register
#   define BPTR_REG asm("g3")
#   define BPTR_MOD register
#  warning "Abusing Sparc registers"
# elif defined __i386__
/* Intel ebp, ebx http://www.swansontec.com/sregisters.html */
#   define WPTR_REG asm("ebp")
#   define WPTR_MOD register
#   define FPTR_REG asm("ebx")
#   define FPTR_MOD register
#   define BPTR_REG
#   define BPTR_MOD extern
#  warning "Abusing Intel registers"
#endif /* end __ppc__, etc. */
#else
# define WPTR_REG
# define WPTR_MOD 
# define FPTR_REG
# define FPTR_MOD 
# define BPTR_REG
# define BPTR_MOD 
#endif

 WPTR_MOD int *wptr WPTR_REG;
 FPTR_MOD int *fptr FPTR_REG;
 BPTR_MOD int *bptr BPTR_REG;
extern int *tptr;
extern int tnext;

/* From runtime.c */

#define pri 0
#define WORDLENGTH 4
#define UNDEFINE(reg) reg

//#       define MIN_INT        0x80000000
#define MIN_INT 0
#       define MAX_INT        (MIN_INT - 1)
#       define RET_VALUE      0xFFFFFFFF

#define ENABELING_P       (MIN_INT + 1)
#define WAITING_P         (MIN_INT + 2)
#define DISABELING_P      (MIN_INT + 3)
#define READY_P      (MIN_INT + 3)
#define TIME_SET_P        (MIN_INT + 1)
#define TIME_NOT_SET_P    (MIN_INT + 2)
#define NONE_SELECTED_O   (-1)

#define EXIT_STACK_BOTTOM 0   /* For what currently ammounts to a good exit */
#define EXIT_DEADLOCK     100 /* When the running program has deadlocked */
#define EXIT_HALTED       200 /* When the program halted due to an error */
#define EXIT_DEBUG_TRAP   300 /* When the program halted due to an J 0 */
#define EXIT_ERROR        400 /* When a runtime error occurs */
#define EXIT_ALIGN_ERROR  666 /* A memory alignment error */
#define EXIT_INS_NOT_IMP  999 /* If we hit an unimplemented instruction */
#define EXIT_INS_INVALID  998 /* If we hit an invalid instruction */
#define EXIT_SCHEDULER_BAD_1 5000 /* If the scheduler  has gone bad */



/* First, the symbols for the various workspace locations */
#define WS_TOP       0
#define WS_IPTR      1 /* Valid when: the process has been descheduled */
#define WS_NEXT      2 /* Valid when: the process is on a scheduling list */
#define WS_CHAN      3 /* Valid when: the process is waiting for */
#define WS_ALT_STATE 3 /*             communication, or is executing an ALT */
#define WS_NEXT_T    4 /* Valid when: the process is on */
#define WS_ALT_T     4 /*             a timer list */
#define WS_TIMEOUT   5 /* Valid when: the process is on a timer list */

#define EXT_CHAN_ENTRIES 19
#define EXT_CHAN_KYB 0
#define EXT_CHAN_SCR 1
#define EXT_CHAN_ERR 2



typedef unsigned char BYTE;
typedef int WORD;
typedef unsigned int UWORD;
typedef short HWORD;
typedef unsigned short UHWORD;

typedef WORD * POOTER;
typedef BYTE * BPOOTER;

typedef void (*EXT_CHAN_FUNCTION)(WORD count, BPOOTER address);

/* Chagned these */
#define read_mem(LOC) (*((int *) (LOC)))
#define write_mem(LOC, VAL) *((int *) (LOC)) = VAL
#define pooter_minus(WPTR, LOC) ((int *)WPTR - LOC)
#define pooter_plus(WPTR, LOC) ((int *)WPTR + LOC)

#define read_byte(LOC) (*((char *) (LOC)))
#define write_byte(LOC, VAL) *((char *) (LOC)) = VAL
#define bpooter_minus(WPTR, LOC) ((char *)WPTR - LOC)
#define bpooter_plus(WPTR, LOC) ((char *)WPTR + LOC)
/*
#define WORKSPACE_GET(WPTR, LOC) \
	read_mem(pooter_minus(WPTR, LOC))
#define WORKSPACE_SET(WPTR, LOC, VAL) \
	write_mem(pooter_minus(WPTR, LOC), VAL)
*/
/* Changed these */
#define WORKSPACE_GET(WPTR, LOC) \
	((int *)WPTR)[-LOC]
#define WORKSPACE_SET(WPTR, LOC, VAL) \
	((int *)WPTR)[-LOC] = VAL
	
/* Prototypes */
inline void add_to_queue(WORD src_reg, WORD iptr_prime);
inline BPOOTER run_next_on_queue();
BPOOTER endp(WORD areg);
inline BPOOTER in(WORD num_bytes, POOTER chan_ptr, BPOOTER write_start, BPOOTER iptr);
inline BPOOTER out(WORD num_bytes, POOTER chan_ptr, BPOOTER read_start, BPOOTER iptr);

WORD get_time(void); 

extern void occam_program();