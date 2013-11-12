#include "efficienC.h"
void occam_program()
{
goto PROCmain;

/* PROC main */
PROCmain:
wptr = wptr - 0;
wptr[-4] = (int) &&return_from_comms_time1546;
wptr[-3] = 3735928559;
wptr[-2] = 3735928559;
wptr[-1] = 3735928559;
debug("call comms_time: [-3 xxx %x] [-2 xxx %x] [-1 xxx %x]\n", wptr[-3], wptr[-2], wptr[-1]);
wptr = wptr - 4;
goto PROCcomms_time;
return_from_comms_time1546:
wptr = wptr + 0;
wptr = wptr + 4;
goto *wptr[-4];

/* PROC id */
PROCid:
wptr = wptr - 1;
if_top1573:
if (1)
{
debug("[in 'b_in' l: 11 c: 12]: %x\n", (int*)wptr[2] /* b_in_1 @2 */);
goto *(in (4, (int *)wptr[2] /* b_in_1 @2 */, (BPOOTER)&wptr[0] /* x_3 @0*/, &&input_jump1547));
input_jump1547:
debug("[out 'a_out' l: 12 c: 12]: %x\n", (int*)wptr[3] /* a_out_2 @3 */);
goto *(out (4, (int *)wptr[3] /* a_out_2 @3 */, (BPOOTER)&wptr[0] /* x_3 @0*/, &&output_jump1548));
output_jump1548:

goto if_top1573;
}
wptr = wptr + 1;
wptr = wptr + 4;
goto *wptr[-4];

/* PROC succ */
PROCsucc:
wptr = wptr - 2;
if_top1574:
if (1)
{
debug("[in 'c_in' l: 20 c: 12]: %x\n", (int*)wptr[3] /* c_in_4 @3 */);
goto *(in (4, (int *)wptr[3] /* c_in_4 @3 */, (BPOOTER)&wptr[0] /* x_6 @0*/, &&input_jump1549));
input_jump1549:
wptr[1] /* val_7 @1*/ = (wptr[0] /* x_6 @0*/) + (1);
debug("[out 'b_out' l: 22 c: 12]: %x\n", (int*)wptr[4] /* b_out_5 @4 */);
goto *(out (4, (int *)wptr[4] /* b_out_5 @4 */, (BPOOTER)&wptr[1] /* val_7 @1*/, &&output_jump1550));
output_jump1550:

goto if_top1574;
}
wptr = wptr + 2;
wptr = wptr + 4;
goto *wptr[-4];

/* PROC delta */
PROCdelta:
wptr = wptr - 3;
if_top1575:
if (1)
{
debug("[in 'in' l: 30 c: 12]: %x\n", (int*)wptr[4] /* in_8 @4 */);
goto *(in (4, (int *)wptr[4] /* in_8 @4 */, (BPOOTER)&wptr[2] /* x_11 @2*/, &&input_jump1551));
input_jump1551:

/* PAR endparlab1556 - 2 START */
wptr[1] = 2;
wptr[0] = &&endparlab1556;
add_to_queue((int)(wptr - 5), (int)&&par1555);
par1554:
debug("[out 'out1' l: 32 c: 13]: %x\n", (int*)wptr[5] /* out1_9 @5 */);
goto *(out (4, (int *)wptr[5] /* out1_9 @5 */, (BPOOTER)&wptr[2] /* x_11 @2*/, &&output_jump1552));
output_jump1552:

goto *(endp((int) (wptr - 5)));
par1555:
debug("[out 'out2' l: 33 c: 13]: %x\n", (int*)wptr[11] /* out2_10 @6 */);
goto *(out (4, (int *)wptr[11] /* out2_10 @6 */, (BPOOTER)&wptr[7] /* x_11 @2*/, &&output_jump1553));
output_jump1553:

goto *(endp((int) (wptr - 10)));
endparlab1556:

goto if_top1575;
}
wptr = wptr + 3;
wptr = wptr + 4;
goto *wptr[-4];

/* PROC prefix */
PROCprefix:
wptr = wptr - 0;
debug("[out 'a_out' l: 39 c: 8]: %x\n", (int*)wptr[3] /* a_out_14 @3 */);
goto *(out (4, (int *)wptr[3] /* a_out_14 @3 */, (BPOOTER)*(wptr + 1) /* n_12 @1*/, &&output_jump1557));
output_jump1557:
wptr[-4] = (int) &&return_from_id1558;
wptr[-3] = *(wptr + 2) /* b_in_13 @2  (locality nonlocal) */;
wptr[-2] = *(wptr + 3) /* a_out_14 @3  (locality nonlocal) */;
wptr[-1] = 3735928559;
debug("call id: [-3 b_in_13 %x] [-2 a_out_14 %x] [-1 xxx %x]\n", wptr[-3], wptr[-2], wptr[-1]);
wptr = wptr - 4;
goto PROCid;
return_from_id1558:
wptr = wptr + 0;
wptr = wptr + 4;
goto *wptr[-4];

/* PROC seq_delta */
PROCseq_delta:
wptr = wptr - 1;
if_top1576:
if (1)
{
debug("[in 'a_in' l: 48 c: 12]: %x\n", (int*)wptr[2] /* a_in_15 @2 */);
goto *(in (4, (int *)wptr[2] /* a_in_15 @2 */, (BPOOTER)&wptr[0] /* x_18 @0*/, &&input_jump1559));
input_jump1559:
debug("[out 'c_out1' l: 50 c: 13]: %x\n", (int*)wptr[3] /* c_out1_16 @3 */);
goto *(out (4, (int *)wptr[3] /* c_out1_16 @3 */, (BPOOTER)&wptr[0] /* x_18 @0*/, &&output_jump1560));
output_jump1560:
debug("[out 'd_out2' l: 51 c: 13]: %x\n", (int*)wptr[4] /* d_out2_17 @4 */);
goto *(out (4, (int *)wptr[4] /* d_out2_17 @4 */, (BPOOTER)&wptr[0] /* x_18 @0*/, &&output_jump1561));
output_jump1561:

goto if_top1576;
}
wptr = wptr + 1;
wptr = wptr + 4;
goto *wptr[-4];

/* PROC consume */
PROCconsume:
wptr = wptr - 5;
wptr[3] /* warm_up_24 @3*/ = 16;
if_top1577:
if ((wptr[3] /* warm_up_24 @3*/) > (0))
{
debug("[in 'd_in' l: 65 c: 16]: %x\n", (int*)wptr[7] /* d_in_20 @7 */);
goto *(in (4, (int *)wptr[7] /* d_in_20 @7 */, (BPOOTER)&wptr[0] /* value_21 @0*/, &&input_jump1562));
input_jump1562:
wptr[3] /* warm_up_24 @3*/ = (wptr[3] /* warm_up_24 @3*/) - (1);

goto if_top1577;
}
if_top1578:
if (1)
{
wptr[4] /* n_25 @4*/ = *((int*)wptr[6]) /* n_loops_19 @6*/;
wptr[1] /* t0_22 @1*/ = get_time();
if_top1579:
if ((wptr[4] /* n_25 @4*/) > (0))
{
debug("[in 'd_in' l: 78 c: 19]: %x\n", (int*)wptr[7] /* d_in_20 @7 */);
goto *(in (4, (int *)wptr[7] /* d_in_20 @7 */, (BPOOTER)&wptr[0] /* value_21 @0*/, &&input_jump1563));
input_jump1563:
wptr[4] /* n_25 @4*/ = (wptr[4] /* n_25 @4*/) - (1);

goto if_top1579;
}
wptr[2] /* t1_23 @2*/ = get_time();

/* FIXME: Temporary print */
{
int time = (wptr[2] - wptr[1]);
long long usec = (time * 1000ll);
printf("Last value: %d\n", wptr[0]);
printf("Time: %d us\n", time);
printf("Time per loop: %lld ns\n", usec/1000000ll);
printf("Context switch: %lld ns\n", usec/1000000ll/4ll);
/* fflush(stdout); */
}



goto if_top1578;
}
wptr = wptr + 5;
wptr = wptr + 4;
goto *wptr[-4];

/* PROC comms_time */
PROCcomms_time:
wptr = wptr - 8;
wptr[2] = /* a_26 @2 */ NOT_PROCESS_P;
debug("[a_26 @wsloc %x]\n", (wptr + 2));
wptr[3] = /* b_27 @3 */ NOT_PROCESS_P;
debug("[b_27 @wsloc %x]\n", (wptr + 3));
wptr[4] = /* c_28 @4 */ NOT_PROCESS_P;
debug("[c_28 @wsloc %x]\n", (wptr + 4));
wptr[5] = /* d_29 @5 */ NOT_PROCESS_P;
debug("[d_29 @wsloc %x]\n", (wptr + 5));
wptr[6] /* zero_30 @6*/ = 0;
wptr[7] /* iterations_31 @7*/ = 100000;

/* PAR endparlab1572 - 4 START */
wptr[1] = 4;
wptr[0] = &&endparlab1572;
add_to_queue((int)(wptr - 19), (int)&&par1569);
add_to_queue((int)(wptr - 32), (int)&&par1570);
add_to_queue((int)(wptr - 45), (int)&&par1571);
par1568:
wptr[-4] = (int) &&return_from_prefix1564;
wptr[-3] = (wptr + 6) /* zero_30 @6  (locality local) */;
wptr[-2] = (wptr + 3) /* b_27 @3  (locality local) */;
wptr[-1] = (wptr + 2) /* a_26 @2  (locality local) */;
debug("call prefix: [-3 zero_30 %x] [-2 b_27 %x] [-1 a_26 %x]\n", wptr[-3], wptr[-2], wptr[-1]);
wptr = wptr - 4;
goto PROCprefix;
return_from_prefix1564:

goto *(endp((int) (wptr - 19)));
par1569:
wptr[-4] = (int) &&return_from_seq_delta1565;
wptr[-3] = (wptr + 21) /* a_26 @2  (locality local) */;
wptr[-2] = (wptr + 23) /* c_28 @4  (locality local) */;
wptr[-1] = (wptr + 24) /* d_29 @5  (locality local) */;
debug("call seq_delta: [-3 a_26 %x] [-2 c_28 %x] [-1 d_29 %x]\n", wptr[-3], wptr[-2], wptr[-1]);
wptr = wptr - 4;
goto PROCseq_delta;
return_from_seq_delta1565:

goto *(endp((int) (wptr - 32)));
par1570:
wptr[-4] = (int) &&return_from_succ1566;
wptr[-3] = (wptr + 36) /* c_28 @4  (locality local) */;
wptr[-2] = (wptr + 35) /* b_27 @3  (locality local) */;
wptr[-1] = (wptr + 24) /* d_29 @5  (locality local) */;
debug("call succ: [-3 c_28 %x] [-2 b_27 %x] [-1 d_29 %x]\n", wptr[-3], wptr[-2], wptr[-1]);
wptr = wptr - 4;
goto PROCsucc;
return_from_succ1566:

goto *(endp((int) (wptr - 45)));
par1571:
wptr[-4] = (int) &&return_from_consume1567;
wptr[-3] = (wptr + 52) /* iterations_31 @7  (locality local) */;
wptr[-2] = (wptr + 50) /* d_29 @5  (locality local) */;
wptr[-1] = (wptr + 24) /* d_29 @5  (locality local) */;
debug("call consume: [-3 iterations_31 %x] [-2 d_29 %x] [-1 d_29 %x]\n", wptr[-3], wptr[-2], wptr[-1]);
wptr = wptr - 4;
goto PROCconsume;
return_from_consume1567:

goto *(endp((int) (wptr - 60)));
endparlab1572:
wptr = wptr + 8;
wptr = wptr + 4;
goto *wptr[-4];
}
