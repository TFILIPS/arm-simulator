.text
.align
.globl	_start
_start:
    ANDLT   r4, r3, r9, LSR #5
    EORLE   r0, r10, r7, ASR r5
    SUBS    pc, r8, r3
    RSBGT   r7, r9, r4
    ADD     r10, sp, r5, ROR #8
    ADCPL   r2, r3, pc, LSL #31
    SBCLTS  r6, r2, #186
    RSCMI   r9, r4, r10
    TST     r2, r6, LSL r1
    TEQ     r4, #0x15000000
    CMP     r12, r12
    CMNGE   r5, r7, ROR r11
    ORRNES  r12, lr, r2
    MOV     r11, r8
    BICEQS  lr, r12, r5, RRX
    MVNVC   r11, #1392

    MULHS r0, r7, sp
    MLAS r2, r1, r6, r9
    SMULLVS sp, r12, r2, r0
    UMULLHIS r7, r9, r0, r10
    SMLAL r4, r10, r1, r12
    UMLALMI lr, r8, r11, r2

    CLZVS r12, r3

    BLS 0x98765
    BL 0xABCDEF
    BXHI r5
    BLX 0x6F8D2B
    BLXNE r9

    LDRGE r10, [r3, #3583]!
    LDRB r5, [r7], r1, ROR #31
    LDRGEBT lr, [r4], lr
    LDRH r12, [r5]
    LDRPLSB r7, [r11, #129]
    LDRLOSH r3, [r6]
    LDRNET r6, [r11], #-8
    STRGT r2, [sp, r5, RRX]
    STRPLB r0, [r1, r10, LSR #4]!
    STRLOBT r11, [r7], r11
    STRVCH r5, [r8, r3]
    STRGET r0, [r12]

    LDMEQFD lr, {r4, r5, r8, r12, sp, pc}
    LDMEA r10!, {r2, r5, sp}
    STMLSFA r4!, {r4, r5, r8, r9, r10, r12, lr, pc}
    STMED r7, {r7-r8}

    SWPLE lr, lr, [r9]
    SWPVSB r2, r1, [r7]

    SWINE #14615902
    BKPT #42291
    MRS R3, CPSR
    MSRHS SPSR_cxsf, R4
    CDPVC p7, #15, c5, c12, c13, #2
    CDP2 p6, #15, c15, c1, c14, #2
    LDCHI p13, c4, [R0, #680]
    LDC2 p15, c11, [R6, #492]
    MCRLS p6, #7, r12, c2, c0
    MCR2 p1, #2, r3, c6, c0, #5
    MRC p10, #6, r8, c14, c11, #1
    MRC2 p7, #5, r13, c5, c2
    STCLE p4, c13, [r0, #852]
    STC2 p5, c0, [pc, #184]

    //Not in armv5 used for unidentified instructions
    MRRC p7, #6, R8, R1, c4
