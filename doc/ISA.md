## Registers

| Name  |   Index   | Alias  | Saver  |
| ----- | --------- | ------ | ------ |
| `pc`  | -         | -      | -      |
|       |           |        |        |
| `r0 ` | `0b00000` | `zero` | -      |
| `r1 ` | `0b00001` | `ra  ` | caller |
| `r2 ` | `0b00010` | `bp  ` | caller |
| `r3 ` | `0b00011` | `sp  ` | -      |
| `r4 ` | `0b00100` | `a0  ` | caller |
| `r5 ` | `0b00101` | `a1  ` | caller |
| `r6 ` | `0b00110` | `a2  ` | caller |
| `r7 ` | `0b00111` | `a3  ` | caller |
| `r8 ` | `0b01000` | `a4  ` | caller |
| `r9 ` | `0b01001` | `a5  ` | caller |
| `r10` | `0b01010` | `a6  ` | caller |
| `r11` | `0b01011` | `a7  ` | caller |
| `r12` | `0b01100` | `t0  ` | caller |
| `r13` | `0b01101` | `t1  ` | caller |
| `r14` | `0b01110` | `t2  ` | caller |
| `r15` | `0b01111` | `t3  ` | caller |
| `r16` | `0b10000` | `t4  ` | caller |
| `r17` | `0b10001` | `t5  ` | caller |
| `r18` | `0b10010` | `t6  ` | caller |
| `r19` | `0b10011` | `t7  ` | caller |
| `r20` | `0b10100` | `t8  ` | caller |
| `r21` | `0b10101` | `t9  ` | caller |
| `r22` | `0b10110` | `s0  ` | callee |
| `r23` | `0b10111` | `s1  ` | callee |
| `r24` | `0b11000` | `s2  ` | callee |
| `r25` | `0b11001` | `s3  ` | callee |
| `r26` | `0b11010` | `s4  ` | callee |
| `r27` | `0b11011` | `s5  ` | callee |
| `r28` | `0b11100` | `s6  ` | callee |
| `r29` | `0b11101` | `s7  ` | callee |
| `r30` | `0b11110` | `s8  ` | callee |
| `r31` | `0b11111` | `s9  ` | callee |


## Flags
| Name | Description |
| ---- | ----------- |
| `C`  | Carry       |
| `Z`  | Zero        |
| `S`  | Sign        |
| `O`  | Overflow    |
| `K`  | Kernel mode |


## Instructions

| Mnemonic + operands  | Bit pattern                              | Operation |
| -------------------- | ---------------------------------------- | --------- |
| `NOP               ` | `0_---------_-----_-----_-----_--00_000` | -         |
| `BRK               ` | `0_---------_-----_-----_-----_--01_000` | Pauses emulation (behaves like NOP in hardware) |
| `HLT               ` | `0_---------_-----_-----_-----_--10_000` | Stops emulation gracefully (behaves like NOP in hardware) |
| `ERR               ` | `0_---------_-----_-----_-----_--11_000` | Stops emulation with an error (behaves like NOP in hardware) |
|                      |                                          |           |
| `ADD     d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_0001_001` | `d = l + r` |
| `ADDC    d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_0010_001` | `d = l + r + C` |
| `SUB     d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_0011_001` | `d = l - r` |
| `SUBB    d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_0100_001` | `d = l - r + C` |
| `AND     d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_0101_001` | `d = l & r` |
| `OR      d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_0110_001` | `d = l \| r` |
| `XOR     d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_0111_001` | `d = l ^ r` |
| `SHL     d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_1000_001` | `d = l << r` |
| `ASR     d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_1001_001` | `d = l >>> r` |
| `LSR     d, l, r   ` | `0_---------_rrrrr_lllll_ddddd_1010_001` | `d = l >> r` |
|                      |                                          |           |
| `ADD     d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_0001_010` | `d = l + v` |
| `ADDC    d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_0010_010` | `d = l + v + C` |
| `SUB     d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_0011_010` | `d = l - v` |
| `SUBB    d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_0100_010` | `d = l - v + C` |
| `AND     d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_0101_010` | `d = l & v` |
| `OR      d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_0110_010` | `d = l \| v` |
| `XOR     d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_0111_010` | `d = l ^ v` |
| `SHL     d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_1000_010` | `d = l << v` |
| `ASR     d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_1001_010` | `d = l >>> v` |
| `LSR     d, l, v   ` | `i_vvvvvvvvv_vvvvv_lllll_ddddd_1010_010` | `d = l >> v` |
|                      |                                          |           |
| `ADD     d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_0001_011` | `d = v + r` |
| `ADDC    d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_0010_011` | `d = v + r + C` |
| `SUB     d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_0011_011` | `d = v - r` |
| `SUBB    d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_0100_011` | `d = v - r + C` |
| `AND     d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_0101_011` | `d = v & r` |
| `OR      d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_0110_011` | `d = v \| r` |
| `XOR     d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_0111_011` | `d = v ^ r` |
| `SHL     d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_1000_011` | `d = v << r` |
| `ASR     d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_1001_011` | `d = v >>> r` |
| `LSR     d, v, r   ` | `i_vvvvvvvvv_vvvvv_rrrrr_ddddd_1010_011` | `d = v >> r` |
|                      |                                          |           |
| `LD      d, [s + o]` | `0_---------_ooooo_sssss_ddddd_0000_100` | `d = mem[s + o]` |
| `LD      d, [s + v]` | `i_vvvvvvvvv_vvvvv_sssss_ddddd_0001_100` | `d = mem[s + v]` |
| `ST      [d + o], s` | `0_---------_ooooo_sssss_ddddd_0010_100` | `mem[d + o] = s` |
| `ST      [d + v], s` | `i_vvvvvvvvv_vvvvv_sssss_ddddd_0011_100` | `mem[d + v] = s` |
| `LD8     d, [s + o]` | `0_---------_ooooo_sssss_ddddd_0100_100` | `d = (i8)mem[s + o]` |
| `LD8     d, [s + v]` | `i_vvvvvvvvv_vvvvv_sssss_ddddd_0101_100` | `d = (i8)mem[s + o]` |
| `ST8     [d + o], s` | `0_---------_ooooo_sssss_ddddd_0110_100` | `mem[d + o] = (i8)s` |
| `ST8     [d + v], s` | `i_vvvvvvvvv_vvvvv_sssss_ddddd_0111_100` | `mem[d + v] = (i8)s` |
| `LD16    d, [s + o]` | `0_---------_ooooo_sssss_ddddd_1000_100` | `d = (i16)mem[s + o]` |
| `LD16    d, [s + v]` | `i_vvvvvvvvv_vvvvv_sssss_ddddd_1001_100` | `d = (i16)mem[s + o]` |
| `ST16    [d + o], s` | `0_---------_ooooo_sssss_ddddd_1010_100` | `mem[d + o] = (i16)s` |
| `ST16    [d + v], s` | `i_vvvvvvvvv_vvvvv_sssss_ddddd_1011_100` | `mem[d + v] = (i16)s` |
|                      |                                          |           |
| `JP.C    s + o     ` | `0_---------_ooooo_sssss_--000_0001_101` | `if C then pc = s + o` |
| `JP.Z    s + o     ` | `0_---------_ooooo_sssss_--000_0010_101` | `if Z then pc = s + o` |
| `JP.S    s + o     ` | `0_---------_ooooo_sssss_--000_0011_101` | `if S then pc = s + o` |
| `JP.O    s + o     ` | `0_---------_ooooo_sssss_--000_0100_101` | `if O then pc = s + o` |
| `JP.NC   s + o     ` | `0_---------_ooooo_sssss_--000_0101_101` | `if !C then pc = s + o` |
| `JP.NZ   s + o     ` | `0_---------_ooooo_sssss_--000_0110_101` | `if !Z then pc = s + o` |
| `JP.NS   s + o     ` | `0_---------_ooooo_sssss_--000_0111_101` | `if !S then pc = s + o` |
| `JP.NO   s + o     ` | `0_---------_ooooo_sssss_--000_1000_101` | `if !O then pc = s + o` |
| `JP.U.LE s + o     ` | `0_---------_ooooo_sssss_--000_1001_101` | `if !C \|\| Z then pc = s + o` |
| `JP.U.G  s + o     ` | `0_---------_ooooo_sssss_--000_1010_101` | `if C && !Z then pc = s + o` |
| `JP.S.L  s + o     ` | `0_---------_ooooo_sssss_--000_1011_101` | `if S != O then pc = s + o` |
| `JP.S.GE s + o     ` | `0_---------_ooooo_sssss_--000_1100_101` | `if S == O then pc = s + o` |
| `JP.S.LE s + o     ` | `0_---------_ooooo_sssss_--000_1101_101` | `if Z \|\| (S != O) then pc = s + o` |
| `JP.S.G  s + o     ` | `0_---------_ooooo_sssss_--000_1110_101` | `if !Z && (S == O) then pc = s + o` |
| `JMP     s + o     ` | `0_---------_ooooo_sssss_--000_1111_101` | `pc = s + o` |
|                      |                                          |           |
| `JP.C    [s + o]   ` | `0_---------_ooooo_sssss_--001_0001_101` | `if C then pc = mem[s + o]` |
| `JP.Z    [s + o]   ` | `0_---------_ooooo_sssss_--001_0010_101` | `if Z then pc = mem[s + o]` |
| `JP.S    [s + o]   ` | `0_---------_ooooo_sssss_--001_0011_101` | `if S then pc = mem[s + o]` |
| `JP.O    [s + o]   ` | `0_---------_ooooo_sssss_--001_0100_101` | `if O then pc = mem[s + o]` |
| `JP.NC   [s + o]   ` | `0_---------_ooooo_sssss_--001_0101_101` | `if !C then pc = mem[s + o]` |
| `JP.NZ   [s + o]   ` | `0_---------_ooooo_sssss_--001_0110_101` | `if !Z then pc = mem[s + o]` |
| `JP.NS   [s + o]   ` | `0_---------_ooooo_sssss_--001_0111_101` | `if !S then pc = mem[s + o]` |
| `JP.NO   [s + o]   ` | `0_---------_ooooo_sssss_--001_1000_101` | `if !O then pc = mem[s + o]` |
| `JP.U.LE [s + o]   ` | `0_---------_ooooo_sssss_--001_1001_101` | `if !C \|\| Z then pc = mem[s + o]` |
| `JP.U.G  [s + o]   ` | `0_---------_ooooo_sssss_--001_1010_101` | `if C && !Z then pc = mem[s + o]` |
| `JP.S.L  [s + o]   ` | `0_---------_ooooo_sssss_--001_1011_101` | `if S != O then pc = mem[s + o]` |
| `JP.S.GE [s + o]   ` | `0_---------_ooooo_sssss_--001_1100_101` | `if S == O then pc = mem[s + o]` |
| `JP.S.LE [s + o]   ` | `0_---------_ooooo_sssss_--001_1101_101` | `if Z \|\| (S != O) then pc = mem[s + o]` |
| `JP.S.G  [s + o]   ` | `0_---------_ooooo_sssss_--001_1110_101` | `if !Z && (S == O) then pc = mem[s + o]` |
| `JMP     [s + o]   ` | `0_---------_ooooo_sssss_--001_1111_101` | `pc = mem[s + o]` |
|                      |                                          |           |
| `JP.C    s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_0001_101` | `if C then pc = s + v` |
| `JP.Z    s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_0010_101` | `if Z then pc = s + v` |
| `JP.S    s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_0011_101` | `if S then pc = s + v` |
| `JP.O    s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_0100_101` | `if O then pc = s + v` |
| `JP.NC   s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_0101_101` | `if !C then pc = s + v` |
| `JP.NZ   s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_0110_101` | `if !Z then pc = s + v` |
| `JP.NS   s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_0111_101` | `if !S then pc = s + v` |
| `JP.NO   s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_1000_101` | `if !O then pc = s + v` |
| `JP.U.LE s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_1001_101` | `if !C \|\| Z then pc = s + v` |
| `JP.U.G  s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_1010_101` | `if C && !Z then pc = s + v` |
| `JP.S.L  s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_1011_101` | `if S != O then pc = s + v` |
| `JP.S.GE s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_1100_101` | `if S == O then pc = s + v` |
| `JP.S.LE s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_1101_101` | `if Z \|\| (S != O) then pc = s + v` |
| `JP.S.G  s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_1110_101` | `if !Z && (S == O) then pc = s + v` |
| `JMP     s + v     ` | `i_vvvvvvvvv_vvvvv_sssss_--010_1111_101` | `pc = s + v` |
|                      |                                          |           |
| `JP.C    [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_0001_101` | `if C then pc = mem[s + v]` |
| `JP.Z    [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_0010_101` | `if Z then pc = mem[s + v]` |
| `JP.S    [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_0011_101` | `if S then pc = mem[s + v]` |
| `JP.O    [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_0100_101` | `if O then pc = mem[s + v]` |
| `JP.NC   [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_0101_101` | `if !C then pc = mem[s + v]` |
| `JP.NZ   [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_0110_101` | `if !Z then pc = mem[s + v]` |
| `JP.NS   [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_0111_101` | `if !S then pc = mem[s + v]` |
| `JP.NO   [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_1000_101` | `if !O then pc = mem[s + v]` |
| `JP.U.LE [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_1001_101` | `if !C \|\| Z then pc = mem[s + v]` |
| `JP.U.G  [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_1010_101` | `if C && !Z then pc = mem[s + v]` |
| `JP.S.L  [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_1011_101` | `if S != O then pc = mem[s + v]` |
| `JP.S.GE [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_1100_101` | `if S == O then pc = mem[s + v]` |
| `JP.S.LE [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_1101_101` | `if Z \|\| (S != O) then pc = mem[s + v]` |
| `JP.S.G  [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_1110_101` | `if !Z && (S == O) then pc = mem[s + v]` |
| `JMP     [s + v]   ` | `i_vvvvvvvvv_vvvvv_sssss_--011_1111_101` | `pc = mem[s + v]` |
|                      |                                          |           |
| `BR.C    v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_0001_101` | `if C then pc += v` |
| `BR.Z    v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_0010_101` | `if Z then pc += v` |
| `BR.S    v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_0011_101` | `if S then pc += v` |
| `BR.O    v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_0100_101` | `if O then pc += v` |
| `BR.NC   v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_0101_101` | `if !C then pc += v` |
| `BR.NZ   v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_0110_101` | `if !Z then pc += v` |
| `BR.NS   v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_0111_101` | `if !S then pc += v` |
| `BR.NO   v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_1000_101` | `if !O then pc += v` |
| `BR.U.LE v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_1001_101` | `if !C \|\| Z then pc += v` |
| `BR.U.G  v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_1010_101` | `if C && !Z then pc += v` |
| `BR.S.L  v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_1011_101` | `if S != O then pc += v` |
| `BR.S.GE v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_1100_101` | `if S == O then pc += v` |
| `BR.S.LE v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_1101_101` | `if Z \|\| (S != O) then pc += v` |
| `BR.S.G  v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_1110_101` | `if !Z && (S == O) then pc += v` |
| `BRA     v         ` | `i_vvvvvvvvv_vvvvv_-----_--1--_1111_101` | `pc += v` |
|                      |                                          |           |
| `IN      d, [s + o]` | `0_---------_ooooo_sssss_ddddd_--00_110` | `d = io[s + o]` |
| `IN      d, [s + v]` | `i_vvvvvvvvv_vvvvv_sssss_ddddd_--01_110` | `d = io[s + v]` |
| `OUT     [d + o], s` | `0_---------_ooooo_sssss_ddddd_--10_110` | `io[d + o] = s` |
| `OUT     [d + v], s` | `i_vvvvvvvvv_vvvvv_sssss_ddddd_--11_110` | `io[d + v] = s` |
|                      |                                          |           |
| `SYS               ` | `0_---------_-----_-----_-----_---0_111` | `K = 1, pc = 0x00000FF0` |
| `CLRK              ` | `0_---------_-----_-----_-----_---1_111` | `K = 0` |


## Assembler Pseudo-Instructions

| Mnemonic + operands | Actual instruction emitted |
| ------------------- | -------------------------- |
| `MOV     d, s     ` | `OR d, s, zero           ` |
| `LD      d, v     ` | `OR d, v, zero           ` |
|                     |                            |
| `CMP     l, r     ` | `SUB zero, l, r          ` |
| `CMP     l, v     ` | `SUB zero, l, v          ` |
| `CMP     v, r     ` | `SUB zero, v, r          ` |
|                     |                            |
| `BIT     l, r     ` | `AND zero, l, r          ` |
| `BIT     l, v     ` | `AND zero, l, v          ` |
| `BIT     v, r     ` | `AND zero, v, r          ` |
|                     |                            |
| `TEST    s        ` | `OR zero, s, zero        ` |
|                     |                            |
| `INC     d        ` | `ADD  d, d, 1            ` |
| `INCC    d        ` | `ADDC d, d, zero         ` |
| `DEC     d        ` | `SUB  d, d, 1            ` |
| `DECB    d        ` | `SUBB d, d, zero         ` |
|                     |                            |
| `NEG     d, s     ` | `SUB  d, zero, s         ` |
| `NEGB    d, s     ` | `SUBB d, zero, s         ` |
|                     |                            |
| `NOT     d, s     ` | `XOR d, s, -1            ` |
|                     |                            |
| `LD      d, [s]   ` | `LD   d, [s + zero]      ` |
| `LD      d, [v]   ` | `LD   d, [zero + v]      ` |
| `ST      [d], s   ` | `ST   [d + zero], s      ` |
| `ST      [v], s   ` | `ST   [zero + v], s      ` |
| `LD8     d, [s]   ` | `LD8  d, [s + zero]      ` |
| `LD8     d, [v]   ` | `LD8  d, [zero + v]      ` |
| `ST8     [d], s   ` | `ST8  [d + zero], s      ` |
| `ST8     [v], s   ` | `ST8  [zero + v], s      ` |
| `LD16    d, [s]   ` | `LD16 d, [s + zero]      ` |
| `LD16    d, [v]   ` | `LD16 d, [zero + v]      ` |
| `ST16    [d], s   ` | `ST16 [d + zero], s      ` |
| `ST16    [v], s   ` | `ST16 [zero + v], s      ` |
|                     |                            |
| `JP.E    s + o    ` | `JP.Z  s + o             ` |
| `JP.NE   s + o    ` | `JP.NZ s + o             ` |
| `JP.U.L  s + o    ` | `JP.NC s + o             ` |
| `JP.U.GE s + o    ` | `JP.C  s + o             ` |
|                     |                            |
| `JP.E    [s + o]  ` | `JP.Z  [s + o]           ` |
| `JP.NE   [s + o]  ` | `JP.NZ [s + o]           ` |
| `JP.U.L  [s + o]  ` | `JP.NC [s + o]           ` |
| `JP.U.GE [s + o]  ` | `JP.C  [s + o]           ` |
|                     |                            |
| `JP.E    s + v    ` | `JP.Z  s + v             ` |
| `JP.NE   s + v    ` | `JP.NZ s + v             ` |
| `JP.U.L  s + v    ` | `JP.NC s + v             ` |
| `JP.U.GE s + v    ` | `JP.C  s + v             ` |
|                     |                            |
| `JP.E    [s + v]  ` | `JP.Z  [s + v]           ` |
| `JP.NE   [s + v]  ` | `JP.NZ [s + v]           ` |
| `JP.U.L  [s + v]  ` | `JP.NC [s + v]           ` |
| `JP.U.GE [s + v]  ` | `JP.C  [s + v]           ` |
|                     |                            |
| `BR.E    v        ` | `BR.Z  v                 ` |
| `BR.NE   v        ` | `BR.NZ v                 ` |
| `BR.U.L  v        ` | `BR.NC v                 ` |
| `BR.U.GE v        ` | `BR.C  v                 ` |
|                     |                            |
| `JP.C    s        ` | `JP.C    s + zero        ` |
| `JP.Z    s        ` | `JP.Z    s + zero        ` |
| `JP.S    s        ` | `JP.S    s + zero        ` |
| `JP.O    s        ` | `JP.O    s + zero        ` |
| `JP.NC   s        ` | `JP.NC   s + zero        ` |
| `JP.NZ   s        ` | `JP.NZ   s + zero        ` |
| `JP.NS   s        ` | `JP.NS   s + zero        ` |
| `JP.NO   s        ` | `JP.NO   s + zero        ` |
| `JP.E    s        ` | `JP.E    s + zero        ` |
| `JP.NE   s        ` | `JP.NE   s + zero        ` |
| `JP.U.L  s        ` | `JP.U.L  s + zero        ` |
| `JP.U.GE s        ` | `JP.U.GE s + zero        ` |
| `JP.U.LE s        ` | `JP.U.LE s + zero        ` |
| `JP.U.G  s        ` | `JP.U.G  s + zero        ` |
| `JP.S.L  s        ` | `JP.S.L  s + zero        ` |
| `JP.S.GE s        ` | `JP.S.GE s + zero        ` |
| `JP.S.LE s        ` | `JP.S.LE s + zero        ` |
| `JP.S.G  s        ` | `JP.S.G  s + zero        ` |
| `JMP     s        ` | `JMP     s + zero        ` |
|                     |                            |
| `JP.C    [s]      ` | `JP.C    [s + zero]      ` |
| `JP.Z    [s]      ` | `JP.Z    [s + zero]      ` |
| `JP.S    [s]      ` | `JP.S    [s + zero]      ` |
| `JP.O    [s]      ` | `JP.O    [s + zero]      ` |
| `JP.NC   [s]      ` | `JP.NC   [s + zero]      ` |
| `JP.NZ   [s]      ` | `JP.NZ   [s + zero]      ` |
| `JP.NS   [s]      ` | `JP.NS   [s + zero]      ` |
| `JP.NO   [s]      ` | `JP.NO   [s + zero]      ` |
| `JP.E    [s]      ` | `JP.E    [s + zero]      ` |
| `JP.NE   [s]      ` | `JP.NE   [s + zero]      ` |
| `JP.U.L  [s]      ` | `JP.U.L  [s + zero]      ` |
| `JP.U.GE [s]      ` | `JP.U.GE [s + zero]      ` |
| `JP.U.LE [s]      ` | `JP.U.LE [s + zero]      ` |
| `JP.U.G  [s]      ` | `JP.U.G  [s + zero]      ` |
| `JP.S.L  [s]      ` | `JP.S.L  [s + zero]      ` |
| `JP.S.GE [s]      ` | `JP.S.GE [s + zero]      ` |
| `JP.S.LE [s]      ` | `JP.S.LE [s + zero]      ` |
| `JP.S.G  [s]      ` | `JP.S.G  [s + zero]      ` |
| `JMP     [s]      ` | `JMP     [s + zero]      ` |
|                     |                            |
| `JP.C    v        ` | `JP.C    zero + v        ` |
| `JP.Z    v        ` | `JP.Z    zero + v        ` |
| `JP.S    v        ` | `JP.S    zero + v        ` |
| `JP.O    v        ` | `JP.O    zero + v        ` |
| `JP.NC   v        ` | `JP.NC   zero + v        ` |
| `JP.NZ   v        ` | `JP.NZ   zero + v        ` |
| `JP.NS   v        ` | `JP.NS   zero + v        ` |
| `JP.NO   v        ` | `JP.NO   zero + v        ` |
| `JP.E    v        ` | `JP.E    zero + v        ` |
| `JP.NE   v        ` | `JP.NE   zero + v        ` |
| `JP.U.L  v        ` | `JP.U.L  zero + v        ` |
| `JP.U.GE v        ` | `JP.U.GE zero + v        ` |
| `JP.U.LE v        ` | `JP.U.LE zero + v        ` |
| `JP.U.G  v        ` | `JP.U.G  zero + v        ` |
| `JP.S.L  v        ` | `JP.S.L  zero + v        ` |
| `JP.S.GE v        ` | `JP.S.GE zero + v        ` |
| `JP.S.LE v        ` | `JP.S.LE zero + v        ` |
| `JP.S.G  v        ` | `JP.S.G  zero + v        ` |
| `JMP     v        ` | `JMP     zero + v        ` |
|                     |                            |
| `JP.C    [v]      ` | `JP.C    [zero + v]      ` |
| `JP.Z    [v]      ` | `JP.Z    [zero + v]      ` |
| `JP.S    [v]      ` | `JP.S    [zero + v]      ` |
| `JP.O    [v]      ` | `JP.O    [zero + v]      ` |
| `JP.NC   [v]      ` | `JP.NC   [zero + v]      ` |
| `JP.NZ   [v]      ` | `JP.NZ   [zero + v]      ` |
| `JP.NS   [v]      ` | `JP.NS   [zero + v]      ` |
| `JP.NO   [v]      ` | `JP.NO   [zero + v]      ` |
| `JP.E    [v]      ` | `JP.E    [zero + v]      ` |
| `JP.NE   [v]      ` | `JP.NE   [zero + v]      ` |
| `JP.U.L  [v]      ` | `JP.U.L  [zero + v]      ` |
| `JP.U.GE [v]      ` | `JP.U.GE [zero + v]      ` |
| `JP.U.LE [v]      ` | `JP.U.LE [zero + v]      ` |
| `JP.U.G  [v]      ` | `JP.U.G  [zero + v]      ` |
| `JP.S.L  [v]      ` | `JP.S.L  [zero + v]      ` |
| `JP.S.GE [v]      ` | `JP.S.GE [zero + v]      ` |
| `JP.S.LE [v]      ` | `JP.S.LE [zero + v]      ` |
| `JP.S.G  [v]      ` | `JP.S.G  [zero + v]      ` |
| `JMP     [v]      ` | `JMP     [zero + v]      ` |
|                     |                            |
| `IN      d, [s]   ` | `IN   d, [s + zero]      ` |
| `IN      d, [v]   ` | `IN   d, [zero + v]      ` |
| `OUT     [d], s   ` | `OUT  [d + zero], s      ` |
| `OUT     [v], s   ` | `OUT  [zero + v], s      ` |


## Assembler Macros

| Mnemonic + operands | Expands to               |
| ------------------- | ------------------------ |
| `PUSH    s        ` | <pre>ST [sp], s<br>SUB sp, sp, 4</pre> |
|                     |                          |
| `POP     d        ` | <pre>ADD sp, sp, 4<br>LD d, [sp]</pre> |
|                     |                          |
| `PUSH8   s        ` | <pre>ST8 [sp], s<br>SUB sp, sp, 4</pre> |
|                     |                          |
| `POP8    d        ` | <pre>ADD sp, sp, 4<br>LD8 d, [sp]</pre> |
|                     |                          |
| `PUSH16  s        ` | <pre>ST16 [sp], s<br>SUB sp, sp, 4</pre> |
|                     |                          |
| `POP16   d        ` | <pre>ADD sp, sp, 4<br>LD16 d, [sp]</pre> |
|                     |                          |
| `CALL    s        ` | <pre>MOV bp, sp<br>LD ra, ret_addr<br>JMP s<br>ret_addr:</pre> |
|                     |                          |
| `CALL    [s]      ` | <pre>MOV bp, sp<br>LD ra, ret_addr<br>JMP [s]<br>ret_addr:</pre> |
|                     |                          |
| `CALL    v        ` | <pre>MOV bp, sp<br>LD ra, ret_addr<br>JMP v<br>ret_addr:</pre> |
|                     |                          |
| `CALL    [v]      ` | <pre>MOV bp, sp<br>LD ra, ret_addr<br>JMP [v]<br>ret_addr:</pre> |
|                     |                          |
| `RET     v        ` | <pre>ADD sp, bp, (v * 4)<br>JMP ra</pre> |
|                     |                          |
| `CALLS            ` | <pre>LD ra, ret_addr<br>SYS<br>ret_addr:</pre> |
|                     |                          |
| `RETS             ` | <pre>CLRK<br>JMP ra</pre> |
