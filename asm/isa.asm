#bits 8

#subruledef reg
{
    r{i: u5} => i
    zero => 0`5
    ra => 1`5
    bp => 2`5
    sp => 3`5
    a{i: u3} => (i`5 + 4)`5
    t{i: u5} => {
        assert(i < 10)
        (i + 12)`5
    }
    s{i: u5} => {
        assert(i < 10)
        (i + 22)`5
    }
}

#subruledef imm
{
    {v: i32} => {
        assert((v < 8192) && (v >= -8192))
        0b0 @ v`14
    }
    {v: i32} => {
        assert((v >= 8192) || (v < -8192))
        0`14 @ (v >> 14)`18 @ 0b1 @ v`14
    }
}

#subruledef rel
{
    {v: i32} => {
        offset = v - $ - 4
        assert((offset < 8192) && (offset >= -8192))
        0b0 @ offset`14
    }
    {v: i32} => {
        offset = v - $ - 4
        assert((offset >= 8192) || (offset < -8192))
        0`14 @ (offset >> 14)`18 @ 0b1 @ offset`14
    }
}

; real instructions
#ruledef
{
    NOP => le(0`10 @ 0`5 @ 0`5 @ 0`5 @ 0x0 @ 0b000)
    BRK => le(0`10 @ 0`5 @ 0`5 @ 0`5 @ 0x1 @ 0b000)
    HLT => le(0`10 @ 0`5 @ 0`5 @ 0`5 @ 0x2 @ 0b000)
    ERR => le(0`10 @ 0`5 @ 0`5 @ 0`5 @ 0x3 @ 0b000)

    ADD    {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x0 @ 0b001)
    ADDC   {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x1 @ 0b001)
    SUB    {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x2 @ 0b001)
    SUBB   {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x3 @ 0b001)
    AND    {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x4 @ 0b001)
    OR     {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x5 @ 0b001)
    XOR    {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x6 @ 0b001)
    SHL    {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x7 @ 0b001)
    LSR    {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x8 @ 0b001)
    ASR    {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0x9 @ 0b001)
    MUL    {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0xA @ 0b001)
    MULHUU {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0xB @ 0b001)
    MULHSS {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0xC @ 0b001)
    MULHSU {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0xD @ 0b001)
    CSUB   {d: reg}, {l: reg}, {r: reg} => le(0`10 @ {r} @ {l} @ {d} @ 0xE @ 0b001)
    SLC    {d: reg}, {l: reg}           => le(0`10 @ 0`5 @ {l} @ {d} @ 0xF @ 0b001)

    ADD    {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x0 @ 0b010)
    ADDC   {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x1 @ 0b010)
    SUB    {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x2 @ 0b010)
    SUBB   {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x3 @ 0b010)
    AND    {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x4 @ 0b010)
    OR     {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x5 @ 0b010)
    XOR    {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x6 @ 0b010)
    SHL    {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x7 @ 0b010)
    LSR    {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x8 @ 0b010)
    ASR    {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0x9 @ 0b010)
    MUL    {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0xA @ 0b010)
    MULHUU {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0xB @ 0b010)
    MULHSS {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0xC @ 0b010)
    MULHSU {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0xD @ 0b010)
    CSUB   {d: reg}, {l: reg}, {v: imm} => le({v} @ {l} @ {d} @ 0xE @ 0b010)
    
    LD   {d: reg}, [{s: reg} + {o: reg}] => le(0`10 @ {o} @ {s} @ {d} @ 0x0 @ 0b011)
    LD   {d: reg}, [{s: reg} + {v: imm}] => le(       {v} @ {s} @ {d} @ 0x1 @ 0b011)
    ST   [{d: reg} + {o: reg}], {s: reg} => le(0`10 @ {o} @ {s} @ {d} @ 0x2 @ 0b011)
    ST   [{d: reg} + {v: imm}], {s: reg} => le(       {v} @ {s} @ {d} @ 0x3 @ 0b011)
    LD8  {d: reg}, [{s: reg} + {o: reg}] => le(0`10 @ {o} @ {s} @ {d} @ 0x4 @ 0b011)
    LD8  {d: reg}, [{s: reg} + {v: imm}] => le(       {v} @ {s} @ {d} @ 0x5 @ 0b011)
    ST8  [{d: reg} + {o: reg}], {s: reg} => le(0`10 @ {o} @ {s} @ {d} @ 0x6 @ 0b011)
    ST8  [{d: reg} + {v: imm}], {s: reg} => le(       {v} @ {s} @ {d} @ 0x7 @ 0b011)
    LD16 {d: reg}, [{s: reg} + {o: reg}] => le(0`10 @ {o} @ {s} @ {d} @ 0x8 @ 0b011)
    LD16 {d: reg}, [{s: reg} + {v: imm}] => le(       {v} @ {s} @ {d} @ 0x9 @ 0b011)
    ST16 [{d: reg} + {o: reg}], {s: reg} => le(0`10 @ {o} @ {s} @ {d} @ 0xA @ 0b011)
    ST16 [{d: reg} + {v: imm}], {s: reg} => le(       {v} @ {s} @ {d} @ 0xB @ 0b011)

    LD8S  {d: reg}, [{s: reg} + {o: reg}] => le(0`10 @ {o} @ {s} @ {d} @ 0xC @ 0b011)
    LD8S  {d: reg}, [{s: reg} + {v: imm}] => le(       {v} @ {s} @ {d} @ 0xD @ 0b011)
    LD16S {d: reg}, [{s: reg} + {o: reg}] => le(0`10 @ {o} @ {s} @ {d} @ 0xE @ 0b011)
    LD16S {d: reg}, [{s: reg} + {v: imm}] => le(       {v} @ {s} @ {d} @ 0xF @ 0b011)

    JMP {s: reg} + {o: reg}   => le(0`10 @ {o} @ {s} @ 0`5 @ 0x0 @ 0b100)
    JMP {s: reg} + {v: imm}   => le(       {v} @ {s} @ 0`5 @ 0x1 @ 0b100)
    JMP [{s: reg} + {o: reg}] => le(0`10 @ {o} @ {s} @ 0`5 @ 0x2 @ 0b100)
    JMP [{s: reg} + {v: imm}] => le(       {v} @ {s} @ 0`5 @ 0x3 @ 0b100)

    BR.C    {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x1 @ 0b101)
    BR.Z    {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x2 @ 0b101)
    BR.S    {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x3 @ 0b101)
    BR.O    {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x4 @ 0b101)
    BR.NC   {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x5 @ 0b101)
    BR.NZ   {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x6 @ 0b101)
    BR.NS   {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x7 @ 0b101)
    BR.NO   {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x8 @ 0b101)
    BR.U.LE {v: rel} => le({v} @ 0`5 @ 4`5 @ 0x9 @ 0b101)
    BR.U.G  {v: rel} => le({v} @ 0`5 @ 4`5 @ 0xA @ 0b101)
    BR.S.L  {v: rel} => le({v} @ 0`5 @ 4`5 @ 0xB @ 0b101)
    BR.S.GE {v: rel} => le({v} @ 0`5 @ 4`5 @ 0xC @ 0b101)
    BR.S.LE {v: rel} => le({v} @ 0`5 @ 4`5 @ 0xD @ 0b101)
    BR.S.G  {v: rel} => le({v} @ 0`5 @ 4`5 @ 0xE @ 0b101)
    BRA     {v: rel} => le({v} @ 0`5 @ 4`5 @ 0xF @ 0b101)

    IN  {d: reg}, [{s: reg} + {o: reg}] => le(0`10 @ {o} @ {s} @ {d} @ 0x0 @ 0b110)
    IN  {d: reg}, [{s: reg} + {v: imm}] => le(       {v} @ {s} @ {d} @ 0x1 @ 0b110)
    OUT [{d: reg} + {o: reg}], {s: reg} => le(0`10 @ {o} @ {s} @ {d} @ 0x2 @ 0b110)
    OUT [{d: reg} + {v: imm}], {s: reg} => le(       {v} @ {s} @ {d} @ 0x3 @ 0b110)

    SYS  => le(0`10 @ 0`5 @ 0`5 @ 0`5 @ 0x0 @ 0b111)
    CLRK => le(0`10 @ 0`5 @ 0`5 @ 0`5 @ 0x1 @ 0b111)
}

; aliases
#ruledef
{
    MOV {d: reg}, {s: reg} => asm { OR {d}, {s}, zero }
    LD  {d: reg}, {v: i32} => asm { OR {d},  v , zero }

    CMP {l: reg}, {r: reg} => asm { SUB zero, {l}, {r}}
    CMP {l: reg}, {v: i32} => asm { SUB zero, {l},  v }

    BIT {l: reg}, {r: reg} => asm { AND zero, {l}, {r}}
    BIT {l: reg}, {v: i32} => asm { AND zero, {l},  v }

    TEST {s: reg} => asm { OR zero, {s}, zero }

    INC  {d: reg} => asm { ADD  {d}, {d}, 1 }
    INCC {d: reg} => asm { ADDC {d}, {d}, 0 }
    DEC  {d: reg} => asm { SUB  {d}, {d}, 1 }
    DECB {d: reg} => asm { SUBB {d}, {d}, 0 }

    NEG  {d: reg}, {s: reg} => asm { SUB  {d}, zero, {s} }
    NEGB {d: reg}, {s: reg} => asm { SUBB {d}, zero, {s} }

    NOT {d: reg}, {s: reg} => asm { XOR {d}, {s}, -1 }

    LD   {d: reg}, [{s: reg}] => asm { LD   {d}, [{s} + zero] }
    LD   {d: reg}, [{v: i32}] => asm { LD   {d}, [zero + v] }
    ST   [{d: reg}], {s: reg} => asm { ST   [{d} + zero], {s} }
    ST   [{v: i32}], {s: reg} => asm { ST   [zero + v], {s} }
    LD8  {d: reg}, [{s: reg}] => asm { LD8  {d}, [{s} + zero] }
    LD8  {d: reg}, [{v: i32}] => asm { LD8  {d}, [zero + v] }
    ST8  [{d: reg}], {s: reg} => asm { ST8  [{d} + zero], {s} }
    ST8  [{v: i32}], {s: reg} => asm { ST8  [zero + v], {s} }
    LD16 {d: reg}, [{s: reg}] => asm { LD16 {d}, [{s} + zero] }
    LD16 {d: reg}, [{v: i32}] => asm { LD16 {d}, [zero + v] }
    ST16 [{d: reg}], {s: reg} => asm { ST16 [{d} + zero], {s} }
    ST16 [{v: i32}], {s: reg} => asm { ST16 [zero + v], {s} }

    LD8S  {d: reg}, [{s: reg}] => asm { LD8S  {d}, [{s} + zero] }
    LD8S  {d: reg}, [{v: i32}] => asm { LD8S  {d}, [zero + v] }
    LD16S {d: reg}, [{s: reg}] => asm { LD16S {d}, [{s} + zero] }
    LD16S {d: reg}, [{v: i32}] => asm { LD16S {d}, [zero + v] }

    BR.EQ   {v: u32} => asm { BR.Z  v }
    BR.NEQ  {v: u32} => asm { BR.NZ v }
    BR.U.L  {v: u32} => asm { BR.NC v }
    BR.U.GE {v: u32} => asm { BR.C  v }

    JMP {s: reg}   => asm { JMP {s} + zero }
    JMP {v: u32}   => asm { JMP zero + v }
    JMP [{s: reg}] => asm { JMP [{s} + zero] }
    JMP [{v: u32}] => asm { JMP [zero + v] }

    IN  {d: reg}, [{s: reg}] => asm { IN   {d}, [{s} + zero] }
    IN  {d: reg}, [{v: i32}] => asm { IN   {d}, [zero + v] }
    OUT [{d: reg}], {s: reg} => asm { OUT  [{d} + zero], {s} }
    OUT [{v: i32}], {s: reg} => asm { OUT  [zero + v], {s} }
}

; macros
#ruledef
{
    PUSH {s: reg} => asm {
        ST [sp], {s}
        SUB sp, sp, 4
    }

    POP {d: reg} => asm {
        ADD sp, sp, 4
        LD {d}, [sp]
    }

    PUSH8 {s: reg} => asm {
        ST8 [sp], {s}
        SUB sp, sp, 4
    }

    POP8 {d: reg} => asm {
        ADD sp, sp, 4
        LD8 {d}, [sp]
    }

    PUSH16 {s: reg} => asm {
        ST16 [sp], {s}
        SUB sp, sp, 4
    }

    POP16 {d: reg} => asm {
        ADD sp, sp, 4
        LD16 {d}, [sp]
    }

    POP8S {d: reg} => asm {
        ADD sp, sp, 4
        LD8S {d}, [sp]
    }

    POP16S {d: reg} => asm {
        ADD sp, sp, 4
        LD16S {d}, [sp]
    }

    CALL {s: reg} => {
        addr = $ + 12
        assert(addr < 8192)

        asm {
            MOV bp, sp
            LD ra, addr`14
            JMP {s}
        }
    }
    CALL {s: reg} => {
        addr = $ + 12
        assert(addr >= 8192)

        asm {
            MOV bp, sp
            LD ra, (addr + 4)
            JMP {s}
        }
    }
    
    CALL [{s: reg}] => {
        addr = $ + 12
        assert(addr < 8192)

        asm {
            MOV bp, sp
            LD ra, addr`14
            JMP [{s}]
        }
    }
    CALL [{s: reg}] => {
        addr = $ + 12
        assert(addr >= 8192)

        asm {
            MOV bp, sp
            LD ra, (addr + 4)
            JMP [{s}]
        }
    }

    CALL {v: u32} => {
        assert(v < 8192)
        addr = $ + 12
        assert(addr < 8192)

        asm {
            MOV bp, sp
            LD ra, addr`14
            JMP v`14
        }
    }
    CALL {v: u32} => {
        assert(v < 8192)
        addr = $ + 12
        assert(addr >= 8192)

        asm {
            MOV bp, sp
            LD ra, (addr + 4)
            JMP v`14
        }
    }
    CALL {v: u32} => {
        assert(v >= 8192)
        addr = $ + 16
        assert(addr < 8192)

        asm {
            MOV bp, sp
            LD ra, addr`14
            JMP v
        }
    }
    CALL {v: u32} => {
        assert(v >= 8192)
        addr = $ + 16
        assert(addr >= 8192)

        asm {
            MOV bp, sp
            LD ra, (addr + 4)
            JMP v
        }
    }

    CALL [{v: u32}] => {
        assert(v < 8192)
        addr = $ + 12
        assert(addr < 8192)

        asm {
            MOV bp, sp
            LD ra, addr`14
            JMP [v`14]
        }
    }
    CALL [{v: u32}] => {
        assert(v < 8192)
        addr = $ + 12
        assert(addr >= 8192)

        asm {
            MOV bp, sp
            LD ra, (addr + 4)
            JMP [v`14]
        }
    }
    CALL [{v: u32}] => {
        assert(v >= 8192)
        addr = $ + 16
        assert(addr < 8192)

        asm {
            MOV bp, sp
            LD ra, addr`14
            JMP [v]
        }
    }
    CALL [{v: u32}] => {
        assert(v >= 8192)
        addr = $ + 16
        assert(addr >= 8192)

        asm {
            MOV bp, sp
            LD ra, (addr + 4)
            JMP [v]
        }
    }

    RET {v: u11} => asm {
        ADD sp, bp, (v`14 << 2)`14
        JMP ra
    }

    CALLS => {
        addr = $ + 8
        assert(addr < 8192)

        asm {
            LD ra, addr`14
            SYS
        }
    }
    CALLS => {
        addr = $ + 8
        assert(addr >= 8192)

        asm {
            LD ra, (addr + 4)
            SYS
        }
    }

    RETS => asm {
        CLRK
        JMP ra
    }
}
