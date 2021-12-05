DMA_SRC = 0x000
DMA_DST = 0x001
DMA_LEN = 0x002
DMA_DIR = 0x003


; mem_copy(src: i32*, dst: i32*, len: u32)
mem_copy:
    test a2
    br.z .return ; zero length

    cmp a0, a1
    br.eq .return ; src and dst are the same

    ld t1, 0

    cmp a0, a1
    br.u.g .copy

    ; we have to copy backwards
    shl t0, a2, 2
    add a0, a0, t0
    add a1, a1, t0
    sub a0, a0, 4
    sub a1, a1, 4
    ld t1, 1

    .copy:
    out [DMA_SRC], a0
    out [DMA_DST], a1
    out [DMA_LEN], a2
    out [DMA_DIR], t1

    .return:
    ret 0


; mem_set(dst: i32*, len: u32, val: i32)
mem_set:
    test a1
    br.z .return ; zero length

    st [a0], a2
    dec a1
    br.z .return ; length of one

    add t0, a0, 4
    out [DMA_SRC], a0
    out [DMA_DST], t0
    out [DMA_LEN], a1
    out [DMA_DIR], zero

    .return:
    ret 0


; mem_zero(dst: i32*, len: u32)
mem_zero:
    ld a2, 0
    jmp mem_set
