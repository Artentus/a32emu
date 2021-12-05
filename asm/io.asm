UART_DATA_IN      = 0x004 ; read-only
UART_DATA_OUT     = 0x005 ; write-only
UART_INPUT_COUNT  = 0x006 ; read-only
UART_OUTPUT_COUNT = 0x007 ; read-only


; serial_read_byte(): u8
serial_read_byte:
    .wait:
        in t0, [UART_INPUT_COUNT]
        test t0
        br.z .wait

    in a0, [UART_DATA_IN]
    ret 0


; serial_write_byte(val: u8)
serial_write_byte:
    .wait:
        in t0, [UART_OUTPUT_COUNT]
        cmp t0, 0xFF
        br.u.g .wait

    out [UART_DATA_OUT], a0
    ret 0


; serial_write(str: u8*)
serial_write:
    push ra
    push bp
    push s0

    mov s0, a0

    .loop:
        ld8 a0, [s0]
        test a0
        br.z .return

        call serial_write_byte

        inc s0
        bra .loop

    .return:
    pop s0
    pop bp
    pop ra
    ret 0


; serial_write_line(str: u8*)
serial_write_line:
    push ra
    push bp
    push s0

    mov s0, a0

    .loop:
        ld8 a0, [s0]
        test a0
        br.z .return

        call serial_write_byte

        inc s0
        bra .loop

    .return:
    ld a0, "\r"a
    call serial_write_byte
    ld a1, "\n"a
    call serial_write_byte

    pop s0
    pop bp
    pop ra
    ret 0

