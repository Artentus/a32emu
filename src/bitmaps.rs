const fn swap_nibbles(value: u8) -> u8 {
    (value << 4) | (value >> 4)
}

const fn reverse_pixels(value: u32) -> u32 {
    let ne = u32::from_be(value);
    let mut bytes = ne.to_le_bytes();
    bytes[0] = swap_nibbles(bytes[0]);
    bytes[1] = swap_nibbles(bytes[1]);
    bytes[2] = swap_nibbles(bytes[2]);
    bytes[3] = swap_nibbles(bytes[3]);
    u32::from_ne_bytes(bytes)
}

macro_rules! define_bitmap {
    (
        $row0:literal,
        $row1:literal,
        $row2:literal,
        $row3:literal,
        $row4:literal,
        $row5:literal,
        $row6:literal,
        $row7:literal,
    ) => {
        Bitmap([
            reverse_pixels($row0),
            reverse_pixels($row1),
            reverse_pixels($row2),
            reverse_pixels($row3),
            reverse_pixels($row4),
            reverse_pixels($row5),
            reverse_pixels($row6),
            reverse_pixels($row7),
        ])
    };
}

impl Bitmap {
    const EMPTY: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const REPLACEMENT_CHAR: Self = define_bitmap!(
        0x00FFFF00,
        0x00F00F00,
        0x00F00F00,
        0x00F00F00,
        0x00F00F00,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );

    const DIGIT_0: Self = define_bitmap!(
        0x000FFF00,
        0x00F000F0,
        0x00F00FF0,
        0x00FFF0F0,
        0x00F000F0,
        0x000FFF00,
        0x00000000,
        0x00000000,
    );

    const DIGIT_1: Self = define_bitmap!(
        0x0000F000,
        0x000FF000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x000FFF00,
        0x00000000,
        0x00000000,
    );

    const DIGIT_2: Self = define_bitmap!(
        0x00FFFF00,
        0x0F0000F0,
        0x00000F00,
        0x000FF000,
        0x00F00000,
        0x0FFFFFF0,
        0x00000000,
        0x00000000,
    );

    const DIGIT_3: Self = define_bitmap!(
        0x00FFFF00,
        0x0F0000F0,
        0x00000F00,
        0x0000FF00,
        0x0F0000F0,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );

    const DIGIT_4: Self = define_bitmap!(
        0x00000F00,
        0x0000FF00,
        0x000F0F00,
        0x00F00F00,
        0x0FFFFFF0,
        0x00000F00,
        0x00000000,
        0x00000000,
    );

    const DIGIT_5: Self = define_bitmap!(
        0x0FFFFFF0,
        0x0F000000,
        0x0FFFFF00,
        0x000000F0,
        0x0F0000F0,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );

    const DIGIT_6: Self = define_bitmap!(
        0x00FFFF00,
        0x0F000000,
        0x0FFFFF00,
        0x0F0000F0,
        0x0F0000F0,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );

    const DIGIT_7: Self = define_bitmap!(
        0x0FFFFFF0,
        0x00000F00,
        0x0000F000,
        0x000F0000,
        0x00F00000,
        0x0F000000,
        0x00000000,
        0x00000000,
    );

    const DIGIT_8: Self = define_bitmap!(
        0x00FFFF00,
        0x0F0000F0,
        0x00FFFF00,
        0x0F0000F0,
        0x0F0000F0,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );

    const DIGIT_9: Self = define_bitmap!(
        0x00FFFF00,
        0x0F0000F0,
        0x0F0000F0,
        0x00FFFFF0,
        0x000000F0,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_A: Self = define_bitmap!(
        0x000FF000,
        0x000FF000,
        0x00F00F00,
        0x00FFFF00,
        0x0F0000F0,
        0x0F0000F0,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_B: Self = define_bitmap!(
        0x0FFFF000,
        0x0F000F00,
        0x0FFFF000,
        0x0F000F00,
        0x0F000F00,
        0x0FFFF000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_C: Self = define_bitmap!(
        0x00FFF000,
        0x0F000F00,
        0x0F000000,
        0x0F000000,
        0x0F000F00,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_D: Self = define_bitmap!(
        0x0FFFF000,
        0x0F000F00,
        0x0F000F00,
        0x0F000F00,
        0x0F000F00,
        0x0FFFF000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_E: Self = define_bitmap!(
        0x0FFFFF00,
        0x0F000000,
        0x0FFFF000,
        0x0F000000,
        0x0F000000,
        0x0FFFFF00,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_F: Self = define_bitmap!(
        0x0FFFFF00,
        0x0F000000,
        0x0FFFF000,
        0x0F000000,
        0x0F000000,
        0x0F000000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_G: Self = define_bitmap!(
        0x00FFF000,
        0x0F000F00,
        0x0F000000,
        0x0F0FFF00,
        0x0F000F00,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_H: Self = define_bitmap!(
        0x0F000F00,
        0x0F000F00,
        0x0FFFFF00,
        0x0F000F00,
        0x0F000F00,
        0x0F000F00,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_I: Self = define_bitmap!(
        0x0FFFFF00,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x0FFFFF00,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_J: Self = define_bitmap!(
        0x00FFFF00,
        0x00000F00,
        0x00000F00,
        0x00000F00,
        0x0F000F00,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_K: Self = define_bitmap!(
        0x0F00F000,
        0x0F0F0000,
        0x0FF00000,
        0x0F0F0000,
        0x0F00F000,
        0x0F000F00,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_L: Self = define_bitmap!(
        0x0F000000,
        0x0F000000,
        0x0F000000,
        0x0F000000,
        0x0F000000,
        0x0FFFFF00,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_M: Self = define_bitmap!(
        0x0F0000F0,
        0x0FF00FF0,
        0x0F0FF0F0,
        0x0F0000F0,
        0x0F0000F0,
        0x0F0000F0,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_N: Self = define_bitmap!(
        0x0F0000F0,
        0x0FF000F0,
        0x0F0F00F0,
        0x0F00F0F0,
        0x0F000FF0,
        0x0F0000F0,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_O: Self = define_bitmap!(
        0x00FFFF00,
        0x0F0000F0,
        0x0F0000F0,
        0x0F0000F0,
        0x0F0000F0,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_P: Self = define_bitmap!(
        0x0FFFF000,
        0x0F000F00,
        0x0FFFF000,
        0x0F000000,
        0x0F000000,
        0x0F000000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_Q: Self = define_bitmap!(
        0x00FFFF00,
        0x0F0000F0,
        0x0F0000F0,
        0x0F0000F0,
        0x0F0000F0,
        0x00FFFF00,
        0x000000F0,
        0x00000000,
    );

    const UPPERCASE_R: Self = define_bitmap!(
        0x0FFFF000,
        0x0F000F00,
        0x0FFFF000,
        0x0F0F0000,
        0x0F00F000,
        0x0F000F00,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_S: Self = define_bitmap!(
        0x00FFF000,
        0x0F000F00,
        0x00F00000,
        0x000FF000,
        0x0F000F00,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_T: Self = define_bitmap!(
        0x0FFFFF00,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_U: Self = define_bitmap!(
        0x0F000F00,
        0x0F000F00,
        0x0F000F00,
        0x0F000F00,
        0x0F000F00,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_V: Self = define_bitmap!(
        0x0F000F00,
        0x0F000F00,
        0x0F000F00,
        0x00F0F000,
        0x00F0F000,
        0x000F0000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_W: Self = define_bitmap!(
        0x0F0000F0,
        0x0F0000F0,
        0x0F0000F0,
        0x0F0FF0F0,
        0x0FF00FF0,
        0x0F0000F0,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_X: Self = define_bitmap!(
        0x0F0000F0,
        0x00F00F00,
        0x000FF000,
        0x000FF000,
        0x00F00F00,
        0x0F0000F0,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_Y: Self = define_bitmap!(
        0x0F000F00,
        0x00F0F000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x00000000,
        0x00000000,
    );

    const UPPERCASE_Z: Self = define_bitmap!(
        0x0FFFFFF0,
        0x00000F00,
        0x0000F000,
        0x000F0000,
        0x00F00000,
        0x0FFFFFF0,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_A: Self = define_bitmap!(
        0x00000000,
        0x00FFFF00,
        0x000000F0,
        0x00FFFFF0,
        0x0F000FF0,
        0x00FFF0F0,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_B: Self = define_bitmap!(
        0x0F000000,
        0x0F000000,
        0x0FFFFF00,
        0x0F0000F0,
        0x0FF000F0,
        0x0F0FFF00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_C: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x000FFF00,
        0x00F00000,
        0x00F00000,
        0x000FFF00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_D: Self = define_bitmap!(
        0x000000F0,
        0x000000F0,
        0x00FFFFF0,
        0x0F0000F0,
        0x0F000FF0,
        0x00FFF0F0,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_E: Self = define_bitmap!(
        0x00000000,
        0x00FFFF00,
        0x0F0000F0,
        0x0FFFFFF0,
        0x0F000000,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_F: Self = define_bitmap!(
        0x000FF000,
        0x000F0000,
        0x00FFF000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_G: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00FF0F00,
        0x0F00FF00,
        0x0F000F00,
        0x00FFFF00,
        0x00000F00,
        0x00FFF000,
    );

    const LOWERCASE_H: Self = define_bitmap!(
        0x00F00000,
        0x00F00000,
        0x00FFF000,
        0x00F00F00,
        0x00F00F00,
        0x00F00F00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_I: Self = define_bitmap!(
        0x0000F000,
        0x00000000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_J: Self = define_bitmap!(
        0x0000F000,
        0x00000000,
        0x000FF000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x00FF0000,
    );

    const LOWERCASE_K: Self = define_bitmap!(
        0x00F00000,
        0x00F00000,
        0x00F00F00,
        0x00F0F000,
        0x00FF0000,
        0x00F0FF00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_L: Self = define_bitmap!(
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000FF000,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_M: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0FF00FF0,
        0x0F0FF0F0,
        0x0F0000F0,
        0x0F0000F0,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_N: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0F0FFF00,
        0x0FF00F00,
        0x0F000F00,
        0x0F000F00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_O: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00FFF000,
        0x0F000F00,
        0x0F000F00,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_P: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0F0FFF00,
        0x0FF000F0,
        0x0F0000F0,
        0x0FFFFF00,
        0x0F000000,
        0x0F000000,
    );

    const LOWERCASE_Q: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00FFF0F0,
        0x0F000FF0,
        0x0F0000F0,
        0x00FFFFF0,
        0x000000F0,
        0x000000F0,
    );

    const LOWERCASE_R: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0F0FF000,
        0x0FF00F00,
        0x0F000000,
        0x0F000000,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_S: Self = define_bitmap!(
        0x00000000,
        0x000FFF00,
        0x00F00000,
        0x000FF000,
        0x00000F00,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_T: Self = define_bitmap!(
        0x000F0000,
        0x000F0000,
        0x00FFF000,
        0x000F0000,
        0x000F0000,
        0x000FF000,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_U: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0F000F00,
        0x0F000F00,
        0x0F00FF00,
        0x00FF0F00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_V: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0F000F00,
        0x0F000F00,
        0x00F0F000,
        0x000F0000,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_W: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0F0000F0,
        0x0F0000F0,
        0x0F0FF0F0,
        0x00F00F00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_X: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0F000F00,
        0x00F0F000,
        0x000F0000,
        0x0FF0FF00,
        0x00000000,
        0x00000000,
    );

    const LOWERCASE_Y: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0F0000F0,
        0x00F000F0,
        0x000F0F00,
        0x0000F000,
        0x000F0000,
        0x0FF00000,
    );

    const LOWERCASE_Z: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00FFFF00,
        0x0000F000,
        0x000F0000,
        0x00FFFF00,
        0x00000000,
        0x00000000,
    );








    const SPACE: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const EXCLAMATION_MARK: Self = define_bitmap!(
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x00000000,
        0x0000F000,
        0x00000000,
        0x00000000,
    );

    const DOUBLE_QUOTATION_MARK: Self = define_bitmap!(
        0x00F0F000,
        0x00F0F000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const POUND: Self = define_bitmap!(
        0x00000000,
        0x00F0F000,
        0x0FFFFF00,
        0x00F0F000,
        0x0FFFFF00,
        0x00F0F000,
        0x00000000,
        0x00000000,
    );

    const DOLLAR_SIGN: Self = define_bitmap!(
        0x0000F000,
        0x000FFFF0,
        0x00F0F000,
        0x000FFF00,
        0x0000F0F0,
        0x00FFFF00,
        0x0000F000,
        0x00000000,
    );

    const PERCENT_SIGN: Self = define_bitmap!(
        0x0FF000F0,
        0xF00F0F00,
        0x0FF0F000,
        0x000F0FF0,
        0x00F0F00F,
        0x0F000FF0,
        0x00000000,
        0x00000000,
    );

    const AMPERSAND: Self = define_bitmap!(
        0x00FFF000,
        0x0F000F00,
        0x0F000000,
        0x00FF00F0,
        0x0F00FF00,
        0x00FF00F0,
        0x00000000,
        0x00000000,
    );

    const SINGLE_QUOTATION_MARK: Self = define_bitmap!(
        0x0000F000,
        0x0000F000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const LEFT_PARENTHESIS: Self = define_bitmap!(
        0x0000F000,
        0x000F0000,
        0x00F00000,
        0x00F00000,
        0x000F0000,
        0x0000F000,
        0x00000000,
        0x00000000,
    );

    const RIGHT_PARENTHESIS: Self = define_bitmap!(
        0x000F0000,
        0x0000F000,
        0x00000F00,
        0x00000F00,
        0x0000F000,
        0x000F0000,
        0x00000000,
        0x00000000,
    );

    const ASTERISK: Self = define_bitmap!(
        0x0000F000,
        0x00F0F0F0,
        0x000FFF00,
        0x000F0F00,
        0x00F000F0,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const PLUS_SIGN: Self = define_bitmap!(
        0x00000000,
        0x0000F000,
        0x0000F000,
        0x00FFFFF0,
        0x0000F000,
        0x0000F000,
        0x00000000,
        0x00000000,
    );

    const COMMA: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x0000F000,
        0x000F0000,
        0x00000000,
    );

    const HYPHEN: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00000000,
        0x00FFFF00,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const PERIOD: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x0000F000,
        0x00000000,
        0x00000000,
    );

    const SLASH: Self = define_bitmap!(
        0x000000F0,
        0x00000F00,
        0x0000F000,
        0x000F0000,
        0x00F00000,
        0x0F000000,
        0x00000000,
        0x00000000,
    );

    const COLON: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0000F000,
        0x00000000,
        0x00000000,
        0x0000F000,
        0x00000000,
        0x00000000,
    );

    const SEMICOLON: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0000F000,
        0x00000000,
        0x00000000,
        0x0000F000,
        0x000F0000,
        0x00000000,
    );

    const LESS_THAN_SIGN: Self = define_bitmap!(
        0x00000000,
        0x00000FF0,
        0x000FF000,
        0x0FF00000,
        0x000FF000,
        0x00000FF0,
        0x00000000,
        0x00000000,
    );

    const EQUAL_SIGN: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x0FFFFFF0,
        0x00000000,
        0x0FFFFFF0,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const GREATER_THAN_SIGN: Self = define_bitmap!(
        0x00000000,
        0x0FF00000,
        0x000FF000,
        0x00000FF0,
        0x000FF000,
        0x0FF00000,
        0x00000000,
        0x00000000,
    );

    const QUESTION_MARK: Self = define_bitmap!(
        0x00FFF000,
        0x0F000F00,
        0x0000F000,
        0x000F0000,
        0x00000000,
        0x000F0000,
        0x00000000,
        0x00000000,
    );

    const AT_SIGN: Self = define_bitmap!(
        0x00FFF000,
        0x0F000F00,
        0x00FFF0F0,
        0x0F00F0F0,
        0x0F00F0F0,
        0x00FF0F00,
        0x00000000,
        0x00000000,
    );

    const LEFT_SQUARE_BRACKET: Self = define_bitmap!(
        0x000FFF00,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000F0000,
        0x000FFF00,
        0x00000000,
        0x00000000,
    );

    const BACKSLASH: Self = define_bitmap!(
        0x0F000000,
        0x00F00000,
        0x000F0000,
        0x0000F000,
        0x00000F00,
        0x000000F0,
        0x00000000,
        0x00000000,
    );

    const RIGHT_SQUARE_BRACKET: Self = define_bitmap!(
        0x00FFF000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const CIRCUMFLEX: Self = define_bitmap!(
        0x000F0000,
        0x00F0F000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const UNDERSCORE: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x0FFFFFF0,
        0x00000000,
        0x00000000,
    );

    const GRAVE: Self = define_bitmap!(
        0x000F0000,
        0x0000F000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
    );

    const LEFT_CURLY_BRACKET: Self = define_bitmap!(
        0x000FFF00,
        0x000F0000,
        0x00FF0000,
        0x00FF0000,
        0x000F0000,
        0x000FFF00,
        0x00000000,
        0x00000000,
    );

    const VERTICAL_BAR: Self = define_bitmap!(
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x0000F000,
        0x00000000,
        0x00000000,
    );

    const RIGHT_CURLY_BRACKET: Self = define_bitmap!(
        0x00FFF000,
        0x0000F000,
        0x0000FF00,
        0x0000FF00,
        0x0000F000,
        0x00FFF000,
        0x00000000,
        0x00000000,
    );

    const TILDE: Self = define_bitmap!(
        0x00000000,
        0x00000000,
        0x00FF00F0,
        0x0F00FF00,
        0x00000000,
        0x00000000,
        0x00000000,
        0x00000000,
    );
}
