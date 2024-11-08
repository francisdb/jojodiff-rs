use log::debug;
use std::io;
use std::io::{Read, Seek, Write};

/// 167 Escape
const ESC: u8 = 0xA7;
/// 166 Modify
const MOD: u8 = 0xA6;
/// 165 Insert
const INS: u8 = 0xA5;
/// 164 Delete
const DEL: u8 = 0xA4;
/// 163 Equal
const EQL: u8 = 0xA3;
/// 162 Backtrace
const BKT: u8 = 0xA2;

// read next operand from input
const READ_NEXT_OPERAND: u8 = 0;

fn operand_name(op: u8) -> String {
    match op {
        ESC => "ESC".into(),
        MOD => "MOD".into(),
        INS => "INS".into(),
        DEL => "DEL".into(),
        EQL => "EQL".into(),
        BKT => "BKT".into(),
        other => format!("UNKNOWN {}", other),
    }
}

type Offset = u64;
type Operand = u8;

/// Read a byte from the input.
/// return the byte read, or EOF if end of stream
/// * `read` Input reader
fn read_byte<R: Read>(read: &mut R) -> io::Result<Option<u8>> {
    let mut byte_buf = [0; 1];
    let n = read.read(&mut byte_buf)?;
    let result = if n == 0 { None } else { Some(byte_buf[0]) };
    Ok(result)
}

/// Copy a series of bytes from input to output.
/// * `source`     Input reader
/// * `dest`       Output writer
/// * `source_pos` Position to copy from
/// * `len`        Number of bytes to copy
fn copy_bytes<R: Read + Seek, W: Write>(
    source: &mut R,
    dest: &mut W,
    source_pos: u64,
    len: usize,
) -> io::Result<()> {
    let mut buf = vec![0; len];
    source.seek(std::io::SeekFrom::Start(source_pos))?;
    source.read_exact(&mut buf)?;
    dest.write_all(&buf)?;
    Ok(())
}

/// Get an offset/len from the input stream
fn get_int<T: Read>(mut input_stream: T) -> io::Result<Offset> {
    let mut buf = [0; 1];
    input_stream.read_exact(&mut buf).map_err(|e| {
        io::Error::new(e.kind(), format!("Failed to read first length byte: {}", e))
    })?;
    let first_byte = buf[0];
    if first_byte < 252 {
        Ok(first_byte as Offset + 1)
    } else if first_byte == 252 {
        let mut buf = [0; 1];
        input_stream.read_exact(&mut buf).map_err(|e| {
            io::Error::new(
                e.kind(),
                format!("Failed to read 1 byte for 253 + 8-bit length: {}", e),
            )
        })?;
        let val = 253 + u8::from_be_bytes(buf) as Offset;
        Ok(val)
    } else if first_byte == 253 {
        let mut buf = [0; 2];
        input_stream.read_exact(&mut buf).map_err(|e| {
            io::Error::new(
                e.kind(),
                format!("Failed to read 2 bytes for 16-bit length: {}", e),
            )
        })?;
        let val = u16::from_be_bytes(buf) as Offset;
        Ok(val)
    } else if first_byte == 254 {
        let mut buf = [0; 4];
        input_stream.read_exact(&mut buf).map_err(|e| {
            io::Error::new(
                e.kind(),
                format!("Failed to read 4 bytes for 32-bit length: {}", e),
            )
        })?;
        let val = u32::from_be_bytes(buf) as Offset;
        Ok(val)
    } else {
        let mut buf = [0; 8];
        input_stream.read_exact(&mut buf).map_err(|e| {
            io::Error::new(
                e.kind(),
                format!("Failed to read 8 bytes for 64-bit length: {}", e),
            )
        })?;
        let val = u64::from_be_bytes(buf) as Offset;
        Ok(val)
    }
}

/// Put one byte of output data
///
/// * `writer` output writer
/// * `data` output byte
/// * `position_original` position on source stream
/// * `position_out` position on output stream
/// * `operand` MOD or INS
/// * `offset` offset
///
/// returns 1
fn write_byte<W: Write>(
    writer: &mut W,
    byte: u8,
    position_original: Offset,
    position_out: Offset,
    operand: u8,
    offset: Offset,
) -> io::Result<Offset> {
    writer.write_all(&[byte])?;
    debug!(
        "put_data {} {} {} {} {}",
        position_original + (if operand == MOD { offset } else { 0 }),
        position_out + offset,
        operand_name(operand),
        byte,
        render_ascii(byte)
    );
    Ok(1)
}

fn render_ascii(data: u8) -> char {
    if (32..=127).contains(&data) {
        data as char
    } else {
        ' '
    }
}

/// Read a data sequence (INS or MOD)
///
/// * `patch_reader`         patch stream
/// * `out_stream`           output stream
/// * `position_original`    position on source stream
/// * `position_out`         position on output stream
/// * `operand`              INS or MOD
/// * `pending`              First pending byte (EOF = no pending byte)
/// * `dbl`                  Second pending byte (EOF = no pending byte)
///
/// returns the next operand and the bytes read
fn copy_data<R: Read, W: Write>(
    patch_reader: &mut R,
    out_writer: &mut W,
    position_original: Offset,
    position_out: Offset,
    operand: Operand,
    pending: Option<u8>,
    pending2: Option<u8>,
) -> io::Result<(Option<Operand>, Offset)> {
    let mut bytes_read: Offset = 0;

    /* First, output the pending bytes:
       pending  pending2     Output
        ESC      ESC          ESC ESC
        ESC      xxx          ESC xxx
        xxx      EOF          xxx
    */
    if let Some(pending_byte) = pending {
        bytes_read += write_byte(
            out_writer,
            pending_byte,
            position_original,
            position_out,
            operand,
            bytes_read,
        )?;
        if pending_byte == ESC {
            if let Some(pending_byte2) = pending2 {
                bytes_read += write_byte(
                    out_writer,
                    pending_byte2,
                    position_original,
                    position_out,
                    operand,
                    bytes_read,
                )?;
            }
        }
    }

    /* Read loop */
    while let Some(mut input) = read_byte(patch_reader)? {
        // Handle ESC-code
        if input == ESC {
            if let Some(next_operand) = read_byte(patch_reader)? {
                match next_operand {
                    DEL | EQL | BKT | MOD | INS => (),
                    ESC => {
                        // Double ESC: drop one
                        debug!(
                            "{} {} ESC ESC",
                            position_original + (if operand == MOD { bytes_read } else { 0 }),
                            position_out + bytes_read
                        );

                        // Write the single ESC and continue
                        bytes_read += write_byte(
                            out_writer,
                            input,
                            position_original,
                            position_out,
                            operand,
                            bytes_read,
                        )?;
                        continue;
                    }
                    _ => {
                        // ESC <xxx> with <xxx> not an opcode: output as they are
                        debug!(
                            "{} {} ESC XXX",
                            position_original + (if operand == MOD { bytes_read } else { 0 }),
                            position_out + bytes_read
                        );

                        // Write the escape, the <xxx> and continue
                        bytes_read += write_byte(
                            out_writer,
                            input,
                            position_original,
                            position_out,
                            operand,
                            bytes_read,
                        )?;
                        bytes_read += write_byte(
                            out_writer,
                            next_operand,
                            position_original,
                            position_out,
                            operand,
                            bytes_read,
                        )?;
                        continue;
                    }
                }
                if next_operand == operand {
                    // <ESC> MOD within an <ESC> MOD is meaningless: handle as data
                    // <ESC> INS within an <ESC> INS is meaningless: handle as data
                    debug!(
                        "{} {} ESC {}",
                        position_original + (if operand == MOD { bytes_read } else { 0 }),
                        position_out + bytes_read,
                        next_operand
                    );

                    bytes_read += write_byte(
                        out_writer,
                        ESC,
                        position_original,
                        position_out,
                        operand,
                        bytes_read,
                    )?;
                    input = next_operand; // will be output below
                } else {
                    return Ok((Some(next_operand), bytes_read));
                }
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "ESC at end of stream",
                ));
            }
        }

        // Handle data
        bytes_read += write_byte(
            out_writer,
            input,
            position_original,
            position_out,
            operand,
            bytes_read,
        )?;
    } /* while ! EOF */

    Ok((None, bytes_read))
}

/// Patch function
///
/// * `in_reader`  Input reader
/// * `patch_reader` Patch reader
/// * `out_writer` Output writer
///
/// *Note: we suggest using `BufReader` and `BufWriter` to improve performance*
///
pub fn patch<R: Read + Seek, W: Write>(
    in_reader: &mut R,
    patch_reader: &mut R,
    out_writer: &mut W,
) -> io::Result<()> {
    // Position in source stream
    let mut position_original: Offset = 0;
    // Position in destination stream
    let mut position_out: Offset = 0;

    // Current operand
    let mut operand = READ_NEXT_OPERAND; // no operand
    loop {
        // 1st Pending byte
        let pending1: Option<u8>;
        // 2nd Pending byte
        let pending2: Option<u8>;
        // Read operand from input, unless this has already been done
        if operand == READ_NEXT_OPERAND {
            let next_byte = if let Some(byte) = read_byte(patch_reader)? {
                byte
            } else {
                // end of stream
                break;
            };

            // Handle ESC <opr>
            if next_byte == ESC {
                let escaped_byte = if let Some(byte) = read_byte(patch_reader)? {
                    byte
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        "ESC at end of stream",
                    ));
                };

                match escaped_byte {
                    EQL | DEL | BKT | MOD | INS => {
                        // new operand found, all ok !
                        operand = escaped_byte;
                        pending1 = None;
                        pending2 = None;
                    }
                    _ => {
                        // ESC xxx or ESC ESC at the start of a sequence
                        // Resolve by double pending bytes
                        operand = MOD;
                        pending1 = Some(ESC);
                        pending2 = Some(escaped_byte);
                    }
                }
            } else {
                operand = MOD; // If an ESC <opr> is missing, set default operand (gaining two bytes)
                pending1 = Some(next_byte);
                pending2 = None;
            }
        } else {
            // There is no lookahead so checking if we should stop copying data involves reading the
            // next operand. This is done by the copy_data function used by the MOD and INS operands.
            pending1 = None;
            pending2 = None;
        }

        match operand {
            MOD => {
                // insert data from the patch file into the output file while also advancing the source file
                let (next_operand, bytes_read) = copy_data(
                    patch_reader,
                    out_writer,
                    position_original,
                    position_out,
                    operand,
                    pending1,
                    pending2,
                )?;
                debug!(
                    "{} {} MOD {} byte(s)",
                    position_original, position_out, bytes_read
                );
                position_original += bytes_read;
                position_out += bytes_read;
                if let Some(o) = next_operand {
                    operand = o;
                } else {
                    // end of stream
                    break;
                }
            }
            INS => {
                // insert data from the patch file at the current position in the output file
                let (next_operand, bytes_read) = copy_data(
                    patch_reader,
                    out_writer,
                    position_original,
                    position_out,
                    operand,
                    pending1,
                    pending2,
                )?;
                debug!(
                    "{} {} INS {} byte(s)",
                    position_original, position_out, bytes_read
                );
                position_out += bytes_read;
                if let Some(o) = next_operand {
                    operand = o;
                } else {
                    // end of stream
                    break;
                }
            }
            DEL => {
                // skip the next <len> bytes in the source file
                let len = get_int(&mut *patch_reader)?;
                debug!("{} {} DEL {} byte(s)", position_original, position_out, len);
                position_original += len;
                operand = READ_NEXT_OPERAND;
            }
            EQL => {
                /* copy the next <len> bytes from the source file to the output file */
                let len = get_int(&mut *patch_reader)?;
                debug!("{} {} EQL {} byte(s)", position_original, position_out, len);
                copy_bytes(in_reader, out_writer, position_original, len as usize)?;
                position_original += len;
                position_out += len;
                operand = READ_NEXT_OPERAND;
            }
            BKT => {
                // go back <offset> bytes in the source file
                let offset = get_int(&mut *patch_reader)?;
                debug!("{} {} BKT {}", position_original, position_out, offset);
                position_original -= offset;
                operand = READ_NEXT_OPERAND;
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Unknown operand {}", operand),
                ));
            }
        }
    }
    out_writer.flush()?;
    debug!("{} {} EOF", position_original, position_out);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::sync::Once;

    static INIT: Once = Once::new();

    /// Setup function that is only run once, even if called multiple times.
    fn setup() {
        INIT.call_once(|| {
            env_logger::init();
        });
    }

    #[test]
    fn get_int_1_byte() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![1]);
        let result = get_int(&mut cursor)?;
        Ok(assert_eq!(result, 2))
    }

    #[test]
    fn get_int_2_bytes() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![252, 1]);
        let result = get_int(&mut cursor)?;
        Ok(assert_eq!(result, 254))
    }

    #[test]
    fn get_int_3_bytes() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![253, 1, 2]);
        let result = get_int(&mut cursor)?;
        Ok(assert_eq!(result, 258))
    }

    #[test]
    fn get_int_4_bytes() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![254, 1, 0, 0, 0]);
        let result = get_int(&mut cursor)?;
        Ok(assert_eq!(result, 1 << 24))
    }

    #[test]
    fn get_int_8_bytes() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![255, 1, 0, 0, 0, 0, 0, 0, 0]);
        let result = get_int(&mut cursor)?;
        Ok(assert_eq!(result, 1 << 56))
    }

    #[test]
    fn get_int_8_bytes_missing() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![255, 1, 0]);
        let result = get_int(&mut cursor);
        Ok(assert_eq!(
            result.unwrap_err().to_string(),
            "Failed to read 8 bytes for 64-bit length: failed to fill whole buffer"
        ))
    }

    #[test]
    fn patch_empty_file_and_empty_patch() -> io::Result<()> {
        setup();
        let mut empty_input_stream = io::Cursor::new(vec![]);
        let mut empty_patch_stream = io::Cursor::new(vec![]);
        let mut empty_output_stream = io::Cursor::new(vec![]);

        patch(
            &mut empty_input_stream,
            &mut empty_patch_stream,
            &mut empty_output_stream,
        )?;

        let out_len = empty_output_stream.get_ref().len();
        Ok(assert_eq!(out_len, 0))
    }

    #[test]
    fn patch_trailing_esc() {
        setup();
        let in_data = vec![];
        let patch_data = vec![ESC];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        let result = patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor);

        assert_eq!(result.is_err(), true);
        assert_eq!(result.unwrap_err().to_string(), "ESC at end of stream");
    }

    #[test]
    fn patch_trailing_esc_with_body() {
        setup();
        let in_data = vec![];
        let patch_data = vec![1, 2, 3, ESC];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        let result = patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor);

        assert_eq!(result.is_err(), true);
        assert_eq!(result.unwrap_err().to_string(), "ESC at end of stream");
    }

    #[test]
    fn patch_delete_1_byte() -> io::Result<()> {
        setup();
        let in_data = vec![1];
        let patch_data = vec![ESC, DEL, 0];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data: [u8; 0] = [];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_identical_copy() -> io::Result<()> {
        setup();
        let in_data = vec![1, 2, 3];
        let patch_data = vec![ESC, EQL, 2];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = [1, 2, 3];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_copy_byte_3_times() -> io::Result<()> {
        setup();
        let in_data = vec![1];
        #[rustfmt::skip]
        let patch_data = vec![
            ESC, EQL, 0,
            ESC, BKT, 0,
            ESC, EQL, 0,
            ESC, BKT, 0,
            ESC, EQL, 0,
        ];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = [1, 1, 1];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_copy_seq_3_times() -> io::Result<()> {
        setup();
        let in_data = vec![1, 2, 3];
        #[rustfmt::skip]
        let patch_data = vec![
            ESC, EQL, 2,
            ESC, BKT, 2,
            ESC, EQL, 2,
            ESC, BKT, 2,
            ESC, EQL, 2,
        ];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = [1, 2, 3, 1, 2, 3, 1, 2, 3];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_insert_into_empty_file_including_escaped_value() -> io::Result<()> {
        setup();
        let in_data = vec![];
        #[rustfmt::skip]
    let patch_data = vec![
        ESC, INS, 1, 2, ESC, ESC, 3
    ];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = [1, 2, ESC, 3];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_overwrite_middle_part() -> io::Result<()> {
        setup();
        let in_data = vec![1, 2, 3, 4, 5, 6, 7];
        #[rustfmt::skip]
        let patch_data = vec![
            ESC, EQL, 1,
            ESC, MOD, 0, 0, 0,
            ESC, EQL, 1,
        ];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = [1, 2, 0, 0, 0, 6, 7];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_mod_ins() -> io::Result<()> {
        // this is a special case where MOD follows INS or the other way round
        setup();
        let in_data = vec![1, 2, 3];
        #[rustfmt::skip]
        let patch_data = vec![
            ESC, MOD, 10,
            ESC, INS, 11,
            ESC, MOD, 12,
            ESC, INS, 13,
            ESC, MOD, 14,
        ];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = [10, 11, 12, 13, 14];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_apply_mod_when_when_operand_missing() -> io::Result<()> {
        setup();
        let in_data = vec![1, 2, 3];
        #[rustfmt::skip]
        let patch_data = vec![
            3,2,1
        ];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = [3, 2, 1];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_ascii() -> io::Result<()> {
        setup();
        let in_data = b"Hello, World!".to_vec();
        // replace World with Universe
        #[rustfmt::skip]
        let patch_data = vec![
            ESC, EQL, 6,
            ESC, MOD, b'U', b'n', b'i', b'v', b'e',
            ESC, INS, b'r', b's', b'e',
            ESC, EQL, 0,
        ];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = "Hello, Universe!";
        Ok(assert_eq!(
            String::from_utf8_lossy(&out_data),
            expected_data
        ))
    }
}
