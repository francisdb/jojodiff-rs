use log::debug;
use std::io;
use std::io::{Read, Seek, Write};

const EOF: i32 = -1;

/// 167 Escape
const ESC: i32 = 0xA7;
/// 166 Modify
const MOD: i32 = 0xA6;
/// 165 Insert
const INS: i32 = 0xA5;
/// 164 Delete
const DEL: i32 = 0xA4;
/// 163 Equal
const EQL: i32 = 0xA3;
/// 162 Backtrace
const BKT: i32 = 0xA2;

// read next operand from input
const READ_NEXT_OPERAND: i32 = 0;

fn operand_name(op: i32) -> String {
    match op {
        ESC => "ESC".into(),
        MOD => "MOD".into(),
        INS => "INS".into(),
        DEL => "DEL".into(),
        EQL => "EQL".into(),
        BKT => "BKT".into(),
        EOF => "EOF".into(),
        other => format!("UNKNOWN {}", other),
    }
}

type OffT = u64;
type Operand = i32;

// TODO do we actually need this struct, can we not have extension methods on Read + Write + Seek?
struct JStream<T: Read + Write + Seek> {
    stream: T,
}

impl<T: Read + Write + Seek> JStream<T> {
    fn new(stream: T) -> Self {
        JStream { stream }
    }

    /// Read a byte from the input.
    /// @return   the byte read, or EOF if end of file
    fn get(&mut self) -> io::Result<i32> {
        let mut byte_buf = [0; 1];
        let n = self.stream.read(&mut byte_buf)?;
        if n == 0 {
            Ok(EOF)
        } else {
            Ok(byte_buf[0] as i32)
        }
    }

    fn get_fail_on_eof(&mut self) -> io::Result<i32> {
        let mut byte_buf = [0; 1];
        let n = self.stream.read(&mut byte_buf)?;
        if n == 0 {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "Unexpected end of file",
            ))
        } else {
            Ok(byte_buf[0] as i32)
        }
    }

    /// Copy a series of bytes from input to output.
    /// @param    apFilInp    Input file
    /// @param    azPos       Position to copy from
    /// @param    azLen       Number of bytes to copy
    fn copyfrom<U: Read + Seek + Write>(
        &mut self,
        other: &mut JStream<U>,
        pos: OffT,
        len: OffT,
    ) -> io::Result<()> {
        let mut buf = vec![0; len as usize];
        other.stream.seek(std::io::SeekFrom::Start(pos))?;
        other.stream.read_exact(&mut buf)?;
        self.stream.write_all(&buf)?;
        Ok(())
    }

    /// Write a byte to the output.
    /// @param    aiDta   data to write
    fn put_byte(&mut self, byte: i32) -> io::Result<()> {
        self.stream.write_all(&[byte as u8])
    }
}

/// Get an offset from the input file
fn get_int<T: Read + Seek + Write>(input_file: &mut JStream<T>) -> io::Result<OffT> {
    let first_byte = input_file.get_fail_on_eof()?;
    if first_byte < 252 {
        Ok(first_byte as OffT + 1)
    } else if first_byte == 252 {
        Ok(253 + input_file.get_fail_on_eof()? as OffT)
    } else if first_byte == 253 {
        let mut val = input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        Ok(val)
    } else if first_byte == 254 {
        let mut val = input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        Ok(val)
    } else {
        // #ifdef JDIFF_LARGEFILE
        let mut val = input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        val = (val << 8) + input_file.get_fail_on_eof()? as OffT;
        Ok(val)
        // #else
        // fprintf(stderr, "64-bit length numbers not supported!\n");
        // return EXI_LRG;
        // #endif
    }
}

/// Put one byte of output data
///
/// * @param    azPosOrg    position on source file
/// * @param    azPosOut    position on output file
/// * @param    aiOpr       MOD or INS
/// * @param    aiOut       output byte
/// * @param    aiOff       offset
/// * @return   1
fn put_data<T: Read + Write + Seek>(
    file_out: &mut JStream<T>,
    position_original: OffT,
    position_out: OffT,
    operand: i32,
    data: i32,
    offset: OffT,
) -> io::Result<OffT> {
    file_out.put_byte(data)?;
    debug!(
        "put_data {} {} {} {} {}",
        position_original + (if operand == MOD { offset } else { 0 }),
        position_out + offset,
        operand_name(operand),
        data,
        (if data >= 32 && data <= 127 {
            data as u8 as char
        } else {
            ' '
        })
    );
    Ok(1)
}

/// Read a data sequence (INS or MOD)
///
/// @param    azPosOrg    position on source file
/// @param    azPosOut    position on output file
/// @param    aiOpr       INS or MOD
/// @param    azMod       out: offset counter
/// @param    aiPnd       First pending byte (EOF = no pending byte)
/// @param    aiDbl       Second pending byte (EOF = no pending byte)
///
/// returns the next operand and the bytes read
fn get_data<T: Read + Write + Seek>(
    file_patch: &mut JStream<T>,
    file_out: &mut JStream<T>,
    position_original: OffT,
    position_out: OffT,
    operand: i32,
    pending: i32,
    dbl: i32,
) -> io::Result<(Option<Operand>, OffT)> {
    let mut bytes_read: OffT = 0;

    /* First, output the pending bytes:
       liPnd  liDbl     Output
        ESC    ESC      ESC ESC
        ESC    xxx      ESC xxx
        xxx    EOF      xxx
    */
    if pending != EOF {
        bytes_read += put_data(
            file_out,
            position_original,
            position_out,
            operand,
            pending,
            bytes_read,
        )?;
        if pending == ESC && dbl != ESC {
            bytes_read += put_data(
                file_out,
                position_original,
                position_out,
                operand,
                dbl,
                bytes_read,
            )?;
        }
    }

    /* Read loop */
    while let Ok(mut input) = file_patch.get() {
        if input == EOF {
            break;
        }
        // Handle ESC-code
        if input == ESC {
            let next_operand = file_patch.get()?;
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
                    bytes_read += put_data(
                        file_out,
                        position_original,
                        position_out,
                        operand,
                        input,
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
                    bytes_read += put_data(
                        file_out,
                        position_original,
                        position_out,
                        operand,
                        input,
                        bytes_read,
                    )?;
                    bytes_read += put_data(
                        file_out,
                        position_original,
                        position_out,
                        operand,
                        next_operand,
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

                bytes_read += put_data(
                    file_out,
                    position_original,
                    position_out,
                    operand,
                    ESC,
                    bytes_read,
                )?;
                input = next_operand; // will be output below
            } else {
                return Ok((Some(next_operand), bytes_read));
            }
        }

        // Handle data
        bytes_read += put_data(
            file_out,
            position_original,
            position_out,
            operand,
            input,
            bytes_read,
        )?;
    } /* while ! EOF */

    Ok((None, bytes_read))
}

/// Patch function
///
/// Input stream consists of a series of
///   <op> (<data> || <len>)
/// where
///   <op>   = <ESC> (<MOD>||<INS>||<DEL>||<EQL>||<BKT>)
///   <data> = <chr>||<ESC><ESC>
///   <chr>  = any byte different from <ESC><MOD><INS><DEL><EQL> or <BKT>
///   <ESC><ESC> yields one <ESC> byte
///
/// Input and patch streams are read only, output stream is written to.
pub fn patch<W: Read + Write + Seek>(
    in_stream: &mut W,
    patch_stream: &mut W,
    out_stream: &mut W,
) -> io::Result<()> {
    // 1st Pending byte (EOF = no pending bye)
    let mut inp: i32;
    // 2nd Pending byte (EOF = no pending bye)
    let mut dbl: i32;
    // Current operand
    let mut operand;

    // Position in source file
    let mut position_original: OffT = 0;
    // Position in destination file
    let mut position_out: OffT = 0;

    // file streams
    let mut file_original = JStream::new(in_stream);
    let mut file_patch = JStream::new(patch_stream);
    let mut file_out = JStream::new(out_stream);

    operand = READ_NEXT_OPERAND; // no operand
    while operand != EOF {
        // Read operand from input, unless this has already been done
        if operand == READ_NEXT_OPERAND {
            inp = file_patch.get()?;
            if inp == EOF {
                break;
            }

            // Handle ESC <opr>
            if inp == ESC {
                dbl = file_patch.get()?;
                match dbl {
                    EQL | DEL | BKT | MOD | INS => {
                        operand = dbl;
                        dbl = EOF;
                        inp = EOF;
                    }
                    // new operand found, all ok !
                    EOF => {
                        // serious error, let's call this a trailing byte
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "ESC at end of file",
                        ));
                    }
                    _ => {
                        // ESC xxx or ESC ESC at the start of a sequence
                        // Resolve by double pending bytes: liInp and liDbl
                        operand = MOD;
                    }
                }
            } else {
                operand = MOD; // If an ESC <opr> is missing, set default operand (gaining two bytes)
                dbl = EOF;
            }
        } else {
            inp = EOF; // only needed when switching between MOD and INS
            dbl = EOF;
        }

        match operand {
            MOD => {
                // insert data from the patch file into the output file while also advancing the source file
                let (next_operand, bytes_read) = get_data(
                    &mut file_patch,
                    &mut file_out,
                    position_original,
                    position_out,
                    operand,
                    inp,
                    dbl,
                )?;
                debug!("{} {} MOD {}", position_original, position_out, bytes_read);
                position_original += bytes_read;
                position_out += bytes_read;
                operand = next_operand.unwrap_or(EOF);
            }
            INS => {
                // insert data from the patch file at the current position in the output file
                let (next_operand, bytes_read) = get_data(
                    &mut file_patch,
                    &mut file_out,
                    position_original,
                    position_out,
                    operand,
                    inp,
                    dbl,
                )?;
                debug!("{} {} INS {}", position_original, position_out, bytes_read);
                position_out += bytes_read;
                operand = next_operand.unwrap_or(EOF);
            }
            DEL => {
                // skip the next <len> bytes in the source file
                let len = get_int(&mut file_patch)?;
                debug!("{} {} DEL {}", position_original, position_out, len);
                position_original += len;
                operand = READ_NEXT_OPERAND;
            }
            EQL => {
                /* copy the next <len> bytes from the source file to the output file */
                let len = get_int(&mut file_patch)?;
                debug!("{} {} EQL {}", position_original, position_out, len);
                file_out.copyfrom(&mut file_original, position_original, len)?;
                position_original += len;
                position_out += len;
                operand = READ_NEXT_OPERAND;
            }
            BKT => {
                // go back <offset> bytes in the source file
                let offset = get_int(&mut file_patch)?;
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
    } /* while ! EOF */

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
        let mut jstream = JStream::new(&mut cursor);
        let result = get_int(&mut jstream)?;
        Ok(assert_eq!(result, 2))
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
    fn patch_empty_file_and_patch_invalid() {
        setup();
        let in_data = vec![];
        let patch_data = vec![ESC as u8];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        let result = patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor);

        assert_eq!(result.is_err(), true);
        assert_eq!(result.unwrap_err().to_string(), "ESC at end of file");
    }

    #[test]
    fn patch_delete_1_byte() -> io::Result<()> {
        setup();
        let in_data = vec![1];
        let patch_data = vec![ESC as u8, DEL as u8, 1];
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
        let patch_data = vec![ESC as u8, EQL as u8, 2];
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
            ESC as u8, EQL as u8, 0,
            ESC as u8, BKT as u8, 0,
            ESC as u8, EQL as u8, 0,
            ESC as u8, BKT as u8, 0,
            ESC as u8, EQL as u8, 0,
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
            ESC as u8, EQL as u8, 2,
            ESC as u8, BKT as u8, 2,
            ESC as u8, EQL as u8, 2,
            ESC as u8, BKT as u8, 2,
            ESC as u8, EQL as u8, 2,
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
        ESC as u8, INS as u8, 1, 2, ESC as u8, ESC as u8, 3
    ];
        let out_data = vec![];

        let mut in_cursor = io::Cursor::new(in_data);
        let mut patch_cursor = io::Cursor::new(patch_data);
        let mut out_cursor = io::Cursor::new(out_data);

        patch(&mut in_cursor, &mut patch_cursor, &mut out_cursor)?;

        let out_data = out_cursor.into_inner();
        let expected_data = [1, 2, ESC as u8, 3];
        Ok(assert_eq!(out_data, expected_data))
    }

    #[test]
    fn patch_overwrite_middle_part() -> io::Result<()> {
        setup();
        let in_data = vec![1, 2, 3, 4, 5, 6, 7];
        #[rustfmt::skip]
        let patch_data = vec![
            ESC as u8, EQL as u8, 1,
            ESC as u8, MOD as u8, 0, 0, 0,
            ESC as u8, EQL as u8, 1,
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
}
