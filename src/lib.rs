use log::debug;
use std::io;
use std::io::{Read, Seek, Write};

const EOF: i32 = -1;

/// 167 Escape
const ESC: u8 = 0xA7;
#[deprecated(note = "Use ESC instead")]
const ESC_I32: i32 = 0xA7;
/// 166 Modify
const MOD: u8 = 0xA6;
#[deprecated(note = "Use MOD instead")]
const MOD_I32: i32 = 0xA6;
/// 165 Insert
const INS: u8 = 0xA5;
#[deprecated(note = "Use INS instead")]
const INS_I32: i32 = 0xA5;
/// 164 Delete
const DEL: u8 = 0xA4;
#[deprecated(note = "Use DEL instead")]
const DEL_I32: i32 = 0xA4;
/// 163 Equal
const EQL: u8 = 0xA3;
#[deprecated(note = "Use EQL instead")]
const EQL_I32: i32 = 0xA3;
/// 162 Backtrace
const BKT: u8 = 0xA2;
#[deprecated(note = "Use BKT instead")]
const BKT_I32: i32 = 0xA2;

// read next operand from input
const READ_NEXT_OPERAND: i32 = 0;

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
    /// return the byte read, or EOF if end of stream
    fn get(&mut self) -> io::Result<Option<u8>> {
        let mut byte_buf = [0; 1];
        let n = self.stream.read(&mut byte_buf)?;
        let result = if n == 0 { None } else { Some(byte_buf[0]) };
        Ok(result)
    }

    fn get_expect(&mut self) -> io::Result<u8> {
        let mut byte_buf = [0; 1];
        let n = self.stream.read(&mut byte_buf)?;
        if n == 0 {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "Unexpected end of stream",
            ))
        } else {
            Ok(byte_buf[0])
        }
    }

    /// Copy a series of bytes from input to output.
    /// * `source`    Input stream
    /// * `pos`       Position to copy from
    /// * `len`       Number of bytes to copy
    fn copyfrom<U: Read + Seek + Write>(
        &mut self,
        source: &mut JStream<U>,
        pos: Offset,
        len: Offset,
    ) -> io::Result<()> {
        let mut buf = vec![0; len as usize];
        source.stream.seek(std::io::SeekFrom::Start(pos))?;
        source.stream.read_exact(&mut buf)?;
        self.stream.write_all(&buf)?;
        Ok(())
    }

    /// Write a byte to the output.
    /// * `byte`   data to write
    fn put_byte(&mut self, byte: u8) -> io::Result<()> {
        self.stream.write_all(&[byte])
    }
}

/// Get an offset/len from the input stream
fn get_int<T: Read + Seek + Write>(input_steam: &mut JStream<T>) -> io::Result<Offset> {
    let first_byte = input_steam.get_expect()?;
    if first_byte < 252 {
        Ok(first_byte as Offset + 1)
    } else if first_byte == 252 {
        Ok(253 + input_steam.get_expect()? as Offset)
    } else if first_byte == 253 {
        let mut val = input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        Ok(val)
    } else if first_byte == 254 {
        let mut val = input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        Ok(val)
    } else {
        // #ifdef JDIFF_LARGEFILE
        let mut val = input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        val = (val << 8) + input_steam.get_expect()? as Offset;
        Ok(val)
        // #else
        // fprintf(stderr, "64-bit length numbers not supported!\n");
        // return EXI_LRG;
        // #endif
    }
}

/// Put one byte of output data
///
/// * `stream_out` output stream
/// * `position_original` position on source stream
/// * `position_out` position on output stream
/// * `operand` MOD or INS
/// * `data` output byte
/// * `offset` offset
///
/// returns 1
fn put_data<T: Read + Write + Seek>(
    stream_out: &mut JStream<T>,
    position_original: Offset,
    position_out: Offset,
    operand: u8,
    data: u8,
    offset: Offset,
) -> io::Result<Offset> {
    stream_out.put_byte(data)?;
    debug!(
        "put_data {} {} {} {} {}",
        position_original + (if operand == MOD { offset } else { 0 }),
        position_out + offset,
        operand_name(operand),
        data,
        render_ascii(data)
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
/// * `patch_stream`         input stream
/// * `out_stream`           output stream
/// * `position_original`    position on source stream
/// * `position_out`         position on output stream
/// * `operand`              INS or MOD
/// * `pending`              First pending byte (EOF = no pending byte)
/// * `dbl`                  Second pending byte (EOF = no pending byte)
///
/// returns the next operand and the bytes read
fn copy_data<T: Read + Write + Seek>(
    patch_stream: &mut JStream<T>,
    out_stream: &mut JStream<T>,
    position_original: Offset,
    position_out: Offset,
    operand: i32,
    pending: i32,
    pending2: i32,
) -> io::Result<(Option<Operand>, Offset)> {
    let mut bytes_read: Offset = 0;

    /* First, output the pending bytes:
       pending  pending2     Output
        ESC      ESC          ESC ESC
        ESC      xxx          ESC xxx
        xxx      EOF          xxx
    */
    if pending != EOF {
        bytes_read += put_data(
            out_stream,
            position_original,
            position_out,
            operand as u8,
            pending as u8,
            bytes_read,
        )?;
        if pending == ESC as i32 && pending2 != ESC as i32 {
            bytes_read += put_data(
                out_stream,
                position_original,
                position_out,
                operand as u8,
                pending2 as u8,
                bytes_read,
            )?;
        }
    }

    /* Read loop */
    while let Some(mut input) = patch_stream.get()? {
        // Handle ESC-code
        if input == ESC as u8 {
            let next_operand = patch_stream.get()?.map(|b| b as i32).unwrap_or(EOF);
            match next_operand {
                DEL_I32 | EQL_I32 | BKT_I32 | MOD_I32 | INS_I32 => (),
                ESC_I32 => {
                    // Double ESC: drop one
                    debug!(
                        "{} {} ESC ESC",
                        position_original + (if operand == MOD_I32 { bytes_read } else { 0 }),
                        position_out + bytes_read
                    );

                    // Write the single ESC and continue
                    bytes_read += put_data(
                        out_stream,
                        position_original,
                        position_out,
                        operand as u8,
                        input,
                        bytes_read,
                    )?;
                    continue;
                }
                _ => {
                    // ESC <xxx> with <xxx> not an opcode: output as they are
                    debug!(
                        "{} {} ESC XXX",
                        position_original + (if operand == MOD_I32 { bytes_read } else { 0 }),
                        position_out + bytes_read
                    );

                    // Write the escape, the <xxx> and continue
                    bytes_read += put_data(
                        out_stream,
                        position_original,
                        position_out,
                        operand as u8,
                        input,
                        bytes_read,
                    )?;
                    bytes_read += put_data(
                        out_stream,
                        position_original,
                        position_out,
                        operand as u8,
                        next_operand as u8,
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
                    position_original + (if operand == MOD_I32 { bytes_read } else { 0 }),
                    position_out + bytes_read,
                    next_operand
                );

                bytes_read += put_data(
                    out_stream,
                    position_original,
                    position_out,
                    operand as u8,
                    ESC as u8,
                    bytes_read,
                )?;
                input = next_operand as u8; // will be output below
            } else {
                return Ok((Some(next_operand), bytes_read));
            }
        }

        // Handle data
        bytes_read += put_data(
            out_stream,
            position_original,
            position_out,
            operand as u8,
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
    // Current operand
    let mut operand;
    // Position in source stream
    let mut position_original: Offset = 0;
    // Position in destination stream
    let mut position_out: Offset = 0;

    // wrapped streams
    let mut stream_original = JStream::new(in_stream);
    let mut stream_patch = JStream::new(patch_stream);
    let mut stream_out = JStream::new(out_stream);

    operand = READ_NEXT_OPERAND; // no operand
    while operand != EOF {
        // 1st Pending byte (EOF = no pending bye)
        let mut inp: i32;
        // 2nd Pending byte (EOF = no pending bye)
        let mut dbl: i32;
        // Read operand from input, unless this has already been done
        if operand == READ_NEXT_OPERAND {
            inp = stream_patch.get()?.map(|b| b as i32).unwrap_or(EOF);
            if inp == EOF {
                break;
            }

            // Handle ESC <opr>
            if inp == ESC as i32 {
                dbl = stream_patch.get()?.map(|b| b as i32).unwrap_or(EOF);
                match dbl {
                    EQL_I32 | DEL_I32 | BKT_I32 | MOD_I32 | INS_I32 => {
                        // new operand found, all ok !
                        operand = dbl;
                        dbl = EOF;
                        inp = EOF;
                    }
                    EOF => {
                        // serious error, let's call this a trailing byte
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "ESC at end of stream",
                        ));
                    }
                    _ => {
                        // ESC xxx or ESC ESC at the start of a sequence
                        // Resolve by double pending bytes: liInp and liDbl
                        operand = MOD_I32;
                    }
                }
            } else {
                operand = MOD_I32; // If an ESC <opr> is missing, set default operand (gaining two bytes)
                dbl = EOF;
            }
        } else {
            inp = EOF; // only needed when switching between MOD and INS
            dbl = EOF;
        }

        match operand {
            MOD_I32 => {
                // insert data from the patch file into the output file while also advancing the source file
                let (next_operand, bytes_read) = copy_data(
                    &mut stream_patch,
                    &mut stream_out,
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
            INS_I32 => {
                // insert data from the patch file at the current position in the output file
                let (next_operand, bytes_read) = copy_data(
                    &mut stream_patch,
                    &mut stream_out,
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
            DEL_I32 => {
                // skip the next <len> bytes in the source file
                let len = get_int(&mut stream_patch)?;
                debug!("{} {} DEL {}", position_original, position_out, len);
                position_original += len;
                operand = READ_NEXT_OPERAND;
            }
            EQL_I32 => {
                /* copy the next <len> bytes from the source file to the output file */
                let len = get_int(&mut stream_patch)?;
                debug!("{} {} EQL {}", position_original, position_out, len);
                stream_out.copyfrom(&mut stream_original, position_original, len)?;
                position_original += len;
                position_out += len;
                operand = READ_NEXT_OPERAND;
            }
            BKT_I32 => {
                // go back <offset> bytes in the source file
                let offset = get_int(&mut stream_patch)?;
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
    fn get_int_2_bytes() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![252, 1]);
        let mut jstream = JStream::new(&mut cursor);
        let result = get_int(&mut jstream)?;
        Ok(assert_eq!(result, 254))
    }

    #[test]
    fn get_int_3_bytes() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![253, 1, 2]);
        let mut jstream = JStream::new(&mut cursor);
        let result = get_int(&mut jstream)?;
        Ok(assert_eq!(result, 258))
    }

    #[test]
    fn get_int_4_bytes() -> io::Result<()> {
        let mut cursor = io::Cursor::new(vec![254, 1, 0, 0, 0]);
        let mut jstream = JStream::new(&mut cursor);
        let result = get_int(&mut jstream)?;
        Ok(assert_eq!(result, 1 << 24))
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
        assert_eq!(result.unwrap_err().to_string(), "ESC at end of stream");
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
            ESC as u8, EQL as u8, 6,
            ESC as u8, MOD as u8, b'U', b'n', b'i', b'v', b'e',
            ESC as u8, INS as u8, b'r', b's', b'e',
            ESC as u8, EQL as u8, 0,
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
