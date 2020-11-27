(*
    Copyright (c) 2017 Chris Cannam

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    Except as contained in this notice, the name of Chris Cannam shall
    not be used in advertising or otherwise to promote the sale, use
    or other dealings in this Software without prior written
    authorization.
*)

signature AUDIO_FILE_READER = sig

    type t
    type vec
    datatype 'a result = ERROR of string | OK of 'a

    val extensionsSupported : string list

    (** Open an audio file and prepare to read from the first sample frame *)
    val openFile : string -> t result

    (** Return the number of audio channels in the file *)
    val channels : t -> int

    (** Return the audio sample rate of the file *)
    val rate : t -> int

    (** Return the expected number of frames in the file *)
    val frameCount : t -> int
            
    (** Seek to a position in the file, given as an audio sample frame
        count. Modifies internal state in t *)
    val seekTo : t * Position.int -> unit result

    (** Read a number of audio sample frames from the file, and return
        them interleaved in a single vector. Modifies internal state in t *)
    val readInterleaved : t * int -> vec

    (** Close an audio file. Modifies internal state in t, which
        cannot be used subsequently *)
    val close : t -> unit

end

structure WaveReader :>
          AUDIO_FILE_READER
              where type vec = RealVector.vector = struct

    type state = {
        buffer : Word8Vector.vector ref,
        buffer_index : int ref
    }
    type t = {
        channels : int,
        rate : int,
        frame_count : int,
        bitdepth : int,
        startpos : Position.int,
        stream : BinIO.instream,
        state : state,
        read_sample : unit -> real option
    }
    type vec = RealVector.vector
    datatype 'a result = ERROR of string | OK of 'a
                                                     
    val extensionsSupported = [ "wav" ]

    fun seek (stream, pos) =
        case BinIO.StreamIO.getReader (BinIO.getInstream stream) of
            (reader as BinPrimIO.RD { setPos = SOME f, ... }, _) =>
            ( f pos;
              BinIO.setInstream
                  (stream,
                   BinIO.StreamIO.mkInstream (reader, Word8Vector.fromList [])))
          | (BinPrimIO.RD { name, ... }, _) =>
            raise IO.Io {
                name = name,
                function = "seek",
                cause = IO.RandomAccessNotSupported
            }

    fun tell stream =
        BinIO.StreamIO.filePosIn (BinIO.getInstream stream)
              
    fun read_exact (stream, n) =
        (* read N bytes and return them; fail, returning NONE, if
           fewer than N available *)
        let val v = BinIO.inputN (stream, n)
        in
            if Word8Vector.length v = n
            then SOME (List.tabulate (n, fn i => Word8Vector.sub (v, i)))
            else NONE
        end
            
    fun skip (stream, n) =
        ignore (BinIO.inputN (stream, n))

    fun bytes_to_unsigned [] = 0
      | bytes_to_unsigned (b::bs) =
        (Word8.toInt b) + (256 * bytes_to_unsigned bs)

    fun read_mandatory_number stream bytes =
        case read_exact (stream, bytes) of
            SOME bb => bytes_to_unsigned bb
          | NONE => raise Fail "Failed to read number value"

    fun fill_buffer (stream, state : state) =
        let val buffer_size = 90000  (* divisible by all of our sample widths *)
            val v = BinIO.inputN (stream, buffer_size)
        in
            #buffer state := v;
            #buffer_index state := 0
        end

    fun clear_buffer (state : state) =
        (#buffer state := Word8Vector.fromList [];
         #buffer_index state := 0)
            
    fun have_enough (state : state, n) =
        Word8Vector.length (! (#buffer state)) >= !(#buffer_index state) + n

    fun fill_maybe (stream, state, n) =
        if have_enough (state, n)
        then ()
        else fill_buffer (stream, state)

    fun read_buffered (stream, state, n) =
        let val _ = fill_maybe (stream, state, n)
        in
            if not (have_enough (state, n))
            then NONE
            else
                let val bb =
                        List.tabulate
                            (n, fn i => Word8Vector.sub
                                            (!(#buffer state),
                                             !(#buffer_index state) + i))
                in
                    #buffer_index state := !(#buffer_index state) + n;
                    SOME bb
                end
        end

    val widen = Word32.fromLargeWord o Word8.toLargeWord

    fun bytes_to_signed16 (b0, b1) = (* lower first, wav is little-endian *)
        let val b10 = Word32.orb (widen b0, Word32.<< (widen b1, 0w8))
        in
            if (Word8.andb (b1, 0wx80) = 0wx80) (* -ve *)
            then Word32.toIntX (Word32.orb (0wxffff0000, b10))
            else Word32.toIntX b10
        end
            
    fun bytes_to_signed24 (b0, b1, b2) =
        (*!!! test *)
        let val b210 = Word32.orb (widen b0,
                                   Word32.orb (Word32.<< (widen b1, 0w8),
                                               Word32.<< (widen b2, 0w16)))
        in
            if (Word8.andb (b2, 0wx80) = 0wx80) (* -ve *)
            then Word32.toIntX (Word32.orb (0wxff000000, b210))
            else Word32.toIntX b210
        end

    fun read_sample_u8 (stream, state) =
        case read_buffered (stream, state, 1) of
            SOME [b] =>
            SOME ((Real.fromInt ((Word8.toInt b) - 128)) / 128.0)
          | _ => NONE

    fun read_sample_s16 (stream, state) =
        case read_buffered (stream, state, 2) of
            SOME [b0, b1] =>
            SOME ((Real.fromInt (bytes_to_signed16 (b0, b1))) / 32768.0)
          | _ => NONE

    fun read_sample_s24 (stream, state) =
        case read_buffered (stream, state, 3) of
            SOME [b0, b1, b2] =>
            SOME ((Real.fromInt (bytes_to_signed24 (b0, b1, b2))) / 8388608.0)
          | _ => NONE
               
    fun read_fmt_contents data_size stream =
        let val num = read_mandatory_number stream
            val audio_format = num 2
            val channels = num 2
            val sample_rate = num 4
            val byte_rate = num 4
            val bytes_per_sample = num 2
            val bits_per_sample = num 2
            val read_sample =
                case bits_per_sample of
                    8 => read_sample_u8
                  | 16 => read_sample_s16
                  | 24 => read_sample_s24
                  | _ => raise Fail ("Unsupported bit depth " ^
                                     (Int.toString bits_per_sample))
            val state = {
                buffer = ref (Word8Vector.fromList []),
                buffer_index = ref 0
            }
        in
            if audio_format <> 1
            then raise Fail "PCM only supported"
            else {
                    rate = sample_rate,
                    channels = channels,
                    frame_count = Int.quot (data_size,
                                            bytes_per_sample * channels),
                    bitdepth = bits_per_sample,
                    startpos = 0,
                    stream = stream,
                    state = state,
                    read_sample = fn () => read_sample (stream, state)
                }
        end
            
    fun read_tag stream =
        case read_exact (stream, 4) of
            SOME v => SOME (implode (map (Char.chr o Word8.toInt) v))
          | NONE => NONE

    fun read_expected_tag stream expected =
        case read_tag stream of
            SOME tag => if tag = expected
                        then ()
                        else raise Fail ("Unexpected chunk tag '" ^ tag ^
                                         "' (expected '" ^ expected ^ "')")
          | NONE => raise Fail "Tag expected"

    fun read_chunk_size stream =
        case read_exact (stream, 4) of
            SOME v => bytes_to_unsigned v
          | NONE => raise Fail "Chunk size expected"
                          
    fun read_expected_chunk_size stream expected =
        let val _ = read_expected_tag stream expected
        in
            read_chunk_size stream
        end

    fun skip_to_data stream =
        case read_tag stream of
            SOME tag =>
            let val size = read_chunk_size stream
            in
                if tag = "data"
                then tell stream
                else (skip (stream, size); skip_to_data stream)
            end
          | NONE => raise Fail "Tag expected"
            
    fun read_headers stream =
        let val overall_size = read_expected_chunk_size stream "RIFF"
            val _ = read_expected_tag stream "WAVE"
            val fmt_size = read_expected_chunk_size stream "fmt "
        in
            if fmt_size = 16
            then
                let val record = read_fmt_contents
                                     (overall_size - fmt_size)
                                     stream
                    val pos = skip_to_data stream
                in
                    OK {
                        rate = #rate record,
                        channels = #channels record,
                        frame_count = #frame_count record,
                        bitdepth = #bitdepth record,
                        startpos = pos,
                        stream = stream,
                        state = #state record,
                        read_sample = #read_sample record
                    }
                end
            else raise Fail ("Unexpected size for 'fmt ' chunk (" ^
                             (Int.toString fmt_size) ^ ", expected 16")
        end

    fun openFile filename =
        let val stream = BinIO.openIn filename
        in read_headers stream
        end
        handle IO.Io e =>
               ERROR ("Unable to open " ^ filename ^ 
                      " for reading: " ^ (exnMessage (#cause e)))
             | Fail msg =>
               ERROR ("Error while reading " ^ filename ^ ": " ^ msg)
                                   
    fun close (t: t) =
        BinIO.closeIn (#stream t)

    fun channels (t: t) =
        #channels t
                               
    fun rate (t: t) =
        #rate t

    fun frameCount (t: t) =
        #frame_count t
              
    fun seekTo (t : t, nframes) =
        let val bytes_per_frame = Position.fromInt
                                      (#channels t * Int.quot (#bitdepth t, 8))
            open Position
            val pos = #startpos t + nframes * bytes_per_frame
            val result =
                (seek (#stream t, pos); OK ())
                handle e => ERROR (exnMessage e)
            val _ = clear_buffer (#state t)
        in
            result
        end
              
    fun readInterleaved (t, nframes) =
        let open RealArray
            val n = nframes * channels t
            val result = array (n, 0.0)
            fun read_aux (i, 0) = i
              | read_aux (i, m) =
                case (#read_sample t) () of NONE => i
                                          | SOME s => (update (result, i, s);
                                                       read_aux (i + 1, m - 1))
            val count = read_aux (0, n)
        in
            vector (if count = n
                    then result
                    else tabulate (count, fn i => sub (result, i)))
        end

end
