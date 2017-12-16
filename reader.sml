
signature AUDIO_FILE_READER = sig
    type t
    datatype 'a result = ERROR of string | OK of 'a

    val extensionsSupported : string list

    val openFile : string -> t result
    val close : t -> unit

    val channels : t -> int
    val rate : t -> int
            
    val readInterleaved : t * int -> real vector
end

structure WaveReader :> AUDIO_FILE_READER = struct

    type t = {
        channels : int,
        rate : int, 
        bitdepth : int,
        stream : BinIO.instream,
        read_sample : unit -> real option
    }
    datatype 'a result = ERROR of string | OK of 'a
                                                     
    val extensionsSupported = [ "wav" ]

    fun bytes_to_unsigned bb =
        let fun bb_aux [] = 0
              | bb_aux (x::xs) =
                (Char.ord x + (256 * bb_aux xs))
        in
            bb_aux (String.explode (Byte.bytesToString bb))
        end

    fun bytes_to_signed16 bb =
        let val b1 = Word8Vector.sub (bb, 1) (* higher byte *)
            val b0 = Word8Vector.sub (bb, 0) (* lower, wav is little-endian *)
            val widen = Word32.fromLargeWord o Word8.toLargeWord
            val b10 = Word32.orb (widen b0, Word32.<< (widen b1, 0w8))
        in
            if (Word8.andb (b1, 0wx80) = 0wx80) (* -ve *)
            then Word32.toIntX (Word32.orb (0wxffff0000, b10))
            else Word32.toIntX b10
        end
            
    fun bytes_to_signed24 bb =
        (*!!! test *)
        let val b2 = Word8Vector.sub (bb, 2) (* highest byte *)
            val b1 = Word8Vector.sub (bb, 1) (* higher byte *)
            val b0 = Word8Vector.sub (bb, 0) (* lower, wav is little-endian *)
            val widen = Word32.fromLargeWord o Word8.toLargeWord
            val b210 = Word32.orb (widen b0,
                                   Word32.orb (Word32.<< (widen b1, 0w8),
                                               Word32.<< (widen b2, 0w16)))
        in
            if (Word8.andb (b2, 0wx80) = 0wx80) (* -ve *)
            then Word32.toIntX (Word32.orb (0wxff000000, b210))
            else Word32.toIntX b210
        end
            
    fun read1 stream =
        BinIO.input1 stream
            
    fun readN stream n =
        let val v = BinIO.inputN (stream, n)
        in
            if Word8Vector.length v = n
            then SOME v
            else NONE
        end

    fun read_mandatory_number stream bytes =
        case readN stream bytes of
            SOME v => bytes_to_unsigned v
          | NONE => raise Fail "Failed to read number value"
            
    fun skipN stream n =
        ignore (BinIO.inputN (stream, n))

    fun read_sample_u8 stream =
        case read1 stream of
            SOME b => SOME ((Real.fromInt ((Word8.toInt b) - 128)) / 128.0)
          | NONE => NONE

    fun read_sample_s16 stream =
        case readN stream 2 of
            SOME bb => SOME ((Real.fromInt (bytes_to_signed16 bb)) / 32768.0)
          | NONE => NONE

    fun read_sample_s24 stream =
        case readN stream 3 of
            SOME bb => SOME ((Real.fromInt (bytes_to_signed24 bb)) / 8388608.0)
          | NONE => NONE
               
    fun read_fmt_contents stream =
        let val n = read_mandatory_number stream
            val audio_format = n 2
            val channels = n 2
            val sample_rate = n 4
            val byte_rate = n 4
            val bytes_per_sample = n 2
            val bits_per_sample = n 2
            val read_sample =
                case bits_per_sample of
                    8 => read_sample_u8
                  | 16 => read_sample_s16
                  | 24 => read_sample_s24
                  | _ => raise Fail ("Unsupported bit depth " ^
                                     (Int.toString bits_per_sample))
        in
            if audio_format <> 1
            then raise Fail "PCM only supported"
            else OK {
                    rate = sample_rate,
                    channels = channels,
                    bitdepth = bits_per_sample,
                    stream = stream,
                    read_sample = fn () => read_sample stream
                }
        end
            
    fun read_tag stream =
        case readN stream 4 of
            SOME v => SOME (Byte.bytesToString v)
          | NONE => NONE

    fun read_expected_tag stream expected =
        case read_tag stream of
            SOME tag => if tag = expected
                        then ()
                        else raise Fail ("Unexpected chunk tag '" ^ tag ^
                                         "' (expected '" ^ expected ^ "')")
          | NONE => raise Fail "Tag expected"

    fun read_chunk_size stream =
        case readN stream 4 of
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
                then size
                else (skipN stream size; skip_to_data stream)
            end
          | NONE => raise Fail "Tag expected"
            
    fun read_headers stream =
        let val overall_size = read_expected_chunk_size stream "RIFF"
            val _ = read_expected_tag stream "WAVE"
            val fmt_size = read_expected_chunk_size stream "fmt "
        in
            if fmt_size = 16
            then
                let val record = read_fmt_contents stream
                    val _ = skip_to_data stream
                in
                    record
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

    fun readInterleaved (t, nframes) =
        let open Array
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
