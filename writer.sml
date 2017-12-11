signature AUDIO_FILE_WRITER = sig

    type t
    datatype 'a result = ERROR of string | OK of 'a

    val extensionsSupported : string list

    val openFile : { filename: string, channels: int, rate: int } -> t result
    val close : t -> unit

    val writeInterleaved : t * real vector -> unit

end

structure WaveWriter :> AUDIO_FILE_WRITER = struct

    type t = {
        stream : BinIO.outstream,
        write_sample : real -> unit,
        written : int ref
    }
    datatype 'a result = ERROR of string | OK of 'a
                 
    val bitdepth = 16  (* our only supported format *)
                       
    val extensionsSupported = [ "wav" ]

    fun write_tag stream tag =
        BinIO.output (stream, Byte.stringToBytes tag)

    fun unsigned_to_bytes (length, x) =
        let fun bb_aux (0, _) = []
              | bb_aux (n, x) =
                (Char.chr (Int.mod (x, 256)) :: bb_aux (n-1, Int.quot (x, 256)))
        in
            Byte.stringToBytes (String.implode (bb_aux (length, x)))
        end

    fun write_unsigned stream (length, x) =
        BinIO.output (stream, unsigned_to_bytes (length, x))
            
    fun write_headers (stream, channels, rate) =
        let val tag = write_tag stream
            val num = write_unsigned stream
            fun num2 x = num (2, x)
            fun num4 x = num (4, x)
        in
            (*!!! todo: check if channels, rate etc are out of range *)
            tag "RIFF";
            num4 0;  (* total length - to be filled in *)
            tag "WAVE";
            tag "fmt ";
            num4 16; (* fmt chunk length - fixed *)
            num2 1;  (* pcm format *)
            num2 channels;
            num4 rate;
            num4 (Int.quot(rate * channels * bitdepth, 8));
            num2 (Int.quot(channels * bitdepth, 8));
            num2 bitdepth;
            tag "data";
            num4 0   (* data length - to be filled in *)
        end

    fun write_sample_s16 stream value =
        let val n = Real.round (value * 32767.0)
            val n = if n < ~32768 then ~32768 else if n > 32767 then 32767 else n
            val w = LargeWord.fromInt n
            val b0 = Word8.fromLargeWord
                         (LargeWord.andb (w, 0wxff))
            val b1 = Word8.fromLargeWord
                         (LargeWord.>> (LargeWord.andb (w, 0wxff00), 0w8))
        in
            BinIO.output1 (stream, b0);
            BinIO.output1 (stream, b1)
        end
              
    fun openFile { filename, channels, rate } =
        let val stream = BinIO.openOut filename
            val written = ref 0
            fun write_sample s =
                (write_sample_s16 stream s; written := !written + 1)
        in
            write_headers (stream, channels, rate);
            OK {
                stream = stream,
                written = written,
                write_sample = write_sample
            }
        end
        handle IO.Io e =>
               ERROR ("Unable to open " ^ filename ^ 
                      " for writing: " ^ (exnMessage (#cause e)))
             | Fail msg =>
               ERROR ("Error while writing " ^ filename ^ ": " ^ msg)

    fun seek (stream, pos) =
        case BinIO.StreamIO.getWriter(BinIO.getOutstream stream) of
            (writer as BinPrimIO.WR { setPos = SOME f, ... }, bufmode) =>
            (f pos;
             BinIO.setOutstream(stream,
                                BinIO.StreamIO.mkOutstream(writer, bufmode)))
          | (BinPrimIO.WR { name, ... }, _) =>
            raise IO.Io {
                name = name,
                function = "seek",
                cause = IO.RandomAccessNotSupported
            }

    fun write_data_length stream n =
        let val bytes_per_sample = Int.quot(bitdepth, 8)
        in
            seek (stream, 40);
            write_unsigned stream (4, n * bytes_per_sample);
            seek (stream, 4);
            write_unsigned stream (4, n * bytes_per_sample + 36)
        end

    fun writeInterleaved (t: t, v) =
        Vector.app (#write_sample t) v
                           
    fun close (t: t) =
        let val stream = #stream t
        in
            write_data_length stream (! (#written t));
            BinIO.closeOut stream
        end
                                                         
end
                                                