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

signature AUDIO_FILE_WRITER = sig

    type t
    datatype 'a result = ERROR of string | OK of 'a

    val extensionsSupported : string list

    (** Open an audio file for writing, with the given channel count
        and sample rate *)
    val openFile : { filename: string, channels: int, rate: int } -> t result

    (** Write a number of audio sample frames to the file. Modifies
        internal state in t *)
    val writeInterleaved : t * real vector -> unit

    (** Close an audio file. Modifies internal state in t, which
        cannot be used subsequently *)
    val close : t -> unit

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
            val n = if n < ~32768 then ~32768
                    else if n > 32767 then 32767
                    else n
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
        case BinIO.StreamIO.getWriter (BinIO.getOutstream stream) of
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
                                                
