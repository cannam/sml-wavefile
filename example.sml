
fun copyData (instr, outstr) =
    let val blockSize = 10240
        val frames = WaveReader.readInterleaved (instr, blockSize)
        val expected = blockSize * WaveReader.channels instr
    in
        WaveWriter.writeInterleaved (outstr, frames);
        if RealVector.length frames < expected
        then ()
        else copyData (instr, outstr)
    end

fun convertFile (infile, outfile) =
    case WaveReader.openFile infile of
        WaveReader.ERROR e => raise Fail e
      | WaveReader.OK instr =>
        case WaveWriter.openFile {
                filename = outfile,
                channels = WaveReader.channels instr,
                rate = WaveReader.rate instr
            } of
            WaveWriter.ERROR e => (WaveReader.close instr;
                                   raise Fail e)
          | WaveWriter.OK outstr => (copyData (instr, outstr);
                                     WaveReader.close instr;
                                     WaveWriter.close outstr)

fun usage () =
    (print "Usage: example <infile> <outfile>\n";
     raise Fail "Incorrect arguments specified")
                                        
fun handleArgs args =
    case args of
        [infile, outfile] => convertFile (infile, outfile)
      | _ => usage ()
           
fun main () =
    handleArgs (CommandLine.arguments ())
    handle Fail msg =>
           (TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n");
            OS.Process.exit OS.Process.failure)

