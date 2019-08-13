# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Printf

@testset "%p" begin

    # pointers
    if Sys.WORD_SIZE == 64
        @test (Prints.@sprintf "%20p" 0) == "  0x0000000000000000"
        @test (Prints.@sprintf "%-20p" 0) == "0x0000000000000000  "
    elseif Sys.WORD_SIZE == 32
        @test (Prints.@sprintf "%20p" 0) == "          0x00000000"
        @test (Prints.@sprintf "%-20p" 0) == "0x00000000          "
    end

end

@testset "%a" begin

    # hex float
    @test (Prints.@sprintf "%a" 1.5) == "0x1.8p+0"
    @test (Prints.@sprintf "%a" 1.5f0) == "0x1.8p+0"
    @test (Prints.@sprintf "%a" big"1.5") == "0x1.8p+0"
    @test (Prints.@sprintf "%#.0a" 1.5) == "0x2.p+0"
    @test (Prints.@sprintf "%+30a" 1/3) == "         +0x1.5555555555555p-2"

    @test Prints.@sprintf("%a", 1.5) == "0x1.8p+0"
    @test Prints.@sprintf("%a", 3.14) == "0x1.91eb851eb851fp+1"
    @test Prints.@sprintf("%.0a", 3.14) == "0x2p+1"
    @test Prints.@sprintf("%.1a", 3.14) == "0x1.9p+1"
    @test Prints.@sprintf("%.2a", 3.14) == "0x1.92p+1"
    @test Prints.@sprintf("%#a", 3.14) == "0x1.91eb851eb851fp+1"
    @test Prints.@sprintf("%#.0a", 3.14) == "0x2.p+1"
    @test Prints.@sprintf("%#.1a", 3.14) == "0x1.9p+1"
    @test Prints.@sprintf("%#.2a", 3.14) == "0x1.92p+1"
    @test Prints.@sprintf("%.6a", 1.5) == "0x1.800000p+0"

end

@testset "%g" begin

    # %g
    for (val, res) in ((12345678., "1.23457e+07"),
                    (1234567.8, "1.23457e+06"),
                    (123456.78, "123457"),
                    (12345.678, "12345.7"),
                    (12340000.0, "1.234e+07"))
        @test (Prints.@sprintf("%.6g", val) == res)
    end
    for (val, res) in ((big"12345678.", "1.23457e+07"),
                    (big"1234567.8", "1.23457e+06"),
                    (big"123456.78", "123457"),
                    (big"12345.678", "12345.7"))
        @test (Prints.@sprintf("%.6g", val) == res)
    end
    for (fmt, val) in (("%10.5g", "     123.4"),
                    ("%+10.5g", "    +123.4"),
                    ("% 10.5g","     123.4"),
                    ("%#10.5g", "    123.40"),
                    ("%-10.5g", "123.4     "),
                    ("%-+10.5g", "+123.4    "),
                    ("%010.5g", "00000123.4")),
        num in (123.4, big"123.4")
        @test Prints.format(Prints.Format(fmt), num) == val
    end
    @test( Prints.@sprintf( "%10.5g", -123.4 ) == "    -123.4")
    @test( Prints.@sprintf( "%010.5g", -123.4 ) == "-0000123.4")
    @test( Prints.@sprintf( "%.6g", 12340000.0 ) == "1.234e+07")
    @test( Prints.@sprintf( "%#.6g", 12340000.0 ) == "1.23400e+07")
    @test( Prints.@sprintf( "%10.5g", big"-123.4" ) == "    -123.4")
    @test( Prints.@sprintf( "%010.5g", big"-123.4" ) == "-0000123.4")
    @test( Prints.@sprintf( "%.6g", big"12340000.0" ) == "1.234e+07")
    @test( Prints.@sprintf( "%#.6g", big"12340000.0") == "1.23400e+07")

    # %g regression gh #14331
    @test( Prints.@sprintf( "%.5g", 42) == "42")
    @test( Prints.@sprintf( "%#.2g", 42) == "42.")
    @test( Prints.@sprintf( "%#.5g", 42) == "42.000")

    @test Prints.@sprintf("%g", 0.00012) == "0.00012"
    @test Prints.@sprintf("%g", 0.000012) == "1.2e-05"
    @test Prints.@sprintf("%g", 123456.7) == "123457"
    @test Prints.@sprintf("%g", 1234567.8) == "1.23457e+06"

end

@testset "%f" begin

    # Inf / NaN handling
    @test (Prints.@sprintf "%f" Inf) == "Inf"
    @test (Prints.@sprintf "%+f" Inf) == "+Inf"
    @test (Prints.@sprintf "% f" Inf) == " Inf"
    @test (Prints.@sprintf "% #f" Inf) == " Inf"
    @test (Prints.@sprintf "%f" -Inf) == "-Inf"
    @test (Prints.@sprintf "%+f" -Inf) == "-Inf"
    @test (Prints.@sprintf "%f" NaN) == "NaN"
    @test (Prints.@sprintf "%+f" NaN) == "NaN"
    @test (Prints.@sprintf "% f" NaN) == "NaN"
    @test (Prints.@sprintf "% #f" NaN) == "NaN"
    @test (Prints.@sprintf "%e" big"Inf") == "Inf"
    @test (Prints.@sprintf "%e" big"NaN") == "NaN"

    @test (Prints.@sprintf "%.0f" 3e142) == "29999999999999997463140672961703247153805615792184250659629251954072073858354858644285983761764971823910371920726635399393477049701891710124032"

    @test Prints.@sprintf("%f", 1.234) == "1.234000"
    @test Prints.@sprintf("%F", 1.234) == "1.234000"
    @test Prints.@sprintf("%+f", 1.234) == "+1.234000"
    @test Prints.@sprintf("% f", 1.234) == " 1.234000"
    @test Prints.@sprintf("%f", -1.234) == "-1.234000"
    @test Prints.@sprintf("%+f", -1.234) == "-1.234000"
    @test Prints.@sprintf("% f", -1.234) == "-1.234000"
    @test Prints.@sprintf("%#f", 1.234) == "1.234000"
    @test Prints.@sprintf("%.2f", 1.234) == "1.23"
    @test Prints.@sprintf("%.2f", 1.235) == "1.24"
    @test Prints.@sprintf("%.2f", 0.235) == "0.23"
    @test Prints.@sprintf("%4.1f", 1.234) == " 1.2"
    @test Prints.@sprintf("%8.1f", 1.234) == "     1.2"
    @test Prints.@sprintf("%+8.1f", 1.234) == "    +1.2"
    @test Prints.@sprintf("% 8.1f", 1.234) == "     1.2"
    @test Prints.@sprintf("% 7.1f", 1.234) == "    1.2"
    @test Prints.@sprintf("% 08.1f", 1.234) == " 00001.2"
    @test Prints.@sprintf("%08.1f", 1.234) == "000001.2"
    @test Prints.@sprintf("%-08.1f", 1.234) == "1.2     "
    @test Prints.@sprintf("%-8.1f", 1.234) == "1.2     "
    @test Prints.@sprintf("%08.1f", -1.234) == "-00001.2"
    @test Prints.@sprintf("%09.1f", -1.234) == "-000001.2"
    @test Prints.@sprintf("%09.1f", 1.234) == "0000001.2"
    @test Prints.@sprintf("%+09.1f", 1.234) == "+000001.2"
    @test Prints.@sprintf("% 09.1f", 1.234) == " 000001.2"
    @test Prints.@sprintf("%+ 09.1f", 1.234) == "+000001.2"
    @test Prints.@sprintf("%+ 09.1f", 1.234) == "+000001.2"
    @test Prints.@sprintf("%+ 09.0f", 1.234) == "+00000001"
    @test Prints.@sprintf("%+ #09.0f", 1.234) == "+0000001."
end

@testset "%e" begin

    # Inf / NaN handling
    @test (Prints.@sprintf "%e" Inf) == "Inf"
    @test (Prints.@sprintf "%+e" Inf) == "+Inf"
    @test (Prints.@sprintf "% e" Inf) == " Inf"
    @test (Prints.@sprintf "% #e" Inf) == " Inf"
    @test (Prints.@sprintf "%e" -Inf) == "-Inf"
    @test (Prints.@sprintf "%+e" -Inf) == "-Inf"
    @test (Prints.@sprintf "%e" NaN) == "NaN"
    @test (Prints.@sprintf "%+e" NaN) == "NaN"
    @test (Prints.@sprintf "% e" NaN) == "NaN"
    @test (Prints.@sprintf "% #e" NaN) == "NaN"
    @test (Prints.@sprintf "%e" big"Inf") == "Inf"
    @test (Prints.@sprintf "%e" big"NaN") == "NaN"

    # scientific notation
    @test (Prints.@sprintf "%.0e" 3e142) == "3e+142"
    @test (Prints.@sprintf "%#.0e" 3e142) == "3.e+142"
    @test (Prints.@sprintf "%.0e" big"3e142") == "3e+142"
    @test (Prints.@sprintf "%#.0e" big"3e142") == "3.e+142"

    @test (Prints.@sprintf "%.0e" big"3e1042") == "3e+1042"

    @test (Prints.@sprintf "%e" 3e42) == "3.000000e+42"
    @test (Prints.@sprintf "%E" 3e42) == "3.000000E+42"
    @test (Prints.@sprintf "%e" 3e-42) == "3.000000e-42"
    @test (Prints.@sprintf "%E" 3e-42) == "3.000000E-42"

    @test Prints.@sprintf("%e", 1.234) == "1.234000e+00"
    @test Prints.@sprintf("%E", 1.234) == "1.234000E+00"
    @test Prints.@sprintf("%+e", 1.234) == "+1.234000e+00"
    @test Prints.@sprintf("% e", 1.234) == " 1.234000e+00"
    @test Prints.@sprintf("%e", -1.234) == "-1.234000e+00"
    @test Prints.@sprintf("%+e", -1.234) == "-1.234000e+00"
    @test Prints.@sprintf("% e", -1.234) == "-1.234000e+00"
    @test Prints.@sprintf("%#e", 1.234) == "1.234000e+00"
    @test Prints.@sprintf("%.2e", 1.234) == "1.23e+00"
    @test Prints.@sprintf("%.2e", 1.235) == "1.24e+00"
    @test Prints.@sprintf("%.2e", 0.235) == "2.35e-01"
    @test Prints.@sprintf("%4.1e", 1.234) == "1.2e+00"
    @test Prints.@sprintf("%8.1e", 1.234) == " 1.2e+00"
    @test Prints.@sprintf("%+8.1e", 1.234) == "+1.2e+00"
    @test Prints.@sprintf("% 8.1e", 1.234) == " 1.2e+00"
    @test Prints.@sprintf("% 7.1e", 1.234) == " 1.2e+00"
    @test Prints.@sprintf("% 08.1e", 1.234) == " 1.2e+00"
    @test Prints.@sprintf("%08.1e", 1.234) == "01.2e+00"
    @test Prints.@sprintf("%-08.1e", 1.234) == "1.2e+00 "
    @test Prints.@sprintf("%-8.1e", 1.234) == "1.2e+00 "
    @test Prints.@sprintf("%-8.1e", 1.234) == "1.2e+00 "
    @test Prints.@sprintf("%08.1e", -1.234) == "-1.2e+00"
    @test Prints.@sprintf("%09.1e", -1.234) == "-01.2e+00"
    @test Prints.@sprintf("%09.1e", 1.234) == "001.2e+00"
    @test Prints.@sprintf("%+09.1e", 1.234) == "+01.2e+00"
    @test Prints.@sprintf("% 09.1e", 1.234) == " 01.2e+00"
    @test Prints.@sprintf("%+ 09.1e", 1.234) == "+01.2e+00"
    @test Prints.@sprintf("%+ 09.1e", 1.234) == "+01.2e+00"
    @test Prints.@sprintf("%+ 09.0e", 1.234) == "+0001e+00"
    @test Prints.@sprintf("%+ #09.0e", 1.234) == "+001.e+00"
end

@testset "strings" begin

    @test Prints.@sprintf("Hallo heimur") == "Hallo heimur"
    @test Prints.@sprintf("+%s+", "hello") == "+hello+"
    @test Prints.@sprintf("%.1s", "foo") == "f"
    @test Prints.@sprintf("%s", "%%%%") == "%%%%"
    @test Prints.@sprintf("%s", "Hallo heimur") == "Hallo heimur"
    @test Prints.@sprintf("%+s", "Hallo heimur") == "Hallo heimur"
    @test Prints.@sprintf("% s", "Hallo heimur") == "Hallo heimur"
    @test Prints.@sprintf("%+ s", "Hallo heimur") == "Hallo heimur"
    @test Prints.@sprintf("%1s", "Hallo heimur") == "Hallo heimur"
    @test Prints.@sprintf("%20s", "Hallo") == "               Hallo"
    @test Prints.@sprintf("%-20s", "Hallo") == "Hallo               "
    @test Prints.@sprintf("%0-20s", "Hallo") == "Hallo               "
    @test Prints.@sprintf("%.20s", "Hallo heimur") == "Hallo heimur"
    @test Prints.@sprintf("%20.5s", "Hallo heimur") == "               Hallo"
    @test Prints.@sprintf("%.0s", "Hallo heimur") == ""
    @test Prints.@sprintf("%20.0s", "Hallo heimur") == "                    "
    @test Prints.@sprintf("%.s", "Hallo heimur") == ""
    @test Prints.@sprintf("%20.s", "Hallo heimur") == "                    "
    @test (Prints.@sprintf "%s" "test") == "test"
    @test (Prints.@sprintf "%s" "tést") == "tést"
    @test Prints.@sprintf("ø%sø", "hey") == "øheyø"
    @test Prints.@sprintf("%4sø", "ø") == "   øø"
    @test Prints.@sprintf("%-4sø", "ø") == "ø   ø"

    @test (Prints.@sprintf "%8s" "test") == "    test"
    @test (Prints.@sprintf "%-8s" "test") == "test    "

    @test (Prints.@sprintf "%s" :test) == "test"
    @test (Prints.@sprintf "%#s" :test) == ":test"
    @test (Prints.@sprintf "%#8s" :test) == "   :test"
    @test (Prints.@sprintf "%#-8s" :test) == ":test   "

    @test (Prints.@sprintf "%8.3s" "test") == "     tes"
    @test (Prints.@sprintf "%#8.3s" "test") == "     \"te"
    @test (Prints.@sprintf "%-8.3s" "test") == "tes     "
    @test (Prints.@sprintf "%#-8.3s" "test") == "\"te     "
    @test (Prints.@sprintf "%.3s" "test") == "tes"
    @test (Prints.@sprintf "%#.3s" "test") == "\"te"
    @test (Prints.@sprintf "%-.3s" "test") == "tes"
    @test (Prints.@sprintf "%#-.3s" "test") == "\"te"

end

@testset "chars" begin

    @test Prints.@sprintf("%c", 'a') == "a"
    @test Prints.@sprintf("%c",  32) == " "
    @test Prints.@sprintf("%c",  36) == "\$"
    @test Prints.@sprintf("%3c", 'a') == "  a"
    @test Prints.@sprintf( "%c", 'x') == "x"
    @test Prints.@sprintf("%+c", 'x') == "x"
    @test Prints.@sprintf("% c", 'x') == "x"
    @test Prints.@sprintf("%+ c", 'x') == "x"
    @test Prints.@sprintf("%1c", 'x') == "x"
    @test Prints.@sprintf("%20c"  , 'x') == "                   x"
    @test Prints.@sprintf("%-20c" , 'x') == "x                   "
    @test Prints.@sprintf("%-020c", 'x') == "x                   "
    @test Prints.@sprintf("%c", 65) == "A"
    @test Prints.@sprintf("%c", 'A') == "A"
    @test Prints.@sprintf("%3c", 'A') == "  A"
    @test Prints.@sprintf("%-3c", 'A') == "A  "
    @test Prints.@sprintf("%c", 248) == "ø"
    @test Prints.@sprintf("%c", 'ø') == "ø"
    @test Prints.@sprintf("%c", "ø") == "ø"
    @test Prints.@sprintf("%c", '𐀀') == "𐀀"

end

@testset "basics" begin

    @test Prints.@sprintf("%%") == "%"
    @test Prints.@sprintf("hey there") == "hey there"
    @test_throws ArgumentError Prints.Format("")
    @test_throws ArgumentError Prints.Format("%+")
    @test_throws ArgumentError Prints.Format("%.")
    @test_throws ArgumentError Prints.Format("%.0")
    @test isempty(Prints.Format("%%").formats)
    @test Prints.@sprintf("%d%d", 1, 2) == "12"
    @test (Prints.@sprintf "%d%d" [1 2]...) == "12"
    @test (Prints.@sprintf("X%d", 2)) == "X2"
    @test (Prints.@sprintf("\u00d0%d", 2)) == "\u00d02"
    @test (Prints.@sprintf("\u0f00%d", 2)) == "\u0f002"
    @test (Prints.@sprintf("\U0001ffff%d", 2)) == "\U0001ffff2"
    @test (Prints.@sprintf("%dX%d", 1, 2)) == "1X2"
    @test (Prints.@sprintf("%d\u00d0%d", 1, 2)) == "1\u00d02"
    @test (Prints.@sprintf("%d\u0f00%d", 1, 2)) == "1\u0f002"
    @test (Prints.@sprintf("%d\U0001ffff%d", 1, 2)) == "1\U0001ffff2"
    @test (Prints.@sprintf("%d\u2203%d\u0203", 1, 2)) == "1\u22032\u0203"
    @test_throws ArgumentError Prints.Format("%y%d")
    @test_throws ArgumentError Prints.Format("%\u00d0%d")
    @test_throws ArgumentError Prints.Format("%\u0f00%d")
    @test_throws ArgumentError Prints.Format("%\U0001ffff%d")
    @test Prints.@sprintf("%10.5d", 4) == "     00004"
    @test (Prints.@sprintf "%d" typemax(Int64)) == "9223372036854775807"

    for (fmt, val) in (("%7.2f", "   1.23"),
                   ("%-7.2f", "1.23   "),
                   ("%07.2f", "0001.23"),
                   ("%.0f", "1"),
                   ("%#.0f", "1."),
                   ("%.4e", "1.2345e+00"),
                   ("%.4E", "1.2345E+00"),
                   ("%.2a", "0x1.3cp+0"),
                   ("%.2A", "0X1.3CP+0")),
        num in (1.2345, big"1.2345")
        @test Prints.format(Prints.Format(fmt), num) == val
    end

    # reasonably complex
    @test (Prints.@sprintf "Test: %s%c%C%c%#-.0f." "t" 65 66 67 -42) == "Test: tABC-42.."

    # combo
    @test (Prints.@sprintf "%f %d %d %f" 1.0 [3 4]... 5) == "1.000000 3 4 5.000000"

    # multi
    @test (Prints.@sprintf "%s %f %9.5f %d %d %d %d%d%d%d" [1:6;]... [7,8,9,10]...) == "1 2.000000   3.00000 4 5 6 78910"

    # comprehension
    @test (Prints.@sprintf "%s %s %s %d %d %d %f %f %f" Any[10^x+y for x=1:3,y=1:3 ]...) == "11 101 1001 12 102 1002 13.000000 103.000000 1003.000000"

    # Check bug with trailing nul printing BigFloat
    @test (Prints.@sprintf("%.330f", BigFloat(1)))[end] != '\0'

    # issue #29662
    @test (Prints.@sprintf "%12.3e" pi*1e100) == "  3.142e+100"

    @test string(Prints.Format("%a").formats[1]) == "%a"
    @test string(Prints.Format("%a").formats[1]; bigfloat=true) == "%Ra"

    @test Prints.@sprintf("%d", 3.14) == "3"
    @test Prints.@sprintf("%2d", 3.14) == " 3"
    @test Prints.@sprintf("%2d", big(3.14)) == " 3"
    @test Prints.@sprintf("%s", 1) == "1"
    @test Prints.@sprintf("%f", 1) == "1.000000"
    @test Prints.@sprintf("%e", 1) == "1.000000e+00"
    @test Prints.@sprintf("%g", 1) == "1"
end

@testset "integers" begin

    @test Prints.@sprintf( "% d",  42) == " 42"
    @test Prints.@sprintf( "% d", -42) == "-42"
    @test Prints.@sprintf( "% 5d",  42) == "   42"
    @test Prints.@sprintf( "% 5d", -42) == "  -42"
    @test Prints.@sprintf( "% 15d",  42) == "             42"
    @test Prints.@sprintf( "% 15d", -42) == "            -42"
    @test Prints.@sprintf("%+d",  42) == "+42"
    @test Prints.@sprintf("%+d", -42) == "-42"
    @test Prints.@sprintf("%+5d",  42) == "  +42"
    @test Prints.@sprintf("%+5d", -42) == "  -42"
    @test Prints.@sprintf("%+15d",  42) == "            +42"
    @test Prints.@sprintf("%+15d", -42) == "            -42"
    @test Prints.@sprintf( "%0d",  42) == "42"
    @test Prints.@sprintf( "%0d", -42) == "-42"
    @test Prints.@sprintf( "%05d",  42) == "00042"
    @test Prints.@sprintf( "%05d", -42) == "-0042"
    @test Prints.@sprintf( "%015d",  42) == "000000000000042"
    @test Prints.@sprintf( "%015d", -42) == "-00000000000042"
    @test Prints.@sprintf("%-d",  42) == "42"
    @test Prints.@sprintf("%-d", -42) == "-42"
    @test Prints.@sprintf("%-5d",  42) == "42   "
    @test Prints.@sprintf("%-5d", -42) == "-42  "
    @test Prints.@sprintf("%-15d",  42) == "42             "
    @test Prints.@sprintf("%-15d", -42) == "-42            "
    @test Prints.@sprintf("%-0d",  42) == "42"
    @test Prints.@sprintf("%-0d", -42) == "-42"
    @test Prints.@sprintf("%-05d",  42) == "42   "
    @test Prints.@sprintf("%-05d", -42) == "-42  "
    @test Prints.@sprintf("%-015d",  42) == "42             "
    @test Prints.@sprintf("%-015d", -42) == "-42            "
    @test Prints.@sprintf( "%0-d",  42) == "42"
    @test Prints.@sprintf( "%0-d", -42) == "-42"
    @test Prints.@sprintf( "%0-5d",  42) == "42   "
    @test Prints.@sprintf( "%0-5d", -42) == "-42  "
    @test Prints.@sprintf( "%0-15d",  42) == "42             "
    @test Prints.@sprintf( "%0-15d", -42) == "-42            "
    @test_throws ArgumentError Prints.Format("%d %")

    @test Prints.@sprintf("%lld", 18446744065119617025) == "18446744065119617025"
    @test Prints.@sprintf("%+8lld", 100) == "    +100"
    @test Prints.@sprintf("%+.8lld", 100) == "+00000100"
    @test Prints.@sprintf("%+10.8lld", 100) == " +00000100"
    @test_throws ArgumentError Prints.Format("%_1lld")
    @test Prints.@sprintf("%-1.5lld", -100) == "-00100"
    @test Prints.@sprintf("%5lld", 100) == "  100"
    @test Prints.@sprintf("%5lld", -100) == " -100"
    @test Prints.@sprintf("%-5lld", 100) == "100  "
    @test Prints.@sprintf("%-5lld", -100) == "-100 "
    @test Prints.@sprintf("%-.5lld", 100) == "00100"
    @test Prints.@sprintf("%-.5lld", -100) == "-00100"
    @test Prints.@sprintf("%-8.5lld", 100) == "00100   "
    @test Prints.@sprintf("%-8.5lld", -100) == "-00100  "
    @test Prints.@sprintf("%05lld", 100) == "00100"
    @test Prints.@sprintf("%05lld", -100) == "-0100"
    @test Prints.@sprintf("% lld", 100) == " 100"
    @test Prints.@sprintf("% lld", -100) == "-100"
    @test Prints.@sprintf("% 5lld", 100) == "  100"
    @test Prints.@sprintf("% 5lld", -100) == " -100"
    @test Prints.@sprintf("% .5lld", 100) == " 00100"
    @test Prints.@sprintf("% .5lld", -100) == "-00100"
    @test Prints.@sprintf("% 8.5lld", 100) == "   00100"
    @test Prints.@sprintf("% 8.5lld", -100) == "  -00100"
    @test Prints.@sprintf("%.0lld", 0) == "0"
    @test Prints.@sprintf("%#+21.18llx", -100) == "-0x000000000000000064"
    @test Prints.@sprintf("%#.25llo", -100) == "-0o0000000000000000000000144"
    @test Prints.@sprintf("%#+24.20llo", -100) == " -0o00000000000000000144"
    @test Prints.@sprintf("%#+18.21llX", -100) == "-0X000000000000000000064"
    @test Prints.@sprintf("%#+20.24llo", -100) == "-0o000000000000000000000144"
    @test Prints.@sprintf("%#+25.22llu", -1) == "  -0000000000000000000001"
    @test Prints.@sprintf("%#+25.22llu", -1) == "  -0000000000000000000001"
    @test Prints.@sprintf("%#+30.25llu", -1) == "    -0000000000000000000000001"
    @test Prints.@sprintf("%+#25.22lld", -1) == "  -0000000000000000000001"
    @test Prints.@sprintf("%#-8.5llo", 100) == "0o00144 "
    @test Prints.@sprintf("%#-+ 08.5lld", 100) == "+00100  "
    @test Prints.@sprintf("%#-+ 08.5lld", 100) == "+00100  "
    @test Prints.@sprintf("%.40lld",  1) == "0000000000000000000000000000000000000001"
    @test Prints.@sprintf("% .40lld",  1) == " 0000000000000000000000000000000000000001"
    @test Prints.@sprintf("% .40d",  1) == " 0000000000000000000000000000000000000001"
    @test Prints.@sprintf("%lld",  18446744065119617025) == "18446744065119617025"

    @test Prints.@sprintf("+%d+",  10) == "+10+"
    @test Prints.@sprintf("%#012x",  1) == "0x0000000001"
    @test Prints.@sprintf("%#04.8x",  1) == "0x00000001"

    @test Prints.@sprintf("%#-08.2x",  1) == "0x01    "
    @test Prints.@sprintf("%#08o",  1) == "0o000001"
    @test Prints.@sprintf("%d",  1024) == "1024"
    @test Prints.@sprintf("%d", -1024) == "-1024"
    @test Prints.@sprintf("%i",  1024) == "1024"
    @test Prints.@sprintf("%i", -1024) == "-1024"
    @test Prints.@sprintf("%u",  1024) == "1024"
    @test Prints.@sprintf("%u",  UInt(4294966272)) == "4294966272"
    @test Prints.@sprintf("%o",  511) == "777"
    @test Prints.@sprintf("%o",  UInt(4294966785)) == "37777777001"
    @test Prints.@sprintf("%x",  305441741) == "1234abcd"
    @test Prints.@sprintf("%x",  UInt(3989525555)) == "edcb5433"
    @test Prints.@sprintf("%X",  305441741) == "1234ABCD"
    @test Prints.@sprintf("%X",  UInt(3989525555)) == "EDCB5433"
    @test Prints.@sprintf("%+d",  1024) == "+1024"
    @test Prints.@sprintf("%+d", -1024) == "-1024"
    @test Prints.@sprintf("%+i",  1024) == "+1024"
    @test Prints.@sprintf("%+i", -1024) == "-1024"
    @test Prints.@sprintf("%+u",  1024) == "+1024"
    @test Prints.@sprintf("%+u",  UInt(4294966272)) == "+4294966272"
    @test Prints.@sprintf("%+o",  511) == "+777"
    @test Prints.@sprintf("%+o",  UInt(4294966785)) == "+37777777001"
    @test Prints.@sprintf("%+x",  305441741) == "+1234abcd"
    @test Prints.@sprintf("%+x",  UInt(3989525555)) == "+edcb5433"
    @test Prints.@sprintf("%+X",  305441741) == "+1234ABCD"
    @test Prints.@sprintf("%+X",  UInt(3989525555)) == "+EDCB5433"
    @test Prints.@sprintf("% d",  1024) == " 1024"
    @test Prints.@sprintf("% d", -1024) == "-1024"
    @test Prints.@sprintf("% i",  1024) == " 1024"
    @test Prints.@sprintf("% i", -1024) == "-1024"
    @test Prints.@sprintf("% u",  1024) == " 1024"
    @test Prints.@sprintf("% u",  UInt(4294966272)) == " 4294966272"
    @test Prints.@sprintf("% o",  511) == " 777"
    @test Prints.@sprintf("% o",  UInt(4294966785)) == " 37777777001"
    @test Prints.@sprintf("% x",  305441741) == " 1234abcd"
    @test Prints.@sprintf("% x",  UInt(3989525555)) == " edcb5433"
    @test Prints.@sprintf("% X",  305441741) == " 1234ABCD"
    @test Prints.@sprintf("% X",  UInt(3989525555)) == " EDCB5433"
    @test Prints.@sprintf("%+ d",  1024) == "+1024"
    @test Prints.@sprintf("%+ d", -1024) == "-1024"
    @test Prints.@sprintf("%+ i",  1024) == "+1024"
    @test Prints.@sprintf("%+ i", -1024) == "-1024"
    @test Prints.@sprintf("%+ u",  1024) == "+1024"
    @test Prints.@sprintf("%+ u",  UInt(4294966272)) == "+4294966272"
    @test Prints.@sprintf("%+ o",  511) == "+777"
    @test Prints.@sprintf("%+ o",  UInt(4294966785)) == "+37777777001"
    @test Prints.@sprintf("%+ x",  305441741) == "+1234abcd"
    @test Prints.@sprintf("%+ x",  UInt(3989525555)) == "+edcb5433"
    @test Prints.@sprintf("%+ X",  305441741) == "+1234ABCD"
    @test Prints.@sprintf("%+ X",  UInt(3989525555)) == "+EDCB5433"
    @test Prints.@sprintf("%#o",  511) == "0o777"
    @test Prints.@sprintf("%#o",  UInt(4294966785)) == "0o37777777001"
    @test Prints.@sprintf("%#x",  305441741) == "0x1234abcd"
    @test Prints.@sprintf("%#x",  UInt(3989525555)) == "0xedcb5433"
    @test Prints.@sprintf("%#X",  305441741) == "0X1234ABCD"
    @test Prints.@sprintf("%#X",  UInt(3989525555)) == "0XEDCB5433"
    @test Prints.@sprintf("%#o",  UInt(0)) == "0o0"
    @test Prints.@sprintf("%#x",  UInt(0)) == "0x0"
    @test Prints.@sprintf("%#X",  UInt(0)) == "0X0"
    @test Prints.@sprintf("%1d",  1024) == "1024"
    @test Prints.@sprintf("%1d", -1024) == "-1024"
    @test Prints.@sprintf("%1i",  1024) == "1024"
    @test Prints.@sprintf("%1i", -1024) == "-1024"
    @test Prints.@sprintf("%1u",  1024) == "1024"
    @test Prints.@sprintf("%1u",  UInt(4294966272)) == "4294966272"
    @test Prints.@sprintf("%1o",  511) == "777"
    @test Prints.@sprintf("%1o",  UInt(4294966785)) == "37777777001"
    @test Prints.@sprintf("%1x",  305441741) == "1234abcd"
    @test Prints.@sprintf("%1x",  UInt(3989525555)) == "edcb5433"
    @test Prints.@sprintf("%1X",  305441741) == "1234ABCD"
    @test Prints.@sprintf("%1X",  UInt(3989525555)) == "EDCB5433"
    @test Prints.@sprintf("%20d",  1024) == "                1024"
    @test Prints.@sprintf("%20d", -1024) == "               -1024"
    @test Prints.@sprintf("%20i",  1024) == "                1024"
    @test Prints.@sprintf("%20i", -1024) == "               -1024"
    @test Prints.@sprintf("%20u",  1024) == "                1024"
    @test Prints.@sprintf("%20u",  UInt(4294966272)) == "          4294966272"
    @test Prints.@sprintf("%20o",  511) == "                 777"
    @test Prints.@sprintf("%20o",  UInt(4294966785)) == "         37777777001"
    @test Prints.@sprintf("%20x",  305441741) == "            1234abcd"
    @test Prints.@sprintf("%20x",  UInt(3989525555)) == "            edcb5433"
    @test Prints.@sprintf("%20X",  305441741) == "            1234ABCD"
    @test Prints.@sprintf("%20X",  UInt(3989525555)) == "            EDCB5433"
    @test Prints.@sprintf("%-20d",  1024) == "1024                "
    @test Prints.@sprintf("%-20d", -1024) == "-1024               "
    @test Prints.@sprintf("%-20i",  1024) == "1024                "
    @test Prints.@sprintf("%-20i", -1024) == "-1024               "
    @test Prints.@sprintf("%-20u",  1024) == "1024                "
    @test Prints.@sprintf("%-20u",  UInt(4294966272)) == "4294966272          "
    @test Prints.@sprintf("%-20o",  511) == "777                 "
    @test Prints.@sprintf("%-20o",  UInt(4294966785)) == "37777777001         "
    @test Prints.@sprintf("%-20x",  305441741) == "1234abcd            "
    @test Prints.@sprintf("%-20x",  UInt(3989525555)) == "edcb5433            "
    @test Prints.@sprintf("%-20X",  305441741) == "1234ABCD            "
    @test Prints.@sprintf("%-20X",  UInt(3989525555)) == "EDCB5433            "
    @test Prints.@sprintf("%020d",  1024) == "00000000000000001024"
    @test Prints.@sprintf("%020d", -1024) == "-0000000000000001024"
    @test Prints.@sprintf("%020i",  1024) == "00000000000000001024"
    @test Prints.@sprintf("%020i", -1024) == "-0000000000000001024"
    @test Prints.@sprintf("%020u",  1024) == "00000000000000001024"
    @test Prints.@sprintf("%020u",  UInt(4294966272)) == "00000000004294966272"
    @test Prints.@sprintf("%020o",  511) == "00000000000000000777"
    @test Prints.@sprintf("%020o",  UInt(4294966785)) == "00000000037777777001"
    @test Prints.@sprintf("%020x",  305441741) == "0000000000001234abcd"
    @test Prints.@sprintf("%020x",  UInt(3989525555)) == "000000000000edcb5433"
    @test Prints.@sprintf("%020X",  305441741) == "0000000000001234ABCD"
    @test Prints.@sprintf("%020X",  UInt(3989525555)) == "000000000000EDCB5433"
    @test Prints.@sprintf("%#20o",  511) == "               0o777"
    @test Prints.@sprintf("%#20o",  UInt(4294966785)) == "       0o37777777001"
    @test Prints.@sprintf("%#20x",  305441741) == "          0x1234abcd"
    @test Prints.@sprintf("%#20x",  UInt(3989525555)) == "          0xedcb5433"
    @test Prints.@sprintf("%#20X",  305441741) == "          0X1234ABCD"
    @test Prints.@sprintf("%#20X",  UInt(3989525555)) == "          0XEDCB5433"
    @test Prints.@sprintf("%#020o",  511) == "0o000000000000000777"
    @test Prints.@sprintf("%#020o",  UInt(4294966785)) == "0o000000037777777001"
    @test Prints.@sprintf("%#020x",  305441741) == "0x00000000001234abcd"
    @test Prints.@sprintf("%#020x",  UInt(3989525555)) == "0x0000000000edcb5433"
    @test Prints.@sprintf("%#020X",  305441741) == "0X00000000001234ABCD"
    @test Prints.@sprintf("%#020X",  UInt(3989525555)) == "0X0000000000EDCB5433"
    @test Prints.@sprintf("%0-20d",  1024) == "1024                "
    @test Prints.@sprintf("%0-20d", -1024) == "-1024               "
    @test Prints.@sprintf("%0-20i",  1024) == "1024                "
    @test Prints.@sprintf("%0-20i", -1024) == "-1024               "
    @test Prints.@sprintf("%0-20u",  1024) == "1024                "
    @test Prints.@sprintf("%0-20u",  UInt(4294966272)) == "4294966272          "
    @test Prints.@sprintf("%-020o",  511) == "777                 "
    @test Prints.@sprintf("%-020o",  UInt(4294966785)) == "37777777001         "
    @test Prints.@sprintf("%-020x",  305441741) == "1234abcd            "
    @test Prints.@sprintf("%-020x",  UInt(3989525555)) == "edcb5433            "
    @test Prints.@sprintf("%-020X",  305441741) == "1234ABCD            "
    @test Prints.@sprintf("%-020X",  UInt(3989525555)) == "EDCB5433            "
    @test Prints.@sprintf("%.20d",  1024) == "00000000000000001024"
    @test Prints.@sprintf("%.20d", -1024) == "-00000000000000001024"
    @test Prints.@sprintf("%.20i",  1024) == "00000000000000001024"
    @test Prints.@sprintf("%.20i", -1024) == "-00000000000000001024"
    @test Prints.@sprintf("%.20u",  1024) == "00000000000000001024"
    @test Prints.@sprintf("%.20u",  UInt(4294966272)) == "00000000004294966272"
    @test Prints.@sprintf("%.20o",  511) == "00000000000000000777"
    @test Prints.@sprintf("%.20o",  UInt(4294966785)) == "00000000037777777001"
    @test Prints.@sprintf("%.20x",  305441741) == "0000000000001234abcd"
    @test Prints.@sprintf("%.20x",  UInt(3989525555)) == "000000000000edcb5433"
    @test Prints.@sprintf("%.20X",  305441741) == "0000000000001234ABCD"
    @test Prints.@sprintf("%.20X",  UInt(3989525555)) == "000000000000EDCB5433"
    @test Prints.@sprintf("%20.5d",  1024) == "               01024"
    @test Prints.@sprintf("%20.5d", -1024) == "              -01024"
    @test Prints.@sprintf("%20.5i",  1024) == "               01024"
    @test Prints.@sprintf("%20.5i", -1024) == "              -01024"
    @test Prints.@sprintf("%20.5u",  1024) == "               01024"
    @test Prints.@sprintf("%20.5u",  UInt(4294966272)) == "          4294966272"
    @test Prints.@sprintf("%20.5o",  511) == "               00777"
    @test Prints.@sprintf("%20.5o",  UInt(4294966785)) == "         37777777001"
    @test Prints.@sprintf("%20.5x",  305441741) == "            1234abcd"
    @test Prints.@sprintf("%20.10x",  UInt(3989525555)) == "          00edcb5433"
    @test Prints.@sprintf("%20.5X",  305441741) == "            1234ABCD"
    @test Prints.@sprintf("%20.10X",  UInt(3989525555)) == "          00EDCB5433"
    @test Prints.@sprintf("%020.5d",  1024) == "               01024"
    @test Prints.@sprintf("%020.5d", -1024) == "              -01024"
    @test Prints.@sprintf("%020.5i",  1024) == "               01024"
    @test Prints.@sprintf("%020.5i", -1024) == "              -01024"
    @test Prints.@sprintf("%020.5u",  1024) == "               01024"
    @test Prints.@sprintf("%020.5u",  UInt(4294966272)) == "          4294966272"
    @test Prints.@sprintf("%020.5o",  511) == "               00777"
    @test Prints.@sprintf("%020.5o",  UInt(4294966785)) == "         37777777001"
    @test Prints.@sprintf("%020.5x",  305441741) == "            1234abcd"
    @test Prints.@sprintf("%020.10x",  UInt(3989525555)) == "          00edcb5433"
    @test Prints.@sprintf("%020.5X",  305441741) == "            1234ABCD"
    @test Prints.@sprintf("%020.10X",  UInt(3989525555)) == "          00EDCB5433"
    @test Prints.@sprintf("%20.0d",  1024) == "                1024"
    @test Prints.@sprintf("%20.d", -1024) == "               -1024"
    @test Prints.@sprintf("%20.d",  0) == "                   0"
    @test Prints.@sprintf("%20.0i",  1024) == "                1024"
    @test Prints.@sprintf("%20.i", -1024) == "               -1024"
    @test Prints.@sprintf("%20.i",  0) == "                   0"
    @test Prints.@sprintf("%20.u",  1024) == "                1024"
    @test Prints.@sprintf("%20.0u",  UInt(4294966272)) == "          4294966272"
    @test Prints.@sprintf("%20.u",  UInt(0)) == "                   0"
    @test Prints.@sprintf("%20.o",  511) == "                 777"
    @test Prints.@sprintf("%20.0o",  UInt(4294966785)) == "         37777777001"
    @test Prints.@sprintf("%20.o",  UInt(0)) == "                   0"
    @test Prints.@sprintf("%20.x",  305441741) == "            1234abcd"
    @test Prints.@sprintf("%20.0x",  UInt(3989525555)) == "            edcb5433"
    @test Prints.@sprintf("%20.x",  UInt(0)) == "                   0"
    @test Prints.@sprintf("%20.X",  305441741) == "            1234ABCD"
    @test Prints.@sprintf("%20.0X",  UInt(3989525555)) == "            EDCB5433"
    @test Prints.@sprintf("%20.X",  UInt(0)) == "                   0"

end
