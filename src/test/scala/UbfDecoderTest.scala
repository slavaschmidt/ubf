/**
  * @since 08.02.2016.
  */
class UbfDecoderTest extends org.scalatest.FunSpec {

  describe("UbfDecoder, continuation passing style") {
    it("should not parse empty string") {
      assert(UbfDecoder.decode("") == Left("EOF"))
    }
    it("should not parse invalid string") {
      assert(UbfDecoder.decode("asdluk") == Left("Invalid start character: 'a' (97) in sequence asdluk"))
    }
    it("should parse valid strings") {
      assert(UbfDecoder.decode("\"foo\"$") == Right(UbfString("foo")))
      assert(UbfDecoder.decode("\"\\\\foo\\\"\\\"\"$") == Right(UbfString("\\foo\"\"")))
    }
    it("should parse valid numbers") {
      assert(UbfDecoder.decode("1234$") == Right(UbfInteger(1234)))
      assert(UbfDecoder.decode("-19876$") == Right(UbfInteger(-19876)))
    }
    it("should not parse invalid numbers") {
      assert(UbfDecoder.decode("-") == Left("No digits in number, just '-'"))
    }
    it("should produce overflow messages for big numbers") {
      assert(UbfDecoder.decode("19223372036854775807$") == Left("Integer overflow:19223372036854775807"))
      assert(UbfDecoder.decode("-19223372036854775808$") == Left("Integer overflow:-19223372036854775808"))
    }
    it("should parse valid atoms") {
      assert(UbfDecoder.decode("'foo'$") == Right(UbfAtom("foo")))
      assert(UbfDecoder.decode("'\\'foo\\''$") == Right(UbfAtom("\'foo\'")))
      assert(UbfDecoder.decode("'\\\'foo\\\''$") == Right(UbfAtom("\'foo\'")))
    }
    it("should parse valid tuples") {
      assert(UbfDecoder.decode("{}$") == Right(UbfTuple()))
      assert(UbfDecoder.decode("{{{}}}$") == Right(UbfTuple(UbfTuple(UbfTuple()))))
      assert(UbfDecoder.decode("{1 \"two\" {3} 'four'}$") == Right(UbfTuple(UbfInteger(1), UbfString("two"), UbfTuple(UbfInteger(3)), UbfAtom("four"))))
    }
    it("should parse valid lists") {
      assert(UbfDecoder.decode("# 1 & 2 & 3 & $") == Right(UbfList(UbfInteger(1), UbfInteger(2), UbfInteger(3))))
      assert(UbfDecoder.decode("#$") == Right(UbfList()))
    }
    val input = Seq("1234$", "-4321$", "\"foo\"$", "\"\\\\foo\\\"\\\"\"$", "'foo'$", "'\\\'foo\\\''$", "{}$",
      "{{{}}}$", "{1 \"two\" {3} 'four'}$", "# 1 & 2 & 3 & $", "# $", """# "a" & "b" & "c" & "d" & $""")

    input foreach { str =>
      it("should encode and decode preserving structure with " + str) {
        val obj = UbfDecoder.decode(str).right.get
        assert(UbfEncoder.encode(obj) == str)
      }
    }
  }

}
