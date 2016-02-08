/**
  * @since 08.02.2016.
  */
case class Input(in: Stream[Char], stack: List[UbfObject], register: Map[Char, UbfObject])

case class InputOp[+A](run: Input => (Either[String, A], Input)) {
  def map[B](f: A => B): InputOp[B] = InputOp(input => run(input) match {
    case (Left(msg), st) => (Left(msg), st)
    case (Right(a), st) => (Right(f(a)), st)
  })
  def flatMap[B](f: A => InputOp[B]): InputOp[B] = InputOp(input => run(input) match {
    case (Left(msg), st) => (Left(msg), st)
    case (Right(a), st) => f(a).run(st)
  })
}

object UbfDecoder {
  def decode(s: String): Either[String, UbfObject] = {
    val in = s.getBytes.map(_.toChar).toStream
    val input = Input(in, Nil, Map.empty[Char, UbfObject])
    val result = new UbfDecoder().read.run(input)
    result._1
  }
}

class UbfDecoder {

  def read: InputOp[UbfObject] = {
    skipWhitespace flatMap { _ =>
      peek flatMap {
        case '}' => nextChar flatMap { _ => InputOp {
          case i@Input(in, stack, register) =>
            (Right(UbfTuple(stack.reverse:_*)), Input(in, List.empty, register))
        }}
        case '$' => nextChar flatMap { _ => InputOp {
          case i@Input(in, stack, register) if stack.size != 1 =>
            (Left(s"Can't end with stack size ${stack.size}"), i)
          case i@Input(in, stack, register) =>
            val out = Input(in, stack.tail, register)
            (Right(stack.head), out)
        }}
        case ch => readObject flatMap { obj => InputOp {
            case i@Input(in, stack, register) =>
              val out = Input(in, obj :: stack, register)
              (Right(obj), out)
        }} flatMap { _ => read }
      }
    }
  }

  protected def readObject: InputOp[UbfObject] = peek flatMap {
    case '#'  =>  newList
    case '&'  =>  listItem
    case '"'  =>  readString
    case '\'' =>  readAtom
    case '{'  =>  readTuple
    case '>'  =>  putIntoRegistry
    case '~'  =>  readBinary
    case '-'  =>  readInteger
    case ch if Character.isDigit(ch) => readInteger
    case ch => tryGetFromRegister(ch)
  }

  protected def putIntoRegistry: InputOp[UbfObject] = fromStackIntoReg flatMap { _ => readObject }

  // here clean stack is needed
  // because of this {run} is called directly
  // this is not pure combinator anymore
  protected def readTuple: InputOp[UbfObject] = nextChar flatMap { _ =>
    InputOp {
      case i @ Input(in, stack, register) =>
        val tempInput = Input(in, List.empty[UbfObject], register)
        val (result, newIn) = read.run(tempInput)
        val out = Input(newIn.in, stack, register)
        result match {
          case Right(t: UbfTuple) => (result, out)
          case Right(other) => (Left(s"Expected tuple: $other"), out)
          case l @ Left(err) => (l, out)
        }
    }
  }


  protected def newList: InputOp[UbfList] = nextChar map { _ => UbfList() }

  protected def listItem: InputOp[UbfList] = nextChar flatMap { _ =>
      InputOp {
        case i @ Input(in, stack, register) =>
          val item = stack.head
          val list = stack.tail.head
          list match {
            case l: UbfList =>
              (Right(l.addItem(item)), Input(in, stack.tail.tail, register))
            case other =>
              (Left(s"Can't make cons with cdr $other"), i)
          }
      }
  }

  protected def tryGetFromRegister(c: Char): InputOp[UbfObject] = InputOp {
    case i @ Input(in, stack, register) if register.contains(c) =>
      val result = Right(register(c))
      val output = Input(in.tail, stack, register)
      (result, output)
    case i => (Left(s"Invalid start character: '$c' (${c.toInt}) in sequence ${i.in.mkString.take(10)}"), i)
  }

  protected def fromStackIntoReg: InputOp[UbfObject] = nextChar flatMap { _ =>
    nextChar flatMap { reg =>
      InputOp {
        case Input(in, stack, register) =>
          val output = Input(in, stack.tail, register.updated(reg, stack.head))
          val result = Right(stack.head)
          (result, output)
      }
    }
  }
  protected def readBinary: InputOp[UbfObject] = nextChar flatMap { _ =>
    InputOp {
      case Input(in, stack, register) =>
        val len = stack.head.asInstanceOf[UbfInteger].value.toInt
        val data = in.take(len)
        (Right(UbfBinary(data.map(_.toByte))), Input(in.drop(len + 1), stack.tail, register))
    }
  }

  protected def readString: InputOp[UbfString] = readDelimitedString map { new UbfString(_) }

  protected def readAtom: InputOp[UbfAtom] = readDelimitedString map { new UbfAtom(_) }

  protected def readDelimitedString: InputOp[String] = {
    nextChar flatMap { delimiter =>
      InputOp {
        case Input(in, stack, register) =>
          val (result, out) = readEscapedString(delimiter, in, Stream.empty)
          (Right(result.mkString), Input(out, stack, register))
      }
    }
  }

  private def unescape(delimiter: Char, s: Seq[Char]): String =
    s.mkString.
      replaceAllLiterally("\\"+delimiter, delimiter.toString).
      replaceAllLiterally("\\\\", "\\")

  private def readEscapedString(delimiter: Char, in: Stream[Char], acc: Stream[Char]): (Seq[Char], Stream[Char]) = {
    val (buf, out) = (in.takeWhile(_ != delimiter), in.dropWhile(_ != delimiter).drop(1))
    if (!buf.endsWith("\\"))
      (unescape(delimiter, acc ++ buf), out)
    else {
      val (newBuf, newOut) = readEscapedString(delimiter, out, acc ++ buf :+ delimiter)
      (unescape(delimiter, newBuf), newOut)
    }
  }

  private def checkNum(sign: Int, numSeq: Seq[Int]) =
    if (sign < 0 && numSeq.isEmpty) Left("No digits in number, just '-'")
    else numSeq.foldLeft(BigInt(sign.max(0))) { (acc, i) => acc * 10 + i * sign } match {
      case num if num.isValidLong => Right(UbfInteger(num.longValue))
      case other => Left("Integer overflow:" + other.toString)
    }

  protected def readInteger: InputOp[UbfInteger] =
    nextChar flatMap { ch =>
      val sign = if (ch == '-') -1 else ch - '0'
      val readNum: Input => Seq[Int] = in => in.in.takeWhile(_.isDigit).map(_.toInt - '0')
      InputOp(in => (checkNum(sign, readNum(in)), in.copy(in = in.in.dropWhile(_.isDigit))))
    }

  protected def nextChar: InputOp[Char] = InputOp { in =>
    in.in.headOption map { i => (Right(i), in.copy(in = in.in.tail)) } getOrElse { (Left("EOF"), in) }
  }

  protected def peek:InputOp[Char] = InputOp { in =>
    val result = in.in.headOption map { Right(_) } getOrElse Left("EOF")
    (result, in)
  }

  private val whiteSpaces = Seq('\n', '\r', '\t', ' ', ',')
  private def isWhitespace(ch: Char): Boolean = whiteSpaces.contains(ch)
  private def skipWhitespace: InputOp[Int] = InputOp { in =>
    (Right(in.in.takeWhile(isWhitespace).size), in.copy(in = in.in dropWhile isWhitespace))
  }
}
