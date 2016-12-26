package toolc
package lexer

import java.io.File

import toolc.utils._

import scala.io.Source

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "program"  => Some(PROGRAM())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "var"      => Some(VAR())
    case "String"   => Some(STRING())
    case "extends"  => Some(EXTENDS())
    case "Int"      => Some(INT())
    case "Bool"     => Some(BOOLEAN()) // Bool pas BOOLEAN !!!!!!!!!
    case "while"    => Some(WHILE())
    case "if"       => Some(IF())
    case "else"     => Some(ELSE())
    case "return"   => Some(RETURN())
    case "length"   => Some(LENGTH())
    case "true"     => Some(TRUE())
    case "false"    => Some(FALSE())
    case "this"     => Some(THIS())
    case "new"      => Some(NEW())
    case "println"  => Some(PRINTLN())
    case "do"       => Some(DO())
    case "value"    => Some(VALUE()) //Extension for the project
    case _          => None
  }


  /** Reads the contents of a file, caching two characters at a time.
    * That way we can have a 2-character lookahead with
    * currentChar and nextChar
    */
  private class SourceReader(f: File) {
    private val source = Source.fromFile(f)

    /** We use this character to mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE
    
    private var currentChar_ : Char = _
    private var nextChar_ : Char = _
    private var currentPos_ : Positioned = _
    private var nextPos_ : Positioned = _

    /** The current character */
    def currentChar = currentChar_
    /** The next character */
    def nextChar = nextChar_
    /** The position of the current character */
    def currentPos = currentPos_

    private def readChar(): Char = if (source.hasNext) {
      source.next
    } else {
      EndOfFile
    }

    /** Consumes a character from the input.
      * nextChar becomes currentChar,
      * nextChar points to the first unread character.
      */
    def consume() = {
      currentChar_ = nextChar_
      currentPos_ = nextPos_
      nextChar_ = readChar()
      nextPos_ = new Positioned{}.setPos(f, source.pos)
    }

    /** Consume n characters */
    def consume(n: Int): Unit = for (i <- 1 to n) consume()

    // To start, read the first two characters of the file
    consume(2)
  }


  def run(ctx: Context)(f: File): Iterator[Token] = {

    val reader = new SourceReader(f)
    import reader._

    /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
    @scala.annotation.tailrec
    def nextToken(): Token = {
      while (Character.isWhitespace(currentChar)) {
        consume()
      }
      if (currentChar == '/' && nextChar == '/') {
        consume(2)
        // Skip until EOL
        while(currentChar != '\n' && currentChar != '\r' && currentChar != EndOfFile) consume()
        nextToken()
      } else if (currentChar == '/' && nextChar == '*') {
        consume(2)
        while((currentChar != '*' || nextChar != '/') && currentChar != EndOfFile) consume()

        if(currentChar == EndOfFile) {
          ctx.reporter.fatal("The commentary is not closed !", currentPos)
          BAD().setPos(currentPos)
        }
        consume(2) // enlever le */
        nextToken()
      } else {
        readToken()
      }
    }

    /** Reads the next token from the stream. */
    def readToken(): Token = {
      // The position at the beginning of the token.
      val tokenPos = currentPos

      val token = currentChar match {
        case EndOfFile => consume(); EOF()
        case ':' => consume(); COLON()
        case ';' => consume(); SEMICOLON()
        case '.' => consume(); DOT()
        case ',' => consume(); COMMA()
        case '=' => {
          if(nextChar == '=') {
            consume(2)
            EQUALS()
          }else{
            consume()
            EQSIGN()
          }
        }
        case '!' => consume(); BANG()
        case '(' => consume(); LPAREN()
        case ')' => consume(); RPAREN()
        case '[' => consume(); LBRACKET()
        case ']' => consume(); RBRACKET()
        case '{' => consume(); LBRACE()
        case '}' => consume(); RBRACE()
        case '&' => {
          if(nextChar == '&'){
            consume(2)
            AND()
          } else{
            consume()
            ctx.reporter.error("The character & is not a valid !", tokenPos)
            BAD()
          }
        }
        case '|' => {
          if(nextChar == '|'){
            consume(2)
            OR()
          } else{
            consume()
            ctx.reporter.error("The character | is not a valid !", tokenPos)
            BAD()
          }
        }
        case '<' => consume(); LESSTHAN()
        case '+' => consume(); PLUS()
        case '-' => consume(); MINUS()
        case '*' => consume(); TIMES()
        case '/' => consume(); DIV()

        case l if(l.isLetter) => {
          val identifier = new StringBuilder()
          identifier.append(currentChar)
          consume()
          while(currentChar.isLetterOrDigit || currentChar == '_') {
            identifier.append(currentChar)
            consume()
          }
          val strId = identifier.toString
          keywords(strId) match {
            case Some(token) => token
            case None => ID(strId)
          }
        }

        case i if(i.isDigit) => {
          val number = new StringBuilder()
          number.append(currentChar)
          consume()
          while(currentChar.isDigit) {
            number.append(currentChar)
            consume()
          }
          val intId = number.toInt
          INTLIT(intId)
        }

        case '"' => {
          val str = new StringBuilder()
          consume()
          var badInput = false
          while(currentChar != '"' && !badInput) {
            if(currentChar == '\n' || currentChar == EndOfFile) {
              badInput = true
              consume()
            }else{
              str.append(currentChar)
              consume()
            }
          }
          consume()
          if(badInput) {
            ctx.reporter.error("The string is not closed !", tokenPos)
            BAD()
          }else STRINGLIT(str.toString())
        }

        case _ => {
          ctx.reporter.error("Invalid character", tokenPos)
          consume()
          BAD()
        }
      }
      token.setPos(tokenPos)
    }

    new Iterator[Token] {
      var tokenCache: Token = nextToken()
      var reachedEnd = false

      def hasNext = !reachedEnd

      def next = {
        val r = tokenCache
        if (r == EOF()) {
          reachedEnd = true
        } else {
          tokenCache = nextToken()
        }
        r
      }
    }
  }
}
/* /* */ */

/** Reads and displays the tokens, then returns a fresh iterator with the same tokens. */
object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    val l = tokens.toList
    l foreach { t => println(s"$t(${t.line}:${t.col})") }
    l.iterator
  }
}



