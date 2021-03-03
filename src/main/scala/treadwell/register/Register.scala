package treadwell.register

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Try

object Register extends App {
  /** The supported dollar denominations. */
  private val Denominations = Vector(20, 10, 5, 2, 1)

  /** The `Register`'s current state. */
  private var register = Cash()

  /** Parses the argument into a single `Int`; if successful, the given delegate
    * is invoked over the parsed value.
    */
  private def sanitizeInt(arg: String)(delegate: Int => AnyVal): Unit =
    sanitizeInts(Seq(arg), 1) { ints => delegate(ints.head) }

  /** Parses a specified number of arguments and passes them to the given
    * delegate upon success.
    */
  private def sanitizeInts(args: Seq[String], reqCount: Int = Denominations.size)
                          (delegate: Seq[Int] => AnyVal): Unit = {
    Try {
      require(args.size == reqCount)
      args.map { s =>
        val k = s.toInt
        require(k >= 0)
        k
      }
    } map delegate getOrElse {
      println(s"Command takes $reqCount non-negative, space-separated integer(s).")
    }
  }

  @tailrec
  private def inputLoop(): Nothing = {
    readLine(">")
      .split("""\s+""")
      .filter(!_.isEmpty)
      .map(_.toLowerCase)
      .toList match {
      // Do nothing when the user enters a blank line.
      case Nil =>

      // Print the register's current contents.
      case "show" :: Nil => println(this)

      // Add the given denomination counts to the register.
      case "put" :: args => sanitizeInts(args) { counts =>
        register += Denominations.lazyZip(counts)
        println(this)
      }

      // Remove the given denomination counts from the register, if present.
      case "take" :: args => sanitizeInts(args) { counts =>
        val debited = register - Denominations.lazyZip(counts)
        if (debited.denoms.exists(d => debited(d) < 0)) {
          println("Not enough notes!")
        } else {
          register = debited
          println(this)
        }
      }

      // Remove change from the register for the given amount, if possible.
      case "change" :: arg :: Nil => sanitizeInt(arg) { amount =>
        val change = register draw amount
        register -= change
        println(if (change.total == amount) formatCounts(change) else "Sorry.")
      }

      case "quit" :: Nil =>
        println("Bye.")
        System.exit(0)

      case _ => println("Invalid command!")
    }

    inputLoop()
  }

  private def formatCounts(reg: Cash): String =
    Denominations.view.map(reg(_)).mkString(" ")

  override def toString: String =
    s"$$${register.total} ${formatCounts(register)}"

  println("Ready.")
  inputLoop()
}
