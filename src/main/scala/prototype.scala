package gitcoin

import sys.process._
import compat.Platform.currentTime

object Prototype {
  val h = new Hasher
  def roundtrip {
    val msg = "foo"

    val tree = "git write-tree".!!
    val parent = "git rev-parse HEAD".!!
    val timestamp = currentTime

/*
    val tree = "63f225ce4031809b2571d68d731e72950a62a555"
    val parent = "3a6496a00bef80bec369d15677d750c5deac9f9e"
    val timestamp = 1390445101
*/

    val body = s"""tree $tree
                  |parent $parent
                  |author Stripe CTF <ctf@stripe.com> $timestamp +0000
                  |committer Stripe CTF <ctf@stripe.com> $timestamp +0000
                  |
                  |Mined a Gitcoin!
                  |nonce %08x""".stripMargin
    val length = body.format(0).length

    val nonce = 0x036d6930
    val full = s"commit $length\0$body".format(nonce)

    val difficulty = io.Source.fromFile(new java.io.File("difficulty.txt")).mkString.split("\n")(0)
    val sha = h.hexdigest(full)

    sha < difficulty
  }
}
