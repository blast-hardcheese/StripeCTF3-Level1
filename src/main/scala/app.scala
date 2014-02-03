package gitcoin

import akka.actor.{Actor,Props}
import sys.process._
import compat.Platform.currentTime

case object Stop
case class Success(nonce: Long)
case object TryNext

class Hasher {
  val md = java.security.MessageDigest.getInstance("SHA-1")
  val encoder = new sun.misc.BASE64Encoder()

  def encode(msg: String): Array[Byte] = md.digest(msg.getBytes)
  def hexdigest(msg: String): String = {
    (for(c <- md.digest(msg.getBytes)) yield {
      val i = c.toInt
      val j = if(i < 0) 0x100 + i else i
      "%02x".format(j)
    }).mkString
  }

  def hexdigest_(msg: String): String = {
    encode(msg)
      .map(_.toInt)
      .map({ i =>
        if(i < 0) 0x100 + i
        else i
      })
      .map("%02x".format(_))
      .mkString
  }
}

class Miner(tree: String, parent: String, timestamp: Long, difficulty: String, seed: Int) extends Actor {
  println(s"Miner $seed initialized")
  val hasher = new Hasher

  def body(nonce: Long) =
    f"""|tree $tree
        |parent $parent
        |author Devon Stewart <blast@hardchee.se> $timestamp +0000
        |committer Devon Stewart <blast@hardchee.se> $timestamp +0000
        |
        |Mined a Gitcoin!
        |nonce $seed%02x$nonce%06x""".stripMargin
  val length = body(0).length

  val each = 100000

  def receiveCur(nonce: Long, second: Long, count: Int): Receive = {
    case TryNext => {
      val milis = (currentTime - second)
      println(s"$seed: $milis")
      print(".")
      for(_nonce <- nonce until nonce + each) {
        val full = s"commit $length\0${body(_nonce)}"
        val sha = hasher.hexdigest(full)

        if(_nonce > 100000000) self ! Stop

        if(sha < difficulty) {
          println("Success!")
          println(sha)
          println(_nonce)
          println(full)
          self ! Success(_nonce)
        }
      }
      context.become(receiveCur(nonce + each, currentTime, each))
      self ! TryNext
    }

    case Success(_nonce) => {
	    val sha1 = (Seq("echo", "-n", body(_nonce)) #| "git hash-object -t commit --stdin -w").!!
	    println(s"git reset --hard $sha1".!!)
      println("git push".!!)
      self ! Stop
    }

    case Stop => {
      context.parent ! Stop
    }
  }

  val receive = receiveCur(0, currentTime, 0)
}

class Master extends Actor {
  Prototype.roundtrip
  val tree = ("git write-tree".!!).trim
  val parent = ("git rev-parse HEAD".!!).trim
  val difficulty = io.Source.fromFile(new java.io.File("difficulty.txt")).mkString.split("\n")(0) // TODO: This sucks

  val miners = for(i <- 0 until 7) yield context.actorOf(Props(new Miner(tree, parent, currentTime, difficulty, i)))

  def receive: Receive = {
    case Stop => {println("Stopping"); context.stop(self)}
  }

  miners.map(_ ! TryNext)
}
