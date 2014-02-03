package gitcoin

import akka.actor.{Actor,Props}
import sys.process._
import compat.Platform.currentTime

case object Stop
case object Success
case object TryNext

object Types {
  type GitTree = String
  type GitParent = String
  type GitTimestamp = Long
  type GitDifficulty = String
}

import Types._

class Hasher {
  val md = java.security.MessageDigest.getInstance("SHA-1")
  val encoder = new sun.misc.BASE64Encoder()

  def encode(msg: String): Array[Byte] = md.digest(msg.getBytes)
  def hexdigest(msg: String): String = {
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

class Miner(_tree: GitTree, _parent: GitParent, _timestamp: GitTimestamp, _difficulty: GitDifficulty, seed: Int) extends Actor {
  println(s"Miner $seed initialized")
  val hasher = new Hasher

  def body(nonce: Long)(implicit tree: GitTree, parent: GitParent, timestamp: GitTimestamp) =
    f"""|tree $tree
        |parent $parent
        |author Devon Stewart <blast@hardchee.se> $timestamp +0000
        |committer Devon Stewart <blast@hardchee.se> $timestamp +0000
        |
        |Mined a Gitcoin!
        |nonce $seed%02x$nonce%06x""".stripMargin
  def length()(implicit tree: GitTree, parent: GitParent, timestamp: GitTimestamp) = body(0)

  def receiveCur(nonce: Long)(implicit tree: GitTree, parent: GitParent, timestamp: GitTimestamp, difficulty: GitDifficulty): Receive = {
    case TryNext => {
      if(nonce % 1000 == 0) print(".")
      val full = s"commit ${length()}\0${body(nonce)}".format(seed, nonce)
      val sha = hasher.hexdigest(full)

      if(nonce > 10000000) self ! Stop

      if(sha < difficulty) {
        println("Success!")
        println(sha)
        println(nonce)
        println(full)
        self ! Success
      }
      else {
        context.become(receiveCur(nonce + 1))
        self ! TryNext
      }
    }

    case Success => {
	    val sha1 = (Seq("echo", "-n", body(nonce)) #| "git hash-object -t commit --stdin -w").!!
	    println(s"git reset --hard $sha1".!!)
      self ! Stop
    }

    case Stop => {
      context.parent ! Stop
    }
  }

  val receive = receiveCur(0)(_tree, _parent, _timestamp, _difficulty)
}

class Master extends Actor {
  Prototype.roundtrip
  val _tree = ("git write-tree".!!).trim
  val _parent = ("git rev-parse HEAD".!!).trim
  val _difficulty = io.Source.fromFile(new java.io.File("difficulty.txt")).mkString.split("\n")(0) // TODO: This sucks

  val miners = for(i <- 0 until 10) yield context.actorOf(Props(new Miner()(_tree, _parent, currentTime, _difficulty, i)))

  def receive: Receive = {
    case Stop => {println("Stopping"); context.stop(self)}
  }

  miners.map(_ ! TryNext)
}
