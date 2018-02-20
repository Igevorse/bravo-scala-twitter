package ru.innopolis.bravo.TweetData

import org.joda.time.{DateTime, DateTimeZone}

import collection.mutable.Map

object DataManager {
  var users = scala.collection.mutable.Map[Int, User]()
  var tweets = scala.collection.mutable.Map[Int, Tweet]()



  def AddUser(email:String, nickname:String, password:String): Boolean = {
    if(!NicknameExists(nickname) && !EmailExists(email))
      return false

    val id = users.size
    val user : User = new User(id, email, nickname, password)
    users += id -> user

    return true
  }

  def AddTweet(text:String, authorId:Integer): Boolean = {
    val id = tweets.size
    val tweet : Tweet = new Tweet(id, text, authorId, DateTime.now(DateTimeZone.UTC).getMillis())

    tweets += id -> tweet

    return true;
  }

  def RemoveTwit : Boolean = {

    return true
  }



  def NicknameExists(nick : String) : Boolean = {
    val nicknames = users.values.map(_.nickname)

    return nicknames.exists(name => name == nick)
  }

  def EmailExists(email : String) : Boolean = {
    val nicknames = users.values.map(_.email)

    return nicknames.exists(mail => mail == email)
  }

}
