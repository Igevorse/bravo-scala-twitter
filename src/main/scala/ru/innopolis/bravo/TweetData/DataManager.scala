package ru.innopolis.bravo.TweetData

import org.joda.time.{DateTime, DateTimeZone}

import collection.mutable.Map

object DataManager {
  var users = scala.collection.mutable.Map[Int, User]()
  var tweets = scala.collection.mutable.Map[Int, Tweet]()


  def getUserByNickname(nickname: String) : Option[User] = {
    users.values.find(_.nickname == nickname)
  }

  def AddUser(email:String, nickname:String, password:String): Boolean = {
    if(NicknameExists(nickname) || EmailExists(email))
      return false

    val id = users.size
    val user : User = new User(id, email, nickname, password)
    users += id -> user

    true
  }

  def AddTweet(text:String, authorId:Int): Boolean = {
    val id = tweets.size
    val tweet : Tweet = new Tweet(id, text, authorId, DateTime.now(DateTimeZone.UTC).getMillis())

    users(authorId).tweetIDs += id
    tweets += id -> tweet

    return true;
  }

  def EditTwit(id: Int, text:String, authorId: Int) : Boolean = {
    if (!tweets.contains(id) || tweets(id).authorId != authorId) {
       return false
    }

    tweets(id).text = text

    true
  }

  def RemoveTwit(tweetId: Int, authorId:Int) : Boolean = {
    if (!tweets.contains(tweetId) || tweets(tweetId).authorId != authorId) {
      return false
    }

    tweets(tweetId).usersLiked
      .foreach(RemoveLike(_, tweetId))
    tweets(tweetId).usersDisliked
      .foreach(RemoveDislike(_, tweetId))

    users(authorId).tweetIDs -= tweetId
    tweets -= tweetId

    true
  }

  def AddLike(userId: Int, tweetId: Int) = {
    if (users.contains(userId) && tweets.contains(tweetId)) {
      if (users(userId).disliked.contains(tweetId))
        RemoveDislike(userId, tweetId)
      users(userId).liked += tweetId
      tweets(tweetId).usersLiked += userId
    }
  }

  def RemoveLike(userId: Int, tweetId: Int) = {
    if (users.contains(userId) && tweets.contains(tweetId)) {
      users(userId).liked -= tweetId
      tweets(tweetId).usersLiked -= userId
    }
  }

  def AddDislike(userId: Int, tweetId: Int) = {
    if (users.contains(userId) && tweets.contains(tweetId)) {
      if (users(userId).liked.contains(tweetId))
        RemoveLike(userId, tweetId)
      users(userId).disliked += tweetId
      tweets(tweetId).usersDisliked += userId
    }
  }

  def RemoveDislike(userId: Int, tweetId: Int) = {
    if (users.contains(userId) && tweets.contains(tweetId)) {
      users(userId).disliked -= tweetId
      tweets(tweetId).usersDisliked -= userId
    }
  }

  def NicknameExists(nick : String) : Boolean = {
    val nicknames = users.values.map(_.nickname)

    nicknames.exists(name => name == nick)
  }

  def EmailExists(email : String) : Boolean = {
    val emails = users.values.map(_.email)

    emails.exists(mail => mail == email)
  }

}
