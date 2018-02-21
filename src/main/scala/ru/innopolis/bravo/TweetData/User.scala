package ru.innopolis.bravo.TweetData

class User(val id:Integer, var email:String, var nickname:String, val password:String) {
  var tweetIDs = scala.collection.mutable.Set[Int]()
  var subscriptionsIDs = scala.collection.mutable.Set[Int]()

  var liked = scala.collection.mutable.Set[Int]()
  var disliked = scala.collection.mutable.Set[Int]()

  def Subscribe(otherID : Int) = {
    if(DataManager.users.contains(otherID))
    subscriptionsIDs += otherID
  }

  def Unsubscribe(otherID : Int) = {
    if(DataManager.users.contains(otherID) && subscriptionsIDs.contains(otherID))
      subscriptionsIDs -= otherID
  }

  def Retweet(id : Int) = {
    if(DataManager.tweets.contains(id))
    tweetIDs += id
  }

  def RemoveRetweet(id: Int) = {
    if(DataManager.tweets.contains(id))
      tweetIDs -= id
  }

  def GenerateFeed(size : Int) : List[Tweet] = {
    subscriptionsIDs
      .flatMap(DataManager.users(_).tweetIDs)
      .map(DataManager.tweets)
      .toList
      .sortBy(_.submissionTime)
      .reverse
      .take(size)
  }

}
