package ru.innopolis.bravo.TweetData

class Tweet(id:Integer, var text:String, val authorId:Integer, val submissionTime:Long){
  var usersLiked = scala.collection.mutable.Set[Int]()
  var usersDisliked = scala.collection.mutable.Set[Int]()
}
