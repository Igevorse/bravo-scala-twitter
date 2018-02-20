package ru.innopolis.bravo.TweetData

class User(id:Integer, var email:String, var nickname:String, password:String) {


  def Subscribe(otherID : Int) = {

  }

  def Unsubscribe(otherID : Int) = {

  }

  def GenerateFeed(size : Int) : Array[Tweet] = {
    return Array.empty[Tweet]
  }

}
