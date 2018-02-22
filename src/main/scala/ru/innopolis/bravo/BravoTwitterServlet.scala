package ru.innopolis.bravo

import org.scalatra._
import org.scalatra.json._
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write

import scala.util.{Try,Success,Failure}
import org.scalatra.ActionResult
import scala.collection.mutable.TreeSet

// JWT
import pdi.jwt.{Jwt, JwtJson4s, JwtHeader, JwtClaim, JwtAlgorithm}
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._


import TweetData._

//case class User(id:Integer, email:String, nickname:String, password:String)
//case class Tweet(id:Integer, text:String, authorId:Integer, submissionTime:Integer)

class BravoTwitterServlet extends ScalatraServlet with MethodOverride with JacksonJsonSupport {

    val jwtKey = "secretKey"
    val jwtAlgo = JwtAlgorithm.HS256
    val jwtExpiresIn = 3600 // seconds
    val jwtIssuedBy = "Bravo team"
    
    val blacklist:TreeSet[String] = TreeSet()
    
    before() {
        contentType = formats("json")
    }

    get("/") {
        getClaim(request.getHeader("Authorization")) match {
            case Failure(t) => Unauthorized()
            case Success(json) => {
                //  redirect("/feed")
                Ok()
            }
        }
    }

    /** Checks if JWT is valid and returns it's claim **/
    def getClaim(token: String): Try[String] = 
        (Jwt.isValid(token, jwtKey, Seq(jwtAlgo)), Jwt.decode(token, jwtKey, Seq(jwtAlgo))) match {
            case (true, Success(t)) if !(blacklist contains token) => Success(t)
            case _ => Failure(new IllegalArgumentException("Token is not valid"))
    }
    
    /** Adds a user to a database 
      *
      * @param email
      * @param nickname
      * @param password
      */
    put("/register/?") {
        val email = parsedBody \ "email" \\ classOf[JString]
        val nickname = parsedBody \ "nickname" \\ classOf[JString]
        val password = parsedBody \ "password" \\ classOf[JString]
        (email.lift(0), nickname.lift(0), password.lift(0)) match {
            case (email: Some[String], nickname: Some[String], password: Some[String]) if !password.get.isEmpty() => {
                DataManager.AddUser(email.get, nickname.get, PasswordHash.createHash(password.get)) match {
                    case newUserId: Some[Integer] => {
                        println(new String(s"Added new user '${nickname.get}' with password '${password.get}'"))
                        Created(newUserId)
                    }
                    case _ => BadRequest()
                }
            }
            case _ => BadRequest()
        }
    }
    
    /** Creates a JWT token for a user
      *
      * @param nickname
      * @param password
      */
    post("/login/?") {
        val nickname = parsedBody \ "nickname" \\ classOf[JString]
        val password = parsedBody \ "password" \\ classOf[JString]
        (nickname.lift(0), password.lift(0)) match {
            case (nickname: Some[String], password: Some[String])
                if DataManager.getUserByNickname(nickname.get) != None && !password.get.isEmpty() => {
                DataManager.getUserByNickname(nickname.get) match {
                    case Some(user) => {
                        // Generate a JWT token
                        if(PasswordHash.validatePassword(user.passwordHash, PasswordHash.createHash(password.get))) {
                            val claimText = ("nickname" -> user.nickname) ~ ("email" -> user.email) ~ ("id" -> scala.Int.unbox(new java.lang.Integer(user.id)))
                            val claim = JwtClaim(compact(render(claimText))).by(jwtIssuedBy).expiresIn(jwtExpiresIn).startsNow.issuedNow
                            val token = Jwt.encode(JwtHeader(jwtAlgo, "JWT"), claim, jwtKey)
                            Ok("""{"token": """" + token +""""}""")
                        }
                        else
                            BadRequest()
                    }
                    case _ => BadRequest()
                }
            }
            case _ => BadRequest()
        }
    }
    
    /** Logs the user out by adding their token to a blacklist */
    post("/logout/?") {
        getClaim(request.getHeader("Authorization")) match {
            case Failure(t) => Unauthorized()
            case Success(json) => {
                blacklist += request.getHeader("Authorization")
                Ok()
            }
        }
    }
    
    /** Extracts id, nickname and email from JWT token if it is valid 
      *
      * @param authHeader JWT token
      */
    def getAuthInfo(authHeader: String) =
        getClaim(request.getHeader("Authorization")) match {
            case Failure(t) => Unauthorized()
            case Success(jsonString) => {
                val json = parse(jsonString)
                val userId = json \ "id" \\ classOf[JInt]
                val nickname = json \ "nickname" \\ classOf[JString]
                val email = json \ "email" \\ classOf[JString]
                (userId.lift(0), nickname.lift(0), email.lift(0)) match {
                    case (id: Some[BigInt], name: Some[String], mail: Some[String]) => (id.get, name.get, mail.get)
                    case _ => BadRequest()
                }
            }
        }
    
    /** Creates a new tweet **/
    put("/tweet/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                // Parse parseBody, create a new tweet
                val tweetText = parsedBody \ "text" \\ classOf[JString]

                (tweetText.lift(0)) match {
                    case (tweetText: Some[String]) => {
                        DataManager.AddTweet(tweetText.get, userId.toInt)
                        Created()
                    }
                    case _ => BadRequest()
                }
            }
        }
    }
    
    /** Returns a tweet with the given id
      *
      * @param id tweet id
      */
    get("/tweet/:id/?") {
        val id = params.getOrElse("id", halt(400))
        if(DataManager.tweets.contains(id.toInt)) {
            val sampleTweet = DataManager.tweets(id.toInt)
            // This would convert case class to JSON. The same with lists - just pass it to write()
            Ok(write(sampleTweet))
        }
        else
            BadRequest()

        // or BadRequest()
        
    }
    
    
    /** Edits a tweet with the given id
      *
      *  @param id tweet id
      */
    post("/edit/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val tweetId = params.getOrElse("id", halt(400))
                // Parse parseBody, edit tweet
                val tweetText = (parsedBody \ "text").extract[String]

                if(DataManager.tweets.contains(tweetId.toInt)) {

                    DataManager.EditTwit(tweetId.toInt, tweetText, userId.toInt)
                    Ok()

                }
                else
                    BadRequest()

            }
        }
    }
    
    /** Removes a tweet
      *
      * @param id tweet id
      */
    delete("/tweet/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val tweetId = params.getOrElse("id", halt(400)).toInt
                if(DataManager.tweets.contains(tweetId.toInt)) {

                    DataManager.RemoveTwit(tweetId.toInt, userId.toInt)
                    Ok()

                }
                else
                    BadRequest()
            }
        }
    }
    
    /** Subscribe to a user
      *
      * @param id user id
      */
    post("/subscribe/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val anotherUserid = params.getOrElse("id", halt(400))
                if(DataManager.users.contains(userId.toInt)) {
                    DataManager.users(userId.toInt).Subscribe(anotherUserid.toInt)
                    Created()
                }
                else
                    BadRequest()
                
            }
        }
    }
    
    /** Unsubscribe from a user
      *
      * @param id user id
      */
    post("/unsubscribe/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val anotherUserid = params.getOrElse("id", halt(400))
                if(DataManager.users.contains(userId.toInt)) {
                    DataManager.users(userId.toInt).Unsubscribe(anotherUserid.toInt)
                    Created()
                }
                else
                    BadRequest()
                
            }
        }
    }
    
    /** Returns user feed that consists of other users tweets **/
    get("/feed/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                if(DataManager.users.contains(userId.toInt)){
                    Ok(write(DataManager.users(userId.toInt).GenerateFeed(10)))
                }
                else
                    BadRequest()
                
            }
        }
    }
    
    /** Returns tweets of particular user
      *
      * @param id user id
      */
    get("/feed/:id/?") {
        val id = params.getOrElse("id", halt(400))
        if(DataManager.users.contains(id.toInt)){
            Ok(write(DataManager.users(id.toInt).GenerateFeed(10)))
        }
        else
            BadRequest()
    }
    
    /** Retweets the given tweet
      *
      * @param id tweet id
      */
    post("/retweet/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val tweetId = params.getOrElse("id", halt(400))

                if(DataManager.tweets.contains(tweetId.toInt)) {
                    DataManager.users(userId.toInt).Retweet(tweetId.toInt)
                    Ok()
                }
                else {
                    BadRequest()
                }
                
            }
        }
    }
    
    /** Removes the given retweet
      *
      * @param id retweet id
      */
    delete("/retweet/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val tweetId = params.getOrElse("id", halt(400))
                if(DataManager.tweets.contains(tweetId.toInt)) {
                    DataManager.users(userId.toInt).RemoveRetweet(tweetId.toInt)
                    Ok()
                }
                else {
                    BadRequest()
                }
            }
        }
    }
    
    /** Puts a like to the given tweet
      *
      * @param id tweet id
      */
    put("/like/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val tweetId = params.getOrElse("id", halt(400))
                if(DataManager.tweets.contains(tweetId.toInt)) {
                    DataManager.AddLike(userId.toInt, tweetId.toInt)
                    Ok()
                }
                else {
                    BadRequest()
                }
                    
            }
        }
    }
    
    /** Removes a like to the given tweet
      *
      * @param id tweet id
      */
    delete("/like/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val tweetId = params.getOrElse("id", halt(400))
                if(DataManager.tweets.contains(tweetId.toInt)) {
                    DataManager.RemoveLike(userId.toInt, tweetId.toInt)
                    Ok()
                }
                else {
                    BadRequest()
                }
            }
        }
    }
    
    /** Puts a dislike to the given tweet
      *
      * @param id tweet id
      */
    put("/dislike/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val tweetId = params.getOrElse("id", halt(400))
                if(DataManager.tweets.contains(tweetId.toInt)) {
                    DataManager.AddDislike(userId.toInt, tweetId.toInt)
                    Ok()
                }
                else {
                    BadRequest()
                }
                
            }
        }
    }
    
    /** Removes a dislike to the given tweet
      *
      * @param id tweet id
      */
    delete("/dislike/:id/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                val tweetId = params.getOrElse("id", halt(400))
                if(DataManager.tweets.contains(tweetId.toInt)) {
                    DataManager.RemoveDislike(userId.toInt, tweetId.toInt)
                    Ok()
                }
                else {
                    BadRequest()
                }
                
            }
        }
    }
    
    notFound {
        """{"error": "Not found"}"""
    }

    protected implicit val jsonFormats: Formats = DefaultFormats
}


