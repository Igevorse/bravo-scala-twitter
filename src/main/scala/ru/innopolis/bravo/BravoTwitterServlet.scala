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


case class User(id:Integer, email:String, nickname:String, password:String)
case class Tweet(id:Integer, text:String, authorId:Integer, submissionTime:Integer)

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
                // TODO
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
    
    /** Checks if email is already registered **/
    def isEmailCorrect(email: String): Boolean = {
        true
    }
    
    /** Gets the user with the given nickname from the database **/
    def getUserByNickname(nickname: String): Option[User] = {
        // TODO
        return Some(new User(0, "m@m.ru", "nick", "123456"))
        // or None if not exists
    }
    
    /** Generates a new ID for a new user **/
    def generateNewId() = 5
    
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
            case (email: Some[String], nickname: Some[String], password: Some[String]) if isEmailCorrect(email.get) && getUserByNickname(nickname.get) == None && !password.get.isEmpty() => {
                println(new String(s"Added new user '${nickname.get}' with password '${password.get}'"))
                val user = new User(generateNewId(), email.get, nickname.get, password.get)
                // TODO: add user to a database
                Created()
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
            case (nickname: Some[String], password: Some[String]) if getUserByNickname(nickname.get) != None && !password.get.isEmpty() => {
                getUserByNickname(nickname.get) match {
                    case Some(user) => {
                        // Generate a JWT token
                        val claimText = ("nickname" -> user.nickname)  ~ ("email" -> user.email) ~("id" -> scala.Int.unbox(new java.lang.Integer(user.id)) )
                        val claim = JwtClaim(compact(render(claimText))).by(jwtIssuedBy).expiresIn(jwtExpiresIn).startsNow.issuedNow
                        val token = Jwt.encode(JwtHeader(jwtAlgo, "JWT"), claim, jwtKey)
                        Ok("""{"token": """"+token+""""}""")
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
            case (userId: Integer, nickname: String, email: String) => {
                // Parse parseBody, create a new tweet
            
                
                Created()
            }
        }
    }
    
    /** Returns a tweet with the given id
      *
      * @param id tweet id
      */
    get("/tweet/:id/?") {
        val id = params.getOrElse("id", halt(400))
        // TODO
        val sampleTweet = new Tweet(3, "Some text", 0, 19376223)
        // This would convert case class to JSON. The same with lists - just pass it to write()
        Ok(write(List(sampleTweet, sampleTweet)))
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

                Ok()
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
                val tweetId = params.getOrElse("id", halt(400))
                // TODO: remove tweet
                
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
                // TODO: Subscribe
                
                Created()
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
                // TODO: Unubscribe
                
            }
        }
    }
    
    /** Returns user feed that consists of other users tweets **/
    get("/feed/?") {
        getAuthInfo(request.getHeader("Authorization")) match {
            case ar: ActionResult => ar
            case (userId: BigInt, nickname: String, email: String) => {
                // TODO: generate feed
                
            }
        }
    }
    
    /** Returns tweets of particular user
      *
      * @param id user id
      */
    get("/feed/:id/?") {
        val id = params.getOrElse("id", halt(400))
        // TODO
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
                // TODO: retweet
                
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
                // TODO: remove retweet
                
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
                // TODO: Like
                    
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
                // TODO: remove like
                
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
                // TODO: Dislike
                
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
                // TODO: Remove a dIslike
                
            }
        }
    }
    
    notFound {
        """{"error": "Not found"}"""
    }

    protected implicit val jsonFormats: Formats = DefaultFormats
}


