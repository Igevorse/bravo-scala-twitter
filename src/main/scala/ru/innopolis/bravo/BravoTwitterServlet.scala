package ru.innopolis.bravo

import org.scalatra._
import org.json4s._
import org.scalatra.json._

class BravoTwitterServlet extends ScalatraServlet with MethodOverride with JacksonJsonSupport{
  before() {
    contentType = formats("json")
  }

  get("/") {
    views.html.hello()
  }

  notFound {
    "Not found"
  }

  protected implicit val jsonFormats: Formats = DefaultFormats
}


