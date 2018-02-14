package ru.innopolis.bravo

import org.scalatra.test.scalatest._

class BravoTwitterServletTests extends ScalatraFunSuite {

  addServlet(classOf[BravoTwitterServlet], "/*")

  test("GET / on BravoTwitterServlet should return status 200"){
    get("/"){
      status should equal (200)
    }
  }

}
