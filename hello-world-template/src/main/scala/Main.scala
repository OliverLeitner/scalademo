import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.util.Random

import scala.collection.mutable.ListBuffer

// https://doc.akka.io/docs/akka-http/current/index.html
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import scala.io.StdIn

import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
// import DefaultJsonProtocol._ // if you don't supply your own Protocol (see below)
// import MyUserJsonProtocol._

import java.sql.{Connection,DriverManager}

/**
 * A Scala JDBC connection example by Alvin Alexander,
 * https://alvinalexander.com
 */
object ScalaJdbcConnectSelect {

  def getDataFromDb(): List[Customer] = {
    // connect to the database named "mysql" on the localhost
    val driver = "com.mysql.cj.jdbc.Driver"
    val url = "jdbc:mysql://localhost/classicmodels"
    val username = "demo"
    val password = "123"

    val customerList = new ListBuffer[Customer]()

    // there's probably a better way to do this
    var connection:Connection = null

    try {
      // make the connection
      Class.forName(driver)
      connection = DriverManager.getConnection(url, username, password)

      // create the statement, and run the select query
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery("SELECT * FROM customers")
      while ( resultSet.next() ) {
        val customerNumber = resultSet.getBigDecimal("customerNumber")
        val customerName = resultSet.getString("customerName")
        // println("customerNumber, customerName = " + customerNumber + ", " + customerName)
        customerList += Customer(customerNumber, customerName)
      }
      return customerList.toList
    } catch {
      case e => e.printStackTrace
    }
    connection.close()
    return customerList.toList
  }

}

case class User(name: String, age: Int)
case class Customer(customerNumber: BigDecimal, customerName: String)

object MyCustomerJsonProtocol extends DefaultJsonProtocol {
  implicit object CustomerJsonFormat extends RootJsonFormat[Customer] {
    def write(c: Customer) =
      JsArray(
        JsString(c.customerName),
        JsNumber(c.customerNumber)
      )

    def read(value: JsValue) = value match {
      case JsArray(
            Vector(
              JsString(customerName),
              JsNumber(customerNumber)
            )
          ) =>
        new Customer(customerNumber, customerName)
      case _ => deserializationError("Customer not found")
    }
  }
}

/*object MyUserJsonProtocol extends DefaultJsonProtocol {
  implicit object UserJsonFormat extends RootJsonFormat[User] {
    def write(c: User) =
      JsArray(
        JsString(c.name),
        JsNumber(c.age)
      )

    def read(value: JsValue) = value match {
      case JsArray(
            Vector(
              JsString(name),
              JsNumber(age)
            )
          ) =>
        new User(name, age.toInt)
      case _ => deserializationError("Color expected")
    }
  }
}*/

// import spray.json._
// import DefaultJsonProtocol._ // if you don't supply your own Protocol (see below)
// import MyUserJsonProtocol._ // we need to import our own protocol eventhough we are in the same file...
import MyCustomerJsonProtocol._ // we need to import our own protocol eventhough we are in the same file...

// base webserver to serve up stuff, lets say, json
object HttpServerRoutingMinimal {

  def main(args: Array[String]): Unit = {

    /*val userBase = List(
      User("Travis", 28),
      User("Kelly", 33),
      User("Jennifer", 44),
      User("Dennis", 23)
    )*/

    val customerBase: List[Customer] = ScalaJdbcConnectSelect.getDataFromDb()

    implicit val system = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext

    val route =
      path("hello") {
        get {
          complete(
            HttpEntity(
              ContentTypes.`application/json`,
              customerBase.toJson.toString()
            )
          )
        }
      }

    val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(route)

    println(
      s"Server online at http://scala.localnet:8080/\nPress RETURN to stop..."
    )
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}


object CustomerID {

  def apply(name: String) = s"$name--${Random.nextLong}"

  def unapply(customerID: String): Option[String] = {
    val stringArray: Array[String] = customerID.split("--")
    if (stringArray.tail.nonEmpty) Some(stringArray.head) else None
  }
}

class Email(val username: String, val domainName: String)

object Email {
  def fromString(emailString: String): Option[Email] = {
    emailString.split('@') match {
      case Array(a, b) => Some(new Email(a, b))
      case _           => None
    }
  }
}

case class Circle(radius: Double) {
  import Circle._
  def area: Double = calculateArea(radius)
}

object Circle {
  private def calculateArea(radius: Double): Double = Pi * pow(radius, 2.0)
}

case class WeeklyWeatherForecast(temperatures: Seq[Double]) {
  private def convertCtoF(temp: Double) =
    temp * 1.8 + 32 // local funct with the conversion
  def forecastInFahrenheit: Seq[Double] =
    temperatures.map(convertCtoF) // <-- passing the method convertCtoF
}

abstract class AbsIterator {
  type T // generic type t
  def hasNext: Boolean //nextval true or false, if next false, lastval
  def next(): T // generic type t, its gonna be a char (counting single chars in string) anywho, could be ... a type
}

// the actual iterator extending the base class absiterator...
class StringIterator(s: String) extends AbsIterator {
  type T = Char // set type to char
  private var i = 0 // local iterator var
  def hasNext =
    i < s.length // our hasnext boolean dynamically from the iteration
  def next() = { // the next function that outputs a char
    val ch =
      s charAt i // s search? charAt (character at position?) i would be our iterator
    i += 1 // iterator iterating...
    ch // output the singlechar "to be println'd"
  }
}

// the trait for later use in main, i think
trait RichIterator extends AbsIterator {
  // our foreach "override", works on the given string f
  def foreach(f: T => Unit): Unit = while (hasNext) f(next())
}

// there we go, our class definition for the call ("Scala" being characterized)
class RichStringIter extends StringIterator("Scala") with RichIterator

abstract class A {
  val message: String
}
class B extends A {
  val message = "I'm an instance of class B"
}
trait C extends A {
  def loudMessage = message.toUpperCase()
}
class D extends B with C

trait Pet {
  val name: String // public var "name" for direct access from println in main
}

class Cat(val name: String) extends Pet
class Dog(val name: String) extends Pet

trait Interface {
  def operation(adjust: Int): Int;
}

trait PointMagic[A, B] {
  def x: B;
  def y: B;
  def runmagic(): A;
}

object MyDummyObject {
  def construct(someVal: Int): Interface = {
    class Impl(var current: Int) extends Interface {
      def operation(adjust: Int): Int = {
        current += adjust
        return current
      }
    }
    var myImpl = new Impl(someVal)
    return myImpl
  }
}

object MyDummyPoint {
  def construct(x: Int, y: Int): PointMagic[String, Int] = {
    var myPoint = new Point(x, y)
    return myPoint
  }
}

class Point(var x: Int, var y: Int) extends PointMagic[String, Int] {
  /*private var _x = 0
  private var _y = 0*/
  private val bound = 100 // 100 is hard boundary

  /*def x = x_
  def y = y_*/

  /*def x_=(valX: Int): Unit = { setCoordinates(valX, y) }
  def y_=(valY: Int): Unit = { setCoordinates(x, valY) }

  def setCoordinates(val1: Int, val2: Int) {
    if (val1 < bound && val2 < bound) printf("val1: %s, val2: %s\n", val1, val2)
    else printWarning
  }*/

  def Point(newX: Int, newY: Int) {
    println("we are running on auto")
  }

  override def runmagic(): String = {
    // var newX: String = x.toString
    return f"x: $x%s, y: $y%s"
  }

  /*def x = _x // gets the x provided
  def x_=(newValue: Int): Unit = { // boundary checker...
    // if (newValue < bound) _x = newValue else printWarning
    setCoordinates(newValue, 0)
  }*/

  /*def y = _y
  def y_=(newValue: Int): Unit = { // second boundary checker...
    // if (newValue < bound) _y = newValue else printWarning
    setCoordinates(0, newValue)
  }*/

  private def printWarning = println("WARNING: Out of bounds")
}

/*class Point(var x: Int, var y: Int) {

  def move(dx: Int, dy: Int): Unit = {
    x = x + dx
    y = y + dy
  }

  override def toString: String =
    s"($x, $y)"
}*/

/*class Greeter(prefix: String, suffix: String) {
  def greet(name: String): Unit =
    println(prefix + name + suffix)
}*/

trait Greeter {
  def greet(name: String): Unit =
    println("Hello, " + name + "!")
}

class DefaultGreeter extends Greeter // has everything of greeter trait...

// overrides greeter trait functionality
class CustomizableGreeter(prefix: String, postfix: String) extends Greeter {
  override def greet(name: String): Unit = {
    println(prefix + name + postfix)
  }
}

class UrlBuilder(var ssl: Boolean, var domainName: String) {
  def urlBuilder(): (String, String) => String = {
    val schema = if (ssl) "https://" else "http://"
    (endpoint: String, query: String) => s"$schema$domainName/$endpoint?$query"
  }
}

// will a command start a rebuild?, yes
object MyMain/* extends App*/ {
  // println("""|Hello, Austria!""".stripMargin)
  // println(200 + 1000000)

  // def name: String = System.getProperty("""|user.name""".stripMargin)
  // println("""|Hello, """.stripMargin + name + "!")

  /*val greeter = new Greeter("""|Hello, """.stripMargin, "!")
  greeter.greet("""|Scala developer""".stripMargin)*/

  // val greeter = new DefaultGreeter()
  // greeter.greet("Scala developer") // Hello, Scala developer!

  // val customGreeter = new CustomizableGreeter("How are you, ", "?")
  // customGreeter.greet("Scala developer") // How are you, Scala developer?

  /*val list: List[Any] = List(
    "a string",
    732, // an integer
    'c', // a character
    true, // a boolean value
    () => "an anonymous function returning a string"
  )

  list.foreach(element => println(element))

  val x: Long = 987654321
  val y: Float =
    x // 9.8765434E8 (note that some precision is lost in this case)

  val face: Char = '☺'
  val number: Int = face // 9786*/

  /*val point1 = new Point(2, 3)
  point1.move(2, 6) // actually move around
  println(point1.x) // 2
  println(point1) // prints (2, 3)*/
  // val point1 =
  //  new Point() // class call with default constructor values and without an ()
  // point1.x = 50 // sets the x coordinate
  // point1.y = 70 // prints the warning
  // point1.checkBoundaries
  // point1(100, 99)

  // var dummyObject = MyDummyObject.construct(200)
  // println(dummyObject.operation(1000))

  // var dummyPoint = MyDummyPoint.construct(100, 200) // add vals
  // println(dummyPoint.runmagic()) // return something... lets check the val content

  // create items of obj
  /*val dog = new Dog("Harry")
  val cat = new Cat("Sally")*/

  // create an empty array of type Pet
  // val animals = ArrayBuffer.empty[Pet]

  // add our pet items to the pet array
  /*animals.append(dog)
  animals.append(cat)*/

  // animals.foreach(pet => println(pet.name)) // Prints Harry Sally (item.name prop)

  /*val planets =
    List(
      ("Mercury", 57.9),
      ("Venus", 108.2),
      ("Earth", 149.6),
      ("Mars", 227.9),
      ("Jupiter", 778.3)
    ) // end of val planets definition*/

  // start of val planets operation
  /*planets.foreach {
    case ("Earth", distance) =>
      println(s"Our planet is $distance million kilometers from the sun")
    case _ => println("not our planet") // switch case default:
  }*/

  /** would be someArray.forEach((item, idx) => { switch(item.prop) { case1: doseomething, break casN..., default: } })
    */

  /*val numPairs = List((2, 5), (3, -7), (20, 56))
  for ((a, b) <- numPairs) { // for ((let a, let b) in numPairs) ... maybe: numPairs...toarraythingy.map((a, b)...)
    println(a * b)
  }*/

  /*val d = new D // d takes from b with uppercase from c
  println(d.message) // I'm an instance of class B
  println(d.loudMessage) // I'M AN INSTANCE OF CLASS B*/

  /*val richStringIter = new RichStringIter
  richStringIter.foreach(println)*/

  val salaries = Seq(20000, 70000, 40000)
  salaries.map(_ * 2).foreach(println)

  /*var temperature = WeeklyWeatherForecast(Seq(-05.00, 10.00, 28.50, 35.50))
  temperature.forecastInFahrenheit.map(println(_))*/

  /** class urlBuilder (ssl: Boolean, domainName: String) {
    *  def urlBuilder(): (String, String) => String = {
    */
  /*val domainName = "www.example.com"
  def getURL = new UrlBuilder(true, domainName).urlBuilder()
  val endpoint = "users"
  val query = "id=1"
  val url =
    getURL(endpoint, query) // "https://www.example.com/users?id=1": String
  println(url)*/

  /*val circle1 = Circle(5.0)
  println(circle1.area)*/

  val scalaCenterEmail = Email.fromString("scala.center@epfl.ch")
  scalaCenterEmail match {
    case Some(email) => println(s"""Registered an email
       |Username: ${email.username}
       |Domain name: ${email.domainName}
     """.stripMargin)
    case None        => println("Error: could not parse email")
  }

  val customer1ID = CustomerID("Sukyoung") // Sukyoung--23098234908
  customer1ID match {
    case CustomerID(name) => println(name) // prints Sukyoung
    case _                => println("Could not extract a CustomerID")
  }

  val customer2ID = CustomerID(
    "TEST--asdfasdfasdf"
  ) // new value "Nico" being operated on applied...
  val CustomerID(name) =
    customer2ID // the "name" in customerid becomes customer2id
  println(name) // prints customerid name (applied)

  /*val userBase = List(
    User("Travis", 28),
    User("Kelly", 33),
    User("Jennifer", 44),
    User("Dennis", 23)
  )

  println(userBase.toJson)*/

  /*val twentySomethings =
    for (user <- userBase if user.age >= 20 && user.age < 30)
      yield user.name // i.e. add this to a list

  twentySomethings.foreach(println) // prints Travis Dennis*/

  /*def foo(n: Int, v: Int) =
    for (
      i <- 0 until n;
      j <- 0 until n if i + j == v
    )
      println(s"($i, $j)".toJson)*/
  // yield (i, j)

  /*foo(10, 10) // foreach { case (i, j) =>
    println(
      s"($i, $j) "
    ) // prints (1, 9) (2, 8) (3, 7) (4, 6) (5, 5) (6, 4) (7, 3) (8, 2) (9, 1)
  }*/
}
