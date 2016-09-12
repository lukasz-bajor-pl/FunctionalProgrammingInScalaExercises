package pers.fpinscala.chapter9

import pers.fpinscala.chapter9.{JSON, Parsers}
import pers.fpinscala.chapter9.JSON.JNull
import pers.fpinscala.chapter9.JSON.JNumber

/**
  * Created by CEX on 12/09/2016.
  */

object MyJSONParser {
  //9.9
  def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    val jNull = string("null").map(_ => JNull)

    val jNumber: Parser[JNumber] = {
      def listIntToNumber(l: List[Int]): Double = l.foldRight(0)({ case (nI, res) => res*10+nI })

      or(many1(digit).map(l => listIntToNumber(l)),
        many1(digit).product(char('.')).product(many1(digit)).map({ case ((integral, dot), fraction) =>
          listIntToNumber(fraction) / 10.0d + listIntToNumber(integral) })).map(d => new JSON.JNumber(d))
    }

    val jString: Parser[JSON.JString] = regex("\\\"\\s+\\\"".r).map(new JSON.JString(_))
    val jBool: Parser[JSON.JBool] = or(string("true"), string("false")).map(s => new JSON.JBool(s.toBoolean))
    def jArray: Parser[JSON.JArray] = char('[').product(many(jsonParser(P))).product(char(']')).map({
      case ((openingBracket, array), closingBracket) => new JSON.JArray(array.toIndexedSeq)
    })

    def jsonKey: Parser[String] = jString.map(jS => jS.get.substring(1, jS.get.length-1)) // Attribute name
    def jsonAttribute: Parser[(String, JSON)] = product(jsonKey, jsonParser(P))
    def jObject: Parser[JSON.JObject] = char('{').product(many(jsonAttribute)).product(char('}')).map({
      case ((opening, attributes), closing) => new JSON.JObject(attributes.toMap)
    })

    List[Parser[JSON]](jNumber,jString,jBool,jArray,jObject).foldRight(jNull:Parser[JSON])({ case (n, current) => current.or(n) })
  }
}
