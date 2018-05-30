package de.ioswarm.pamina

import java.util.{Base64, Date}
import java.sql.{Time, Timestamp, Date => SQLDate}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import scala.util.matching.Regex

object Attributes extends CollectionTypeEncoder {



}

case class Attribute(
                      name: String
                      , label: String
                      , value: Option[String]
                      , description: Option[String]
                      , attributeType: Type
                      , regex: Option[Regex]
                      , format: Option[String]
                      , attributes: List[Attribute]
                    ) {

  def valueFrom[T](v: T)(implicit enc: TypeEncoder[T]): Attribute = copy(value = enc.encodeOption(v))
  def set[T](v: T)(implicit enc: TypeEncoder[T]): Attribute = valueFrom(v)

}

sealed abstract class Type(val identifier: String)

case object StringType extends Type("string")
case object IntType extends Type("int")
case object LongType extends Type("long")
case object ByteType extends Type("byte")
case object DoubleType extends Type("double")
case object BooleanType extends Type("boolean")

case object DateType extends Type("date")
case object TimeType extends Type("time")
case object DateTimeType extends Type("datetime")

case object BinaryType extends Type("binary")
case object ListType extends Type("list")
case object SetType extends Type("set")
case object MapType extends Type("map")

case object GroupType extends Type("group")
case object RadioGroupType extends Type("radio")
case object CheckboxType extends Type("checkbox")

trait TypeEncoder[T] {
  def encodeEither(t: T): Either[String, String]
  def encodeOption(t: T): Option[String] = encodeEither(t) match {
    case Left(_) => None
    case Right(s) => Some(s)
  }
}

trait TypeDecoder[T] {
  def decodeEither(value: String): Either[String, T]
  def decodeOption(value: String): Option[T] = decodeEither(value) match {
    case Left(_) => None
    case Right(t) => Some(t)
  }
}

trait TypeFormat[T] extends TypeEncoder[T] with TypeDecoder[T]

trait BasicTypeEncoder {

  implicit val stringEncoder: TypeEncoder[String] = new TypeEncoder[String] {
    def encodeEither(t: String): Either[String, String] = Right(t)
  }

  implicit val intEncoder: TypeEncoder[Int] = new TypeEncoder[Int] {
    def encodeEither(t: Int): Either[String, String] = try{
      Right(t.toString)
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

  implicit val longEncoder: TypeEncoder[Long] = new TypeEncoder[Long] {
    override def encodeEither(t: Long): Either[String, String] = try {
      Right(t.toString)
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

  implicit val byteEncoder: TypeEncoder[Byte] = new TypeEncoder[Byte] {
    override def encodeEither(t: Byte): Either[String, String] = try {
      Right(t.toString)
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

  implicit val doubleEncoder: TypeEncoder[Double] = new TypeEncoder[Double] {
    override def encodeEither(t: Double): Either[String, String] = try {
      Right(t.toString)
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

  implicit val booleanEncoder: TypeEncoder[Boolean] = new TypeEncoder[Boolean] {
    override def encodeEither(t: Boolean): Either[String, String] = try {
      Right(t.toString)
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

}

trait DateTypeEncoder extends BasicTypeEncoder {

  implicit val localDateEncoder: TypeEncoder[LocalDate] = ???

  implicit val localTimeEncoder: TypeEncoder[LocalTime] = ???

  implicit val localDateTimeEncoder: TypeEncoder[LocalDateTime] = ???

  implicit val instantEncoder: TypeEncoder[Instant] = ???

  implicit val dateEncoder: TypeEncoder[Date] = ???

  implicit val sqlDateEncoder: TypeEncoder[SQLDate] = ???

  implicit val sqlTimeEncoder: TypeEncoder[Time] = ???

  implicit val sqlTimestampEncoder: TypeEncoder[Timestamp] = ???

}

trait ExtendedTypeEncoder extends DateTypeEncoder {

  implicit def optionTypeEncoder[T](implicit enc: TypeEncoder[T]): TypeEncoder[Option[T]] = new TypeEncoder[Option[T]] {
    override def encodeEither(t: Option[T]): Either[String, String] = t match {
      case Some(tx) => enc.encodeEither(tx)
      case None => Left("NULL")
    }
  }

  implicit val binaryEncoder: TypeEncoder[Array[Byte]] = new TypeEncoder[Array[Byte]] {
    override def encodeEither(t: Array[Byte]): Either[String, String] = try {
      Right(Base64.getEncoder.encodeToString(t))
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

}

trait CollectionTypeEncoder extends ExtendedTypeEncoder {

  implicit def listTypeEncoder[T](implicit enc: TypeEncoder[T]): TypeEncoder[List[T]] = new TypeEncoder[List[T]] {
    override def encodeEither(t: List[T]): Either[String, String] = Right(t.flatMap(enc.encodeOption).mkString("[", ",", "]"))
  }

  implicit def setTypeEncoder[T](implicit enc: TypeEncoder[T]): TypeEncoder[Set[T]] = new TypeEncoder[Set[T]] {
    override def encodeEither(t: Set[T]): Either[String, String] = Right(t.flatMap(enc.encodeOption).mkString("(", ",", ")"))
  }

  implicit def setMapEncoder[K, V](implicit kenc: TypeEncoder[K], venc: TypeEncoder[V]): TypeEncoder[Map[K,V]] = new TypeEncoder[Map[K, V]] {
    override def encodeEither(t: Map[K, V]): Either[String, String] = Right(t.map{ case (k,v) => kenc.encodeOption(k).map(kx => kx+":"+venc.encodeOption(v).getOrElse(")"))}.mkString("{", ",", "}"))
  }

}