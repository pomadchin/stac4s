package com.azavea.stac4s

import cats.Eq
import cats.syntax.either._
import cats.syntax.functor._
import io.circe._
import io.circe.syntax._

sealed trait Bbox {
  val xmin: Double
  val ymin: Double
  val xmax: Double
  val ymax: Double
  val toList: List[Double]
}

final case class TwoDimBbox(xmin: Double, ymin: Double, xmax: Double, ymax: Double) extends Bbox {
  val toList = List(xmin, ymin, xmax, ymax)
}

final case class ThreeDimBbox(
    xmin: Double,
    ymin: Double,
    zmin: Double,
    xmax: Double,
    ymax: Double,
    zmax: Double
) extends Bbox {
  val toList = List(xmin, ymin, zmin, xmax, ymax, zmax)
}

object TwoDimBbox {

  implicit val eqTwoDimBbox: Eq[TwoDimBbox] = Eq.fromUniversalEquals

  implicit val decoderTwoDBox: Decoder[TwoDimBbox] =
    Decoder.decodeList[Double].emap {
      case twodim if twodim.length == 4 =>
        Either.right(TwoDimBbox(twodim(0), twodim(1), twodim(2), twodim(3)))
      case other =>
        Either.left(
          s"Incorrect number of values for 2d box - found ${other.length}, expected 4"
        )
    }

  implicit val encoderTwoDimBbox: Encoder[TwoDimBbox] = _.toList.asJson
}

object ThreeDimBbox {

  implicit val eqThreeDimBbox: Eq[ThreeDimBbox] = Eq.fromUniversalEquals

  implicit val decoderThreeDimBox: Decoder[ThreeDimBbox] =
    Decoder.decodeList[Double].emap {
      case threeDim if threeDim.length == 6 =>
        Either.right(
          ThreeDimBbox(
            threeDim(0),
            threeDim(1),
            threeDim(2),
            threeDim(3),
            threeDim(4),
            threeDim(5)
          )
        )
      case other =>
        Either.left(
          s"Incorrect number of values for 2d box - found ${other.length}, expected 4"
        )
    }

  implicit val encoderThreeDimBbox: Encoder[ThreeDimBbox] = _.toList.asJson
}

object Bbox {

  implicit val encoderBbox: Encoder[Bbox] = {
    case two: TwoDimBbox     => two.asJson
    case three: ThreeDimBbox => three.asJson
  }

  implicit val decoderBbox: Decoder[Bbox] = Decoder[TwoDimBbox].widen or Decoder[ThreeDimBbox].widen

  implicit val eqBbox: Eq[Bbox] = Eq.fromUniversalEquals

}
