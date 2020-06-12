package com.azavea.stac4s

import com.azavea.stac4s.extensions.ItemExtension
import com.azavea.stac4s.extensions.layer._
import cats.data.NonEmptyList
import cats.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import geotrellis.vector.{io => _, _}
import io.circe.syntax._

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CatalogLayerSpec extends AnyFunSpec with Matchers {
  import JsonUtils._

  describe("CatalogLayerSpec") {
    it("Create LC8 Layers Catalog") {
      val root =
        StacCatalog(
          id = "landsat-stac-layers",
          stacVersion = "0.9.0",
          stacExtensions = Nil,
          title = "STAC for Landsat data".some,
          description = "STAC for Landsat data",
          links = List(
            StacLink(
              href = "./catalog.json",
              rel = StacLinkType.Self,
              _type = None,
              title = None
            ),
            StacLink(
              href = "./catalog.json",
              rel = StacLinkType.StacRoot,
              _type = None,
              title = None
            ),
            StacLink(
              href = "./landsat-8-l1/catalog.json",
              rel = StacLinkType.Child,
              _type = None,
              title = None
            ),
            StacLink(
              href = "./layers/pa/catalog.json",
              rel = StacLinkType.Child,
              _type = None,
              title = None
            ),
            StacLink(
              href = "./layers/us/catalog.json",
              rel = StacLinkType.Child,
              _type = None,
              title = None
            )
          )
        )

      root.asJson.deepDropNullValues shouldBe getJson("/catalogs/landsat-stac-layers/catalog.json")
      root.asJson.as[StacCatalog].valueOr(throw _) shouldBe root
    }

    it("Create LC8 Layer Catalog") {

      val baseLinkItems = List(
        StacLink(
          href = "../../catalog.json",
          rel = StacLinkType.StacRoot,
          _type = None,
          title = None
        ),
        StacLink(
          href = "../../catalog.json",
          rel = StacLinkType.Parent,
          _type = None,
          title = None
        ),
        StacLink(
          href = "./catalog.json",
          rel = StacLinkType.Self,
          _type = None,
          title = None
        )
      )

      val paLinkItems = List(
        StacLink(
          href = "../../landsat-8-l1/2018-06/LC80140332018166LGN00.json",
          rel = StacLinkType.Item,
          _type = None,
          title = None
        ),
        StacLink(
          href = "../../landsat-8-l1/2018-05/LC80150322018141LGN00.json",
          rel = StacLinkType.Item,
          _type = None,
          title = None
        ),
        StacLink(
          href = "../../landsat-8-l1/2018-07/LC80150332018189LGN00.json",
          rel = StacLinkType.Item,
          _type = None,
          title = None
        )
      )

      val usLinkItems = paLinkItems ++ List(
        StacLink(
          href = "../../landsat-8-l1/2018-06/LC80300332018166LGN00.json",
          rel = StacLinkType.Item,
          _type = None,
          title = None
        )
      )

      val layerUS = StacCatalog(
        stacVersion = "0.9.0",
        stacExtensions = List("eo", "view", "https://example.com/stac/landsat-extension/1.0/schema.json"),
        id = "layer-us",
        title = "Landsat 8 L1".some,
        description = "US STAC Layer",
        links = baseLinkItems ++ usLinkItems
      )

      val layerPA = layerUS.copy(
        id = "layer-pa",
        description = "PA STAC Layer",
        links = baseLinkItems ++ paLinkItems
      )

      layerUS.asJson.deepDropNullValues shouldBe getJson("/catalogs/landsat-stac-layers/layers/us/catalog.json")
      layerUS.asJson.as[StacCatalog].valueOr(throw _) shouldBe layerUS

      layerPA.asJson.deepDropNullValues shouldBe getJson("/catalogs/landsat-stac-layers/layers/pa/catalog.json")
      layerPA.asJson.as[StacCatalog].valueOr(throw _) shouldBe layerPA
    }

    it("Create LC8 Layer Item") {
      val collection =
        getJson("/catalogs/landsat-stac-layers/landsat-8-l1/catalog.json").as[StacCollection].valueOr(throw _)
      val layerUS = getJson("/catalogs/landsat-stac-layers/layers/us/catalog.json").as[StacCatalog].valueOr(throw _)

      // ../../landsat-8-l1/2018-06/LC80300332018166LGN00.json

      val item = StacItem(
        id = "LC80300332018166LGN00",
        stacVersion = "0.9.0",
        stacExtensions = List("eo", "view", "layers", "https://example.com/stac/landsat-extension/1.0/schema.json"),
        geometry = """
                     | {
                     |    "type": "Polygon",
                     |    "coordinates": [
                     |      [
                     |        [
                     |          -100.84368079413701,
                     |          39.97210491033466
                     |        ],
                     |        [
                     |          -98.67492641719046,
                     |          39.54833037653145
                     |        ],
                     |        [
                     |          -99.23946071016417,
                     |          37.81370881408165
                     |        ],
                     |        [
                     |          -101.40560438472555,
                     |          38.24476872678675
                     |        ],
                     |        [
                     |          -100.84368079413701,
                     |          39.97210491033466
                     |        ]
                     |      ]
                     |    ]
                     |  }""".stripMargin.parseGeoJson[Polygon],
        bbox = TwoDimBbox(-101.40793, 37.81084, -98.6721, 39.97469),
        properties = Map(
          "collection"               -> "landsat-8-l1".asJson,
          "datetime"                 -> "2018-06-15T17:18:03.154639+00:00".asJson,
          "eo:sun_azimuth"           -> 125.5799919.asJson,
          "eo:sun_elevation"         -> 66.54407242.asJson,
          "eo:cloud_cover"           -> 0.asJson,
          "eo:row"                   -> "033".asJson,
          "eo:column"                -> "030".asJson,
          "landsat:product_id"       -> "LC08_L1TP_030033_20180615_20180703_01_T1".asJson,
          "landsat:scene_id"         -> "LC80300332018166LGN00".asJson,
          "landsat:processing_level" -> "L1TP".asJson,
          "landsat:tier"             -> "T1".asJson,
          "eo:epsg"                  -> 32614.asJson,
          "eo:instrument"            -> "OLI_TIRS".asJson,
          "eo:off_nadir"             -> 0.asJson,
          "eo:platform"              -> "landsat-8".asJson,
          "eo:gsd"                   -> 15.asJson
        ).asJsonObject.deepMerge(
          LayerItemExtension(NonEmptyList.of(NonEmptyString.unsafeFrom(layerUS.id))).asJsonObject
        ), // layer extension
        links = List(
          StacLink(
            href = "./LC80300332018166LGN00.json",
            rel = StacLinkType.Self,
            _type = None,
            title = None
          ),
          StacLink(
            href = "../catalog.json",
            rel = StacLinkType.Parent,
            _type = None,
            title = None
          ),
          StacLink(
            href = "../catalog.json",
            rel = StacLinkType.Collection,
            _type = None,
            title = None
          ),
          StacLink(
            href = "../../catalog.json",
            rel = StacLinkType.StacRoot,
            _type = None,
            title = None
          )
        ),
        assets = Map(
          "index" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/index.html",
            title = "HTML index page".some,
            description = None,
            roles = Set.empty,
            _type = `text/html`.some
          ),
          "thumbnail" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_thumb_large.jpg",
            title = "Thumbnail image".some,
            description = None,
            roles = Set(StacAssetRole.Thumbnail),
            _type = `image/jpeg`.some
          ),
          "B1" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B1.TIF",
            title = "Band 1 (coastal)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(0)).asJsonObject
          ),
          "B2" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B2.TIF",
            title = "Band 2 (blue)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(1)).asJsonObject
          ),
          "B3" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B3.TIF",
            title = "Band 3 (green)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(2)).asJsonObject
          ),
          "B4" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B4.TIF",
            title = "Band 4 (red)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(3)).asJsonObject
          ),
          "B5" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B5.TIF",
            title = "Band 5 (nir)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(4)).asJsonObject
          ),
          "B6" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B6.TIF",
            title = "Band 6 (swir16)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(5)).asJsonObject
          ),
          "B7" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B7.TIF",
            title = "Band 7 (swir22)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(6)).asJsonObject
          ),
          "B8" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B8.TIF",
            title = "Band 8 (pan)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(7)).asJsonObject
          ),
          "B9" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B9.TIF",
            title = "Band 9 (cirrus)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(8)).asJsonObject
          ),
          "B10" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B10.TIF",
            title = "Band 10 (lwir)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(9)).asJsonObject
          ),
          "B11" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_B11.TIF",
            title = "Band 11 (lwir)".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some,
            Map("eo:bands" -> List(10)).asJsonObject
          ),
          "ANG" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_ANG.txt",
            title = "Angle coefficients file".some,
            description = None,
            roles = Set.empty,
            _type = `text/plain`.some
          ),
          "MTL" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_MTL.txt",
            title = "original metadata file".some,
            description = None,
            roles = Set.empty,
            _type = `text/plain`.some
          ),
          "BQA" -> StacItemAsset(
            href =
              "https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/030/033/LC08_L1TP_030033_20180615_20180703_01_T1/LC08_L1TP_030033_20180615_20180703_01_T1_BQA.TIF",
            title = "Band quality data".some,
            description = None,
            roles = Set.empty,
            _type = `image/tiff`.some
          )
        ),
        collection = collection.id.some
      )

      ItemExtension[LayerItemExtension].getExtensionFields(item) shouldBe LayerItemExtension(
        NonEmptyList.fromListUnsafe(List("layer-us") map { NonEmptyString.unsafeFrom })
      ).valid
      item.asJson.deepDropNullValues shouldBe getJson(
        "/catalogs/landsat-stac-layers/landsat-8-l1/2018-06/LC80300332018166LGN00.json"
      )
      item.asJson.as[StacItem].valueOr(throw _) shouldBe item
    }
  }
}
