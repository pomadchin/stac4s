package com.azavea.stac4s

import io.circe.syntax._
import cats.syntax.either._
import cats.syntax.option._
import geotrellis.vector._
import java.time._

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CatalogSpec extends AnyFunSpec with Matchers {
  import JsonUtils._

  describe("CatalogSpec") {
    it("Create LC8 Catalog") {
      val root =
        StacCatalog(
          id = "landsat-stac",
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
            )
          )
        )

      root.asJson.deepDropNullValues shouldBe getJson("/catalogs/landsat-stac/catalog.json")
      root.asJson.as[StacCatalog].valueOr(throw _) shouldBe root
    }

    it("Create LC8 Collection") {
      val collection = StacCollection(
        stacVersion = "0.9.0",
        stacExtensions = List("eo", "view", "https://example.com/stac/landsat-extension/1.0/schema.json"),
        id = "landsat-8-l1",
        title = "Landsat 8 L1".some,
        description =
          "Landat 8 imagery radiometrically calibrated and orthorectified using gound points and Digital Elevation Model (DEM) data to correct relief displacement.",
        keywords = List("landsat", "earth observation", "usgs"),
        license = Proprietary(), //SPDX("PDDL-1.0"), // PDDL-1.0
        providers = List(
          StacProvider(
            name = "Development Seed",
            description = None,
            roles = List(Processor),
            url = "https://github.com/sat-utils/sat-api".some
          )
        ),
        extent = StacExtent(
          spatial = SpatialExtent(List(TwoDimBbox(-180, -90, 180, 90))),
          temporal = Interval(
            List(TemporalExtent(Instant.parse("2013-06-01T00:56:49.001Z"), Instant.parse("2020-01-01T00:56:49.001Z")))
          )
        ),
        summaries = ().asJsonObject,
        // properties can be anything
        // it is a part where extensions can be
        // at least EO, Label and potentially the layer extension
        properties = Map(
          "collection"         -> "landsat-8-l1".asJson,
          "eo:gsd"             -> 15.asJson,
          "eo:platform"        -> "landsat-8".asJson,
          "eo:instrument"      -> "OLI_TIRS".asJson,
          "view:off_nadir"     -> 0.asJson,
          "view:sun_azimuth"   -> 149.01607154.asJson,
          "view:sun_elevation" -> 59.21424700.asJson,
          "view:azimuth"       -> 0.asJson,
          "eo:bands" -> List(
            Map(
              "name"                -> "B1".asJson,
              "common_name"         -> "coastal".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 0.44.asJson,
              "full_width_half_max" -> 0.02.asJson
            ).asJson,
            Map(
              "name"                -> "B2".asJson,
              "common_name"         -> "blue".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 0.48.asJson,
              "full_width_half_max" -> 0.06.asJson
            ).asJson,
            Map(
              "name"                -> "B3".asJson,
              "common_name"         -> "green".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 0.56.asJson,
              "full_width_half_max" -> 0.06.asJson
            ).asJson,
            Map(
              "name"                -> "B4".asJson,
              "common_name"         -> "red".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 0.65.asJson,
              "full_width_half_max" -> 0.04.asJson
            ).asJson,
            Map(
              "name"                -> "B5".asJson,
              "common_name"         -> "nir".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 0.86.asJson,
              "full_width_half_max" -> 0.03.asJson
            ).asJson,
            Map(
              "name"                -> "B6".asJson,
              "common_name"         -> "swir16".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 1.6.asJson,
              "full_width_half_max" -> 0.08.asJson
            ).asJson,
            Map(
              "name"                -> "B7".asJson,
              "common_name"         -> "swir22".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 2.2.asJson,
              "full_width_half_max" -> 0.22.asJson
            ).asJson,
            Map(
              "name"                -> "B8".asJson,
              "common_name"         -> "pan".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 0.59.asJson,
              "full_width_half_max" -> 0.18.asJson
            ).asJson,
            Map(
              "name"                -> "B9".asJson,
              "common_name"         -> "cirrus".asJson,
              "gsd"                 -> 30.asJson,
              "center_wavelength"   -> 1.37.asJson,
              "full_width_half_max" -> 0.02.asJson
            ).asJson,
            Map(
              "name"                -> "B10".asJson,
              "common_name"         -> "lwir11".asJson,
              "gsd"                 -> 100.asJson,
              "center_wavelength"   -> 10.9.asJson,
              "full_width_half_max" -> 0.8.asJson
            ).asJson,
            Map(
              "name"                -> "B11".asJson,
              "common_name"         -> "lwir2".asJson,
              "gsd"                 -> 100.asJson,
              "center_wavelength"   -> 12.asJson,
              "full_width_half_max" -> 1.asJson
            ).asJson
          ).asJson
        ).asJsonObject,
        links = List(
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
          ),
          StacLink(
            href = "./2014-153/LC81530252014153LGN00.json",
            rel = StacLinkType.Item,
            _type = None,
            title = None
          )
        )
      )

      collection.asJson.deepDropNullValues shouldBe getJson("/catalogs/landsat-stac/landsat-8-l1/catalog.json")
      collection.asJson.as[StacCollection].valueOr(throw _) shouldBe collection
    }

    it("Create LC8 Item") {
      val collection = getJson("/catalogs/landsat-stac/landsat-8-l1/catalog.json").as[StacCollection].valueOr(throw _)

      val item = StacItem(
        id = "LC81530252014153LGN00",
        stacVersion = "0.9.0",
        stacExtensions = List("eo", "view", "https://example.com/stac/landsat-extension/1.0/schema.json"),
        geometry = """
                     |{
                     |    "type": "Polygon",
                     |    "coordinates": [
                     |      [
                     |        [
                     |          72.41047929658137,
                     |          49.7177153248036
                     |        ],
                     |        [
                     |          72.4099,
                     |          49.7178
                     |        ],
                     |        [
                     |          72.42304462193712,
                     |          49.7506571512844
                     |        ],
                     |        [
                     |          72.43942174696257,
                     |          49.79159447737395
                     |        ],
                     |        [
                     |          72.97737644295803,
                     |          51.13630099803396
                     |        ],
                     |        [
                     |          72.99109436013279,
                     |          51.1705911953537
                     |        ],
                     |        [
                     |          73.0069,
                     |          51.2101
                     |        ],
                     |        [
                     |          73.00767938517399,
                     |          51.209985918418724
                     |        ],
                     |        [
                     |          75.5702,
                     |          50.8349
                     |        ],
                     |        [
                     |          75.55241658634094,
                     |          50.79567530937191
                     |        ],
                     |        [
                     |          74.91339206721759,
                     |          49.386185570959974
                     |        ],
                     |        [
                     |          74.8988,
                     |          49.354
                     |        ],
                     |        [
                     |          72.41047929658137,
                     |          49.7177153248036
                     |        ]
                     |      ]
                     |    ]
                     |  }""".stripMargin.parseGeoJson[Polygon],
        bbox = TwoDimBbox(72.27502, 72.27502, 51.36812, 75.67662),
        properties = Map(
          "collection"               -> "landsat-8-l1".asJson,
          "datetime"                 -> "2014-06-02T09:22:02Z".asJson,
          "eo:sun_azimuth"           -> 149.01607154.asJson,
          "eo:sun_elevation"         -> 59.214247.asJson,
          "eo:cloud_cover"           -> 10.asJson,
          "eo:row"                   -> "025".asJson,
          "eo:column"                -> "153".asJson,
          "landsat:product_id"       -> "LC08_L1TP_015032_20180521_20180605_01_T1".asJson,
          "landsat:scene_id"         -> "LC81530252014153LGN00".asJson,
          "landsat:processing_level" -> "L1T".asJson,
          "landsat:tier"             -> "pre-collection".asJson,
          "eo:epsg"                  -> 32643.asJson,
          "eo:instrument"            -> "OLI_TRIS".asJson,
          "eo:off_nadir"             -> 0.asJson,
          "eo:platform"              -> "landsat-8".asJson,
          "eo:gsd"                   -> 15.asJson
        ).asJsonObject,
        links = List(
          StacLink(
            href = "../../catalog.json",
            rel = StacLinkType.StacRoot,
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
            href = "./LC81530252014153LGN00.json",
            rel = StacLinkType.Self,
            _type = None,
            title = None
          ),
          //  { "rel":"alternate", "href": "https://landsatonaws.com/L8/153/025/LC81530252014153LGN00", "type": "text/html"},
          StacLink(
            href = "https://landsatonaws.com/L8/153/025/LC81530252014153LGN0",
            rel = StacLinkType.Alternate,
            _type = `text/html`.some,
            title = None
          )
        ),
        assets = Map(
          "thumbnail" -> StacItemAsset(
            href =
              "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_thumb_large.jpg",
            title = "Thumbnail".some,
            description = "A medium sized thumbnail".some,
            roles = Set(StacAssetRole.Thumbnail),
            _type = `image/jpeg`.some
          ),
          "metadata" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_MTL.txt",
            title = "Original Metadata".some,
            description = "The original MTL metadata file provided for each Landsat scene".some,
            roles = Set(StacAssetRole.Metadata),
            _type = VendorMediaType("mtl").some
          ),
          "B1" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B1.TIF",
            title = "Coastal Band (B1)".some,
            description = "Coastal Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [0],
          ),
          "B2" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B2.TIF",
            title = "Blue Band (B2)".some,
            description = "Blue Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [1],
          ),
          "B3" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B3.TIF",
            title = "Green Band (B3)".some,
            description = "Green Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [2],
          ),
          "B4" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B4.TIF",
            title = "Red Band (B4)".some,
            description = "Red Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [3],
          ),
          "B5" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B5.TIF",
            title = "NIR Band (B5)".some,
            description = "NIR Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [4],
          ),
          "B6" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B6.TIF",
            title = "SWIR (B6)".some,
            description = "SWIR Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [5],
          ),
          "B7" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B7.TIF",
            title = "SWIR Band (B7)".some,
            description = "SWIR Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [6],
          ),
          "B8" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B8.TIF",
            title = "Panchromatic Band (B8)".some,
            description = "Panchromatic Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [7],
          ),
          "B9" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B9.TIF",
            title = "Cirrus Band (B9)".some,
            description = "Cirrus Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [8],
          ),
          "B10" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B10.TIF",
            title = "LWIR Band (B10)".some,
            description = "LWIR Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [9],
          ),
          "B11" -> StacItemAsset(
            href = "http://landsat-pds.s3.amazonaws.com/L8/153/025/LC81530252014153LGN00/LC81530252014153LGN00_B11.TIF",
            title = "LWIR Band (B11)".some,
            description = "LWIR Band Top Of the Atmosphere".some,
            roles = Set.empty,
            _type = `image/tiff`.some
            // "eo:bands": [10],
          )
        ),
        collection = collection.id.some
      )

      item.asJson.deepDropNullValues shouldBe getJson(
        "/catalogs/landsat-stac/landsat-8-l1/2014-153/LC81530252014153LGN00.json"
      )
      item.asJson.as[StacItem].valueOr(throw _) shouldBe item
    }
  }
}
