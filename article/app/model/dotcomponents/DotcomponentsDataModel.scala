package model.dotcomponents

import common.Edition
import conf.Configuration
import controllers.ArticlePage
import model.SubMetaLinks
import model.dotcomrendering.pageElements._
import navigation.NavMenu
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.RequestHeader
import views.support.{CamelCase, GUDateTimeFormat}
import common.Maps.RichMap
import navigation.UrlHelpers.{Footer, Header, SideMenu, getReaderRevenueUrl}
import navigation.ReaderRevenueSite.{Support, SupportContribute, SupportSubscribe}

// We have introduced our own set of objects for serializing data to the DotComponents API,
// because we don't want people changing the core frontend models and as a side effect,
// making them incompatible with Dotcomponents. By having our own set of models, there's
// only one reason for change. exceptions: we do resuse the existing Nav classes right now

case class TagProperties(
    id: String,
    tagType: String,
    webTitle: String,
    twitterHandle: Option[String]
)

case class Tag(
    properties: TagProperties
)

case class Block(
    bodyHtml: String,
    elements: List[PageElement]
)

case class Blocks(
    main: Option[Block],
    body: List[Block]
)

case class ReaderRevenueLink(
  contribute: String,
  subscribe: String,
  support: String
)

case class ReaderRevenueLinks(
  header: ReaderRevenueLink,
  footer: ReaderRevenueLink,
  sideMenu: ReaderRevenueLink
)

case class Meta(
  isImmersive: Boolean,
  isHosted: Boolean,
  shouldHideAds: Boolean,
  hasStoryPackage: Boolean,
  hasRelated: Boolean
)

case class Tags(
  authorIds: Option[String],
  toneIds: Option[String],
  keywordIds: Option[String],
  commissioningDesks: Option[String],
  all: List[Tag]
)

// APPLICATION LEVEL config
// Configuration belongs here if it is to do with infrastructure or application itself

case class DCConfig(
  ajaxUrl: String,
  guardianBaseURL: String,
  sentryHost: Option[String],
  sentryPublicApiKey: Option[String],
  switches: Map[String,Boolean],
  beaconUrl: String
)

// SITE LEVEL config
// Configuration belongs here if it is required for the guardian site, but doesn't change based on the page we're on

case class DCSite(
  nav: NavMenu,
  readerRevenueLinks: ReaderRevenueLinks
)

// PAGE LEVEL config
// Configuration belongs here if it's dependent on the page we're on (the content we're showing)

case class DCContent(
    standfirst: Option[String],
    main: String,
    body: String,
    blocks: Blocks,
    tags: Tags,
    author: String,
    pageId: String,
    pillar: Option[String],
    webPublicationDate: Long,
    webPublicationDateDisplay: String,
    section: Option[String],
    headline: String,
    webTitle: String,
    byline: String,
    contentId: Option[String],
    seriesId: Option[String],
    editionId: String,
    edition: String,
    contentType: Option[String],
    subMetaLinks: SubMetaLinks,
    webURL: String,
    meta: Meta
)

// JSON writers

object Block {
  implicit val writes = Json.writes[Block]
}

object Blocks {
  implicit val writes = Json.writes[Blocks]
}

object TagProperties {
  implicit val writes = Json.writes[TagProperties]
}

object Tag {
  implicit val writes = Json.writes[Tag]
}

object Tags {
  implicit val writes = Json.writes[Tags]
}

object ReaderRevenueLink {
  implicit val writes = Json.writes[ReaderRevenueLink]
}

object ReaderRevenueLinks {
  implicit val writes = Json.writes[ReaderRevenueLinks]
}

object Meta {
  implicit val writes = Json.writes[Meta]
}

object DCContent {
  implicit val writes = Json.writes[DCContent]
}

object DCSite {
  implicit val writes = Json.writes[DCSite]
}

object DCConfig {
  implicit val writes = Json.writes[DCConfig]
}


// The final composite data model

case class DotcomponentsDataModel(
  content: DCContent,
  site: DCSite,
  config: DCConfig,
  version: Int
)

object DotcomponentsDataModel {

  val VERSION = 2

  def fromArticle(articlePage: ArticlePage, request: RequestHeader): DotcomponentsDataModel = {

    val article = articlePage.article

    val bodyBlocks: List[Block] = article.blocks match {
      case Some(bs) => bs.body.map(bb => Block(bb.bodyHtml, bb.dotcomponentsPageElements.toList)).toList
      case None => List()
    }

    val mainBlock: Option[Block] = article.blocks.flatMap(
      _.main.map(bb=>Block(bb.bodyHtml, bb.dotcomponentsPageElements.toList))
    )

    val dcBlocks = Blocks(mainBlock, bodyBlocks)

    val jsConfig = (k: String) => articlePage.getJavascriptConfig.get(k).map(_.as[String])

    val jsPageData = Configuration.javascript.pageData mapKeys { key =>
      CamelCase.fromHyphenated(key.split('.').lastOption.getOrElse(""))
    }

    val switches = conf.switches.Switches.all.filter(_.exposeClientSide).foldLeft(Map.empty[String,Boolean])( (acc, switch) => {
      acc + (CamelCase.fromHyphenated(switch.name) -> switch.isSwitchedOn)
    })

    val allTags = article.tags.tags.map(
      t => Tag(
        TagProperties(
          t.id,
          t.properties.tagType,
          t.properties.webTitle,
          t.properties.twitterHandle
        )
      )
    )

    val navMenu = NavMenu(articlePage, Edition(request))

    val headerReaderRevenueLink: ReaderRevenueLink = ReaderRevenueLink(
      getReaderRevenueUrl(SupportContribute, Header)(request),
      getReaderRevenueUrl(SupportSubscribe, Header)(request),
      getReaderRevenueUrl(Support, Header)(request)
    )

    val footerReaderRevenueLink: ReaderRevenueLink = ReaderRevenueLink(
      getReaderRevenueUrl(SupportContribute, Footer)(request),
      getReaderRevenueUrl(SupportSubscribe, Footer)(request),
      getReaderRevenueUrl(Support, Footer)(request)
    )

    val sideMenuReaderRevenueLink: ReaderRevenueLink = ReaderRevenueLink(
      getReaderRevenueUrl(SupportContribute, SideMenu)(request),
      getReaderRevenueUrl(SupportSubscribe, SideMenu)(request),
      getReaderRevenueUrl(Support, SideMenu)(request)
    )

    val readerRevenueLinks = ReaderRevenueLinks(
      headerReaderRevenueLink,
      footerReaderRevenueLink,
      sideMenuReaderRevenueLink
    )

    val site = DCSite(
      navMenu,
      readerRevenueLinks
    )

    val tags = Tags(
      jsConfig("authorIds"),
      jsConfig("keywordIds"),
      jsConfig("toneIds"),
      jsConfig("commissioningDesks"),
      allTags
    )

    val content = DCContent(
      article.fields.standfirst,
      article.fields.main,
      article.fields.body,
      dcBlocks,
      tags,
      article.tags.contributors.map(_.name).mkString(","),
      article.metadata.id,
      article.metadata.pillar.map(_.toString),
      article.trail.webPublicationDate.getMillis,
      GUDateTimeFormat.formatDateTimeForDisplay(article.trail.webPublicationDate, request),
      article.metadata.section.map(_.value),
      article.trail.headline,
      article.metadata.webTitle,
      article.trail.byline.getOrElse(""),
      jsConfig("contentId"),   // source: content.scala
      jsConfig("seriesId"),    // source: content.scala
      Edition(request).id,
      Edition(request).displayName,
      jsConfig("contentType"),
      article.content.submetaLinks,
      article.metadata.webUrl,
      meta = Meta (
        article.isImmersive,
        article.metadata.isHosted,
        article.content.shouldHideAdverts,
        hasStoryPackage = article.content.hasStoryPackage,
        hasRelated = article.content.showInRelated
      )
    )

    val config = DCConfig(
      Configuration.ajax.url,
      Configuration.site.host,
      jsPageData.get("sentryHost"),
      jsPageData.get("sentryPublicApiKey"),
      switches,
      Configuration.debug.beaconUrl
    )

    DotcomponentsDataModel(
      content,
      site,
      config,
      VERSION
    )

  }

  def toJson(model: DotcomponentsDataModel): JsValue = {

    implicit val DotComponentsDataModelWrites = new Writes[DotcomponentsDataModel] {
      def writes(model: DotcomponentsDataModel) = Json.obj(
        "content" -> model.content,
        "site" -> model.site,
        "config" -> model.config,
        "version" -> model.version
      )
    }

    Json.toJson(model)

  }

  def toJsonString(model: DotcomponentsDataModel): String = {
    Json.prettyPrint(toJson(model))
  }

}
