package com.theoryinpractise

import java.lang.System.{currentTimeMillis => now}
import java.net.URLEncoder
import java.util.Date
import java.util.concurrent.TimeUnit

import com.typesafe.scalalogging.LazyLogging
import dispatch.Defaults._
import dispatch._

import scala.Predef._
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.concurrent.stm._

package object scalatrack {

  def makeCache[K, V](ttl: Long, f: K => V) = new SimpleLookupCache[K, V](ttl, f)

  def dotTrim(l: Int, s: String) = if (s.length >= l) {
    s.substring(0, l - 3).trim + "..."
  } else s

  /**
   * Return a sorted list of issue keys found in git commits
   */
  def issuesInMessage(issueKey: String)(message: String) = {
    val regex = s"\\s?$issueKey-([0-9]+)\\s".r
    regex.findAllIn(message).map(_.trim).toSet.toList.sorted
  }

  implicit object YouTrackIssueOrdering extends Ordering[YouTrackIssue] {
    private def extractKeyAndId(issueKey: String) = {
      val parts = issueKey.split("-")
      (parts.head, parts.last.toInt)
    }

    def compare(x: YouTrackIssue, y: YouTrackIssue): Int = {
      val (xkey, xval) = extractKeyAndId(x.id)
      val (ykey, yval) = extractKeyAndId(y.id)
      xkey.compareTo(ykey) match {
        case 0 => xval.compareTo(yval)
        case c => c
      }
    }
  }

}


package scalatrack {

class SimpleLookupCache[K, V](ttl: Long, f: K => V) {

  private case class Timestamped[T](value: T, ts: Long)

  private val cache = scala.concurrent.stm.Ref(Map[K, Timestamped[V]]())

  private def stale(ts: Long) = (now - ts) > ttl

  private def updateCacheValue(key: K) = {
    atomic {
      implicit txn =>
        val v = f(key)
        cache() = cache() + ((key, Timestamped(v, new Date().getTime)))
        v
    }
  }

  def apply(key: K) = {
    cache.single().get(key) match {
      case Some(Timestamped(v, ts)) if (!stale(ts)) => v
      case _ => updateCacheValue(key)
    }
  }

}

case class WorkItem(url: String, id: String, date: java.util.Date, duration: Int, description: String, author: String)

case class IssueLink(linkType: String, role: String, key: String)

case class YouTrackIssue(id: String, summary: String, assignee: String, state: String, fixFor: String, tags: String,
                         estimated: Int, spent: Int, comments: List[String], links: List[IssueLink],
                         workItems: List[WorkItem])

class YouTrackClient(val href: String, cookies: List[String]) extends LazyLogging {

  import com.ning.http.client.Response

import scala.xml.Node

  def issues(keys: Seq[String]) = keys.par.map(apply)

  def apply(key: String): YouTrackIssue = issueCache(key)

  private val issueCache = makeCache(TimeUnit.MINUTES.toMillis(1), {
    key: String => loadIssueXml(key)
  })

  def loadIssueXml(key: String): YouTrackIssue = {
    loadYouTrackUrl(s"$href/rest/issue/$key", loadYouTrackIssueXml)
  }

  def loadIssueHistory(key: String): Seq[YouTrackIssue] = {
    loadYouTrackUrl(s"$href/rest/issue/$key/history", { r =>
      val issueXml = scala.xml.XML.loadString(r.getResponseBody)
      val issues = ((issueXml \ "issue") map { elem => loadYouTrackIssueNode(elem)}).toList
      issues
    })
  }

  private def attrVal(node: Node, field: String, default: String = "") = node.attribute(field).map(_.text).getOrElse(default)

  private def loadYouTrackUrl[T](href: String, f: Response => T): T = {
    import scala.concurrent.Await
    logger.info(s"Loading youtrack data from $href")
    val request = url(s"${href.trim}").GET
    val authRequest = cookies.foldLeft(request) { (r, c) => r.addHeader("Cookie", c)}
    Await.result(Http(authRequest OK f), 10 seconds);
  }

  private def findWorkItems(issueKey: String): List[WorkItem] = {
    loadYouTrackUrl(s"$href/rest/issue/$issueKey/timetracking/workitem", { r =>
      val xml = scala.xml.XML.loadString(r.getResponseBody)
      val workItems = ((xml \ "workItem") map { elem =>
        WorkItem(
          elem.attribute("url").get.text,
          (elem \ "id").head.text,
          new Date((elem \ "date").head.text.toLong),
          (elem \ "duration").head.text.toInt,
          (elem \ "description").head.text,
          (elem \ "author").head.attribute("login").get.text
        )
      }).toList
      workItems
    })
  }

  def search(search: String): List[YouTrackIssue] = {
    val urlEncodedSearch = URLEncoder.encode(search, "UTF-8")
    loadYouTrackUrl(s"$href/rest/issue?max=1000&filter=$urlEncodedSearch", { r =>
      val issueXml = scala.xml.XML.loadString(r.getResponseBody)
      val issues = ((issueXml \ "issue") map { elem => loadYouTrackIssueNode(elem)}).toList

      issues
    }).sorted
  }

  private def loadYouTrackIssueXml(resp: Response) = {
    val issueXml = scala.xml.XML.loadString(resp.getResponseBody)
    loadYouTrackIssueNode(issueXml)
  }

  private def loadYouTrackIssueNode(issueXml: scala.xml.Node) = {

    val id = issueXml.attribute("id").get.text
    val fields = issueXml \ "field"
    val comments = ((issueXml \ "comment") map (_.attribute("text").get.text)).toList

    val tags = ((issueXml \ "tag") map (_.text)).toList.mkString(" ")

    val links = fields.find(f => attrVal(f, "name") == "links").map(f => f \ "value").getOrElse(List())
      .map(f => IssueLink(attrVal(f, "type"), attrVal(f, "role"), f.text)).toList

    val fieldTuple = fields.map {
      f =>
        (f.attribute("name").map(_.text).getOrElse(""), f.text)
    }

    val fieldMap = Map(fieldTuple: _*)

    YouTrackIssue(
      id,
      dotTrim(80, fieldMap("summary")),
      fieldMap.getOrElse("Assignee", ""),
      fieldMap.getOrElse("State", ""),
      fieldMap.getOrElse("Fix versions", ""),
      tags,
      fieldMap.get("Estimation").map(_.toInt / 60).getOrElse(0),
      fieldMap.get("Spent time").map(_.toInt / 60).getOrElse(0),
      comments,
      links,
      findWorkItems(id)
    )

  }

  def applyCommand(issue: YouTrackIssue, command: String, comment: String): Unit = applyCommand(issue.id, command, comment)

  def applyCommand(issueKey: String, command: String, comment: String, disableNotifications: Boolean = false): Unit = {
    import scala.concurrent.Await
    val request = url(s"$href/rest/issue/$issueKey/execute").POST << Map("command" -> command, "comment" -> comment, "disableNotifications" -> disableNotifications.toString)

    val authRequest = cookies.foldLeft(request) { (r, c) => r.addHeader("Cookie", c)}

    Await.result(Http(authRequest OK as.String), 10 seconds);
  }

}


object YouTrackClient {
  type HostURL = String
  type Username = String
  type Password = String

  def apply(host: HostURL, username: Username, password: Password) = clientCache(host, username, password)

  private val clientCache = makeCache(TimeUnit.HOURS.toMillis(2), buildYouTrackClient)

  private def buildYouTrackClient(key: (String, String, String)): YouTrackClient = {
    import scala.concurrent.Await
    val (host, username, password) = key
    val loginPost = url(s"$host/rest/user/login").POST << Map("login" -> username, "password" -> password)
    val login = Await.result(Http(loginPost), 5.seconds)
    login.getStatusCode match {
      case 200 => {
        val credentials = login.getHeaders("Set-Cookie").filter(c => c.startsWith("jetbrains.charisma.main.security")).toList
        new YouTrackClient(host, credentials)
      }
      case _ => throw new IllegalArgumentException("Invalid credentials")
    }
  }

}

}
