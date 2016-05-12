package com.kunyan

import java.net.{SocketException, SocketTimeoutException}
import java.util.regex.Pattern

import com.kunyan.util.WeiBo
import org.jsoup.{HttpStatusException, Jsoup}

import scala.collection.immutable.HashSet
import scala.io.Source

/**
 * Created by lcm on 2016/5/10.
 */
object Controller {

  val ua = "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36 QIHU 360SE se.360.cn"

  var ips: Array[String] = null

  def main(args: Array[String]) {

    //保存微博的信息
    var setUid = new HashSet[Array[String]]


    //切换代理ip
    ips = getIPs
    changIP()

    //读取源数据文件
    for (line <- Source.fromFile(args(0))("UTF-8").getLines) {

      val lineArr = line.split("\t")

      if (lineArr.size == 8) {

        //保存微博的信息
        if (lineArr(3).contains("weibo.com")) {

          //保存单条信息的ua、uid
          val pattern = Pattern.compile("/\\d+")
          var m = pattern.matcher(lineArr(4))
          var uid = ""
          if (m.find()) {
            uid = m.group()
            setUid = setUid.+(Array(lineArr(2), uid))
          }
          m = pattern.matcher(lineArr(5))
          if (m.find()) {
            uid = m.group()
            setUid = setUid.+(Array(lineArr(2), uid))
          }
        }

      }

    }
    val ids = WeiBo.crawlWeiBoInfo(setUid)
    //println("over")

    //ids.foreach(id => println(id))

  }

  //获取ip
  def getIPs(): Array[String] = {

    var ipPort: Array[String] = null
    try {
      val doc = Jsoup.connect("http://qsdrk.daili666api.com/ip/?tid=558465838696598&num=500&delay=5&foreign=none&ports=80,8080")
        .userAgent(ua)
        .timeout(30000)
        .followRedirects(true)
        .execute()
      ipPort = doc.body().split("\r\n")

    } catch {
      case ex: HttpStatusException => {
        println(ex)
        //changIP(ua)
      }
      case ex: SocketTimeoutException => {
        println(ex)
        //changIP(ua)
      }
      case ex: SocketException => println(ex)
    }
    ipPort
  }

  //设置代理ip
  def changIP(): Unit = {

    val ipPort = ips((Math.random() * 200).toInt).split(":")
    val ip = ipPort(0)
    val port = ipPort(1)
    println(ip + "  " + port)
    System.getProperties().setProperty("http.proxyHost", ip);
    System.getProperties().setProperty("http.proxyPort", port);

  }

}


