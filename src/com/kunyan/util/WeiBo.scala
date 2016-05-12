package com.kunyan.util

import java.io.IOException
import java.net.{ConnectException, SocketTimeoutException}
import java.util
import java.util.concurrent.Executors
import com.kunyan.Controller
import org.jsoup.Connection.Method
import org.jsoup.{HttpStatusException, Jsoup}

import scala.collection.immutable.HashSet

/**
 * Created by lcm on 2016/5/10.
 */
object WeiBo {


  def crawlWeiBoInfo(datas: HashSet[Array[String]]): HashSet[String] = {

    val cookieStr = "SINAGLOBAL=8127220470923.931.1462868168233; wvr=6; YF-Ugrow-G0=9642b0b34b4c0d569ed7a372f8823a8e; login_sid_t=7fcf14d791eb7a8273ef3785bc5f3087; _s_tentry=-; Apache=4640738116577.268.1462931647687; ULV=1462931647692:2:2:2:4640738116577.268.1462931647687:1462868168240; SUS=SID-5510690438-1462931606-GZ-0bpn7-d73b8c39584d3c9dfa0e204b5dd30dc2; SUE=es%3Ded9ebbb59b091611164f749ca9c31cfd%26ev%3Dv1%26es2%3D77afdf51aeacd9242e69157d3c397a04%26rs0%3DHWzzKFqSeLDqtoMNnWYqHllF%252BILWmi1zLMPpjFB%252BFlglkxYBU1ZDk69%252BhtXs%252FbyU4wR%252FnhdeDJA0QQCkHH0vSZ6uLhnex%252FLcNNgcVHAFnC%252FAhlMoEU7sW6S4812Jxr%252FCSYW%252FYfpsLaXJf6kPapk1S0%252FZMOhLJ7xAEwCTH%252B6Pxnk%253D%26rv%3D0; SUP=cv%3D1%26bt%3D1462931606%26et%3D1463018006%26d%3Dc909%26i%3D0dc2%26us%3D1%26vf%3D0%26vt%3D0%26ac%3D0%26st%3D0%26uid%3D5510690438%26name%3Djoyisverygood%2540gmail.com%26nick%3D%25E4%25B9%2590yiyiyiyiyiyi%26fmp%3D%26lcp%3D; SUB=_2A256NuDGDeTxGeNL6lIX-S7IyDSIHXVZQlUOrDV8PUNbu9BeLRTakW9LHetBuBHVMkrbDrYaRTQJheEcM6Q4CQ..; SUBP=0033WrSXqPxfM725Ws9jqgMF55529P9D9Whjoy4yJ9ExUe3545ocyRFn5JpX5KzhUgL.Fo-feK5c1K5Xe0nt; SUHB=0Jvw5uGGAQsh4W; ALF=1494467606; SSOLoginState=1462931606; YF-V5-G0=5f9bd778c31f9e6f413e97a1d464047a; YF-Page-G0=0dccd34751f5184c59dfe559c12ac40a"

    //获取可爬取的微博id
    val ids = getWeiBoInfo(datas, getCookies(cookieStr))

    ids

  }

  //根据cookie字符串获取cookie的map
  def getCookies(cookieStr: String): util.HashMap[String, String] = {
    val cookieMap = new util.HashMap[String, String]()
    val cookieArr = cookieStr.split(";")
    for (line <- cookieArr) {
      val lineArr = line.split("=")
      if (lineArr.length > 1) {
        cookieMap.put(lineArr(0), lineArr(1))
      }
    }
    cookieMap
  }

  def getWeiBoInfo(datas: HashSet[Array[String]], cookies: util.HashMap[String, String]): HashSet[String] = {

    var weiboInfos = new HashSet[String]

    val listId = datas.toList

    //创建一个可重用固定线程数的线程池

    val pool = Executors.newFixedThreadPool(80);

    for (index <- 0 to listId.size - 1) {

      val thread = new Thread(new Runnable {
        override def run(): Unit = {
          //用url获取id
          val id = matchAndGetId(listId(index)(0), listId(index)(1), cookies)
          if (id != "") {
            val info = getUserInfoById(cookies, id, listId(index)(0))
            //if (info != "") {
              println("info--" + info)
              weiboInfos = weiboInfos.+(info)
            //}
          }
        }
      })

      pool.execute(thread)

    }

    pool.shutdown()
    weiboInfos
  }

  def matchAndGetId(ua: String, uid: String, cookies: util.HashMap[String, String]): String = {

    var id = ""

    try {
      val doc = Jsoup.connect("http://weibo.com/u" + uid)
        .userAgent(ua)
        .timeout(3000)
        .cookies(cookies)
        .method(Method.GET)
        .followRedirects(true)
        .execute()
      doc.body().split("\\$CONFIG").foreach(f => {
        if (f.contains("['page_id']")) {
          id = f.replace(";", "").split("=")(1).replace("'", "").trim
        }
      })
    }
    catch {
      case ex: SocketTimeoutException => {
        //println(ex)
        //Controller.changIP()
        matchAndGetId(ua, uid, cookies)
      }
      case ex: ConnectException => {
        println(ex)
        //Controller.changIP()
        //matchAndGetId(ua,uid,cookies)
      }
      case ex: HttpStatusException => {
        println(ex)
        Controller.changIP()
        //matchAndGetId(ua,uid,cookies)
      }
      case ex: IOException => println(ex)
    }

    id

  }

  //获取用户信息
  def getUserInfoById(cookies: util.HashMap[String, String], id: String, ua: String): String = {
    //用户信息
    var userInfo = ""
    //微博账号
    val weiBoId = id.substring(6)
    //QQ
    var QQ = "NoDef"
    //邮箱
    var email = "NoDef"
    //职业(不可获得)
    val job = "NoDef"
    //身份
    var position = "NoDef"
    //真是姓名(不可获得)
    val realName = "NoDef"
    //公司
    var company = "NoDef"
    //地址（所在地）
    var address = "NoDef"
    //var
    val url = "http://weibo.com/p/" + id + "/info?mod=pedit_more"
    //println(url)
    try {
      val doc = Jsoup.connect(url)
        .userAgent(ua)
        .timeout(3000)
        .cookies(cookies)
        .method(Method.GET)
        .followRedirects(true)
        .execute()
      for (x <- doc.body().split("<script>FM.view")) {
        if (x.contains("\"ns\":\"\",\"domid\":\"Pl_Official_PersonalInfo__62\"")) {
          val data = x.replace("\\t", "").replace("\\n", "").replace("\\r", "")
          val datas = data.split("<span class=\\\\\"pt_title S_txt2\\\\\">")
          datas.foreach(d => {
            //获取QQ信息
            if (d.contains("QQ")) {
              QQ = parserInfo("QQ", d)
            }
            //获取邮箱
            if (d.contains("邮箱")) {
              email = parserInfo("邮箱", d)
            }
            //获取所在地
            if (d.contains("所在地")) {
              address = parserInfo("所在地", d)
            }
            //获取公司和身份信息
            if (d.contains("公司")) {
              val workInfo = parserInfo("公司", d).split("=")
              company = workInfo(0)
              if (workInfo.length == 2) {
                position = workInfo(1)
              }
            }
          })
        }
      }
      if (address != "NoDef") {
        userInfo = weiBoId + "-->" + QQ + "-->" + email + "-->" + job + "-->" + position + "-->" + realName + "-->" + company + "-->" + address
      }
    } catch {
      case ex: SocketTimeoutException => {
        //println(ex)
        //Controller.changIP()
        getUserInfoById(cookies, id, ua)
      }
      case ex: ConnectException => {
        println(ex)
        //Controller.changIP()
        //getUserInfoById(cookies,id,ua)
      }
      case ex: HttpStatusException => {
        println(ex)
        Controller.changIP()
        //getUserInfoById(cookies,id,ua)
      }
      case ex: IOException => println(ex)
    }
    userInfo
  }


  def parserInfo(infoName: String, infoStr: String): String = {
    if (infoName == "公司") {
      val workInfo = infoStr.split("<\\\\/span>")(1)
      val company = workInfo.split("<\\\\/a>")(0).split(">").last
      var position = ""
      if (workInfo.contains("职位")) {
        position = workInfo.split("职位：").last
      }
      return company + "=" + position
    } else {
      val anyInfo = infoStr.split("<\\\\/span>")(1).split(">").last
      if (anyInfo.contains("pt_detail")) {
        return "NoDef"
      } else {
        return anyInfo
      }
    }
  }
}
