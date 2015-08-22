import scala.io.Source
import math._
import scala.util.Random
import scala.util.Random._
import scala.collection.parallel.immutable._

import akka.actor._
import akka.pattern.ask
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.util.Timeout
import scala.language.postfixOps

object TspAkka {

  //datファイル読み込み
  def tsuhakoMethod(file: String): IndexedSeq[(Double, Double)] = {
    val source = Source.fromFile(file)
    val lines = source.getLines
    val arr = (for(line <- lines) yield (line.split(" ")(0).toDouble, line.split(" ")(1).toDouble)).toIndexedSeq
    source.close
    arr
  }

  //citycordiから各都市間の距離を算出するメソッド
  def getOverallDistance(cor: IndexedSeq[(Double, Double)]): Array[Array[Double]]= {
    val len = cor.length
    var distance = Array.ofDim[Double](len, len)
    for(i <- 0 until len; j <- 0 until len) {
      distance(i)(j) = sqrt( pow((cor(i)._1 - cor(j)._1), 2) + pow((cor(i)._2 - cor(j)._2), 2) )
    }
    distance
  }

  //都市の巡る順番をバラバラにするメソッド(初期探索点)
  def shuffleRoute(order: List[Int]): List[Int] = {
    shuffle(order)
  }

  //経路の道のりを求めるメソッド
  def getRouteDistance(distanceTable: Array[Array[Double]])(order: List[Int]): Double = {
    val len = order.length
    var dis = 0.0
    for (i <- 0 until len-1) {
      val front = order(i); val back = order(i+1)
      dis += distanceTable(front)(back)
    }
    dis
  }

  //最良の経路を返す（並列化してるときに利用）
  def returnBestRoute(funcObj: List[Int] => Double)(candidates: ParSeq[List[Int]])(len: Double): ParSeq[List[Int]] = {
    val dis = candidates.map(x => funcObj(x))
    val neighborhood = dis.filter( _ < len ).map(x => candidates(dis.indexOf(x)))
    neighborhood.par
    //val bestNeighborhood = candidates(dis.indexOf(dis.min))
    //list.filter(_>4).map(x => l(list.indexOf(x)))
    //bestNeighborhood
  }

  ////最良の経路を返す（並列化してるときに利用）
  //def returnBestRoute(funcObj: List[Int] => Double)(candidates: ParSeq[List[Int]]): List[Int] = {
  //  val dis = candidates.map(x => funcObj(x))
  //  val bestNeighborhood = candidates(dis.indexOf(dis.min))
  //  bestNeighborhood
  //}

  def main(args: Array[String]): Unit = {
    if(args.length != 3) {
      println("Please designate these 3, as arguments.")
      println(">>> scala Tsp <Seed> <The number of start point> <Filename>")
      sys.exit(-1)
    } else {

      //問題の初期設定
      val seed = args(0).toInt
      val genNum = args(1).toInt //探索開始地点数．これも実行時に指定できるようにする．
      val filename = args(2)
      Random.setSeed(seed)
      //datファイルから読み込む
      val citycordi:IndexedSeq[(Double, Double)] = tsuhakoMethod(filename)

      //都市間の距離をあらかじめ求めておく
      val distanceTable = getOverallDistance(citycordi)
      //都市数
      val numOfCity = citycordi.length
      //都市の巡回順序
      val defaultRoute = (0 until numOfCity).toList

      //関数オブジェクトの生成
      val getDis = getRouteDistance(distanceTable)_
      val retRou = returnBestRoute(getDis)_

      //ルートをgenNumの数だけシャッフル
      val routes = for(i <- 0 until genNum) yield shuffleRoute(defaultRoute)

      val system = ActorSystem("system")

      val act = system.actorOf(Props(classOf[SamplePoint], getDis, retRou, genNum))

      implicit val timeout = Timeout(1 seconds)

      for (i <- 0 until routes.length) (act ! routes(i))
    }
  }
}

class SamplePoint(getDis: List[Int] => Double, retRou: ParSeq[List[Int]] => Double => ParSeq[List[Int]], genNum: Int) extends Actor {

  //2-optして近傍で最良の解を求めるメソッド
  def twoOpt(order: List[Int]): List[List[Int]] = {
    val len = order.length
    var candidates = for(i <- 0 until len-1; j <- i+1 until len) yield order.updated(i,order(j)).updated(j,order(i)).toList
    order :: candidates.toList
  }

  val start = System.currentTimeMillis()
  //比較対象を入れておくリスト
  var results = ParSeq[List[Int]]()

  //setterメソッド
  def setter(list: List[Int]) = {
    this.synchronized {
      results :+= list
    }
  }


  val act = context.actorOf(Props(classOf[FindBest], retRou))
  implicit val timeout = Timeout(1 seconds)

  def receive: Receive = {
    case route: List[Int] => {

      val candidates = twoOpt(route)

      val future = act ? (candidates.par, getDis(route))
      val best = Await.result(future, timeout.duration).asInstanceOf[ParSeq[List[Int]]]

      best.foreach{
        x =>
          println("The distance is " + getDis(x))
          self ! x
      }

      //println("THREAD ID : " + Thread.currentThread.getName + " | DISTANCE : " + getDis(route))

      //if(getDis(route) <= getDis(best)) {
      //  setter(route)
      //  if (results.length == genNum) {
      //    println("candidates are " + results)
      //    val answer = retRou(results)
      //    println("the best one is " + answer)
      //    println("The distance is " + getDis(answer))
      //    val end = System.currentTimeMillis()
      //    val interval = end - start
      //    println(interval + " milli sec.")
      //    sys.exit(0)
      //  }
      //} else {
      //  //println("===a recursive call===")
      //  self ! best.toList
      //}
    }
  }
}


class FindBest(retRou: ParSeq[List[Int]] => Double => ParSeq[List[Int]]) extends Actor {
  def receive: Receive = {
    //case candidates: ParSeq[List[Int]] => {
    //  sender ! retRou(candidates) //List[Int]

    case candidates: (scala.collection.parallel.immutable.ParSeq[List[Int]], Double) => {
      //case candidates: List[List[Int]] => {
      //println("===findBest===")
      val newCandidates = retRou(candidates._1)(candidates._2)
      sender ! newCandidates
      //newCandidates.foreach {
      //  x =>
      //    println("The distance is " + getDis(x))
      //    (samplepoint ! x)
      //}
    }
  }
}
