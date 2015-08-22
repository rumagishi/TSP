/*
 *
 * updated on 8/9
 *
 * parメソッドをつけて探索する．
 * 探索開始地点数genNum個分だけ解の候補が出てきて，その中から一番いい経路を選択するというもの．
 *
 * */

import scala.io.Source
import math._
import scala.util.Random
import scala.util.Random._
import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.parallel.immutable._

object TspCol {

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

  //最良の経路を返す（並列化してるときに利用）
  def returnBestRoute2(funcObj: List[Int] => Double)(candidates: ParSeq[List[Int]]): List[Int] = {
    val dis = candidates.map(x => funcObj(x))
    //val neighborhood = dis.filter( _ > len ).map(x => candidates(dis.indexOf(x)))
    //neighborhood.par
    val bestNeighborhood = candidates(dis.indexOf(dis.min))
    bestNeighborhood
    
  }

  //2-optして近傍で最良の解を求めるメソッド
  def twoOpt(order: List[Int]): List[List[Int]] = {
    val len = order.length
    var candidates = for(i <- 0 until len-1; j <- i+1 until len) yield order.updated(i,order(j)).updated(j,order(i)).toList
    candidates.toList
  }

  def main(args: Array[String]): Unit = {
    if(args.length != 3) {
      println("Please designate these 3, as arguments.")
      println(">>> scala Tsp <Seed> <The number of start point> <Filename>")
      sys.exit(-1)
    } else {

      //val state = Actor.State.Suspended

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
      val retRou2 = returnBestRoute2(getDis)_

      //ルートをgenNumの数だけシャッフル
      val routes = for(i <- 0 until genNum) yield shuffleRoute(defaultRoute)
      //println(routes)

      //比較対象を入れておくリスト
      var results = ParSeq[List[Int]]()

      //setterメソッド
      def setter(list: List[Int]) = {
        this.synchronized {
          results :+= list
        }
      }

      val findBest: scala.actors.Actor = actor {
        loop {
          react {
            case candidates: (scala.collection.parallel.immutable.ParSeq[List[Int]], List[Int]) => {
              //case candidates: List[List[Int]] => {
              //println("===findBest===")
              val newCandidates = retRou(candidates._1)(getDis(candidates._2))
              newCandidates.foreach {
                x =>
                println("The distance is " + getDis(x))
                (samplepoint ! x)
              }
              //reply(hoge) //ParSeq[List[Int]]
              //for (i <- 0 until newCandidates.length) {
              //  println("the best one is " + newCandidates(i))
              //  println("The distance is " + getDis(newCandidates(i)))
              //  (samplepoint ! newCandidates(i))
              //}
            }
          }
        }
      }

      val samplepoint: scala.actors.Actor = actor {
          var running = true
          loopWhile(running) {
            react {
              case route: List[Int] => {
                val candidates = twoOpt(route)

                //println("===samplepoint===")
                //println("the selected route is " + route)
                //println("THREAD ID : " + Thread.currentThread.getName + " | DISTANCE : " + getDis(route))

                val tuple = (candidates.par, route)
                (findBest ! tuple)
                //val newCandidates = retRou(candidates.par)(getDis(route))
                //reply(hoge) //ParSeq[List[Int]]
                //newCandidates.foreach{
                //  x => 
                //  //println("the best one is " + x);
                //  println("The distance is " + getDis(x))

                //  //(scala.actors.Actor.self) ! x
                //}
                //for (i <- 0 until newCandidates.length) {
                //  println("the best one is " + newCandidates(i))
                //  println("The distance is " + getDis(newCandidates(i)))
                //  (scala.actors.Actor.self) !! newCandidates(i)
                //}

                //val best = (findBest ! tuple).asInstanceOf[ParSeq[List[Int]]] //ParSeq[List[Int]]
                //(findBest ! tuple)//.asInstanceOf[ParSeq[List[Int]]] //ParSeq[List[Int]]
                //println("===return(findBest)===")

                //for (i <- 0 until best.length) {
                //if(getDis(route) <= getDis(best(i))) {
                //setter(route)
                //if (results.length == genNum) {
                //  running = false
                //  println("candidates are " + results)
                //    val answer = retRou2(results)
                //    println("the best one is " + best(i))
                //    println("The distance is " + getDis(best(i)))
                //    sys.exit(0)
                //  } 
                //} else {
                //  println("===a recursive call===")
                //  (scala.actors.Actor.self) ! best(i)//.toList
                //}

                //}
              }
            }
          }
      }

      for (i <- 0 until genNum) (samplepoint ! routes(i))
      //while(state != samplepoint.getState) { /*println("waiting")*/ }
    }
  }
}

