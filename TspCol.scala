import scala.io.Source
import math._
import scala.util.Random
import scala.util.Random._
//import akka.actor._
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
  def returnBestRoute(funcObj: List[Int] => Double)(candidates: ParSeq[List[Int]]): List[Int] = {
    val dis = candidates.map(x => funcObj(x))
    val bestNeighborhood = candidates(dis.indexOf(dis.min))
    bestNeighborhood
  }

  //最良の経路を返す（結果集計するときに利用）
  def returnBestRoute2(funcObj: List[Int] => Double)(candidates: List[List[Int]]): List[Int] = {
    val dis = candidates.map(x => funcObj(x))
    val bestNeighborhood = candidates(dis.indexOf(dis.min))
    bestNeighborhood
  }

  //2-optして近傍で最良の解を求めるメソッド
  def twoOpt(order: List[Int]): List[List[Int]] = {
    val len = order.length
    var candidates = for(i <- 0 until len-1; j <- i+1 until len) yield order.updated(i,order(j)).updated(j,order(i)).toList
    order :: candidates.toList
  }

  //内包できるメソッドはしたほうがいいかも

  def main(args: Array[String]): Unit = {
    if(args.length != 3) {
      println("引数として，")
      println(">>> scala Tsp <シード値> <探索開始地点数> <ファイル名>")
      println("を指定してください")
      sys.exit(-1)
    } else {

      //問題の初期設定
      val seed = args(0).toInt
      val genNum = args(1).toInt //探索開始地点数．これも実行時に指定できるようにする．
      val filename = args(2)
      Random.setSeed(seed)
      //ここはdatファイルから読み込めるようにする
      //val citycordi:Array[(Int, Int)] = Array( (0,0), (1,0), (2,0), (2,1), (2,2), (1,2), (0,2), (0,1) )
      //val citycordi:Array[(Int, Int)] = Array( (0,0), (1,0), (2,0), (0,2), (2,2), (1,2), (2,1), (0,1) )
      //val citycordi:Array[(Int, Int)] = (for (i <- 0 until 100) yield (nextInt(100), nextInt(100))).toArray
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
      var routes = for(i <- 0 until genNum-1) yield shuffleRoute(defaultRoute)
      routes :+= defaultRoute

      val findBest = actor {
        loop {
          react {
            case candidates: ParSeq[List[Int]] => {
              reply(retRou(candidates)) //List[Int]
            }
          }
        }
      }

      val samplepoint = actor {
        var tempList = List[Int]()

        def func(route: List[Int]): Unit = {
          tempList = route
          val candidates = twoOpt(route)
          val div = 4
          val hop = candidates.length/div
          val slicedList = for(i <- 0 until div) yield candidates.slice(hop*i, hop*i+hop)
          val best = for(i <- 0 until div) yield (findBest !? slicedList(i).par).asInstanceOf[List[Int]]
          
          //val best = (findBest !? candidates.par).asInstanceOf[List[Int]]
          //if(getDis(tempList) >= getDis(best)) {
          //  reply(best)
          //} else {
          //  func(best)
          //}

          ////println("the best one is " + best) //2-Optした後４分割する．分割したそれぞれのグループ内でのベストな解を表示
          for(i <- 0 until div) {
            if(getDis(tempList) >= getDis(best(i))) {
              reply(best(i))
            } else {
              func(best(i))
            }
          }
        }

        loop {
          react {
            case route: List[Int] => {
              func(route)
            }
          }
        }
      }

      val better = for (i <- 0 until routes.length) yield (samplepoint !? routes(i)).asInstanceOf[List[Int]]
      while(samplepoint.getState.toString == "Runnable") {println("waiting")}
      val answer = retRou2(better.toList)
      println("解は " + answer)
      println("その距離は " + getDis(answer))
      println(samplepoint.getState)
      sys.exit(0)
    }
  }
}

