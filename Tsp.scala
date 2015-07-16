import scala.io.Source
import math._
import scala.util.Random
import scala.util.Random._

object Tsp {

  /*citycordiから各都市間の距離を算出するメソッド*/
  def getOverallDistance(cor: Array[(Int, Int)]) = {
    val len = cor.length
    var distance = Array.ofDim[Double](len, len)
    for(i <- 0 until len; j <- 0 until len) {
      distance(i)(j) = sqrt( pow((cor(i)._1 - cor(j)._1), 2) + pow((cor(i)._2 - cor(j)._2), 2) )
    }
    distance
  }

  /*経路の道のりを求めるメソッド*/
  def getRouteDistance(order: List[Int], distanceTable: Array[Array[Double]]) = {
    val len = order.length
    var dis = 0.0
    for (i <- 0 until len-1) {
      val front = order(i); val back = order(i+1)
      dis += distanceTable(front)(back)
    }
    dis
  }

  /*都市の巡る順番をバラバラにするメソッド(初期探索点)*/
  def shuffleRoute(order: List[Int], seed: Int) = {
    Random.setSeed(seed)
    shuffle(order)
  }

  /*2-optして近傍で最良の解を求めるメソッド*/
  def twoOpt(order: List[Int], distanceTable: Array[Array[Double]]) = {
    val len = order.length
    val candidates = for(i <- 0 until len-1; j <- i+1 until len) yield order.updated(i,order(j)).updated(j,order(i))
    val dis = candidates.map(x => getRouteDistance(x, distanceTable))
    val bestNeighborhood = candidates(dis.indexOf(dis.min))
    if (dis.min < getRouteDistance(order, distanceTable)) {
      bestNeighborhood
    } else {
      order
    }
  }

  /*内包できるメソッドはしたほうがいいかも*/

  def main(args: Array[String]): Unit = {
    /*ここはdatファイルから読み込めるようにする*/
   val citycordi:Array[(Int, Int)] = Array( (0,0), (1,0), (2,0), (2,1), (2,2), (1,2), (0,2), (0,1) )
   val defaultRoute = (0 until citycordi.length).toList
   val seed = args(0).toInt
   //val numberOfCity = citycordi.length
   val distanceTable = getOverallDistance(citycordi)
    /*2-optする．タプルの最初から都市名を特定*/
    /*2-optしたものの距離をもとめる*/
   val routeDistance = getRouteDistance(defaultRoute, distanceTable)
    /*訪問順番のシャッフル*/
   val newOrder = shuffleRoute(defaultRoute, seed)
   println(newOrder)
   println(defaultRoute)
   val checkTwoOpt = twoOpt(defaultRoute, distanceTable)
   println(checkTwoOpt)
  }



}

//class MultiPoint() extends Actor {}
//class HillClimbing() extends Actor {}

/*都市の巡回する順番をかえる関数*/
/*citycordiから各都市間の距離を算出する関数*/
/*経路の道のりを求める関数*/
/*都市の巡る順番をバラバラにする(初期探索点)*/
/*2-optして近傍で最良の解を求める．*/

/******流れ******/
/*dataファイルを与える*/
/*都市の巡る順番をバラバラにする(初期探索点)*/
/*上の操作を何回か実行してクローンを作る*/

/*局所探索*/
/*while( これ以上2-optの余地がない )*/
  /*2-optする*/
/*end while*/

/*Join*/
/*巡回経路の距離を求める*/
/*ベストな経路を求める*/
/******流れ終了******/

/*参考ページ*/
/*2-opt*/
/*なんか一般的な2-optと違うみたい*/
/*https://en.wikipedia.org/wiki/2-opt*/
  //def twoOpt(order: List[Int], seed: Int) = {
  //  Random.setSeed(seed)
  //  def generateIndexNumber(): (Int, Int) = {
  //    val n1 = nextInt(order.length)
  //    val n2 = nextInt(order.length)
  //    if(n2 < n1) {
  //      (n2, n1)
  //    } else if ( n1 < n2 ) {
  //      (n1, n2)
  //    } else {
  //      generateIndexNumber()
  //    }
  //  }
  //  generateIndexNumber()
  //  /*1.list.take(r)*/
  //  /*2.list.slice(r,t).reverse*/
  //  /*3.*/
  //}

