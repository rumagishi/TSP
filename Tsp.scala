import scala.io.Source
import math._
import scala.util.Random
import scala.util.Random._
import scala.actors.Actor
import scala.actors.Actor._

object Tsp {

  /*citycordiから各都市間の距離を算出するメソッド*/
  def getOverallDistance(cor: Array[(Int, Int)]): Array[Array[Double]]= {
    val len = cor.length
    var distance = Array.ofDim[Double](len, len)
    for(i <- 0 until len; j <- 0 until len) {
      distance(i)(j) = sqrt( pow((cor(i)._1 - cor(j)._1), 2) + pow((cor(i)._2 - cor(j)._2), 2) )
    }
    distance
  }

  /*都市の巡る順番をバラバラにするメソッド(初期探索点)*/
  def shuffleRoute(order: List[Int]): List[Int] = {
    shuffle(order)
  }

  /*経路の道のりを求めるメソッド*/
  def getRouteDistance(order: List[Int], distanceTable: Array[Array[Double]]): Double = {
    val len = order.length
    var dis = 0.0
    for (i <- 0 until len-1) {
      val front = order(i); val back = order(i+1)
      dis += distanceTable(front)(back)
    }
    dis
  }

  /*最良の経路を返す*/
  def returnBestRoute(candidates: List[List[Int]], distanceTable: Array[Array[Double]]): List[Int] = {
    val dis = candidates.map(x => getRouteDistance(x, distanceTable))
    val bestNeighborhood = candidates(dis.indexOf(dis.min))
    bestNeighborhood
  }

/*2-optして近傍で最良の解を求めるメソッド*/
  def twoOpt(order: List[Int]): List[List[Int]] = {
    val len = order.length
    var candidates = for(i <- 0 until len-1; j <- i+1 until len) yield order.updated(i,order(j)).updated(j,order(i)).toList
    order :: candidates.toList
  }

  /*内包できるメソッドはしたほうがいいかも*/

  def main(args: Array[String]): Unit = {
    /*問題の初期設定*/
    /*ここはdatファイルから読み込めるようにする*/
   val citycordi:Array[(Int, Int)] = Array( (0,0), (1,0), (2,0), (2,1), (2,2), (1,2), (0,2), (0,1) )
   /*都市間の距離をあらかじめ求めておく*/
   val distanceTable = getOverallDistance(citycordi)
   /*都市の巡回順序*/
   val defaultRoute = (0 until citycordi.length).toList
   /*シード値を読み込む*/
   val seed = args(0).toInt
   Random.setSeed(seed)
   val genNum = 4 //探索開始地点数．これも実行時に指定できるようにする．

   /*ルートをgenNumの数だけシャッフル*/
   var routes = for(i <- 0 until genNum) yield shuffleRoute(defaultRoute)
   routes :+= defaultRoute
   println(routes)
   val findBest = actor {
     loop {
       react {
         case candidates: List[List[Int]] => {
           reply(returnBestRoute(candidates, distanceTable)) //Vector(List[Int]) length = 4(routes.length)
         }
       }
     }
   }
   val samplepoint = actor {
     var tempList = List[Int]()
     def func(route: List[Int]): Unit = {
       tempList = route
       val candidates = twoOpt(route)
       val best = (findBest !? candidates.toList).asInstanceOf[List[Int]]
       if(tempList == best) {
         reply(best)
       } else {
         func(best)
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
   val better = for (i <- 0 until routes.length) yield samplepoint !? routes(i)
   println(better) //Vector(List[Int]) length = 4(routes.length)
   println(getRouteDistance(better(1).asInstanceOf[List[Int]], distanceTable)) //距離を取得
   println("actors are executed.")
  }
}

/*各メソッドの挙動チェック - Start*/
   //println(routes.toList)
   //println(routes().map(x => twoOpt(x, distanceTable)))
   ///*訪問順番のシャッフル*/
   ///*ここで初期探索点を増やす．*/
   //val newOrder = shuffleRoute(defaultRoute, seed)
   ///*経路の距離をもとめる*/
   //val routeDistance = getRouteDistance(defaultRoute, distanceTable)
   ///*2-optの実行*/
   //val candidates = twoOpt(defaultRoute)
   //val div = 4
   //val hop = candidates.length/div
   //for(i <- 0 until div) {
   //  println(candidates.slice(hop*i, hop*i+hop))
   //}
   //println(checkTwoOpt)
/*各メソッドの挙動チェック - End*/

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

