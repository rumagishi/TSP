import scala.io.Source
import math._
import scala.util.Random
import scala.util.Random._

object Tsp {

	//citycordiから各都市間の距離を算出するメソッド
	def getOverallDistance(cor: Array[(Double, Double)]): Array[Array[Double]]= {
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
	def getRouteDistance(order: List[Int], distanceTable: Array[Array[Double]]): Double = {
		val len = order.length
			var dis = 0.0
			for (i <- 0 until len-1) {
				val front = order(i); val back = order(i+1)
					dis += distanceTable(front)(back)
			}
		dis
	}

	//最良の経路を返す
	def returnBestRoute(candidates: List[List[Int]], distanceTable: Array[Array[Double]]): List[Int] = {
		val dis = candidates.map(x => getRouteDistance(x, distanceTable))
			val bestNeighborhood = candidates(dis.indexOf(dis.min))
			bestNeighborhood
	}

	//2-optして近傍で最良の解を求めるメソッド
	def twoOpt(order: List[Int]): List[List[Int]] = {
		val len = order.length
			var candidates = for(i <- 0 until len-1; j <- i+1 until len) yield order.updated(i,order(j)).updated(j,order(i)).toList
			order :: candidates.toList
	}

	def tsuhakoMethod(file: String): Array[(Double, Double)] = {
		val source = Source.fromFile(file)
		val lines = source.getLines
		val arr = (for(line <- lines) yield (line.split(" ")(0).toDouble, line.split(" ")(1).toDouble)).toArray
		source.close
		arr
	}

	/*内包できるメソッドはしたほうがいいかも*/

	def main(args: Array[String]): Unit = {

		val seed = args(0).toInt
		val startpoint = args(1).toInt
		val file = args(2)
		Random.setSeed(seed)
		/*ここはdatファイルから読み込めるようにする*/
		//val citycordi:Array[(Int, Int)] = Array( (0,0), (1,0), (2,0), (2,1), (2,2), (1,2), (0,2), (0,1) )
		val citycordi:Array[(Double, Double)] = tsuhakoMethod(file)
			val defaultRoute = (0 until citycordi.length).toList
			//val numberOfCity = citycordi.length
			val distanceTable = getOverallDistance(citycordi)
			/*2-optする．タプルの最初から都市名を特定*/
			/*2-optしたものの距離をもとめる*/
			val routeDistance = getRouteDistance(defaultRoute, distanceTable)
			/*訪問順番のシャッフル*/
			//val newOrder = shuffleRoute(defaultRoute)
			//println(newOrder)
			//println(defaultRoute)
			//val checkTwoOpt = twoOpt(defaultRoute)
			//println(checkTwoOpt)

			
			def func(route: List[Int]): List[Int] = {
				val opted: List[List[Int]] = twoOpt(route)
				val bestroute: List[Int] = returnBestRoute(opted, distanceTable)
				//getRouteDistance(order: List[Int], distanceTable: Array[Array[Double]])
				//if(/*bestrouteの距離 >= routeの距離*/) {
				if(getRouteDistance(bestroute, distanceTable) >= getRouteDistance(route, distanceTable)) {
					/*結果を返す List[Int]*/
					route	
				} else {
					func(bestroute)
				}
			}
			   
			//for( i <- 0 to 3){
     			//val newOrder = shuffleRoute(defaultRoute)
			//println(newOrder)          
			//}           

			val newOrder = for(i <- 0 until startpoint) yield shuffleRoute(defaultRoute)

			val betterOne = (for(i <- 0 until startpoint) yield func(newOrder(i))).toList

			println(betterOne)
			
			//returnBestRoute(candidates: List[List[Int]], distanceTable: Array[Array[Double]]): List[Int] 

			val best = returnBestRoute(betterOne, distanceTable)
			println("解は " + best)
			println("総距離は " + getRouteDistance(best, distanceTable))

                        //println(checkTwoOpt)

		       

	}



}
