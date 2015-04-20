package execute

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable.Map
import scala.collection.Set
import scala.util.control.Breaks._

object ExecuteUtil {

  val rand = new Random(System.currentTimeMillis());
  val conf = ConfigFactory.load

	def getTwoPerson(testData: List[(Int, Int)])={
    
    val testDataSize  = testData.size;
    val pair = testData(rand.nextInt(testDataSize));

    var src1 = pair._1; var src2 = pair._2;
    if(src1 > src2){
      var temp = src1;
      src1 = src2
      src2 = temp;
    }

    (src1, src2)
  }
}