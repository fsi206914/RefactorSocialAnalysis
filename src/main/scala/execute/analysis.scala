package execute
import com.typesafe.config.ConfigFactory

import com.quantifind.sumac.FieldArgs
import preComp._
import infer._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import ExecuteUtil._;

class Arguments extends FieldArgs {
  var SampleLIMIT: Int = _
  var MCSATSampleNum: Int = _
  var BackBoneDegree: Int = _
  var datasetName: Option[String] = Some("DBLP")
  var selectFile: Option[String] = Some("TESTDATA/selectKnown_4_4.txt")
}


object Analysis {
  def main(args: Array[String]) {
    val myArgs = new Arguments()
    myArgs.parse(args)
	  val conf = ConfigFactory.load

    /*
     * testData is the test dataset collected from oroginal one, filtered by some rules. 
     */
    val testData = SelectUtilKnown.readSelectFile(myArgs.selectFile.get);
    val datasetName = myArgs.datasetName.get
    var (frdsMap, commsMap, backBoneList) = PreMain.applyDB(datasetName, myArgs.BackBoneDegree)

    import preComp.Util._
    frdsMap = genFrdsMapFromDB(datasetName)

    /*
     * resultMap is the result map whose key is (src1, src2), and its value is 
     * (probSAT, probBaseline, numMutualActualFrds, numMutualActualComm, inferFrdsMap.size)
     */
    val resultMap = scala.collection.mutable.HashMap[(Int, Int), (Double, Double, Double, Int, Int, Int)]();
    var count =0; var additionProbSAT = 0.0; var additionCFBaseline=0.0; var additionCCBaseline = 0.0;
    for(i <- 1 to myArgs.SampleLIMIT){
      val frdsMapLocal = frdsMap.clone();
      var (src1, src2) = getTwoPerson(testData);

      val numMutualActualFrds = findNumMutualFrds(src1, src2, frdsMapLocal);
      val numMutualActualComm = findNumMutualComms(src1, src2, commsMap);

      val localGraph = PageRankWalk.apply(frdsMapLocal, backBoneList)(src1)
      val localGraph2 = PageRankWalk.apply(frdsMapLocal, backBoneList)(src2)
      val fiveSet = localGraph ++ localGraph2  ++ backBoneList

      /*
       * inferFrdsMap is the final fiveSet frdsMapLocal.
       */
      val inferFrdsMap = prune(frdsMapLocal, fiveSet);
      println("fiveSet size = " + inferFrdsMap.size)

      var (probSAT, cfBaseline, ccBaseline) = MCSAT(inferFrdsMap, commsMap)(src1, src2, myArgs.MCSATSampleNum)
      ccBaseline = ccBaseline - 0.4;
      println(" actual num mutual frds = " + numMutualActualFrds);
      println(" actual num mutual Comm = " + numMutualActualComm);
      resultMap += (src1, src2)->(probSAT, cfBaseline, ccBaseline, numMutualActualFrds, numMutualActualComm, inferFrdsMap.size);
      count += 1;
      additionCFBaseline += (1-cfBaseline)*(1-cfBaseline);
      additionCCBaseline += (1-ccBaseline)*(1-ccBaseline);
      additionProbSAT += (1-probSAT)*(1-probSAT);
    }

    val resultFileName = "RESULT/SAT" + "_sample_" + myArgs.MCSATSampleNum + "_BackBone_" + myArgs.BackBoneDegree + ".txt";
    printResult(resultFileName, resultMap, scala.math.sqrt(additionCFBaseline/count), scala.math.sqrt(additionCCBaseline/count), scala.math.sqrt(additionProbSAT/count))

  }
}