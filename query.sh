for mcsat in 5
do
	for sample in 10
	do
		sbt "run-main execute.AnalysisLJ --SampleLIMIT $sample --MCSATSampleNum $mcsat --BackBoneDegree 1200"
	done
done