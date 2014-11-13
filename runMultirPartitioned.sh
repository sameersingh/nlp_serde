
input="nigeria_dataset_v04.nlp.in.json.gz"
output="nigeria_dataset_v04.nlp.out.json.gz"

cp "nigeria_dataset_v04.nlp.cw.json.gz" $input
for a1 in PERSON ORGANIZATION MISC
do
  for a2 in PERSON ORGANIZATION LOCATION MISC
  do
     mvn scala:run -DmainClass=nlp_serde.annotators.relations.TestMultiRAnnotator -DjavaOpts.Xmx=30g -DaddArgs="${a1}|${a2}|${input}|${output}"
     mv $output $input
  done
done

mv $output nigeria_dataset_v04.nlp.cw.multir_partitioned.json.gz
