#input="nigeria_dataset_v04.nlp.lr.json.gz"
#output="nigeria_dataset_v04.nlp.lrf.json.gz"
input=$1
output=$2
mvn scala:run -DmainClass=nlp_serde.annotators.FigerAnnotator -DjavaOpts.Xmx=30g -DaddArgs="figer.model.gz|${input}|${output}"
