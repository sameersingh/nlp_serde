input="nigeria_dataset_v04.nlp.lr.json.gz"
output="nigeria_dataset_v04.nlp.lrf.json.gz"

mvn scala:run -DmainClass=nlp_serde.annotators.FigerAnnotator -DjavaOpts.Xmx=30g -DaddArgs="figer.model.gz|${input}|${output}"
