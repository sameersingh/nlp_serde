echo "scp the figer model..."
scp bicycle.cs.washington.edu:/projects/ai/www/figer/figer.liblinear.model.gz figer.model.gz

echo "scp the multir models..."
scp bicycle.cs.washington.edu:/projects/pardosa/s4/xiaoling/multir-partitioned.models.tgz .
echo "unpacking..."
tar zxf -C multir-partition-models multir-partitioned.models.tgz
echo "done"
