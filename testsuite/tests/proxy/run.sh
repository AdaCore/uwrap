for j in *2proxy; do
  test_dir $j
done

for j in `ls | grep -v 2proxy`; do
  test_dir $j
done
