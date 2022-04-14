snowsql -c ywei \
  -d SANDBOX_KOMODO \
  -s ACIC_CAUSALITY_CHALLENGE_2022 \
  -q 'SELECT DATASET_NUM, ID_PATIENT, YEAR, Z_DGP_1, Z_DGP_2, Y_DGP_1_WITH_Z_DGP_1, ETA_Y_DGP_1_WITH_Z_DGP_1, Y_DGP_2_WITH_Z_DGP_1, ETA_Y_DGP_2_WITH_Z_DGP_1, Y_DGP_1_WITH_Z_DGP_2, ETA_Y_DGP_1_WITH_Z_DGP_2, Y_DGP_2_WITH_Z_DGP_2, ETA_Y_DGP_2_WITH_Z_DGP_2 FROM DFP_Y WHERE DATASET_NUM <= 100;' \
  -o output_format=csv \
  -o header=true \
  -o timing=false > DGP_Y_NEW_100.csv

snowsql -c ywei \
  -d SANDBOX_KOMODO \
  -s ACIC_CAUSALITY_CHALLENGE_2022 \
  -q 'SELECT * FROM DGP_ATT_NEW;' \
  -o output_format=csv \
  -o header=true \
  -o timing=false > DGP_ATT_NEW.csv
   

sed -i '' 1,2d DGP_Y_NEW_100.csv
sed -i '' 1,2d DGP_ATT_NEW.csv