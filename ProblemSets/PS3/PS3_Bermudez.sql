#  Read in Data
.mode csv
.import /home/ouecon001/DScourseS25/ProblemSets/PS3/FL_insurance_sample.csv FLinsurance

#  Print out first 10 rows of the data set
SELECT * FROM FLinsurance LIMIT 10;

# List which countries are in the sample
PRAGMA table_info(FLinsurance)
SELECT DISTINCT county FROM FLinsurance;

# Compute the average property appreciation from 2011 to 2012
SELECT avg(tiv_2012 - tiv_2011) AS avg_prop_app FROM FLinsurance; 

# avg_prop_app  = 398,129.10

# Create a frequency table of the construction variable to see what fraction of buildings are made out of wood or some other material
SELECT construction, COUNT(construction) AS freq, COUNT(construction)*1.0 / (SELECT COUNT(construction) FROM FLinsurance) AS rel_freq FROM FLinsurance
 GROUP BY construction ORDER BY rel_freq DESC;
