register s3n://uw-cse-344-oregon.aws.amazon.com/myudfs.jar
raw = LOAD 's3n://uw-cse-344-oregon.aws.amazon.com/cse344-test-file' USING TextLoader as (line:chararray);

-- parse each line into ntriples
ntriples = foreach raw generate FLATTEN(myudfs.RDFSplit3(line)) as (subject:chararray,predicate:chararray,object:chararray);

-- Filter subject matches '.*business.*'
-- ntriples_filtered = FILTER ntriples BY subject == '.*business.*';
ntriples_filtered = FILTER ntriples BY subject matches '.*business.*';

-- Copy of the filtered data
-- ntriples_filtered_2 = ntriples_filtered as (subject2:chararray,predicate2:chararray,object2:chararray);
ntriples_filtered_2 = FOREACH ntriples_filtered GENERATE * AS (subject2:chararray,predicate2:chararray,object2:chararray);

-- Join by subject=subject2
joined = JOIN ntriples_filtered BY subject, ntriples_filtered_2 BY subject2;

-- Remove duplicates
joined_clean = DISTINCT joined PARALLEL 50;

store joined_clean into '/user/hadoop/example-results-2' using PigStorage();