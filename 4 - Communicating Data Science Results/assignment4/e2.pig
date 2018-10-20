
register s3n://uw-cse-344-oregon.aws.amazon.com/myudfs.jar
raw = LOAD 's3n://uw-cse-344-oregon.aws.amazon.com/cse344-test-file' USING TextLoader as (line:chararray);

-- parse each line into ntriples
ntriples = foreach raw generate FLATTEN(myudfs.RDFSplit3(line)) as (subject:chararray,predicate:chararray,object:chararray);

-- group the n-triples by subject column
subjects = group ntriples by (subject) PARALLEL 50;

-- flatten the subjects out (because group by produces a tuple of each subject
-- in the first column, and we want each subject to be a string, not a tuple),
-- and count the number of tuples associated with each subject
count_by_subject = foreach subjects generate flatten($0), COUNT($1) as count PARALLEL 50;

-- group by count column
counts = group count_by_subject by (count) PARALLEL 50;

-- compute the frequency of each count
count_by_counts = foreach counts generate flatten($0), COUNT($1) as count_total PARALLEL 50;

store count_by_counts into '/user/hadoop/example-results' using PigStorage();