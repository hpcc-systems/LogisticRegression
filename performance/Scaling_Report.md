# Environment
The test environment consists of an AWS based cluster of five (5) *r3.8xlarge* instances and one (1) *r3.large* instance.  There were one hundred (100) THOR slave nodes split evenly on the five (5) r3.8xlarge instances.  The THOR master and all of the other platform components were run on the r3.large instance.
The THOR configuration was:
``` xml
<Thor xmlns:fo="http://www.w3.org/1999/XSL/Format" name="mythor" nodeGroup="mythor" outputNodeGroup="mythor" channelsPerSlave="1"daliServers="10.0.0.252:7070" globalMemorySize="11448" masterMemorySize="28939" monitorDaliFileServer="true" pluginsPath="/opt/HPCCSystems/plugins//"replicateOutputs="true" slavesPerNode="20" watchdogEnabled="true" watchdogProgressEnabled="true">
```
# Test desription
There were two series of tests run.  The first series was to establish scaling behavior for the *Myriad* performance profile.  The second series was to establish scaling behavior for the *Large* performance profile.
The test data was randomly generated.  The test data generation is provided by the *performance/GenData.ecl* attribute.
The *Myriad* tests were performed with 10,000 observation datasets with 50 explanatory variables.  The number of concurrent problems was varied between 1 and 400.
The *Large* tests used 1,000 explanatory variables, one problem, and varied the number of observations between 250 thousand and 2 million.
# Test results
The times reported do not include the compilation time.

## *Myriad* profile
The *Myriad* tests used problems with 10 thousand observations and 50 explanatory variables.  The results are:
Problems | Time (seconds)
---------|----------------
1 | 14
10 | 19
50 | 22
100 | 23
200 | 32
400 | 47

Recall that this is a 100 node cluster, so the "flat" time values between 10 and 100 simply reflect that the additional problems are using the previously unused nodes.

The cases between 100 and 400 problems show a linear change, but slightly slower than the growth in problems.  This is probably due to the fact that the AWS instances have more than enough memory to allow the platform to pipeline the work and handle more than one problem concurrently in each THOR node.

## *Large* profile
The *Large* tests used a single problems with one thousand explanatory variables.  The results are:
Observations | Time (seconds)
-------------|---------------
250,000      | 1,127
500,000      | 2,381
1,000,000    | 4,724
2,000,000    | 9,280

The reslts show a linear increase in time proportional to the increase in observations.

The filesystem size for the *r3.8xlarge* instances was insufficient to process datasets with more two (2) million records with one (1) thousand explanatory variables.
