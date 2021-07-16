# Usage of CP4MIS

This is the usage of CP4MIS, the implementation of a constraint programming model for itemset mining with multiple minimum supports.

The implementation was carried out in the Oscar solver (bitbucket.org/oscarlib/oscar/) using Scala.

The jar file is available on https://drive.google.com/file/d/1aitzvYpDgKqW7pBhOwgoktL0usOmJkAD/view?usp=sharing

You can use the jar file for direct usage.


```
java -jar CP4MIS.jar <TDB File> <Lsup> <Beta> [options]

  <TDB File>
        the input transactions database
  <Lsup>
        the lower support - the lower bound of the frequency (MIS-min) - represents the minimum number of transactions. If <lsup> is between 0 and 1 it the relative support and the absolute support otherwise
  <Beta>
        Beta Value
  -D <Distance-UB> | --Distance <Distance-UB>
        Distance constraint: the upper bound of the distance between MISs in the itemset
  -C <Cardinality-LB> | --Cardinality <Cardinality-LB>
        Cardinality constraint: the lower bound size of the itemset
  -K <K> | --K-pattern <K>
        K pattern mining: mine K distinct itemsets
  -v | --verbose
        output all result with every details
  -mis | --mis-values
        Store the MIS values in a file 'MIS-TDBName-MISmin-beta'
  -csv | --CSV
        Put results in a CSV file
  -to <value> | --timeout <value>
        the timeout in seconds
  --help
        Usage of CP4MIS

```
        
# Example 1
On the dataset "chess" mine frequent itemsets with multiple supports with a lower support (MIS-min) of 90% and beta of 0.8. 

This is done by the following command:

```
java -jar CP4MIS.jar Datasets/chess 0.9 0.8 
```

# Example 2
On the dataset "chess" mine frequent itemsets with multiple supports with a lower support (MIS-min) of 1000 and beta of 0.5 and print the itemsets.

This is done by the following command:

```
java -jar CP4MIS.jar Datasets/chess 1500 0.5 -v
```

# Example 3
On the dataset "chess" mine frequent itemsets with multiple supports with a lower support (MIS-min) of 2500 and beta of 0.9.
In addition the itemsets sould satisfy the constraint of distance with an upper-bound of 150 and the cardinality constraint with a lower bound of 3.

This is done by the following command:

```
java -jar CP4MIS.jar Datasets/chess 2500 0.9 -D 150 -C 3 -v
```
# Example 4
Mine 5 distinct itemsets on "chess" with MIS-min = 1500, beta =  0.9, ub of the distance = 150 and lb of the size = 3.

This is done by the following command:

```
java -jar CP4MIS.jar Datasets/chess 2500 0.9 -K 5 -D 150 -C 3 -v
```


