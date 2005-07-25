
mysql> CREATE TABLE pet (name VARCHAR(20), owner VARCHAR(20),
    -> species VARCHAR(20), sex CHAR(1), birth DATE, death DATE);

 After creating your table, you need to populate it. The LOAD DATA and INSERT  statements are useful for this.

 You could create a text file pet.txt  containing one record per line, with values separated by tabs, and given in the order in which the columns were listed in the CREATE TABLE statement.
 For missing values (such as unknown sexes or death dates for animals that are still living), you can use NULL values. To represent these in your text file, use \N  (backslash, capital-N). 

mysql> LOAD DATA LOCAL INFILE '/path/pet.txt' INTO TABLE pet;

 When you want to add new records one at a time, the INSERT statement is useful. 

mysql> INSERT INTO pet
    -> VALUES ('Puffball','Diane','hamster','f','1999-03-30',NULL);

 The simplest form of SELECT retrieves everything from a table:

mysql> SELECT * FROM pet;

 Fix only the erroneous record with an UPDATE statement:

mysql> UPDATE pet SET birth = '1989-08-31' WHERE name = 'Bowser';

 You can select only particular rows from your table. 

mysql> SELECT * FROM pet WHERE name = 'Bowser';

mysql> SELECT * FROM pet WHERE birth >= '1998-1-1';

mysql> SELECT * FROM pet WHERE species = 'dog' AND sex = 'f';

mysql> SELECT * FROM pet WHERE species = 'snake' OR species = 'bird';

SELECT name, birth FROM pet;

 To find out who owns pets, use this query:

mysql> SELECT owner FROM pet;

 However, notice that the query simply retrieves the owner field from each record, and some of them appear more than once. To minimize the output, retrieve each unique output record just once by adding the keyword DISTINCT:

mysql> SELECT DISTINCT owner FROM pet;

 Here are animal birthdays, sorted by date:

mysql> SELECT name, birth FROM pet ORDER BY birth;

You can force a case-sensitive sort for a column by using the BINARY cast: ORDER BY BINARY col_name.

The default sort order is ascending, with smallest values first. To sort in reverse (descending) order, add the DESC keyword to the name of the column you are sorting by:

mysql> SELECT name, birth FROM pet ORDER BY birth DESC;

 You can sort on multiple columns, and you can sort columns in different directions. For example, to sort by type of animal in ascending order, then by birth date within animal type in descending order (youngest animals first), use the following query:

mysql> SELECT name, species, birth FROM pet
    -> ORDER BY species, birth DESC;

 To determine how many years old each of your pets is, compute the difference in the year part of the current date and the birth date, then subtract one if the current date occurs earlier in the calendar year than the birth date. The following query shows, for each pet, the birth date, the current date, and the age in years.

mysql> SELECT name, birth, CURDATE(),
    -> (YEAR(CURDATE())-YEAR(birth))
    -> - (RIGHT(CURDATE(),5)<RIGHT(birth,5))
    -> AS age
    -> FROM pet;

 Here, YEAR() pulls out the year part of a date and RIGHT() pulls off the rightmost five characters that represent the MM-DD  (calendar year) part of the date. The part of the expression that compares the MM-DD values evaluates to 1 or 0, which adjusts the year difference down a year if CURDATE() occurs earlier in the year than birth. The full expression is somewhat ungainly, so an alias (age) is used to make the output column label more meaningful.

The query works, but the result could be scanned more easily if the rows were presented in some order. This can be done by adding an ORDER BY name clause to sort the output by name:

mysql> SELECT name, birth, CURDATE(),
    -> (YEAR(CURDATE())-YEAR(birth))
    -> - (RIGHT(CURDATE(),5)<RIGHT(birth,5))
    -> AS age
    -> FROM pet ORDER BY name;

 To sort the output by age rather than name, just use a different ORDER BY clause:

mysql> SELECT name, birth, CURDATE(),
    -> (YEAR(CURDATE())-YEAR(birth))
    -> - (RIGHT(CURDATE(),5)<RIGHT(birth,5))
    -> AS age
    -> FROM pet ORDER BY age;

 A similar query can be used to determine age at death for animals that have died. You determine which animals these are by checking whether the death value is NULL. Then, for those with non-NULL values, compute the difference between the death and birth values:

mysql> SELECT name, birth, death,
    -> (YEAR(death)-YEAR(birth)) - (RIGHT(death,5)<RIGHT(birth,5))
    -> AS age
    -> FROM pet WHERE death IS NOT NULL ORDER BY age;

 The query uses death IS NOT NULL rather than death <> NULL because NULL is a special value that cannot be compared using the usual comparison operators. 

 Finding animals with birthdays in the upcoming month is easy, too. Suppose that the current month is April. Then the month value is 4 and you look for animals born in May (month 5) like this:

mysql> SELECT name, birth FROM pet WHERE MONTH(birth) = 5;

 You can even write the query so that it works no matter what the current month is. That way you don't have to use a particular month number in the query. DATE_ADD() allows you to add a time interval to a given date. If you add a month to the value of CURDATE(), then extract the month part with MONTH(), the result produces the month in which to look for birthdays:

mysql> SELECT name, birth FROM pet
    -> WHERE MONTH(birth) = MONTH(DATE_ADD(CURDATE(),INTERVAL 1 MONTH));

A different way to accomplish the same task is to add 1 to get the next month after the current one (after using the modulo function (MOD) to wrap around the month value to 0 if it is currently 12):

mysql> SELECT name, birth FROM pet
    -> WHERE MONTH(birth) = MOD(MONTH(CURDATE()), 12) + 1;

Note that MONTH returns a number between 1 and 12. And MOD(something,12) returns a number between 0 and 11. So the addition has to be after the MOD(), otherwise we would go from November (11) to January (1). 

Use the IS NULL and IS NOT NULL operators instead:

mysql> SELECT 1 IS NULL, 1 IS NOT NULL;

 Note that in MySQL, 0 or NULL means false and anything else means true. The default truth value from a boolean operation is 1.

 SQL pattern matching allows you to use '_' to match any single character and '%' to match an arbitrary number of characters (including zero characters). In MySQL, SQL patterns are case-insensitive by default. Some examples are shown here. Note that you do not use = or <> when you use SQL patterns; use the LIKE or NOT LIKE comparison operators instead.

To find names beginning with 'b':

mysql> SELECT * FROM pet WHERE name LIKE 'b%';

 To find names ending with 'fy':

mysql> SELECT * FROM pet WHERE name LIKE '%fy';

 To find names containing a 'w':

mysql> SELECT * FROM pet WHERE name LIKE '%w%';

 To find names containing exactly five characters, use five instances of the '_' pattern character:

mysql> SELECT * FROM pet WHERE name LIKE '_____';

 The other type of pattern matching provided by MySQL uses extended regular expressions. When you test for a match for this type of pattern, use the REGEXP and NOT REGEXP operators (or RLIKE and NOT RLIKE, which are synonyms).

 To demonstrate how extended regular expressions work, the LIKE queries shown previously are rewritten here to use REGEXP.

To find names beginning with 'b', use '^' to match the beginning of the name:

mysql> SELECT * FROM pet WHERE name REGEXP '^b';

 To find names ending with 'fy', use '$' to match the end of the name:

mysql> SELECT * FROM pet WHERE name REGEXP 'fy$';

 To find names containing a 'w', use this query:

mysql> SELECT * FROM pet WHERE name REGEXP 'w';

 To find names containing exactly five characters, use '^' and '$' to match the beginning and end of the name, and five instances of '.' in between:

mysql> SELECT * FROM pet WHERE name REGEXP '^.....$';

 You could also write the previous query using the '{n}' ``repeat-n-times'' operator:

mysql> SELECT * FROM pet WHERE name REGEXP '^.{5}$';

You can also store pattern in you database fields and use syntax like this:

SELECT country FROM ip_list WHERE '127.0.0.1' like ip

where ip is the field name with patterns.

 Counting the total number of animals you have is the same question as ``How many rows are in the pet  table?'' because there is one record per pet. COUNT(*) counts the number of rows, so the query to count your animals looks like this:

mysql> SELECT COUNT(*) FROM pet;

 Earlier, you retrieved the names of the people who owned pets. You can use COUNT() if you want to find out how many pets each owner has:

mysql> SELECT owner, COUNT(*) FROM pet GROUP BY owner;

 Note the use of GROUP BY to group together all records for each owner. 

 COUNT() and GROUP BY are useful for characterizing your data in various ways. The following examples show different ways to perform animal census operations.

Number of animals per species:

mysql> SELECT species, COUNT(*) FROM pet GROUP BY species;

 Number of animals per sex:

mysql> SELECT sex, COUNT(*) FROM pet GROUP BY sex;

 Number of animals per combination of species and sex:

mysql> SELECT species, sex, COUNT(*) FROM pet GROUP BY species, sex;
+---------+------+----------+
| species | sex  | COUNT(*) |
+---------+------+----------+
| bird    | NULL |        1 |
| bird    | f    |        1 |
| cat     | f    |        1 |
| cat     | m    |        1 |
| dog     | f    |        1 |
| dog     | m    |        2 |
| hamster | f    |        1 |
| snake   | m    |        1 |
+---------+------+----------+

 You need not retrieve an entire table when you use COUNT(). For example, the previous query, when performed just on dogs and cats, looks like this:

mysql> SELECT species, sex, COUNT(*) FROM pet
    -> WHERE species = 'dog' OR species = 'cat'
    -> GROUP BY species, sex;
+---------+------+----------+
| species | sex  | COUNT(*) |
+---------+------+----------+
| cat     | f    |        1 |
| cat     | m    |        1 |
| dog     | f    |        1 |
| dog     | m    |        2 |
+---------+------+----------+

Or, if you wanted the number of animals per sex only for known-sex animals:

mysql> SELECT species, sex, COUNT(*) FROM pet
    -> WHERE sex IS NOT NULL
    -> GROUP BY species, sex;


the CREATE TABLE statement for the event  table might look like this:

mysql> CREATE TABLE event (name VARCHAR(20), date DATE,
    -> type VARCHAR(15), remark VARCHAR(255));

 Suppose that you want to find out the ages at which each pet had its litters. We saw earlier how to calculate ages from two dates. The litter date of the mother is in the event table, but to calculate her age on that date you need her birth date, which is stored in the pet table. This means the query requires both tables:

mysql> SELECT pet.name,
    -> (YEAR(date)-YEAR(birth)) - (RIGHT(date,5)<RIGHT(birth,5)) AS age,
    -> remark
    -> FROM pet, event
    -> WHERE pet.name = event.name AND type = 'litter';
+--------+------+-----------------------------+
| name   | age  | remark                      |
+--------+------+-----------------------------+
| Fluffy |    2 | 4 kittens, 3 female, 1 male |
| Buffy  |    4 | 5 puppies, 2 female, 3 male |
| Buffy  |    5 | 3 puppies, 3 female         |
+--------+------+-----------------------------+


Sometimes it is useful to join a table to itself, if you want to compare records in a table to other records in that same table. For example, to find breeding pairs among your pets, you can join the pet table with itself to produce candidate pairs of males and females of like species:

mysql> SELECT p1.name, p1.sex, p2.name, p2.sex, p1.species
    -> FROM pet AS p1, pet AS p2
    -> WHERE p1.species = p2.species AND p1.sex = 'f' AND p2.sex = 'm';
+--------+------+--------+------+---------+
| name   | sex  | name   | sex  | species |
+--------+------+--------+------+---------+
| Fluffy | f    | Claws  | m    | cat     |
| Buffy  | f    | Fang   | m    | dog     |
| Buffy  | f    | Bowser | m    | dog     |
+--------+------+--------+------+---------+

In this query, we specify aliases for the table name in order to refer to the columns and keep straight which instance of the table each column reference is associated with. 

3.4. Getting Information About Databases and Tables

What if you forget the name of a database or table, or what the structure of a given table is (for example, what its columns are called)? MySQL addresses this problem through several statements that provide information about the databases and tables it supports.

You have previously seen SHOW DATABASES, which lists the databases managed by the server. To find out which database is currently selected, use the DATABASE() function:

mysql> SELECT DATABASE();
+------------+
| DATABASE() |
+------------+
| menagerie  |
+------------+

If you haven't selected any database yet, the result is NULL (or the empty string before MySQL 4.1.1).

To find out what tables the current database contains (for example, when you're not sure about the name of a table), use this command:

mysql> SHOW TABLES;
+---------------------+
| Tables in menagerie |
+---------------------+
| event               |
| pet                 |
+---------------------+

If you want to find out about the structure of a table, the DESCRIBE command is useful; it displays information about each of a table's columns:

mysql> DESCRIBE pet;
+---------+-------------+------+-----+---------+-------+
| Field   | Type        | Null | Key | Default | Extra |
+---------+-------------+------+-----+---------+-------+
| name    | varchar(20) | YES  |     | NULL    |       |
| owner   | varchar(20) | YES  |     | NULL    |       |
| species | varchar(20) | YES  |     | NULL    |       |
| sex     | char(1)     | YES  |     | NULL    |       |
| birth   | date        | YES  |     | NULL    |       |
| death   | date        | YES  |     | NULL    |       |
+---------+-------------+------+-----+---------+-------+

Field indicates the column name, Type is the data type for the column, NULL indicates whether the column can contain NULL values, Key indicates whether the column is indexed, and Default specifies the column's default value.

If you have indexes on a table, SHOW INDEX FROM tbl_name produces information about them. 

