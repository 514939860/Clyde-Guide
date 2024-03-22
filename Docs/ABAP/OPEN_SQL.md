## SELECT
> **OPEN SQL中标准的代码语法样例**
~~~
SELECT [SINGLE/DISTINCT/AGGREGATE] <fields>/<field as field name>
  FROM <table>
  [WHERE <condition>]
  [ORDER BY <fields> [ASC|DESC]]
  [GROUP BY <fields> [HAVING <condition>]]
  [INTO/APPENDING <CORRESPONDING FIELDS OF> <internal table>]
  [FOR ALL ENTRIES IN <internal table>]
  [UP TO <n> ROWS]
  [HINT <hint>]
  [FOR UPDATE].

- 参数介绍：
[SINGLE]：可选项，从数据库中检索单个行，如果检索到多行数据，则只返回第一行数据。
[DISTINCT]：可选项，自动删除所查询数据的重复项。
[AGGREGATE]：AVG    → 取平均值
             COUNT  → 取个数
             MAX    → 取最大值
             MIN    → 取最小值
             STDDEV → 取标准偏差
             SUM    → 取合计

[WHERE]：可选项，查询条件。
[ORDER BY]：可选项，按字段对查询结果进行排序。
[GROUP BY]：使用GROUP BY语句的前提条件是查询数据时使用了聚合函数
[INTO/APPENDING]：覆盖资料/追加资料。
[FOR ALL ENTRIES IN]：可选项，
[HINT]：可选项，用于优化数据库查询。
[FOR UPDATE]：锁定检索到的行，防止其他用户同时修改相同的数据

<fields>：需要检索的字段列表。
<field as field name>：为字段名定义别名
<table>：需要从中检索数据的数据库表。
<condition>：可选项，用于过滤检索结果的条件。
<internal table>：可选项，表示检索结果将被存储在内部表中。
FOR ALL ENTRIES IN <internal table>：可选项，表示使用内部表中的值作为条件来检索数据。
<n>：可选项，表示返回的最大行数。
<hint>：可选项，用于优化数据库查询。
<CORRESPONDING FIELDS OF>：可选项，匹配字段名和内表栏位名赋值。
~~~

## INSERT
> **插入单条**
~~~
INSERT INTO <tabname> VALUES wa.
INSERT      <tabname> FROM wa.
INSERT      <tabname> FROM SQL.
~~~
> **插入多条**
~~~
INSERT <tabname> FROM TABLE it_tab [ACCEPTING DUPLICATE KEYS].

- 参数介绍：
[ACCEPTING DUPLICATE KEYS]：遇到重复键值的跳过不插入，非重复键值的数据插入。
                            插入的数据中只要有一条记录键值是重复的，sy-subrc返回的结果都是非0
~~~
## UPDATE
## DELETE
## MODIFY