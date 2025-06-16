## SELECT

> **OPEN SQL中标准的代码语法样例**

```
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
```

## INSERT

> **插入单条**

```abap
INSERT INTO <tabname> VALUES wa.
INSERT      <tabname> FROM wa.
```

> **插入多条**

```abap
INSERT <tabname> FROM TABLE it_tab [ACCEPTING DUPLICATE KEYS].

- 说明：
[ACCEPTING DUPLICATE KEYS]：遇到重复键值的跳过不插入，非重复键值的数据插入。
                            插入的数据中只要有一条记录键值是重复的，sy-subrc返回的结果都是非0
```

## UPDATE

> **更新单条**

```abap
UPDATE <tabname> FROM wa.
```

> **更新多条**

```abap
UPDATE <tabname> FROM TABLE it_tab.
UPDATE <tabname> SET <field> = <value> WHERE <condition>.
```

## DELETE

> **删除单条**

```abap
DELETE <tabname> FROM wa.
```

> **删除多条**

```abap
DELETE <tabname> FROM TABLE it_tab.
DELETE FROM <tabname> WHERE <condition>.
```

## MODIFY

> **修改单条**

```abap
MODIFY <tabname> FROM wa.
```

> **修改多条**

```abap
MODIFY <tabname> FROM TABLE it_tab.
MODIFY <tabname> FROM VALUE #( <field> = <value> )
                      TRANSPORTING <field>
                      WHERE <condition>.
```

## RANGE

> **定义方式**

```abap
DATA r_range LIKE RANGE OF <object> WITH HEADER LINE.
SELECT-OPTIONS r_range FOR <object>.

- 说明：
<object>：自定义变量、某个表的字段、字段类型

上面两个语句会生成如下结构的内表，该条件内表的每一行都代表一个逻辑条件：
DATA: BEGIN OF r_range OCCURS 0, 
        sign   TYPE c LENGTH 1, 允许值为I和E，I表示包含 Include，E表示排除Exclude
        option TYPE c LENGTH 2, option 表示选择运算符，
        low    LIKE dobj, 下界，相当于前面文本框中的值
        high   LIKE dobj, 上界，相当于后面文本框中的值
END OF r_range.

option: high 字段为空，则取值可以为：EQ（=）、NE（<>）、GT（>）、GE（>=）、LE（<=）、LT（<）
        CP、NP、CP（集合之内的数据）和NP（集合之外数据）只有当在输入字段中使用了通配符（“*”或“+”）时它们才是有效的
```

## FOR ALL ENTRIES

> **注意事项**

1. 使用该选项后，对于最后得出的结果集系统会自动删除重复行。因此如果你要保留重复行记录时，记得在SELECT语句中添加足够字段。
2. FOR ALL ENTRIES IN后面使用的内部表itab如果为空，将查出当前CLIENT端所有数据（即忽略整个WHERE语句，即使Where后面还有其它条件,其他条件都会被忽略）。
3. 内表中的条件字段不能使用BETWEEN、LIKE、IN比较操作符。
4. 使用该语句时，ORDER BY语句和HAVING语句将不能使用。
5. 使用该语句时，除COUNT( * )（并且如果有了COUNT函数，则不能再选择其他字段，只能使用在Select ... ENDSelect语句中了）以外的所有合计函数（MAX,MIN,AVG,SUM）都不能使用。

> **HINTS**

1. 使用FOR ALL ENTRY 两个栏位以上的，可以使用如下语句可以提升性能。

   ```abap
   %_HINTS HDB 'prefer_join_with_fda 1&&max_blocking_factor 888&'
   ```
2. 只有一个栏位的话，使用如下hints。

   ```abap
   %_HINTS HDB 'prefer_in_itab_opt 1&&max_in_blocking_factor 888&'
   ```

## INNER JOIN & LEFT OUTER JOIN

> **图示**

![JOIN](../../images/OPEN_SQL/JOIN.png)

> **注意事项**

1. 必需有ON条件语句，且多个条件之间只能使用AND连接。
2. 每个条件表达式中两个操作数之中必须有一个字段是来自于JOIN右表。
3. 如果是LEFT OUTER JOIN，则至少有一个条件表达式的两个操作数一个是来自于左表，另一个来自右表。
4. 不能使用NOT、LIKE、IN（但如果是 INNER JOIN，则>、<、BETWEEN …AND、<>都可用）。
5. 如果是LEFT OUTER JOIN，则只能使用等号操作符：(=、EQ)。
6. 如果是LEFT OUTER JOIN，同一右表不能多次出现在不同的LEFT OUTER JOIN的ON条件表达式中。
7. LEFT OUTER JOIN的右表所有字段不能出现在WHERE中。
8. 如果是LEFT OUTER JOIN，则在同一个ON条件语句中只能与同一个左表进行关联。
