## 基本数据类型
| 类型 | 最大长度（字符数） | 默认长度 | 说明|
|---|---|---|---|
| C | 1~262143个字符       | 1 字符   | |
| N | 1~262143个字符       | 1 字符   | 0到9之间字符组成的数字字符串 |
| D | 16 bytes             |         | 日期格式必须为 YYYYMMDD |
| T | 12 bytes             |         | 格式为 24-hour的 HHMMSS |
| I | 4 bytes              |         | -2.147.483.648 to +2.147.483.647 |
| F | 8 bytes              |         | 小数位最大可以到17位，即可精确到小数点后17位 |
| P | 1~16 bytes           | 8 bytes | 两个数字位压缩后才占一个字节，由于0-9的数字只需要4Bit位，所以一个字节实质上允许存储二位数字，这就是P数据类型为压缩数据类型的由来。并借用半个字节来存储小数点位置、正号、负号相关信息<br><br>如果不指定小数位，则将视为I类型。在使用P类型时，要先选择程序属性中的选项 Fixed point arithmetic（即定点算法，一般默认选中），否则系统将P类型看用整型。其效率低于I或F类型。<br><br>最大整数位：16*2 = 32 - 1 = 31 -14(允许最大小数位数) = 17位整数位<br><br>正负符号与小数点固定要占用半个字节<br><br>可用于精确运算（这里的精确指的是存储中所存储的数据与定义时字面上所看到的大小相同，而不存在精度丢失问题——看到的就是内存中实实在在的大小）|
| X | 1~524,287 bytes      | 1 byte  |十六进制字符 0-9, A-F具体的范围为：00~FF<br><br>类型X是十六进制类型，可表示内存字节实际内容，使用两个十六制字符表示一个字节中所存储的内容。但直接打印输出时，输出的还是赋值时字面意义上的值，而不是Unicode解码后的字符<br><br>如果未在 DATA 语句中指定参数'length'，则创建长度为 1<br><br>注：如果值是字母，则一定要大写 |
> Bits（比特）&emsp;&emsp; Byte（字节B）&emsp;&emsp; 1 字符 = 2 字节<br>
> 1 Byte = 8 Bits &emsp; 1 KB = 1024 Bytes &emsp; 1 MB = 1024 KB &emsp; 1 GB = 1024 MB &emsp; 1 TB = 1024  GB

<!-- ============================================================分割线=====================================================================-->

## Data element & Domain
> **Data element是构成结构、表的基本组件,Data element主要附带Search Help、Parameter ID、以及标签描述，而类型是由Domain域来决定的。**

> **Domain主要从技术属性方面描述了Data element，如Data Type数据类型、Output Length输出长度、Convers. Routine转换规则、以及Value Range取值范围。**

> **将技术信息从Data element提取出来为Domain域的好处：<br>技术信息形成的Domain可以共用，而每个表字段的业务含意不一样，会导致其描述标签、搜索帮助不一样，所以牵涉到业务部分的信息直接Data element中进行描述，而与业务无关的技术信息部分则分离出来形成Domain**

<!-- ============================================================分割线=====================================================================-->
## Type & Like
> **TYPE后面跟随的只能是某种类型,而LIKE后面可以跟实例对象。**<br>

> **参照数据类型（标准表、add-on表、structure、表的栏位）声明变量时，因为可以视作对象，所以既可以用TYPE，也可以用LIKE。**
~~~abap
DATA: lt_marc1 TYPE marc,
      lt_marc2 LIKE marc.

DATA: lt_ftp1 TYPE zbaftpma0001,
      lt_ftp2 LIKE zbaftpma0001.

DATA: l_werks1 TYPE marc-werks,
      l_werks2 LIKE marc-werks.
~~~

> **参照结构体对象生成内表时只能用LIKE,不能用TYPE。**
~~~abap
DATA: BEGIN OF st_marc,
        werks TYPE marc-werks,
        matnr TYPE marc-matnr,
      END OF st_marc.

DATA lt_marc LIKE st_marc.
~~~

> **定义的变量名不能与词典中的类型同名，否则表面上TYPE、LIKE都可以使用，实际上是指向不同的类型。**
~~~abap
DATA: BEGIN OF marc,
        werks TYPE marc-werks,
        matnr TYPE marc-matnr,
      END OF marc.

DATA: lt_marc1 TYPE marc,
      lt_marc2 LIKE marc.
~~~
- 上述例子中，"lt_marc1"参照的是数据字典中的marc，"lt_marc2"参照的是代码前面声明的结构marc。

![TYPE](../../images/Basics/TYPE.png)
![LIKE](../../images/Basics/LIKE.png)

<!-- ============================================================分割线=====================================================================-->

## 字符串表达式
~~~abap
DATA: str1 TYPE string VALUE 'One Team',
      str2 TYPE string VALUE 'One Goal!'.

WRITE: 'One Team One Goal!',
       / str1 && ` ` && str2,
       / |{ str1 } One Goal!|,
       / |{ str1 } { str2 }|,
       / |{ str1 } { str2 } 这句话一共有： { strlen( str1 && ` ` && str2 ) } 个字符。|.
~~~
**Result:**

![STR](../../images/Basics/STR.png)

<!-- ============================================================分割线=====================================================================-->