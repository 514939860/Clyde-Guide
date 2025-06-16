## **AbapCatalog Annotations**

* **AbapCatalog.sqlViewName**

  * CDS 视图，数据库视图的名称
  * 任意 16 个字符的值（对于自定义对象，从 Z 开始）
  * 仅 CDS 视图需要
* **AbapCatalog.sqlViewAppendName**

  * CDS 视图扩展，附加视图的名称
  * 任意 16 个字符的值（对于自定义对象，从 Z 开始）
  * 扩展 CDS 视图时需要
* **AbapCatalog.viewEnhancementCategory[ ]**

  * CDS 视图，指定如何使用 CDS 视图扩展来扩展视图
  * **#PROJECTION_LIST**: 允许扩展SELECT列表和附加的CDS关联；不允许扩展其SELECT列表包含聚合表达式或包含集合操作符子句的CDS视图。
  * **#GROUP_BY**: 只能与#PROJECTION_LIST一起指定。允许扩展在SELECT列表中具有聚合表达式的视图。可以通过扩展将聚合或非聚合元素添加到SELECT列表中。GROUP-BY子句必须使用GROUP BY进行相应的扩展。
  * **#UNION**: 只能与#PROJECTION_LIST一起指定。允许用set操作符扩展使用子句的视图。
  * **#NONE**:不允许拓展；不能与其他值一起指定。
* AbapCatalog.dataMaintenance

  * 可用于限制CDS视图实体的数据预览
  * **#RESTRICTED:** 允许显示，但有限制
  * **#DISPLAY_ONLY**:只允许数据预览
  * **#NOT_ALLOWED**:不允许显示

## **AccessControl Annotations**

* **AccessControl.authorizationCheck**
  * CDS 访问控制，指定隐式访问控制
  * **#CHECK:** 如果使用ABAP SQL访问视图，则在将CDS角色分配给视图时隐式执行访问控制。如果视图没有角色，则会出现语法检查警告。
  * **#NOT_ALLOWED**: 类似于#CHECK，但是没有语法检查警告。
  * **#NOT_REQUIRED**: 不进行访问控制。如果在DCL源代码中将角色分配给此视图，则会出现语法警告。
  * **#PRIVILEGED_ONLY**: 特权CDS关联（由SADL评估）。
  * 默认值为 #NOT_REQUIRED
  * 对于测试 RAP 服务，请使用默认值 #NOT_REQUIRED

## **ClientHandling Annotations**

* **ClientHandling.type**
  * 指定客户端依赖项
  * 这不是强制性的，但可用于基于 DDIC 的视图和表函数
  * **#CLIENT_DEPENDENT**
  * **#CLIENT_INDEPENDENT**
  * **#INHERITED**
  * **默认值为 #INHERITED**

## **EndUserText Annotations**

* **EndUserText.label**
  * 短文本
  * 标签自动来自数据元素，但在使用基本类型时，标签很有用
* **EndUserText.quickInfo**
  * 工具提示信息

## **Environment Annotations**

* **Environment.systemField**
  * Environment，分配 ABAP 系统字段
  * #CLIENT
  * #SYSTEM_DATE
  * #SYSTEM_LANGUAGE
  * #SYSTEM_TIME
  * #USER
  * #USER_DATE
  * #USER_TIMEZONE
  * 这些字段可用于将默认系统变量值分配给参数或在列表中分配

## **MappingRole Annotations**

* **MappingRole**
  * 访问控制，指定向用户分配 CDS 角色
  * 默认值为 true
  * 出于测试服务或学习目的，请忽略此注释

## **Metadata Annotations**

* **Metadata.allowExtensions**
  * 使用元数据扩展指定扩展性
  * 默认值为 true
* **Metadata.ignorePropagatedAnnotations**
  * 指定如何评估传播的批注
  * **#true:** 只接受当前CDS视图实体的直接和派生注释。
  * **#false:** 接受所使用的CDS视图实体的继承注释
* **Metadata.layer**
  * CDS 元数据扩展模块中的指定图层
  * #CORE
  * #CUSTOMER
  * #INDUSTRY
  * #LOCALIZATION
  * #PARTNER
  * 通常，我们使用 value #CUSTOMER

## **Consumption Annotations**

> **Filter**

* **Consumption.filter.defaultValue**
  * 指定视图元素过滤器的默认值。
* **Consumption.filter.defaultValueHigh**
  * 此注释与 defaultValue 结合使用，以指定视图元素筛选器的默认间隔。
* **Consumption.filter.hidden**
  * 指定隐藏过滤器
* **Consumption.filter.mandatory**
  * 强制用户输入值，即使存在默认值。
* **Consumption.filter.multipleSelections**
  * 可以在过滤器输入中输入多个值
* **Consumption.filter.selectionType**
  * 定义如何输入值
  * #SINGLE。#INTERVAL、#RANGE、#HIERARCHY_NODE 是可能的值

> **Element / Property**

* **Consumption.hidden**
  * 防止 OData 公开字段
* **Consumption.defaultValue**
  * 操作导入参数，用于通过 UI 使用定义操作使用的默认值。
* **Consumption.semanticObject**
  * 用于应用程序之间的互操作性
  * 必须在 Fiori Launchpad 中定义的语义语义对象指定为值。
  * SAP Fiori 引入了基于意图的导航概念，其中意图是 `<semanticObject>` `<action>` 的组合。在 SAP Fiori UI 中使用 semantic Object 注释，以动态派生带注释视图的导航目标作为源。

> **Value Help**

* **Consumption.valueHelpDefinition.distinctValues**
  * 指定值帮助结果列表是否应仅包含不同的值
* **Consumption.valueHelpDefinition.entity[element** , **name]**
  * **name**: 指定包含提供值 help 的元素的实体
  * **element**: 名称中引用的实体中提供值 help 的元素
* **Consumption.valueHelpDefinition.label**
  * 包含用于标记值列表的与语言相关的文本
* **Consumption.valueHelpDefinition.useForValidation**
  * 标记应用于验证用户输入的值帮助
* **Consumption.ranked**
  * 此注释使值帮助视图中的搜索结果能够按搜索分数自动排序。

## **ObjectModel Annotations**

* **ObjectModel.filter.enabled**

  * 根据值 true 或 false 将实体元素设置为可筛选或不可筛选。
* **ObjectModel.sort.enabled**

  * 根据值 true 或 false 将实体元素设置为可排序或不可排序。
* **ObjectModel.text.association**

  * 定义关联的视图，该视图提供文本描述。
* **ObjectModel.text.element[ ]**

  * 将字段与其描述性语言无关的文本连接起来。
* **ObjectModel.usageType.sizeCategory**

  * sizeCategory 用于支持 HANA 中的资源消耗。
  * **#S**: 预期笔数< 1.000
  * **#M**: 预期笔数介于 1.000 和 99.999 之间
  * **#L**: 预期笔数介于 100.000 和 9.999.999 之间
  * **#XL**: 预期笔数介于 10.000.000 和 99.999.999 之间
  * **#XXL**: 预期笔数为 >= 100.000.000
* **ObjectModel.query.implementedBy**

  * 引用非托管查询的查询实现类。
* **ObjectModel.virtualElementCalculatedBy**

  * 引用带批注的虚拟元素的计算类。
* **ObjectModel.usageType.dataClass**

  * 指定CDS视图实体的数据类别。
  * **#TRANSACTIONAL**: CDS视图实体交付在大容量事务或后台事务中写入或修改的数据。
  * **#MASTER**: CDS视图实体交付主数据。主数据在大容量事务或后台事务中读取，但不写入或修改。
  * **#ORGANIZATIONAL**: CDS视图实体提供描述组织结构和客户流程的数据。
  * **#CUSTOMIZING**: CDS视图实体包含描述客户自定义数据的数据。
  * **#META**: CDS视图实体提供技术系统配置数据或实体的结构。
  * **#MIXED**: CDS视图实体包含混合数据类别的数据。
* **ObjectModel.usageType.serviceQuality**

  * CDS视图实体可以被分配到以下质量类别之一：
  * **#A**: CDS视图实体可用于大容量事务或后台事务。访问不超过三个DDIC数据库表，不得调用任何函数，不得聚合大量表行进行直接访问，并且不得访问具有混合数据类别的DDIC数据库表。如果底层表被缓冲，CDS视图实体也应该被缓冲。从字段列表中选择字段时，读取带有完整指定键的单行的运行时间必须小于1毫秒，而选择带有*的所有字段时，则必须小于2毫秒。
  * **#B**: CDS视图实体可用于事务或后台事务。访问的DDIC数据库表不得超过5个，不得调用任何函数，不得聚合大量的表行进行直接访问，也不得访问混合数据类别的DDIC数据库表。如果底层表被缓冲，CDS视图实体也应该被缓冲。对于字段列表中的字段，读取带有完整指定键的单行的运行时间必须小于2毫秒，对于选择带有*的所有字段，运行时间必须小于5毫秒。
  * **#C**: CDS视图实体可用于查询事务中的单个对象。访问不超过15个DDIC数据库表，不得聚合大量表行进行直接访问，也不得访问具有混合数据类别的DDIC数据库表。对于字段列表中的字段，读取带有完整指定键的单行的运行时间必须小于10毫秒，对于选择带有*的所有字段，运行时间必须小于20毫秒。
  * **#D**: CDS视图实体可用于分析查询。访问不超过100个DDIC数据库表。性能应该由测试框架使用实际数据进行检查和监控。
  * **#X**: CDS视图实体是为特殊的应用案例而设计的，比如数据迁移。可以访问100多个数据库表。性能必须通过测试框架用实际数据进行检查和监控。
  * **#P**: CDS视图实体用于构建CDS实体的层次结构，不能在这种层次结构之外使用。不打算在业务应用程序中使用。不需要测试框架进行性能检查。
  * 对于所有serviceQuality，都应该指定子注释sizeCategory和dataClass。只有#P，不需要指定。

## **OData Annotations**

* **OData.action**
  * 表示操作的外部名称
* **OData.entitySet.name**
  * 表示实体集的外部名称
* **OData.entityType.name**
  * 表示实体类型的外部名称
* **OData.etag**
  * 在行为定义中声明 ETag。根据 SAP 文档 – 不应使用此注释

## **Search Annotations**

* **Search.searchable**

  * 定义 CDS 实体是否通常与搜索方案相关。
  * 可以分配*布尔值 （true， false）*
* **Search.defaultSearchElement**

  * 指定在未指定列的自由式搜索中要考虑该元素。
  * 可以分配*布尔值 （true， false）*
* **Search.ranking**

  * 指定元素的值与排名的相关性
  * HIGH – 元素具有高相关性
  * MEDIUM – 元素具有中等相关性
  * LOW – 尽管该元素与自由式搜索相关，但此元素中的命中对排名没有实际意义
* **Search.fuzzinessThreshold**

  * 指定最小级别的模糊度
  * 值格式为  *Decimal （3,2），* 值介于 0 到 1 之间。例如，0.8
  * 介于 0.7 和 0.99 之间的值将最有用。1 表示完全匹配。

## **Semantics Annotations**

* **Semantics.address.type []**

  * #HOME – 家庭住址
  * #WORK – 工作地址
  * #PREF – 首选地址 （默认）
  * OTHER – 其他地址
* **Semantics.address.city， Semantics.address.country， Semantics.address.number， Semantics.address.postBox Semantics.address.region， Semantics.address.zipCode**

  * 这些注释表示该字段是 address 的一部分，表示根据注释名称
* **Semantics.amount.currencyCode**

  * 指定投影列表中的货币代码字段
* **Semantics.currencyCode**

  * 指定字段为货币
* **Semantics.quantity.unitOfMeasure**

  * 从投影列表中指定 UOM 字段
* **Semantics.unitOfMeasure**

  * 指定字段为度量单位
* **Semantics.calendar.dayOfMonth， Semantics.calendar.dayOfYear， Semantics.calendar.halfyear， Semantics.calendar.month， Semantics.calendar.quarter， Semantics.calendar.week， Semantics.calendar.year**

  * 带注释的字段的值会相应地进行编码。
* **Semantics.dateTime**

  * 标记包含日期和时间值的字段。
* **Semantics.language**

  * 标识语言字段。
* **Semantics.systemDateTime.createdAt， Semantics.systemDateTime.lastChangedAt， Semantics.systemDateTime.LocalInstanceLastChangedAt， Semantics.user.createdBy， Semantics.user.lastChangedBy**

  * 这些可用于注释实体中的更改日志记录和 etag 字段

## **UI Annotations**

## **Aggregation Annotations**
