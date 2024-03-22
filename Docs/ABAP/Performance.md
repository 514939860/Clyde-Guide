## LOOP WHERE
> **要点**
1. 根据`WHERE`条件对内表进行排序。
2. 使用`BINARY SEARCH`来获取符合条件的第一笔记录。
3. 使用索引来`LOOP`内表。
4. `LOOP`中添加`IF`判断，`LOOP`到不符合条件的资料时退出`LOOP`。

> **代码示例**
~~~abap
DATA: BEGIN OF lt_material occurs 0,
        werks LIKE marc-werks,
        matnr LIKE marc-matnr,
      END OF lt_material.

DATA l_tabix LIKE sy-tabix.

SORT lt_material by werks matnr.

READ TABLE lt_material WITH KEY werks = 'FXXX'
                                matnr = 'MATXXX'
                                BINARY SEARCH.
  IF sy-subrc = 0.
    l_tabix = sy-tabix.

    LOOP AT lt_material FROM l_tabix.
      IF lt_material-werks <> 'FXXX'.
        EXIT.
      ENDIF.
      ...  "此处插入要处理的逻辑
    ENDLOOP.
  ENDIF.
~~~

<!-- ============================================================分割线=====================================================================-->

## LOOP 嵌套 LOOP
> **要点**
1. 根据`WHERE`条件对内表进行排序。
2. 使用`BINARY SEARCH`来获取符合条件的第一笔记录。
3. 使用索引来`LOOP`内表。
4. `LOOP`中添加`IF`判断，`LOOP`到不符合条件的资料时退出`LOOP`。

> **代码示例**
~~~abap
DATA: BEGIN OF lt_matnr1 occurs 0,
        werks LIKE marc-werks,
        matnr LIKE marc-matnr,
      END OF lt_matnr1,

      BEGIN OF lt_matnr1 occurs 0,
        werks LIKE marc-werks,
        matnr LIKE marc-matnr,
      END OF lt_matnr1.

DATA l_tabix LIKE sy-tabix.

SORT lt_matnr1 by werks matnr.
SORT lt_matnr2 by werks matnr.

LOOP AT lt_matnr1
  READ TABLE lt_matnr2 WITH KEY werks = lt_matnr1-werks
                                matnr = lt_matnr1-matnr
                                BINARY SEARCH.
    IF sy-subrc = 0.
      l_tabix = sy-tabix.

      LOOP AT lt_matnr2 FROM l_tabix.
        IF lt_matnr2-matnr <> lt_matnr1-matnr.
          EXIT.
        ENDIF.
        ...  "此处插入要处理的逻辑
      ENDLOOP.
    ENDIF.
ENDLOOP.
~~~