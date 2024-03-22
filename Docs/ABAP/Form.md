## 上传文档（Excel）
> **代码示例**
~~~abap
PARAMETERS p_upath LIKE rlgrap-filename.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upath .
  PERFORM get_local_filename CHANGING p_upath.

START-OF-SELECTION.
  PERFORM check_path.
  PERFORM upload_data.

FORM get_local_filename  CHANGING p_fname.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_fname.
ENDFORM.

FORM check_path.
  DATA: l_file TYPE string,
        l_bool TYPE abap_bool.

  IF p_upath IS INITIAL.
    MESSAGE s398(00) WITH 'Local file/path is required' DISPLAY LIKE 'E'.
    STOP.
  ELSE.
    l_file = p_upath.
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = l_file
      RECEIVING
        result               = l_bool
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE s398(00) WITH 'GUI error when check file exist' DISPLAY LIKE  'E'.
      STOP.
    ELSE.
      IF l_bool = abap_false.
        MESSAGE s398(00) WITH 'File not exit' l_file DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

FORM upload_data .
  DATA: BEGIN OF it_upload OCCURS 0,
          matnr LIKE mara-matnr,
        END OF it_upload.

  DATA: lt_excel  TYPE TABLE OF alsmex_tabline,
        lst_excel TYPE alsmex_tabline,
        l_tabix1  LIKE sy-tabix,
        lst_tab   LIKE it_upload.

  CONSTANTS: c_bcol TYPE i VALUE '1',
             c_brow TYPE i VALUE '2',
             c_ecol TYPE i VALUE '1',
             c_erow TYPE i VALUE '65535'.

  FIELD-SYMBOLS: <field> TYPE any.

  CLEAR: it_upload, it_upload[].

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_upath
      i_begin_col             = c_bcol  "从第几列开始
      i_begin_row             = c_brow  "从第几行开始
      i_end_col               = c_ecol  "到第几列为止
      i_end_row               = c_erow  "到第几行为止
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE s398(00) DISPLAY LIKE 'E'  WITH 'Upload file fail!'.
    STOP.
  ENDIF.

  IF lt_excel[] IS INITIAL.
    MESSAGE s398(00) DISPLAY LIKE 'E' WITH 'No data in upload file!'..
    STOP.
  ENDIF.

  LOOP AT lt_excel INTO lst_excel.

    AT NEW row.
      l_tabix1 = sy-tabix.
    ENDAT.

    AT END OF row.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE lst_tab TO <field>.
        IF sy-subrc = 0.
          READ TABLE lt_excel INDEX l_tabix1 INTO lst_excel.
          IF sy-subrc = 0 AND lst_excel-col = sy-index.
            TRANSLATE lst_excel-value TO UPPER CASE.
            CONDENSE lst_excel-value.
            <field> = lst_excel-value.
            ADD 1 TO l_tabix1.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      IF lst_tab IS NOT INITIAL.
        MOVE-CORRESPONDING lst_tab TO it_upload.
        APPEND: it_upload.
      ENDIF.
      CLEAR: it_upload.
    ENDAT.
  ENDLOOP.

  IF it_upload[] IS INITIAL.
    MESSAGE s398(00) WITH 'No data upload, please check the file!' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
~~~

<!-- ============================================================分割线=====================================================================-->

## ALV Report（Function）
> **代码示例**
~~~abap

~~~