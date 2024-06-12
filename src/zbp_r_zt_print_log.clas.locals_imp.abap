CLASS lsc_zr_zt_print_log DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_zt_print_log IMPLEMENTATION.

  METHOD save_modified.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_log DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    " 创建打印记录方法，提供BAS OData V4 调用
    METHODS createprintrecord FOR MODIFY
      IMPORTING keys FOR ACTION log~createprintrecord.
    METHODS createprintfile FOR DETERMINE ON SAVE
      IMPORTING keys FOR log~createprintfile.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR log RESULT result.

ENDCLASS.

CLASS lhc_log IMPLEMENTATION.

  METHOD createprintrecord.
    DATA:lt_create_printing TYPE TABLE FOR CREATE zr_zt_print_log,
         create_printing    LIKE LINE OF lt_create_printing.
    READ TABLE keys INTO DATA(key) INDEX 1.
    CHECK sy-subrc = 0.
*    SELECT SINGLE * FROM zr_zt_prt_template WHERE "#EC CI_ALL_FIELDS_NEEDED
    SELECT  *
      FROM zr_zt_print_config
       FOR ALL ENTRIES IN @keys
     WHERE templatename = @keys-%param-templatename "#EC CI_ALL_FIELDS_NEEDED
      INTO TABLE @DATA(lt_config).
    SORT lt_config BY templatename.

    IF sy-subrc = 0.
      LOOP AT keys INTO key.
        READ TABLE lt_config INTO DATA(ls_config) WITH KEY templatename = key-%param-templatename BINARY SEARCH.
        IF sy-subrc = 0.
          create_printing = VALUE #( %cid                   = key-%cid
                                     templateuuid           = ls_config-uuid
                                     isexternalprovideddata = key-%param-isexternalprovideddata
                                     providedkeys           = key-%param-providedkeys
                                     externalprovideddata   = key-%param-externalprovideddata
                                     sendtoprintqueue       = key-%param-sendtoprintqueue
                                     numberofcopies         = key-%param-numberofcopies
                                     printqueue             = key-%param-printqueue ) .
          APPEND create_printing TO lt_create_printing.
        ENDIF.
      ENDLOOP.

      MODIFY ENTITIES OF zr_zt_print_log
          IN LOCAL MODE
          ENTITY log
          CREATE FIELDS ( templateuuid
                          isexternalprovideddata
                          providedkeys
                          externalprovideddata
                          sendtoprintqueue
                          numberofcopies
                          printqueue ) WITH lt_create_printing
          MAPPED mapped
          REPORTED reported
          FAILED failed.

    ENDIF.
  ENDMETHOD.

  METHOD createprintfile.
    DATA : lv_xml TYPE xstring.
    DATA : lv_error   TYPE abap_boolean,
           lv_message TYPE string.
*&---调用方法时，获取实体数据内容
    READ ENTITIES OF zr_zt_print_log IN LOCAL MODE
        ENTITY log ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(results).

*&---获取实体数据后处理
    LOOP AT results ASSIGNING FIELD-SYMBOL(<file>).
      lv_error = abap_false.
*&---获取UUID 设置为打印记录uuid
      TRY.
          DATA(lv_uuid) = cl_system_uuid=>create_uuid_c36_static(  ).
        CATCH cx_uuid_error.
          "handle exception
      ENDTRY.

*&---设置文件内容
      <file>-mimetype = 'application/pdf'.
      " begin
*      TYPES:BEGIN OF ty_file,
*              manufacturingorder(20),
*            END OF ty_file.
*      DATA:gs_file TYPE ty_file.
*      /ui2/cl_json=>deserialize(
*                EXPORTING
*                  json         = <file>-providedkeys
*                  pretty_name  = /ui2/cl_json=>pretty_mode-user
*                  assoc_arrays = abap_true
*                CHANGING
*                  data         = gs_file ).
*      gs_file-manufacturingorder = |{ gs_file-manufacturingorder ALPHA = OUT }|.
*
*
*      SPLIT <file>-providedkeys AT ':' INTO DATA(lv_order) DATA(lv_value).
*      REPLACE ALL OCCURRENCES OF '"' IN: lv_value WITH ''.
*      REPLACE ALL OCCURRENCES OF '{' IN: lv_value WITH ''.
*      REPLACE ALL OCCURRENCES OF '}' IN: lv_value WITH ''.
*      REPLACE ALL OCCURRENCES OF '"' IN: lv_order WITH ''.
*      REPLACE ALL OCCURRENCES OF '{' IN: lv_order WITH ''.
*      REPLACE ALL OCCURRENCES OF '}' IN: lv_order WITH ''.
*      IF lv_order = 'DeliveryDocument'.
*        lv_order = '交货单_'.
*      ELSE.
*        lv_order = '生产订单_'.
*      ENDIF.
      <file>-filename = |{ lv_uuid }.pdf|.
      <file>-mimetypedata = 'application/xml'.
      <file>-datafilename = 'Data.xml'.

*&---若打印参数为空，则直接打印pdf 文件，否则为打印队列，调用打印队列服务CPM
      IF <file>-printqueue IS INITIAL.
        <file>-printqueue = 'COMMON'.
      ENDIF.

      IF <file>-numberofcopies IS INITIAL.
        <file>-numberofcopies = 1.
      ENDIF.

*&---通过打印参数获取对应模板服务数据
      SELECT SINGLE *
        FROM zr_zt_print_config
       WHERE uuid = @<file>-templateuuid      "#EC CI_ALL_FIELDS_NEEDED
        INTO @DATA(ls_template).
      IF sy-subrc = 0.
        "Get Data
        IF <file>-isexternalprovideddata = abap_false.
          TRY.
              "FDP utils
              " 获取模板表中打印服务
              DATA(lo_fdp_util) = cl_fp_fdp_services=>get_instance( CONV zzeservicename( ls_template-servicedefinitionname ) ).
              DATA(lt_keys)     = lo_fdp_util->get_keys( ).

              " Get Key values
              DATA : lo_data TYPE REF TO data.
              FIELD-SYMBOLS : <fo_data> TYPE any.

              " 打印数据获取
              /ui2/cl_json=>deserialize(
                    EXPORTING
                       json = <file>-providedkeys
                    CHANGING
                        data = lo_data ).

              " 分配获取数据
              ASSIGN lo_data->* TO <fo_data>.
              IF sy-subrc = 0.
                DATA(lt_key_l) = lt_keys.
                lt_keys = VALUE #( FOR key IN lt_key_l
                                    ( name      = key-name
                                      value     = <fo_data>-(key-name)->*
                                      data_type = key-data_type ) ).
              ENDIF.
              " 获取完成后释放，否则下次数据类型不更新
              UNASSIGN <fo_data>.
              FREE lo_data.

              " 数据设置为 xml
              lv_xml = lo_fdp_util->read_to_xml( lt_keys ).

              " xml 格式数据提供record 表external 数据提供者字段
              <file>-externalprovideddata = lv_xml.

            CATCH cx_fp_fdp_error INTO DATA(lx_fdp).
            CATCH cx_fp_ads_util INTO DATA(lx_ads).
*        "handle exception
              lv_message = lx_fdp->get_longtext(  ).
              lv_message = lx_ads->get_longtext(  ).
              lv_error = abap_true.
          ENDTRY.
        ELSE.
          lv_xml = <file>-externalprovideddata.
        ENDIF.

        " 数据转换为pdf 文件
        TRY.
            DATA:lv_options TYPE cl_fp_ads_util=>ty_gs_options_pdf.
            lv_options-trace_level = 4.
            cl_fp_ads_util=>render_pdf( EXPORTING
                                           iv_locale     = 'en_US'
                                           iv_xdp_layout = ls_template-template
                                           iv_xml_data   = lv_xml
                                           is_options    = lv_options
                                        IMPORTING
                                           ev_pdf        = DATA(lv_pdf) ).

            <file>-pdf = lv_pdf.
          CATCH cx_fp_ads_util INTO lx_ads.
            lv_message = lx_ads->get_longtext(  ).
            lv_error = abap_true.
        ENDTRY.
      ELSE.
        lv_error = abap_true.
        lv_message = |Template not found|.
      ENDIF.

      " 错误消息处理
      IF lv_error = abap_true.
        APPEND VALUE #(  uuid = <file>-uuid
            %msg = new_message(
            id       = '00'
            number   = 000
            severity = if_abap_behv_message=>severity-error
            v1       = lv_message
            )
        )  TO reported-log.
      ENDIF.
    ENDLOOP.
    " 若数据无误，则保存实体
    IF lv_error = abap_false.
      MODIFY ENTITIES OF zr_zt_print_log IN LOCAL MODE
          ENTITY log UPDATE FIELDS ( filename mimetype pdf mimetypedata printqueue
                                        numberofcopies externalprovideddata datafilename )
              WITH VALUE #( FOR file IN results ( %tky                 = file-%tky
                                                  filename             = file-filename
                                                  mimetype             = file-mimetype
                                                  pdf                  = file-pdf
                                                  mimetypedata         = file-mimetypedata
                                                  numberofcopies       = file-numberofcopies
                                                  printqueue           = file-printqueue
                                                  externalprovideddata = file-externalprovideddata
                                                  datafilename         = file-datafilename ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

ENDCLASS.
