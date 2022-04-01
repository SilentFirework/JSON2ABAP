class YCL_JSON_TO_DATA definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_declare,
        lever   TYPE i,
        name    TYPE string,
        declare TYPE srt_strings,
      END OF ty_declare .
  types:
    tt_declare TYPE TABLE OF ty_declare .
  types:
    BEGIN OF ty_name_dref,
        name TYPE string,
        dref TYPE REF TO data,
      END OF ty_name_dref .
  types:
    tt_name_dref TYPE TABLE OF ty_name_dref .

  data MV_JSON_IN type STRING .
  class-data MO_DATA type ref to DATA .

  class-methods CONVERT_JSON_DECLARE .
  class-methods CONVERT_JSON .
  class-methods PRETTY_PRINT_JSON
    importing
      !JSON_IN type STRING optional .
  class-methods CLASS_CONSTRUCTOR .
  class-methods UNESCAPE .
  class-methods ESCAPE .
  class-methods CHANGE_JSON .
  methods DESERIALIZE_JSON_DECLARE_MAIN
    exporting
      !DATA type ref to DATA .
  methods DESERIALIZE_JSON
    importing
      !LENGTH type I
      !NAME type STRING default 'FINAL_JSON'
      !LEVER type I default 1
    changing
      !OFFSET type I default 0
      !DATA type ref to DATA .
protected section.
private section.

  constants C_MARK1 type CHAR1 value '{' ##NO_TEXT.
  constants C_MARK2 type CHAR1 value '}' ##NO_TEXT.
  constants C_MARK3 type CHAR1 value '[' ##NO_TEXT.
  constants C_MARK4 type CHAR1 value ']' ##NO_TEXT.
  constants C_MARK5 type CHAR1 value '"' ##NO_TEXT.
  constants C_MARK6 type CHAR1 value ':' ##NO_TEXT.
  constants C_MARK7 type CHAR1 value ',' ##NO_TEXT.
  constants C_MARK8 type CHAR1 value '\' ##NO_TEXT.
  class-data MO_ME type ref to YCL_JSON_TO_DATA .
  data MT_DECLARE type TT_DECLARE .
  data MS_DECLARE type TY_DECLARE .

  methods DESERIALIZE_JSON_DECLARE
    importing
      !LENGTH type I
      !NAME type STRING default 'FINAL_JSON'
      !LEVER type I default 1
    changing
      !OFFSET type I default 0
      !COMPONENT type ABAP_COMPONENT_TAB optional .
  methods DESERIALIZE_ARRAY_LINEDECLARE
    importing
      !LENGTH type I
      !LEVER type I default 1
    changing
      !OFFSET type I default 0
      !COMPONENT type ABAP_COMPONENT_TAB .
  methods DESERIALIZE_ARRAY_LINE
    importing
      !LENGTH type I
      !LEVER type I default 1
    changing
      !OFFSET type I default 0
      !DATA type ref to DATA
      !COMPONENT type ABAP_COMPONENT_TAB .
  methods SIMPLIFY_JSONSTR .
  methods SET_BEGIN_DECLARE
    importing
      !NAME type STRING
      !LEVER type I .
  methods SET_END_DECLARE
    importing
      !NAME type STRING
      !LEVER type I .
  methods ADD_ITAB_CODE
    importing
      !NAME type STRING
      !LEVER type I .
  methods SPECIAL_TABLE_DECLARE
    importing
      value(DATA) type ref to DATA optional
      !PARNAME type STRING
      !NAME type STRING optional
      !LEVER type I
      value(TYPE_DESCR) type ref to CL_ABAP_TYPEDESCR optional .
  methods ADD_DECLARE_CODE
    importing
      value(DATA) type ref to DATA optional
      !PARNAME type STRING
      !NAME type STRING
      !LEVER type I
      value(TYPE_DESCR) type ref to CL_ABAP_TYPEDESCR optional .
  methods CONVERT_DECLARE_TO_DATAREF
    exporting
      value(DATA) type ref to DATA .
  methods ADD_COMP_BY_DATA_REF
    importing
      !P_DATA_REF type ref to DATA
      !IV_NAME type STRING
      !IV_AS_INCLUDE type ABAP_BOOL optional
      !IV_SUFFIX type C optional
    changing
      !CT_COMP type ABAP_COMPONENT_TAB .
  methods ADD_COMP_BY_DATA
    importing
      !P_DATA type ANY
      !IV_NAME type STRING
      !IV_AS_INCLUDE type ABAP_BOOL optional
      !IV_SUFFIX type C optional
    changing
      !CT_COMP type ABAP_COMPONENT_TAB .
  methods ADD_COMP_BY_NAME
    importing
      !P_NAME type ANY
      !IV_NAME type STRING
      !IV_AS_INCLUDE type ABAP_BOOL optional
      !IV_SUFFIX type C optional
    changing
      !CT_COMP type ABAP_COMPONENT_TAB .
  methods CREATE_TABLE
    importing
      !IT_COMP type ABAP_COMPONENT_TAB
    returning
      value(RO_DATA) type ref to DATA .
  methods CREATE_STRUCT
    importing
      !IT_COMP type ABAP_COMPONENT_TAB
    returning
      value(RO_DATA) type ref to DATA .
  methods PRETTY_CODE
    changing
      !SOURCE type RSWSOURCET .
  methods UNESCAPE_STRING
    importing
      value(ESCAPED) type STRING
    returning
      value(UNESCAPED) type STRING .
ENDCLASS.



CLASS YCL_JSON_TO_DATA IMPLEMENTATION.


  METHOD ADD_COMP_BY_DATA.

    DATA ls_comp LIKE LINE OF ct_comp.

    ls_comp-type        ?= cl_abap_typedescr=>describe_by_data(  p_data  = p_data ).
    ls_comp-name         = iv_name.
    ls_comp-as_include   = iv_as_include.
    ls_comp-suffix       = iv_suffix.
    APPEND ls_comp TO ct_comp.

  ENDMETHOD.


  METHOD ADD_COMP_BY_DATA_REF.

    DATA ls_comp LIKE LINE OF ct_comp.

    ls_comp-type        ?= cl_abap_typedescr=>describe_by_data_ref( p_data_ref ).
    ls_comp-name         = iv_name.
    ls_comp-as_include   = iv_as_include.
    ls_comp-suffix       = iv_suffix.
    APPEND ls_comp TO ct_comp.

  ENDMETHOD.


  METHOD add_comp_by_name.

    DATA ls_comp LIKE LINE OF ct_comp.
    DATA lo_type TYPE REF TO cl_abap_typedescr.

    cl_abap_typedescr=>describe_by_name( EXPORTING p_name  = p_name RECEIVING p_descr_ref = lo_type EXCEPTIONS type_not_found = 1 ).

    IF sy-subrc = 1." If P_NAME not exist, Using string type
      cl_abap_typedescr=>describe_by_name( EXPORTING p_name  = 'SMDSTRING'  RECEIVING p_descr_ref = lo_type EXCEPTIONS type_not_found = 1 ).
    ENDIF.

    ls_comp-type         ?= lo_type.
    ls_comp-name         = iv_name.
    ls_comp-as_include   = iv_as_include.
    ls_comp-suffix       = iv_suffix.
    APPEND ls_comp TO ct_comp.

  ENDMETHOD.


  METHOD add_declare_code.

    DATA: elem_descr TYPE REF TO cl_abap_elemdescr.
    DATA: lv_tabix TYPE sy-tabix,
          lv_code  TYPE string.

    IF type_descr IS INITIAL AND data IS NOT INITIAL.
      type_descr = cl_abap_typedescr=>describe_by_data_ref( data ).
    ENDIF.

    READ TABLE mt_declare INTO ms_declare WITH KEY name = parname lever = lever.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
    ENDIF.

    TRY.
        IF type_descr->kind = cl_abap_typedescr=>kind_struct .
          lv_code = `  ` && name && `  LIKE LS_` && name  && `,` .
        ELSEIF type_descr->kind = cl_abap_typedescr=>kind_table.
          lv_code = `  ` &&  name && `  LIKE LT_` && name && `,` .
        ELSE.
          elem_descr ?= type_descr.

          CASE elem_descr->type_kind.
            WHEN cl_abap_typedescr=>typekind_char.
              IF elem_descr->output_length EQ 1.
                lv_code = `  ` && name && `  TYPE BOOLE_D,`.
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_date.
              lv_code = `  ` && name && `  TYPE DATUM,`.
            WHEN cl_abap_typedescr=>typekind_time.
              lv_code = `  ` && name && `  TYPE UZEIT,` .
            WHEN cl_abap_typedescr=>typekind_int.
              lv_code = `  ` && name && `  TYPE I,` .
            WHEN cl_abap_typedescr=>typekind_float.
              lv_code = `  ` && name && `  TYPE F,` .
            WHEN cl_abap_typedescr=>typekind_packed.
              lv_code = `  ` && name && `  TYPE P,` .
            WHEN OTHERS.
              lv_code = `  ` && name && `  TYPE STRING,` .
          ENDCASE.

        ENDIF.

        READ TABLE ms_declare-declare TRANSPORTING NO FIELDS WITH KEY table_line = lv_code.
        IF sy-subrc <> 0.
          APPEND lv_code TO ms_declare-declare.
        ENDIF.

      CATCH cx_sy_move_cast_error.

    ENDTRY.
    IF lv_tabix IS NOT INITIAL.
      MODIFY mt_declare FROM ms_declare INDEX lv_tabix.
    ENDIF.

  ENDMETHOD.


  METHOD add_itab_code.

    CLEAR ms_declare.
    ms_declare-name   = `zzz` && name.
    ms_declare-lever  = lever.
    APPEND `DATA: LT_` && name && ` LIKE TABLE OF LS_` && name && `.` TO ms_declare-declare.
    APPEND ms_declare TO mt_declare.
    CLEAR ms_declare.

  ENDMETHOD.


  METHOD change_json.

    pretty_print_json( mo_me->mv_json_in ).
    cl_demo_text=>edit_string( EXPORTING title = 'Change Input JSON String'  CHANGING text_string = mo_me->mv_json_in
      EXCEPTIONS canceled = 1 ).

  ENDMETHOD.


  METHOD class_constructor.
    IF mo_me IS INITIAL.
      CREATE OBJECT mo_me.
      " 获取输入json字符串
      cl_demo_text=>edit_string( EXPORTING title = 'Please Input JSON String'  CHANGING text_string = mo_me->mv_json_in
        EXCEPTIONS canceled = 1 ).
    ENDIF.
  ENDMETHOD.


  METHOD convert_declare_to_dataref.

    DATA: lt_source TYPE TABLE OF string WITH EMPTY KEY,
          prog      TYPE sy-repid,
          message   TYPE string.

    DATA: ls_declare LIKE LINE OF mo_me->mt_declare,
          lv_data    TYPE string.

    lt_source = VALUE #(
      ( `program.`                     )
      ( ` FORM CREATE_DYN_DATA.`       )
      ).

    SORT mt_declare BY lever DESCENDING name.
    DELETE ADJACENT DUPLICATES FROM mt_declare COMPARING lever name.

    LOOP AT mt_declare INTO ls_declare.
      APPEND LINES OF ls_declare-declare TO lt_source.
    ENDLOOP.

    IF lines( ls_declare-declare ) > 1.
      lv_data = 'LS_FINAL_JSON.'.
    ELSEIF lines( ls_declare-declare ) = 1.
      lv_data = 'LT_FINAL_JSON.'.
    ENDIF.

    APPEND `    DATA: lo_data TYPE REF TO data.`      TO lt_source.
    APPEND `    CREATE DATA lo_data LIKE ` && lv_data TO lt_source.
    APPEND `    ycl_json_to_data=>mo_data = lo_data.` TO lt_source.
    APPEND `  ENDFORM.`                                 TO lt_source.

    TRY .
        GENERATE SUBROUTINE POOL lt_source  NAME prog MESSAGE message.
        IF prog IS NOT INITIAL.
          PERFORM ('CREATE_DYN_DATA') IN PROGRAM (prog) IF FOUND.

          data = ycl_json_to_data=>mo_data.

        ENDIF.
      CATCH cx_sy_generate_subpool_full .
      CATCH cx_sy_gen_source_too_wide  .
    ENDTRY.

  ENDMETHOD.


  METHOD convert_json.

    DATA: lv_length  TYPE i,
          lt_source  TYPE rswsourcet,
          lv_source  TYPE string,
          ls_declare LIKE LINE OF mo_me->mt_declare.

    DATA: lo_data TYPE REF TO data.

    FIELD-SYMBOLS: <data> TYPE any.

    IF lv_length IS INITIAL.
      lv_length = strlen( mo_me->mv_json_in ).
    ENDIF.

    mo_me->simplify_jsonstr( ).
    mo_me->deserialize_json( EXPORTING length = lv_length CHANGING data = lo_data ).

    SORT mo_me->mt_declare BY lever DESCENDING name.
    DELETE ADJACENT DUPLICATES FROM mo_me->mt_declare COMPARING lever name.

    LOOP AT mo_me->mt_declare INTO ls_declare.
      APPEND LINES OF ls_declare-declare TO lt_source.
    ENDLOOP.

    mo_me->pretty_code( CHANGING source = lt_source ).

    CONCATENATE LINES OF lt_source INTO lv_source
            SEPARATED BY cl_abap_char_utilities=>cr_lf+1(1).

    cl_demo_text=>display_string( lv_source ).

  ENDMETHOD.


  METHOD convert_json_declare.

    DATA: lv_length  TYPE i,
          lt_source  TYPE rswsourcet,
          lv_source  TYPE string,
          ls_declare LIKE LINE OF mo_me->mt_declare.

    DATA: lo_data TYPE REF TO data.

    FIELD-SYMBOLS: <data> TYPE any.

    IF lv_length IS INITIAL.
      lv_length = strlen( mo_me->mv_json_in ).
    ENDIF.

    mo_me->simplify_jsonstr( ).
    mo_me->deserialize_json_declare_main( IMPORTING data = lo_data ).

    SORT mo_me->mt_declare BY lever DESCENDING name.
    DELETE ADJACENT DUPLICATES FROM mo_me->mt_declare COMPARING lever name.

    LOOP AT mo_me->mt_declare INTO ls_declare.
      APPEND LINES OF ls_declare-declare TO lt_source.
    ENDLOOP.

    mo_me->pretty_code( CHANGING source = lt_source ).

    CONCATENATE LINES OF lt_source INTO lv_source
            SEPARATED BY cl_abap_char_utilities=>cr_lf+1(1).

    cl_demo_text=>display_string( lv_source ).

  ENDMETHOD.


  METHOD CREATE_STRUCT.

    DATA lr_struct TYPE REF TO cl_abap_structdescr.
    lr_struct = cl_abap_structdescr=>create( p_components = it_comp ) .
    CREATE DATA ro_data TYPE HANDLE lr_struct.

  ENDMETHOD.


  METHOD CREATE_TABLE.

    DATA lr_table TYPE REF TO cl_abap_tabledescr.

    lr_table = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( p_components = it_comp p_strict = '' ) ).
    CREATE DATA ro_data TYPE HANDLE lr_table.

  ENDMETHOD.


  METHOD deserialize_array_line.

    DATA: key_value TYPE string,
          mark      TYPE i,
          pos       TYPE i,
          match     TYPE i,
          lv_lever  TYPE i.

    DATA: value      TYPE string,
          temp_value TYPE string,
          lv_uzeit   TYPE uzeit,
          lv_datum   TYPE datum.

    DATA: lo_data      TYPE REF TO data,
          lo_line      TYPE REF TO data,
          lt_name_dref TYPE tt_name_dref.

    FIELD-SYMBOLS: <line>  TYPE any,
                   <value> TYPE any,
                   <data>  TYPE data,
                   <field> TYPE any,
                   <table> TYPE ANY TABLE.

    CHECK offset < length.

    TRY.
        IF mo_me->mv_json_in+offset(1) = '{'." object
          eat_char `{`.
          skip_to_next_character.
          IF mv_json_in+offset(1) NE `}`.
            WHILE offset < length AND mv_json_in+offset(1) NE `}`.
              skip_to_next_character.
              FREE lo_data.
              CLEAR key_value.
              eat_name key_value.
              skip_to_next_character.
              eat_char `:`.
              skip_to_next_character.
              lv_lever = lever + 1.
              deserialize_json( EXPORTING length = length name = key_value lever = lv_lever CHANGING data = lo_data offset = offset ).
              skip_to_next_character.
              IF lo_data IS NOT INITIAL.
                TRANSLATE key_value TO UPPER CASE.
                READ TABLE component TRANSPORTING NO FIELDS WITH KEY name = key_value.
                IF sy-subrc <> 0.
                  add_comp_by_data_ref(  EXPORTING p_data_ref = lo_data iv_name = key_value CHANGING ct_comp = component ).
                ENDIF.
                APPEND VALUE #( name = key_value dref = lo_data ) TO lt_name_dref.
              ENDIF.

              IF offset < length AND mv_json_in+offset(1) NE `}`.
                eat_char `,`.
              ELSE.
                EXIT.
              ENDIF.

            ENDWHILE.

            IF component IS NOT INITIAL.

              lo_line = create_struct( it_comp = component ).
              ASSIGN lo_line->* TO <line>.
              LOOP AT lt_name_dref INTO DATA(ls_name_dref).

                ASSIGN COMPONENT ls_name_dref-name OF STRUCTURE <line> TO <field>.
                IF sy-subrc = 0.
                  ASSIGN ls_name_dref-dref->* TO <value>.
                  IF sy-subrc = 0.
                    <field> = <value>.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              FREE lo_data.
              lo_data = create_table( it_comp = component ).
              ASSIGN lo_data->* TO <table>.
              IF data IS NOT INITIAL .
                ASSIGN data->* TO <data>.
                MOVE-CORRESPONDING <data> TO <table>.
              ENDIF.
              INSERT <line> INTO TABLE <table>.
              FREE data.
              data = lo_data.

            ENDIF.

          ELSE.
            FREE data.
          ENDIF.

          eat_char `}`.
        ELSEIF mo_me->mv_json_in+offset(1) = '"'." Single value table

          eat_string value.

          value = unescape_string( value ).
          IF data IS INITIAL.
            CREATE DATA data TYPE TABLE OF scstring.
          ENDIF.
          ASSIGN data->* TO <table>.
          INSERT value INTO TABLE <table>.

        ENDIF.

      CATCH  cx_sy_move_cast_error cx_sy_conversion_no_number cx_sy_conversion_overflow.
        MESSAGE 'ERROR' TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD deserialize_array_linedeclare.

    DATA: key_value TYPE string,
          mark      TYPE i,
          pos       TYPE i,
          match     TYPE i,
          lv_lever  TYPE i.

    DATA: value      TYPE string,
          temp_value TYPE string,
          lv_uzeit   TYPE uzeit,
          lv_datum   TYPE datum.

    DATA: lt_comp TYPE  abap_component_tab,
          ls_comp TYPE  abap_componentdescr.

    FIELD-SYMBOLS: <line>  TYPE any,
                   <value> TYPE any,
                   <data>  TYPE data,
                   <field> TYPE any,
                   <table> TYPE ANY TABLE.

    CHECK offset < length.

    TRY.
        IF mo_me->mv_json_in+offset(1) = '{'." object
          eat_char `{`.
          skip_to_next_character.
          IF mv_json_in+offset(1) NE `}`.
            WHILE offset < length AND mv_json_in+offset(1) NE `}`.
              skip_to_next_character.
              CLEAR key_value.
              eat_name key_value.
              TRANSLATE key_value TO UPPER CASE.
              skip_to_next_character.
              eat_char `:`.
              skip_to_next_character.
              lv_lever = lever + 1.
              deserialize_json_declare( EXPORTING length = length name = key_value lever = lv_lever CHANGING component = lt_comp offset = offset ).
              skip_to_next_character.

              IF offset < length AND mv_json_in+offset(1) NE `}`.
                eat_char `,`.
              ELSE.
                EXIT.
              ENDIF.

            ENDWHILE.

            LOOP AT lt_comp INTO ls_comp.
              READ TABLE component TRANSPORTING NO FIELDS WITH KEY name = key_value.
              IF sy-subrc <> 0.
                APPEND ls_comp TO component.
              ENDIF.
            ENDLOOP.

          ENDIF.

          eat_char `}`.
        ELSEIF mo_me->mv_json_in+offset(1) = '"'." Single value table

          eat_string value.

        ENDIF.

      CATCH  cx_sy_move_cast_error cx_sy_conversion_no_number cx_sy_conversion_overflow.
        MESSAGE 'ERROR' TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD deserialize_json.

    DATA: key_value  TYPE string,
          value      TYPE string,
          temp_value TYPE string,
          mark       TYPE i,
          pos        TYPE i,
          match      TYPE i,
          pnum_len   TYPE i,
          pdec_len   TYPE i,
          lv_lever   TYPE i.

    DATA: lo_data      TYPE REF TO data,
          lt_comp      TYPE  abap_component_tab,
          ls_comp      TYPE  abap_componentdescr,
          lt_name_dref TYPE tt_name_dref.

    FIELD-SYMBOLS: <line>  TYPE any,
                   <value> TYPE any,
                   <data>  TYPE data,
                   <field> TYPE any,
                   <table> TYPE ANY TABLE.

    CHECK offset < length.

    IF lever = 1.
      CLEAR mt_declare.
    ENDIF.
    TRY.
        CASE mo_me->mv_json_in+offset(1).

          WHEN '{'." object
            eat_char `{`.
            skip_to_next_character.
            IF mv_json_in+offset(1) NE `}`.
              set_begin_declare( name = name lever = lever ).

              WHILE offset < length AND mv_json_in+offset(1) NE `}`.
                FREE lo_data.
                skip_to_next_character.
                CLEAR key_value.
                eat_name key_value.
                skip_to_next_character.
                eat_char `:`.
                skip_to_next_character.
                lv_lever = lever + 1.
                deserialize_json( EXPORTING length = length lever = lv_lever name = key_value CHANGING data = lo_data offset = offset ).
                skip_to_next_character.
                IF lo_data IS NOT INITIAL.
                  TRANSLATE key_value TO UPPER CASE.
                  add_comp_by_data_ref(  EXPORTING p_data_ref = lo_data iv_name = key_value CHANGING ct_comp = lt_comp ).
                  APPEND VALUE #( name = key_value dref = lo_data ) TO lt_name_dref.
                  add_declare_code( parname = name name = key_value lever = lever data = lo_data  ).
                ENDIF.

                IF offset < length AND mv_json_in+offset(1) NE `}`.
                  eat_char `,`.
                ELSE.
                  EXIT.
                ENDIF.

              ENDWHILE.

              set_end_declare( name = name lever = lever ).

              IF lt_comp IS NOT INITIAL.
                data = create_struct( it_comp = lt_comp ).
                ASSIGN data->* TO <line>.
                LOOP AT lt_name_dref INTO DATA(ls_name_dref).

                  ASSIGN COMPONENT ls_name_dref-name OF STRUCTURE <line> TO <field>.
                  IF sy-subrc = 0.
                    ASSIGN ls_name_dref-dref->* TO <value>.
                    IF sy-subrc = 0.
                      <field> = <value>.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
              ENDIF.

            ELSE.
              FREE data.
            ENDIF.

            eat_char `}`.
          WHEN `[`." array
            eat_char `[`.
            skip_to_next_character.
            IF mv_json_in+offset(1) NE `]`.
              set_begin_declare( name = name lever = lever ).

              CLEAR lt_comp.
              WHILE offset < length AND mv_json_in+offset(1) NE `]`.
                skip_to_next_character.
                lv_lever = lever + 1.
                deserialize_array_line( EXPORTING length = length lever = lv_lever CHANGING data = data offset = offset component = lt_comp ).
                skip_to_next_character.
                IF offset < length AND mv_json_in+offset(1) NE `]`.
                  eat_char `,`.
                ELSE.
                  EXIT.
                ENDIF.
              ENDWHILE.

              IF lt_comp IS NOT INITIAL.
                LOOP AT lt_comp INTO ls_comp.
                  add_declare_code( parname = name name = ls_comp-name lever = lever type_descr = ls_comp-type ).
                ENDLOOP.

                set_end_declare( name = name lever = lever ).
                add_itab_code( name = name lever = lever ).
              ELSE.
                special_table_declare( parname = name lever = lever data = data  ).
              ENDIF.
            ELSE.
              FREE data.
            ENDIF.

            eat_char `]`.
          WHEN `"`." string
            eat_string value.
            IF value IS NOT INITIAL.

              " date
              temp_value = value.
              CONDENSE temp_value NO-GAPS.
              REPLACE FIRST OCCURRENCE OF REGEX `^(\d{4})-(\d{2})-(\d{2})` IN temp_value WITH `$1$2$3`
              REPLACEMENT LENGTH match.                     "#EC NOTEXT
              IF sy-subrc EQ 0.
                temp_value = temp_value(match).
                FREE data.
                CREATE DATA data TYPE datum.
                ASSIGN data->* TO <data>.
                <data> = temp_value.
              ELSE.

                " time
                REPLACE FIRST OCCURRENCE OF REGEX `^(\d{2}):(\d{2}):(\d{2})` IN temp_value WITH `$1$2$3`
                REPLACEMENT LENGTH match. "#EC NOTEXT                  "#EC NOTEXT
                IF sy-subrc EQ 0.
                  temp_value = temp_value(match).
                  FREE data.
                  CREATE DATA data TYPE uzeit.
                  ASSIGN data->* TO <data>.
                  <data> = temp_value.
                ELSE.
                  value = unescape_string( value ).
                  FREE data.
                  CREATE DATA data TYPE scstring.
                  ASSIGN data->* TO <data>.
                  <data> = value.
                ENDIF.

              ENDIF.

            ELSE.
              FREE data.
              CREATE DATA data TYPE scstring.
            ENDIF.

          WHEN `-` OR `0` OR `1` OR `2` OR `3` OR `4` OR `5` OR `6` OR `7` OR `8` OR `9`." number
            eat_number value.
            match = strlen( value ).
            IF value CS 'E+'. " float.
              CREATE DATA data TYPE f.
            ELSEIF value CS '.' AND match GT 9. " packed
              CREATE DATA data TYPE p.
            ELSE. " integer
              CREATE DATA data TYPE i.
            ENDIF.
            ASSIGN data->* TO <data>.
            <data> = value.
          WHEN OTHERS. " boolean
            CREATE DATA data TYPE boole_d.
            ASSIGN data->* TO <data>.
            eat_bool <data>.                                "#EC NOTEXT
        ENDCASE.
      CATCH cx_sy_move_cast_error cx_sy_conversion_no_number cx_sy_conversion_overflow cx_sy_range_out_of_bounds.
        MESSAGE 'ERROR' TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD deserialize_json_declare.

    TYPES: BEGIN OF ty_struct,
             field TYPE string,
           END OF ty_struct.

    DATA: key_value  TYPE string,
          value      TYPE string,
          temp_value TYPE string,
          mark       TYPE i,
          pos        TYPE i,
          match      TYPE i,
          pnum_len   TYPE i,
          pdec_len   TYPE i,
          lv_lever   TYPE i.

    DATA: f      TYPE f,
          p      TYPE p,
          i      TYPE i,
          table  TYPE TABLE OF ty_struct,
          strtab TYPE TABLE OF scstring,
          struc  TYPE ty_struct.

    DATA: lt_comp TYPE  abap_component_tab,
          ls_comp TYPE  abap_componentdescr.

    FIELD-SYMBOLS: <line>  TYPE any,
                   <value> TYPE any,
                   <data>  TYPE data,
                   <field> TYPE any,
                   <table> TYPE ANY TABLE.

    CHECK offset < length.

    IF lever = 1.
      CLEAR mt_declare.
    ENDIF.
    TRY.
        CASE mo_me->mv_json_in+offset(1).

          WHEN '{'." object
            eat_char `{`.
            skip_to_next_character.
            IF mv_json_in+offset(1) NE `}`.
              set_begin_declare( name = name lever = lever ).

              WHILE offset < length AND mv_json_in+offset(1) NE `}`.
                skip_to_next_character.
                CLEAR key_value.
                eat_name key_value.
                TRANSLATE key_value TO UPPER CASE.
                skip_to_next_character.
                eat_char `:`.
                skip_to_next_character.
                lv_lever = lever + 1.
                deserialize_json_declare( EXPORTING length = length lever = lv_lever name = key_value CHANGING component = lt_comp offset = offset ).
                skip_to_next_character.

                IF offset < length AND mv_json_in+offset(1) NE `}`.
                  eat_char `,`.
                ELSE.
                  EXIT.
                ENDIF.

              ENDWHILE.

              LOOP AT lt_comp INTO ls_comp.
                add_declare_code( parname = name name = ls_comp-name lever = lever type_descr = ls_comp-type  ).
              ENDLOOP.

              set_end_declare( name = name lever = lever ).

            ENDIF.

            add_comp_by_data( EXPORTING p_data = struc iv_name = name CHANGING ct_comp = component ).

            eat_char `}`.
          WHEN `[`." array
            eat_char `[`.
            skip_to_next_character.
            IF mv_json_in+offset(1) NE `]`.
              set_begin_declare( name = name lever = lever ).

              CLEAR lt_comp.
              WHILE offset < length AND mv_json_in+offset(1) NE `]`.
                skip_to_next_character.
                lv_lever = lever + 1.
                deserialize_array_linedeclare( EXPORTING length = length lever = lv_lever CHANGING offset = offset component = lt_comp ).
                skip_to_next_character.
                IF offset < length AND mv_json_in+offset(1) NE `]`.
                  eat_char `,`.
                ELSE.
                  EXIT.
                ENDIF.
              ENDWHILE.

              IF lt_comp IS NOT INITIAL.
                LOOP AT lt_comp INTO ls_comp.
                  add_declare_code( parname = name name = ls_comp-name lever = lever type_descr = ls_comp-type ).
                ENDLOOP.

                set_end_declare( name = name lever = lever ).
                add_itab_code( name = name lever = lever ).

                add_comp_by_data( EXPORTING p_data = table iv_name = name CHANGING ct_comp = component ).
              ELSE.
                ls_comp-type ?= cl_abap_typedescr=>describe_by_data( strtab ).
                special_table_declare( parname = name name = ls_comp-name lever = lever type_descr = ls_comp-type ).
                add_comp_by_data( EXPORTING p_data = strtab iv_name = name CHANGING ct_comp = component ).
              ENDIF.
            ENDIF.

            eat_char `]`.
          WHEN `"`." string
            eat_string value.
            IF value IS NOT INITIAL.

              " date
              temp_value = value.
              CONDENSE temp_value NO-GAPS.
              REPLACE FIRST OCCURRENCE OF REGEX `^(\d{4})-(\d{2})-(\d{2})` IN temp_value WITH `$1$2$3`
              REPLACEMENT LENGTH match.                     "#EC NOTEXT
              IF sy-subrc EQ 0.
                add_comp_by_name( EXPORTING p_name = 'DATUM' iv_name = name CHANGING ct_comp = component ).
              ELSE.

                " time
                REPLACE FIRST OCCURRENCE OF REGEX `^(\d{2}):(\d{2}):(\d{2})` IN temp_value WITH `$1$2$3`
                REPLACEMENT LENGTH match. "#EC NOTEXT                  "#EC NOTEXT
                IF sy-subrc EQ 0.
                  add_comp_by_name( EXPORTING p_name = 'UZEIT' iv_name = name CHANGING ct_comp = component ).
                ELSE.
                  add_comp_by_name( EXPORTING p_name = 'SCSTRING' iv_name = name CHANGING ct_comp = component ).
                ENDIF.

              ENDIF.

            ELSE.
              add_comp_by_name( EXPORTING p_name = 'SCSTRING' iv_name = name CHANGING ct_comp = component ).
            ENDIF.

          WHEN `-` OR `0` OR `1` OR `2` OR `3` OR `4` OR `5` OR `6` OR `7` OR `8` OR `9`." number
            eat_number value.
            match = strlen( value ).
            IF value CS 'E+'. " float.
              add_comp_by_data( EXPORTING p_data = f iv_name = name CHANGING ct_comp = component ).

            ELSEIF value CS '.' AND match GT 9. " packed
              add_comp_by_data( EXPORTING p_data = p iv_name = name CHANGING ct_comp = component ).

            ELSE. " integer
              add_comp_by_data( EXPORTING p_data = i iv_name = name CHANGING ct_comp = component ).

            ENDIF.
          WHEN OTHERS. " boolean
            add_comp_by_name( EXPORTING p_name = 'BOOLE_D' iv_name = name CHANGING ct_comp = component ).
            eat_bool value.                                 "#EC NOTEXT
        ENDCASE.
      CATCH cx_sy_move_cast_error cx_sy_conversion_no_number cx_sy_conversion_overflow cx_sy_range_out_of_bounds.
        MESSAGE 'ERROR' TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD deserialize_json_declare_main.

    DATA: lo_data    TYPE REF TO data,
          lv_bool    TYPE char1,
          lv_length  TYPE i,
          type_descr TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <data> TYPE any.

    IF lv_length IS INITIAL.
      lv_length = strlen( mo_me->mv_json_in ).
    ENDIF.

    mo_me->deserialize_json_declare( EXPORTING length = lv_length ).
    mo_me->convert_declare_to_dataref( IMPORTING data = lo_data ).

    IF lo_data IS NOT INITIAL.

      type_descr = cl_abap_typedescr=>describe_by_data_ref( lo_data ).
      IF type_descr->kind = cl_abap_typedescr=>kind_table.
        lv_bool = abap_true.
      ENDIF.

      ASSIGN lo_data->* TO <data>.
      /ui2/cl_json=>deserialize(
         EXPORTING
           json             = mv_json_in
           assoc_arrays     = lv_bool
        CHANGING
          data             = <data>
             ).

      data = lo_data.
    ENDIF.

  ENDMETHOD.


  METHOD escape.

    REPLACE ALL OCCURRENCES OF `\` IN mo_me->mv_json_in WITH `\\`.
    REPLACE ALL OCCURRENCES OF `"` IN mo_me->mv_json_in WITH `\"`.
    pretty_print_json( mo_me->mv_json_in ).
    cl_demo_text=>edit_string( CHANGING text_string = mo_me->mv_json_in EXCEPTIONS canceled = 1 ).

  ENDMETHOD.


  method PRETTY_CODE.

  DATA:
    lo_pp       TYPE REF TO cl_sedi_pretty_printer,
    lo_exc      TYPE REF TO cx_sedi_pretty_printer,
    lo_settings TYPE REF TO if_pretty_printer_settings.

  CREATE OBJECT lo_pp.

  TRY.

      CREATE OBJECT lo_settings TYPE cl_pretty_printer_wb_settings.

      lo_pp->format_source(
        EXPORTING
          i_settings = lo_settings
        CHANGING
          c_source   = source ).

    CATCH cx_sedi_pretty_printer INTO lo_exc.
      MESSAGE lo_exc TYPE sy-msgty DISPLAY LIKE 'S'.
      RETURN.
  ENDTRY.

  endmethod.


  METHOD pretty_print_json.

    DATA: input_length          TYPE i,
          input_pos             TYPE i,
          prev_input_char       TYPE c,
          input_char            TYPE c,
          next_input_pos        TYPE i,
          next_input_char       TYPE c,
          in_string             TYPE flag,
          skip_chars            TYPE i,
          result_pos            TYPE i,
          indent_level          TYPE i,
          start_new_line_before TYPE flag,
          start_new_line_after  TYPE flag,
          result                TYPE TABLE OF string,
          result_line(1024)     TYPE c,
          json_out              TYPE string.

    IF json_in IS NOT INITIAL.
      mo_me->mv_json_in = json_in.
    ENDIF.


    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf      IN mo_me->mv_json_in WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+0(1) IN mo_me->mv_json_in WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+1(1) IN mo_me->mv_json_in WITH ''.

    "// Go through the input string and ident it, creating a line table
    input_length = strlen( mo_me->mv_json_in ).

    DO input_length TIMES.
      input_char = mo_me->mv_json_in+input_pos(1).
      next_input_pos = input_pos + 1.
      IF next_input_pos < input_length.
        next_input_char = mo_me->mv_json_in+next_input_pos(1).
      ELSE.
        CLEAR next_input_char.
      ENDIF.

      IF skip_chars = 0.
        CASE input_char.
            "// Escaped character
          WHEN '\'.
            skip_chars = 1.
            IF next_input_char = 'u'.
              skip_chars = 5.
            ENDIF.

            "// String
          WHEN '"'.
            IF in_string IS INITIAL.
              in_string = 'X'.
            ELSE.
              CLEAR in_string.
            ENDIF.

            "// Opening blocks
          WHEN '{' OR '['.
            IF in_string IS INITIAL.
              ADD 1 TO indent_level.
              IF next_input_char <> '}' AND
                 next_input_char <> ']'.
                start_new_line_after = 'X'.
              ENDIF.
            ENDIF.

            "// Closing blocks
          WHEN '}' OR ']'.
            IF in_string IS INITIAL.
              SUBTRACT 1 FROM indent_level.
              IF prev_input_char <> '{' AND
                 prev_input_char <> '['.
                start_new_line_before = 'X'.
              ENDIF.
            ENDIF.

            "// Between members
          WHEN ','.
            IF in_string IS INITIAL.
              start_new_line_after = 'X'.
            ENDIF.
        ENDCASE.
      ELSE.
        SUBTRACT 1 FROM skip_chars.
      ENDIF.

      IF start_new_line_before IS NOT INITIAL.
        CLEAR start_new_line_before.
        APPEND result_line TO result.
        CLEAR result_line.
        result_pos = indent_level * 2.
      ENDIF.

      result_line+result_pos = input_char.
      ADD 1 TO result_pos.

      IF start_new_line_after IS NOT INITIAL.
        CLEAR start_new_line_after.
        APPEND result_line TO result.
        CLEAR result_line.
        result_pos = indent_level * 2.
      ENDIF.

      prev_input_char = input_char.
      input_pos = next_input_pos.
    ENDDO.
    APPEND result_line TO result.

    "// Glue the lines together
    CONCATENATE LINES OF result INTO json_out
      SEPARATED BY cl_abap_char_utilities=>cr_lf+1(1)." %_cr_lf.
    IF json_in IS  INITIAL.
      cl_demo_text=>edit_string( CHANGING text_string = json_out EXCEPTIONS canceled = 1 ).
    ENDIF.
    mo_me->mv_json_in = json_out.

  ENDMETHOD.


  METHOD set_begin_declare.
    DATA: lv_tabix   TYPE sy-tabix.

    IF ms_declare IS NOT INITIAL.
      READ TABLE mt_declare TRANSPORTING NO FIELDS WITH KEY name = ms_declare-name lever = ms_declare-lever.
      IF sy-subrc = 0.
        MODIFY mt_declare FROM ms_declare INDEX sy-tabix.
      ELSE.
        APPEND ms_declare TO mt_declare.
      ENDIF.

      CLEAR ms_declare.
    ENDIF.

    ms_declare-name   = name.
    ms_declare-lever  = lever.
    APPEND `DATA: BEGIN OF LS_` && name && `,` TO ms_declare-declare.

  ENDMETHOD.


  METHOD set_end_declare.

    DATA: lv_tabix TYPE sy-tabix,
          lv_code  TYPE string.

    READ TABLE mt_declare INTO ms_declare WITH KEY name = name lever = lever.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
      lv_code = `      END OF LS_` && name && `.`.
      READ TABLE ms_declare-declare TRANSPORTING NO FIELDS WITH KEY table_line = lv_code.
      IF sy-subrc <> 0.
        APPEND lv_code TO ms_declare-declare.
        MODIFY mt_declare FROM ms_declare INDEX lv_tabix.
      ENDIF.
    ELSE.
      APPEND `      END OF LS_` && name && `.` TO ms_declare-declare.
      APPEND ms_declare TO mt_declare.
    ENDIF.

    CLEAR ms_declare.


  ENDMETHOD.


  METHOD SIMPLIFY_JSONSTR.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf      IN mo_me->mv_json_in WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+0(1) IN mo_me->mv_json_in WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+1(1) IN mo_me->mv_json_in WITH ''.

    REPLACE REGEX '\s+$'        IN mv_json_in WITH ''.
    REPLACE REGEX '\\[r]'       IN mv_json_in WITH cl_abap_char_utilities=>cr_lf+1(1).
    REPLACE REGEX '\\[n]'       IN mv_json_in WITH cl_abap_char_utilities=>cr_lf+0(1).

  ENDMETHOD.


  METHOD special_table_declare.

    DATA: elem_descr  TYPE REF TO cl_abap_elemdescr,
          table_descr TYPE REF TO cl_abap_tabledescr.
    DATA: lv_tabix TYPE sy-tabix.

    IF type_descr IS INITIAL AND data IS NOT INITIAL.
      type_descr = cl_abap_typedescr=>describe_by_data_ref( data ).
    ENDIF.

    READ TABLE mt_declare INTO ms_declare WITH KEY name = parname lever = lever.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
    ENDIF.
    CLEAR ms_declare-declare.

    TRY.
        IF type_descr->kind = cl_abap_typedescr=>kind_table.
          table_descr ?= type_descr.
          elem_descr 	?= table_descr->get_table_line_type( ).
          DATA(p_flddescr) = elem_descr->get_ddic_field( ).
          APPEND `DATA: LT_` && parname && ` TYPE TABLE OF ` && p_flddescr-tabname && `.` TO ms_declare-declare.
          APPEND ms_declare TO mt_declare.
        ENDIF.

      CATCH cx_sy_move_cast_error.

    ENDTRY.

    IF lv_tabix IS NOT INITIAL.
      MODIFY mt_declare FROM ms_declare INDEX lv_tabix.
    ENDIF.

    CLEAR ms_declare.

  ENDMETHOD.


  METHOD unescape.

    DATA: lv_offset          TYPE i,
          lv_match           TYPE i,
          lv_delta           TYPE i,
          lv_cov_error       TYPE c,
          lv_length          TYPE i,
          lv_offset_e        TYPE i,
          lv_length_e        TYPE i,
          lv_unicode_symb    TYPE c,
          lv_unicode_escaped TYPE string,
          lt_matches         TYPE match_result_tab.

    FIELD-SYMBOLS: <match> LIKE LINE OF lt_matches.

    " see reference for escaping rules in JSON RFC
    " https://www.ietf.org/rfc/rfc4627.txt

    lv_cov_error = cl_abap_conv_in_ce=>uccp( '0000' ).

    lv_length = strlen( mo_me->mv_json_in ).

    FIND FIRST OCCURRENCE OF REGEX `\\[rntfbu]` IN mo_me->mv_json_in RESPECTING CASE.
    IF sy-subrc IS INITIAL.
      FIND ALL OCCURRENCES OF REGEX `\\.` IN mo_me->mv_json_in RESULTS lt_matches RESPECTING CASE.
      LOOP AT lt_matches ASSIGNING <match>.
        lv_match  = <match>-offset - lv_delta.
        lv_offset = lv_match + 1.
        CASE mo_me->mv_json_in+lv_offset(1).
          WHEN `r`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF mo_me->mv_json_in WITH cl_abap_char_utilities=>cr_lf(1).
            lv_delta = lv_delta + 1.
          WHEN `n`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF mo_me->mv_json_in WITH cl_abap_char_utilities=>newline.
            lv_delta = lv_delta + 1.
          WHEN `t`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF mo_me->mv_json_in WITH cl_abap_char_utilities=>horizontal_tab.
            lv_delta = lv_delta + 1.
          WHEN `f`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF mo_me->mv_json_in WITH cl_abap_char_utilities=>form_feed.
            lv_delta = lv_delta + 1.
          WHEN `b`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF mo_me->mv_json_in WITH cl_abap_char_utilities=>backspace.
            lv_delta = lv_delta + 1.
          WHEN `u`.
            lv_offset   = lv_offset + 1.
            lv_offset_e = lv_offset + 4.
            lv_length_e = lv_length + lv_delta.
            IF lv_offset_e LE lv_length_e.
              lv_unicode_escaped = mo_me->mv_json_in+lv_offset(4).
              TRANSLATE lv_unicode_escaped TO UPPER CASE.
              lv_unicode_symb = cl_abap_conv_in_ce=>uccp( lv_unicode_escaped ).
              IF lv_unicode_symb NE lv_cov_error.
                REPLACE SECTION OFFSET lv_match LENGTH 6 OF mo_me->mv_json_in WITH lv_unicode_symb.
                lv_delta = lv_delta + 5.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    " based on RFC mentioned above, _any_ character can be escaped, and so shall be enscaped
    " the only exception is Unicode symbols, that shall be kept untouched, while serializer does not handle them
    " mo_me->mv_json_in singe characters, e.g \\, \", \/ etc
    REPLACE ALL OCCURRENCES OF REGEX `\\(.)` IN mo_me->mv_json_in WITH `$1` RESPECTING CASE.
    pretty_print_json( mo_me->mv_json_in ).
    cl_demo_text=>edit_string( CHANGING text_string = mo_me->mv_json_in EXCEPTIONS canceled = 1 ).

  ENDMETHOD.


METHOD unescape_string.

  DATA: lv_offset          TYPE i,
        lv_match           TYPE i,
        lv_delta           TYPE i,
        lv_length          TYPE i,
        lv_cov_error       TYPE c,
        lv_offset_e        TYPE i,
        lv_length_e        TYPE i,
        lv_unicode_symb    TYPE c,
        lv_unicode_escaped TYPE string,
        lt_matches         TYPE match_result_tab.

  FIELD-SYMBOLS: <match> LIKE LINE OF lt_matches.

  " see reference for escaping rules in JSON RFC
  " https://www.ietf.org/rfc/rfc4627.txt

  unescaped = escaped.

  lv_cov_error = cl_abap_conv_in_ce=>uccp( '0000' ).

  lv_length = strlen( unescaped ).

  FIND FIRST OCCURRENCE OF REGEX `\\[rntfbu]` IN unescaped RESPECTING CASE.
  IF sy-subrc IS INITIAL.
    FIND ALL OCCURRENCES OF REGEX `\\.` IN unescaped RESULTS lt_matches RESPECTING CASE.
    LOOP AT lt_matches ASSIGNING <match>.
      lv_match  = <match>-offset - lv_delta.
      lv_offset = lv_match + 1.
      CASE unescaped+lv_offset(1).
        WHEN `r`.
          REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>cr_lf(1).
          lv_delta = lv_delta + 1.
        WHEN `n`.
          REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>newline.
          lv_delta = lv_delta + 1.
        WHEN `t`.
          REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>horizontal_tab.
          lv_delta = lv_delta + 1.
        WHEN `f`.
          REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>form_feed.
          lv_delta = lv_delta + 1.
        WHEN `b`.
          REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>backspace.
          lv_delta = lv_delta + 1.
        WHEN `u`.
          lv_offset   = lv_offset + 1.
          lv_offset_e = lv_offset + 4.
          lv_length_e = lv_length + lv_delta.
          IF lv_offset_e LE lv_length_e.
            lv_unicode_escaped = unescaped+lv_offset(4).
            TRANSLATE lv_unicode_escaped TO UPPER CASE.
            lv_unicode_symb = cl_abap_conv_in_ce=>uccp( lv_unicode_escaped ).
            IF lv_unicode_symb NE lv_cov_error.
              REPLACE SECTION OFFSET lv_match LENGTH 6 OF unescaped WITH lv_unicode_symb.
              lv_delta = lv_delta + 5.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  " based on RFC mentioned above, _any_ character can be escaped, and so shall be enscaped
  " the only exception is Unicode symbols, that shall be kept untouched, while serializer does not handle them
  " unescaped singe characters, e.g \\, \", \/ etc
  REPLACE ALL OCCURRENCES OF REGEX `\\(.)` IN unescaped WITH `$1` RESPECTING CASE.

ENDMETHOD.
ENDCLASS.
