CLASS zcl_abaplint_abapgit_ext_rules DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* abaplint Extension for abapGit
*
* https://github.com/Marc-Bernard-Tools/abaplint-Ext-for-abapGit
*
* Copyright 2023 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_menu_provider.

    CLASS-METHODS create
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        repo_rules    TYPE string VALUE 'mbt_repo_rules',
        default_rules TYPE string VALUE 'mbt_default_rules',
        diff          TYPE string VALUE 'mbt_diff',
      END OF c_action.

    CONSTANTS:
      c_abaplint_host     TYPE string VALUE 'https://schema.abaplint.org/',
      c_abaplint_defaults TYPE string VALUE 'default.json'.

    DATA:
      mo_repo             TYPE REF TO zcl_abapgit_repo_online,
      mv_page             TYPE string,
      mv_default_filename TYPE string,
      mi_default_rules    TYPE REF TO zif_abapgit_ajson,
      mv_repo_filename    TYPE string,
      mi_repo_rules       TYPE REF TO zif_abapgit_ajson.

    METHODS get_repo_rules
      RAISING
        zcx_abapgit_ajson_error
        zcx_abapgit_exception.

    METHODS get_default_rules
      RAISING
        zcx_abapgit_ajson_error
        zcx_abapgit_exception.

    METHODS get_rules_for_diff
      IMPORTING
        ii_rules_left  TYPE REF TO zif_abapgit_ajson
        ii_rules_right TYPE REF TO zif_abapgit_ajson
      RETURNING
        VALUE(result)  TYPE REF TO zif_abapgit_ajson
      RAISING
        zcx_abapgit_ajson_error
        zcx_abapgit_exception.

    METHODS get_logo
      IMPORTING
        !iv_filename  TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_scripts
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_styles
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header
      IMPORTING
        iv_header     TYPE string
        iv_filename   TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_rules
      IMPORTING
        iv_header     TYPE string
        ii_rules      TYPE REF TO zif_abapgit_ajson
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_ajson_error
        zcx_abapgit_exception.

    METHODS render_diff
      IMPORTING
        iv_header_left  TYPE string
        iv_header_right TYPE string
        ii_rules_left   TYPE REF TO zif_abapgit_ajson
        ii_rules_right  TYPE REF TO zif_abapgit_ajson
      RETURNING
        VALUE(result)   TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_ajson_error
        zcx_abapgit_exception.

    METHODS render_diff_lines
      IMPORTING
        ii_rules_left  TYPE REF TO zif_abapgit_ajson
        ii_rules_right TYPE REF TO zif_abapgit_ajson
      RETURNING
        VALUE(result)  TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_diff_line
      IMPORTING
        is_diff       TYPE zif_abapgit_definitions=>ty_diff
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_rules IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    IF iv_key IS INITIAL.
      zcx_abapgit_exception=>raise( 'No repository key supplied' ).
    ELSE.
      mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    ENDIF.

    mv_page = c_action-repo_rules.

    gui_services( )->cache_asset(
      iv_type    = 'image'
      iv_subtype = 'png'
      iv_url     = |{ zcl_abaplint_abapgit_ext_logo=>c_logo }|
      iv_xdata   = zcl_abaplint_abapgit_ext_logo=>get_logo_mime( ) ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abaplint_abapgit_ext_rules.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key = iv_key.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'abaplint Rules'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD get_default_rules.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_ajson_error,
      li_agent TYPE REF TO zif_abapgit_http_agent.

    mv_default_filename = c_abaplint_defaults.

    IF mi_default_rules IS INITIAL.

      li_agent = zcl_abapgit_factory=>get_http_agent( ).

      TRY.
          mi_default_rules = li_agent->request( c_abaplint_host && c_abaplint_defaults )->json( ).
        CATCH zcx_abapgit_ajson_error INTO lx_error.
          zcx_abapgit_exception=>raise_with_text( lx_error ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_logo.

    " TODO: Return different icon based on filename extension
    result = zcl_abapgit_html=>icon( iv_name = 'file-code/darkgrey'
                                     iv_hint = 'JSON' ).

  ENDMETHOD.


  METHOD get_repo_rules.

    DATA:
      lv_filename TYPE string,
      lt_files    TYPE zif_abapgit_git_definitions=>ty_files_tt,
      ls_file     LIKE LINE OF lt_files,
      lv_warning  TYPE abap_bool,
      lv_json     TYPE string,
      lt_json     TYPE string_table.

    lt_files = mo_repo->get_files_remote( ).

    DO 4 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_filename = 'abaplint.json'.
        WHEN 2.
          lv_filename = 'abaplint.jsonc'.
        WHEN 3.
          lv_filename = 'abaplint-app.json'.
        WHEN 4.
          zcx_abapgit_exception=>raise( 'No abaplint rules found' ).
      ENDCASE.
      READ TABLE lt_files INTO ls_file WITH KEY file_path
        COMPONENTS path = '/' filename = lv_filename.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDDO.

    IF sy-index = 3. " app
      " TODO: Example https://github.com/abap2UI5/abap2UI5/blob/main/abaplint-app.json
      BREAK-POINT.
    ENDIF.

    mv_repo_filename = lv_filename.
    lv_json = zcl_abapgit_convert=>xstring_to_string_utf8( ls_file-data ).

    " TODO: Workaround until ajson parses JSON5 with comments
    " Removes end of line // comments
    SPLIT lv_json AT cl_abap_char_utilities=>newline INTO TABLE lt_json.
    LOOP AT lt_json INTO lv_json.
      FIND REGEX '(.*)\/\/[^"]*$' IN lv_json IGNORING CASE SUBMATCHES lv_json.
      IF sy-subrc = 0.
        lv_warning = abap_true.
        MODIFY lt_json FROM lv_json.
      ENDIF.
    ENDLOOP.
    CONCATENATE LINES OF lt_json INTO lv_json SEPARATED BY cl_abap_char_utilities=>newline.

    IF lv_warning = abap_true.
      MESSAGE 'Rules contain comments which are not shown here!' TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.

    mi_repo_rules = zcl_abapgit_ajson=>parse( lv_json ).

  ENDMETHOD.


  METHOD get_rules_for_diff.

    DATA:
      lt_rules     TYPE string_table,
      lv_rule      TYPE string,
      lv_exists    TYPE abap_bool,
      lv_node_type TYPE string.

    result = ii_rules_right->clone( ).

    " For repo rules that are true/false,
    " remove default rule parameters and replace with default of true
    lt_rules = ii_rules_left->members( '/rules' ).

    LOOP AT lt_rules INTO lv_rule.
      lv_node_type = ii_rules_left->get_node_type( |/rules/{ lv_rule }| ).

      IF lv_node_type = 'bool'.
        result->delete( |/rules/{ lv_rule }| ).
        result->setx( |/rules/{ lv_rule }: true| ).
      ELSE.
        " If severity is not set, remove it from default
        lv_exists = ii_rules_left->exists( |/rules/{ lv_rule }/severity| ).

        IF lv_exists = abap_false.
          result->delete( |/rules/{ lv_rule }/severity| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " For default rules that do not exist in repo rules,
    " remove default rule parameters and replace with default of true
    lt_rules = ii_rules_right->members( '/rules' ).

    LOOP AT lt_rules INTO lv_rule.
      lv_exists = ii_rules_left->exists( |/rules/{ lv_rule }| ).

      IF lv_exists = abap_false.
        result->delete( |/rules/{ lv_rule }| ).
        result->setx( |/rules/{ lv_rule }: true| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD render_diff.

    DATA li_rules_compare TYPE REF TO zif_abapgit_ajson.

    li_rules_compare = get_rules_for_diff(
      ii_rules_left  = ii_rules_left
      ii_rules_right = ii_rules_right ).

    result = zcl_abapgit_html=>create( ).

    result->add( |<div class="diff_content">| ).
    result->add( |<table class="diff_tab syntax-hl" id="rules">| ).
    result->add( '<thead class="header">' ).
    result->add( '<tr>' ).
    result->add( '<th class="num"></th>' ).
    result->add( '<th class="mark"></th>' ).
    result->add( |<th>{ iv_header_left }</th>| ).
    result->add( '<th class="num"></th>' ).
    result->add( '<th class="mark"></th>' ).
    result->add( |<th>{ iv_header_right }</th>| ).
    result->add( '</tr>' ).
    result->add( '</thead>' ).
    result->add( render_diff_lines(
                   ii_rules_left   = ii_rules_left
                   ii_rules_right  = li_rules_compare ) ).
    result->add( '</table>' ).

  ENDMETHOD.


  METHOD render_diff_line.

    DATA:
      lv_new  TYPE string,
      lv_old  TYPE string,
      lv_mark TYPE string,
      lv_bg   TYPE string.

    result = zcl_abapgit_html=>create( ).

    lv_mark = ` `.
    IF is_diff-result IS NOT INITIAL.
      IF is_diff-result = zif_abapgit_definitions=>c_diff-update.
        lv_bg = ' diff_upd'.
        lv_mark = `~`.
      ELSEIF is_diff-result = zif_abapgit_definitions=>c_diff-insert.
        lv_bg = ' diff_ins'.
        lv_mark = `+`.
      ENDIF.
    ENDIF.

    lv_new = |<td class="num diff_others" line-num="{ is_diff-new_num }"></td>|
          && |<td class="mark diff_others">{ lv_mark }</td>|
          && |<td class="code{ lv_bg } diff_left new">{ is_diff-new }</td>|.

    CLEAR lv_bg.
    lv_mark = ` `.
    IF is_diff-result IS NOT INITIAL.
      IF is_diff-result = zif_abapgit_definitions=>c_diff-update.
        lv_bg = ' diff_upd'.
        lv_mark = `~`.
      ELSEIF is_diff-result = zif_abapgit_definitions=>c_diff-delete.
        lv_bg = ' diff_del'.
        lv_mark = `-`.
      ENDIF.
    ENDIF.

    lv_old = |<td class="num diff_others" line-num="{ is_diff-old_num }"></td>|
          && |<td class="mark diff_others">{ lv_mark }</td>|
          && |<td class="code{ lv_bg } diff_right old">{ is_diff-old }</td>|.

    result->add( '<tr class="diff_line">' ).
    result->add( lv_old ).
    result->add( lv_new ).
    result->add( '</tr>' ).

  ENDMETHOD.


  METHOD render_diff_lines.

    DATA:
      lx_error       TYPE REF TO zcx_abapgit_ajson_error,
      lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter,
      lo_diff        TYPE REF TO zcl_abapgit_diff,
      lt_diffs       TYPE zif_abapgit_definitions=>ty_diffs_tt.

    FIELD-SYMBOLS:
      <ls_diff> LIKE LINE OF lt_diffs.

    result = zcl_abapgit_html=>create( ).

    TRY.
* TODO: With trailing comma
**        CREATE OBJECT lo_diff
**          EXPORTING
**            iv_new = zcl_abapgit_convert=>string_to_xstring(
**              ii_rules_left->stringify(
**                iv_indent         = 2
**                iv_trailing_comma = abap_true ) )
**            iv_old = zcl_abapgit_convert=>string_to_xstring(
**              ii_rules_right->stringify(
**                iv_indent         = 2
**                iv_trailing_comma = abap_true ) ).
        CREATE OBJECT lo_diff
          EXPORTING
            iv_old = zcl_abapgit_convert=>string_to_xstring( ii_rules_left->stringify( 2 ) )
            iv_new = zcl_abapgit_convert=>string_to_xstring( ii_rules_right->stringify( 2 ) ).
      CATCH zcx_abapgit_ajson_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    lt_diffs = lo_diff->get( ).

    lo_highlighter = zcl_abapgit_syntax_factory=>create( 'rules.json' ).

    LOOP AT lt_diffs ASSIGNING <ls_diff>.
      <ls_diff>-new = lo_highlighter->process_line( <ls_diff>-new ).
      <ls_diff>-old = lo_highlighter->process_line( <ls_diff>-old ).

      CONDENSE <ls_diff>-new_num.
      CONDENSE <ls_diff>-old_num.

      result->add( render_diff_line( <ls_diff> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD render_header.

    result = zcl_abapgit_html=>create( ).

    result->add( '<div class="head">' ).
    result->add( |{ get_logo( iv_filename ) }&nbsp;&nbsp;| ).
    result->add( |{ iv_filename }&nbsp;({ iv_header })| ).
    result->add( '</div>' ).

  ENDMETHOD.


  METHOD render_rules.

    DATA:
      lx_error       TYPE REF TO zcx_abapgit_ajson_error,
      lv_line        TYPE string,
      lt_source      TYPE string_table,
      lv_source      TYPE string,
      lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter.

    result = zcl_abapgit_html=>create( ).

    TRY.
        lv_source = ii_rules->stringify( 2 ).
      CATCH zcx_abapgit_ajson_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    SPLIT lv_source AT cl_abap_char_utilities=>newline INTO TABLE lt_source.

    lo_highlighter = zcl_abapgit_syntax_factory=>create( 'rules.json' ).

    " Use same styles as diff pages. Number of columns and classes have to remain
    " although some columns are empty in order for the column selector to work
    " (see render_script)
    result->add( '<div class="diff_content">' ).
    result->add( |<table class="diff_tab syntax-hl" id="rules">| ).
    result->add( '<thead class="header">' ).
    result->add( '<tr>' ).
    result->add( '<th class="num"></th>' ).
    result->add( '<th class="num" style="display:none"></th>' ).
    result->add( '<th class="mark"></th>' ).
    result->add( |<th>{ iv_header }</th>| ).
    result->add( '</tr>' ).
    result->add( '</thead>' ).

    LOOP AT lt_source INTO lv_source.
      lv_line  = sy-tabix.

      IF lo_highlighter IS NOT INITIAL.
        lv_source = lo_highlighter->process_line( lv_source ).
      ENDIF.

      result->add( '<tr class="diff_line">' ).
      result->add( |<td class="num diff_others" line-num="{ lv_line }"></td>| ).
      result->add( '<td class="num diff_others" line-num="" style="display:none"></td>' ).
      result->add( '<td class="mark diff_others"></td>' ).
      result->add( |<td class="code diff_unified">{ lv_source }</td>| ).
      result->add( '</tr>' ).
    ENDLOOP.

    result->add( '</table>' ).
    result->add( '</div>' ).

  ENDMETHOD.


  METHOD render_scripts.

    result = zcl_abapgit_html=>create( ).

    result->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).

    " Feature for selecting ABAP code by column and copy to clipboard
    result->add( 'var columnSelection = new DiffColumnSelection();' ).

  ENDMETHOD.


  METHOD render_styles.

    result = zcl_abapgit_html=>create( ).

    result->add( '<style>' ).
    result->add( '.markdown' ).
    result->add( '{ background-color: #f2f2f2; padding: 15px; }' ).
    result->add( '.markdown .logo' ).
    result->add( '{ width: 36px; height: 22px; margin-top: -4px; }' ).
    result->add( '.markdown .head,' ).
    result->add( '.markdown .content' ).
    result->add( '{ background-color: #ffffff; border: 1px solid #d8dee4; display: block; }' ).
    result->add( '.markdown .head' ).
    result->add( '{ font-size: larger; margin-bottom: 15px; padding: 15px; }' ).
    result->add( '.markdown .diff_content' ).
    result->add( '{ padding: 25px; }' ).
    result->add( '.markdown .diff_tab' ).
    result->add( '{ border: 1px solid #d8dee4 !important; }' ).
    result->add( '.markdown .html' ).
    result->add( '{ max-width: 1024px; margin: 0 auto; padding: 25px; }' ).
    result->add( '</style>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-repo_rules.

        mv_page = c_action-repo_rules.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-default_rules.

        mv_page = c_action-default_rules.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-diff.

        mv_page = c_action-diff.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'Repo Rules'
      iv_act = c_action-repo_rules
    )->add(
      iv_txt = 'Default Rules'
      iv_act = c_action-default_rules
    )->add(
      iv_txt = 'Diff'
      iv_act = c_action-diff
    )->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lx_error TYPE REF TO zcx_abapgit_ajson_error.

    register_handlers( ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="repo">' ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_false ) ).
    ri_html->add( '</div>' ).

    ri_html->add( render_styles( ) ).

    ri_html->add( '<div class="markdown">' ).

    TRY.
        CASE mv_page.
          WHEN c_action-repo_rules.

            get_repo_rules( ).

            ri_html->add( render_header(
              iv_header   = 'Repository Rules'
              iv_filename = mv_repo_filename ) ).
            ri_html->add( render_rules(
              iv_header = 'REPOSITORY'
              ii_rules  = mi_repo_rules ) ).

          WHEN c_action-default_rules.

            get_default_rules( ).

            ri_html->add( render_header(
              iv_header   = 'Default Rules'
              iv_filename = mv_default_filename ) ).
            ri_html->add( render_rules(
              iv_header = 'DEFAULT'
              ii_rules  = mi_default_rules ) ).

          WHEN c_action-diff.

            get_repo_rules( ).
            get_default_rules( ).

            ri_html->add( render_diff(
              iv_header_left  = 'REPOSITORY'
              iv_header_right = 'DEFAULT'
              ii_rules_left   = mi_repo_rules
              ii_rules_right  = mi_default_rules ) ).

          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.
      CATCH zcx_abapgit_ajson_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_html->add( '</div>' ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
ENDCLASS.
