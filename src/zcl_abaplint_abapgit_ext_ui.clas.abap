CLASS zcl_abaplint_abapgit_ext_ui DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    CLASS-METHODS create
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_check_run  TYPE string
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !iv_key       TYPE zif_abapgit_persistence=>ty_repo-key OPTIONAL
        !iv_check_run TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        go_back            TYPE string VALUE 'go_back',
        sort_1             TYPE string VALUE 'sort_1',
        sort_2             TYPE string VALUE 'sort_2',
        sort_3             TYPE string VALUE 'sort_3',
        jump_edit          TYPE string VALUE 'jump_edit',
        toggle_view_source TYPE string VALUE 'toggle_view_source',
      END OF c_action .
    CONSTANTS c_lines_before TYPE i VALUE 5.
    CONSTANTS c_lines_after TYPE i VALUE 5.
    CONSTANTS c_logo TYPE string VALUE 'abaplint_logo.png' ##NO_TEXT.
    CLASS-DATA gv_view_source TYPE abap_bool .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .
    DATA mv_check_run TYPE string .
    DATA mt_issues TYPE zcl_abaplint_abapgit_ext_issue=>ty_issues .

    CLASS-METHODS _build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    METHODS _get_issues
      RETURNING
        VALUE(rt_issues) TYPE zcl_abaplint_abapgit_ext_issue=>ty_issues
      RAISING
        zcx_abapgit_exception .
    METHODS _get_logo
      IMPORTING
        !iv_title      TYPE string OPTIONAL
      RETURNING
        VALUE(rv_html) TYPE string .
    METHODS _get_mime
      IMPORTING
        !iv_mime_name   TYPE csequence
      RETURNING
        VALUE(rv_xdata) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS _render_issue
      IMPORTING
        !is_issue      TYPE zcl_abaplint_abapgit_ext_issue=>ty_issue
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS _render_issues
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS _render_source
      IMPORTING
        !is_issue      TYPE zcl_abaplint_abapgit_ext_issue=>ty_issue
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_ui IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    IF iv_key IS INITIAL.
      zcx_abapgit_exception=>raise( 'No repository key supplied' ).
    ELSE.
      mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    ENDIF.

    IF iv_check_run IS INITIAL.
      zcx_abapgit_exception=>raise( 'No check run supplied' ).
    ELSE.
      mv_check_run = iv_check_run.
    ENDIF.

    mt_issues = _get_issues( ).

    " Could be a user setting
    gv_view_source = abap_true.

    gui_services( )->cache_asset(
      iv_type    = 'image'
      iv_subtype = 'png'
      iv_url     = |{ c_logo }|
      iv_xdata   = _get_mime( 'ZABAPLINT_LOGO' ) ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abaplint_abapgit_ext_ui.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key       = iv_key
        iv_check_run = iv_check_run.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'abaplint Issues'
      io_page_menu       = _build_menu( )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_jump_type TYPE tadir-object,
      lv_jump_name TYPE tadir-obj_name,
      lv_include   TYPE progname,
      lv_line      TYPE i,
      lv_position  TYPE string.

    CASE ii_event->mv_action.
      WHEN c_action-go_back.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_action-toggle_view_source.

        "todo, update menu which requires enhancement of ZCL_ABAPGIT_GUI_PAGE_HOC
        gv_view_source = boolc( gv_view_source = abap_false ) ##TODO.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-jump_edit.

        lv_jump_type = ii_event->query( )->get( 'TYPE' ).
        lv_jump_name = ii_event->query( )->get( 'NAME' ).

        IF lv_jump_type = 'PROG'.
          lv_include = lv_jump_name.
          lv_line = ii_event->query( )->get( 'LINE' ).
          lv_position = nmax( val1 = 1 val2 = lv_line ).
        ENDIF.

        CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING
            operation           = 'EDIT'
            object_name         = lv_jump_name
            object_type         = lv_jump_type
            include             = lv_include
            position            = lv_position
            in_new_window       = abap_false
          EXCEPTIONS
            not_executed        = 1
            invalid_object_type = 2
            OTHERS              = 3.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN c_action-sort_1.
        SORT mt_issues BY obj_type obj_name obj_subtype line url title.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-sort_2.
        SORT mt_issues BY obj_type obj_name url title obj_subtype line.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-sort_3.
        SORT mt_issues BY url title obj_type obj_name obj_subtype line.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    gui_services( )->register_event_handler( me ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_false ) ).
    ri_html->add( `</div>` ).

    ri_html->add( _render_issues( ) ).

  ENDMETHOD.


  METHOD _build_menu.

    DATA:
      lo_sort_menu TYPE REF TO zcl_abapgit_html_toolbar,
      lo_view_menu TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT lo_sort_menu.

    lo_sort_menu->add(
      iv_txt = 'By Object, Sub-object, Line'
      iv_act = c_action-sort_1
      )->add(
      iv_txt = 'By Object, Check, Sub-object'
      iv_act = c_action-sort_2
      )->add(
      iv_txt = 'By Check, Object, Sub-object'
      iv_act = c_action-sort_3 ).

    CREATE OBJECT lo_view_menu.

    lo_view_menu->add(
      iv_txt = 'Source Code'
      iv_chk = gv_view_source
      iv_act = c_action-toggle_view_source ).

    CREATE OBJECT ro_menu.

    ro_menu->add(
      iv_txt = 'Sort'
      io_sub = lo_sort_menu
      )->add(
      iv_txt = 'View'
      io_sub = lo_view_menu
      )->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD _get_issues.

    DATA:
      lo_annotations TYPE REF TO zcl_abaplint_abapgit_ext_annot,
      ls_annotation  TYPE zcl_abaplint_abapgit_ext_annot=>ty_annotation,
      lt_annotations TYPE zcl_abaplint_abapgit_ext_annot=>ty_annotations,
      lo_issue       TYPE REF TO zcl_abaplint_abapgit_ext_issue,
      ls_issue       TYPE zcl_abaplint_abapgit_ext_issue=>ty_issue.

    CREATE OBJECT lo_annotations
      EXPORTING
        iv_url       = mo_repo->get_url( )
        iv_check_run = mv_check_run.

    lt_annotations = lo_annotations->get( ).

    LOOP AT lt_annotations INTO ls_annotation.

      CREATE OBJECT lo_issue
        EXPORTING
          is_annotation = ls_annotation.

      ls_issue = lo_issue->get( ).

      INSERT ls_issue INTO TABLE rt_issues.

    ENDLOOP.

    SORT rt_issues BY obj_type obj_name obj_subtype line url title.

  ENDMETHOD.


  METHOD _get_logo.
    rv_html =
      |<img src="{ c_logo }" width="25px" height-"25px" | &&
      |title="{ iv_title }" | &&
      |style="background-color:lightgrey;border-radius:6px;">|.
  ENDMETHOD.


  METHOD _get_mime.

    DATA:
      ls_key    TYPE wwwdatatab,
      lv_size_c TYPE wwwparams-value,
      lv_size   TYPE i,
      lt_w3mime TYPE STANDARD TABLE OF w3mime,
      ls_w3mime LIKE LINE OF lt_w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_mime_name.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_w3mime INTO ls_w3mime.
      CONCATENATE rv_xdata ls_w3mime-line INTO rv_xdata IN BYTE MODE.
    ENDLOOP.
    rv_xdata = rv_xdata(lv_size).

  ENDMETHOD.


  METHOD _render_issue.

    DATA:
      ls_mtdkey    TYPE seocpdkey,
      lv_class     TYPE string,
      lv_icon      TYPE string,
      lv_jump_type TYPE tadir-object,
      lv_jump_name TYPE tadir-obj_name,
      lv_obj_text  TYPE string,
      lv_obj_link  TYPE string,
      lv_msg_text  TYPE string,
      lv_msg_link  TYPE string,
      lv_msg_code  TYPE string,
      lv_rest      TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    CASE is_issue-level.
      WHEN 'failure'.
        lv_class = 'ci-error'.
        lv_icon  = ri_html->icon(
          iv_name = 'exclamation-circle/red'
          iv_hint = 'Failure' ).
      WHEN 'warning'.
        lv_class = 'ci-warning'.
        lv_icon  = ri_html->icon(
          iv_name = 'exclamation-triangle/yellow'
          iv_hint = 'Warning' ).
      WHEN OTHERS.
        lv_class = 'ci-info'.
    ENDCASE.

    " Default jump is to source
    lv_jump_type = 'PROG'.
    lv_jump_name = is_issue-program.

    CASE is_issue-obj_type.
      WHEN 'CLAS'.
        CASE to_lower( is_issue-obj_subtype ).
          WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_def.
            lv_obj_text = |CLAS { is_issue-obj_name } : Local Definitions|.
          WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_imp.
            lv_obj_text = |CLAS { is_issue-obj_name } : Local Implementations|.
          WHEN zif_abapgit_oo_object_fnc=>c_parts-macros.
            lv_obj_text = |CLAS { is_issue-obj_name } : Macros|.
          WHEN zif_abapgit_oo_object_fnc=>c_parts-testclasses.
            lv_obj_text = |CLAS { is_issue-obj_name } : Test Classes|.
          WHEN OTHERS.
            cl_oo_classname_service=>get_method_by_include(
              EXPORTING
                incname             = is_issue-program
              RECEIVING
                mtdkey              = ls_mtdkey
              EXCEPTIONS
                class_not_existing  = 1
                method_not_existing = 2
                OTHERS              = 3 ).
            IF sy-subrc = 0.
              lv_obj_text = |CLAS { is_issue-obj_name }->{ ls_mtdkey-cpdname }|.
            ELSE.
              lv_obj_text = |CLAS { is_issue-obj_name }|.
            ENDIF.
        ENDCASE.
      WHEN 'FUGR'.
        lv_obj_text = |FUGR { is_issue-obj_name } { is_issue-obj_subtype }|.
      WHEN OTHERS.
        lv_obj_text = |{ is_issue-obj_type } { is_issue-obj_name }|.
        lv_jump_type = is_issue-obj_type.
        lv_jump_name = is_issue-obj_name.
    ENDCASE.

    lv_msg_text = escape(
      val    = is_issue-title
      format = cl_abap_format=>e_html_text ).

    lv_msg_code = reverse( is_issue-url ).
    SPLIT lv_msg_code AT '/' INTO lv_msg_code lv_rest.
    lv_msg_code = reverse( lv_msg_code ).

    lv_msg_link = ri_html->a(
      iv_txt   = lv_msg_code
      iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ is_issue-url }|
      iv_class = 'url' ).

    lv_obj_text = |{ lv_obj_text } [ @{ is_issue-line } ]|.
    lv_obj_link = |{ c_action-jump_edit }?type={ lv_jump_type }&name={ lv_jump_name }&line={ is_issue-line }|.

    ri_html->add( |<li class="{ lv_class }">| ).
    ri_html->add_a(
      iv_txt = lv_obj_text
      iv_act = lv_obj_link
      iv_typ = zif_abapgit_html=>c_action_type-sapevent ).
    ri_html->add( |<span class="margin-v5">{ _get_logo( lv_msg_code ) } { lv_msg_text } ({ lv_msg_link })</span>| ).

    IF gv_view_source = abap_true.
      ri_html->add( _render_source( is_issue ) ).
    ENDIF.

    ri_html->add( |</li>| ).

  ENDMETHOD.


  METHOD _render_issues.

    DATA:
      ls_issue LIKE LINE OF mt_issues,
      lv_max   TYPE i,
      lv_url   TYPE string.

    lv_max = zcl_abaplint_abapgit_ext_agent=>c_max_annotations.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="ci-result"><ul>' ).

    LOOP AT mt_issues INTO ls_issue TO lv_max.

      ri_html->add( _render_issue( ls_issue ) ).

    ENDLOOP.

    ri_html->add( '</ul></div>' ).

    IF lines( mt_issues ) = 0.
      ri_html->add( '<div class="dummydiv success">' ).
      ri_html->add( ri_html->icon( 'check' ) ).
      ri_html->add( 'No abaplint findings' ).
      ri_html->add( '</div>' ).
    ELSEIF lines( mt_issues ) > lv_max.
      lv_url = zcl_abaplint_abapgit_ext_exit=>get_instance( )->get_last_url( ).
      ri_html->add( '<div class="dummydiv warning">' ).
      ri_html->add( ri_html->icon( 'exclamation-triangle' ) ).
      ri_html->add( |Only first { lv_max } findings shown in list.| ).
      ri_html->add_a(
        iv_txt  = 'Show more...'
        iv_act  = |{ zif_abapgit_definitions=>c_action-url }?url={ lv_url }| ).
      ri_html->add( '</div>' ).
    ENDIF.

  ENDMETHOD.


  METHOD _render_source.

    DATA:
      lv_source LIKE LINE OF is_issue-source,
      lv_line   TYPE i,
      lv_class  TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF is_issue-source IS INITIAL.
      RETURN.
    ENDIF.

    " Use same styles as diff pages
    ri_html->add( '<div class="diff_content">' ).
    ri_html->add( '<table class="diff_tab syntax-hl" i>' ).
    ri_html->add( '<thead class="nav_line">' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<th class="num"></th>' ).
    ri_html->add( '<th></th>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

    LOOP AT is_issue-source INTO lv_source FROM is_issue-line - c_lines_before TO is_issue-line + c_lines_after.
      lv_line = sy-tabix.
      lv_source = escape(
        val    = lv_source
        format = cl_abap_format=>e_html_text ).
      ri_html->add( '<tr>' ).
      IF lv_line = is_issue-line.
        CASE is_issue-level.
          WHEN 'failure'.
            "red
            lv_class = 'diff_del'.
          WHEN 'warning'.
            "yellow
            lv_class = 'diff_upd'.
          WHEN OTHERS.
            "green
            lv_class = 'diff_ins'.
        ENDCASE.
      ELSE.
        lv_class = 'diff_others'.
      ENDIF.
      ri_html->add( |<td class="num { lv_class }">{ lv_line }</td><td class="code { lv_class }">{ lv_source }</td>| ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    IF sy-subrc <> 0.
      ri_html->add( '<tr>' ).
      ri_html->add( '<td class="num diff_upd">0</td>' ).
      ri_html->add( '<td class="code diff_upd">Source location does not exist (anymore)</td>' ).
      ri_html->add( '</tr>' ).
    ENDIF.

    ri_html->add( '</table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
