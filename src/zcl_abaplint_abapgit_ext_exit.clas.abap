CLASS zcl_abaplint_abapgit_ext_exit DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abaplint_abapgit_ext_exit .
    METHODS on_event
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    METHODS wall_message_repo
      IMPORTING
        !is_repo_meta TYPE zif_abapgit_persistence=>ty_repo
        !ii_html      TYPE REF TO zif_abapgit_html .
    METHODS get_last_url
      RETURNING
        VALUE(rv_url) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_wall,
        commit TYPE string,
        html   TYPE REF TO zif_abapgit_html,
        url    TYPE string,
      END OF ty_wall .

    CONSTANTS:
      BEGIN OF c_action,
        go_abaplint TYPE string VALUE 'go_abaplint',
      END OF c_action .
    CONSTANTS:
      BEGIN OF c_git_status,
        queued      TYPE string VALUE 'queued',
        in_progress TYPE string VALUE 'in_progress',
        completed   TYPE string VALUE 'completed',
      END OF c_git_status .
    CONSTANTS:
      BEGIN OF c_git_conclusion,
        neutral TYPE string VALUE 'neutral',
        success TYPE string VALUE 'success',
        failure TYPE string VALUE 'failure',
      END OF c_git_conclusion .
    CLASS-DATA go_instance TYPE REF TO zcl_abaplint_abapgit_ext_exit.
    DATA:
      mt_wall TYPE HASHED TABLE OF ty_wall WITH UNIQUE KEY commit .
    DATA mv_last_url TYPE string.

    METHODS _wall_message_abaplint
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        !is_check_run  TYPE zcl_abaplint_abapgit_ext_chkrn=>ty_check_run
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_exit IMPLEMENTATION.


  METHOD get_instance.
    IF go_instance IS INITIAL.
      CREATE OBJECT go_instance.
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD get_last_url.
    rv_url = mv_last_url.
  ENDMETHOD.


  METHOD on_event.

    IF ii_event->mv_action = c_action-go_abaplint.
      rs_handled-page  = zcl_abaplint_abapgit_ext_ui=>create(
        iv_key       = |{ ii_event->query( )->get( 'KEY' ) }|
        iv_check_run = |{ ii_event->query( )->get( 'CHECKRUN' ) }| ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDIF.

  ENDMETHOD.


  METHOD wall_message_repo.

    DATA:
      lx_error       TYPE REF TO zcx_abapgit_exception,
      lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
      lv_commit      TYPE zif_abapgit_definitions=>ty_sha1,
      ls_wall        TYPE ty_wall,
      lo_check_run   TYPE REF TO zcl_abaplint_abapgit_ext_chkrn,
      ls_check_run   TYPE zcl_abaplint_abapgit_ext_chkrn=>ty_check_run,
      li_html        TYPE REF TO zif_abapgit_html.

    IF is_repo_meta-offline = abap_true.
      RETURN.
    ENDIF.

    TRY.
        lo_repo_online ?= zcl_abapgit_repo_srv=>get_instance( )->get( is_repo_meta-key ).

        lv_commit = lo_repo_online->get_current_remote( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        ii_html->add( |<div>{ lx_error->get_text( ) }</div>| ).
        RETURN.
    ENDTRY.

    READ TABLE mt_wall INTO ls_wall WITH TABLE KEY commit = lv_commit.
    IF sy-subrc <> 0.

      TRY.
          CREATE OBJECT lo_check_run
            EXPORTING
              iv_url    = is_repo_meta-url
              iv_commit = lv_commit.

          ls_check_run = lo_check_run->get( ).

          IF ls_check_run IS INITIAL.
            RETURN.
          ENDIF.

        CATCH zcx_abapgit_exception INTO lx_error.
          ii_html->add( |<div>{ lx_error->get_text( ) }</div>| ).
          RETURN.
      ENDTRY.

      li_html = _wall_message_abaplint(
        iv_key       = is_repo_meta-key
        is_check_run = ls_check_run ).

      ls_wall-commit = lv_commit.
      ls_wall-html   = li_html.
      ls_wall-url    = ls_check_run-url.

      " Cache result of completed checkruns (others might change)
      IF ls_check_run-status = c_git_status-completed.
        INSERT ls_wall INTO TABLE mt_wall.
      ENDIF.

    ENDIF.

    ii_html->add( ls_wall-html->render( ) ).

    " Remember URL of last shown check run
    mv_last_url = ls_wall-url.

  ENDMETHOD.


  METHOD _wall_message_abaplint.

    DATA:
      lv_msg     TYPE string,
      lv_summary TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="abaplint-message" style="margin-top:10px;">' ).

    CASE is_check_run-status.
      WHEN c_git_status-queued.
        ri_html->add_a(
          iv_txt  = zcl_abapgit_html=>icon(
            iv_name = 'circle-solid'
            iv_hint = is_check_run-status )
          iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ is_check_run-url }| ).
      WHEN c_git_status-in_progress.
        ri_html->add_a(
          iv_txt  = zcl_abapgit_html=>icon(
            iv_name  = 'circle-solid'
            iv_class = 'warning'
            iv_hint  = is_check_run-status )
          iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ is_check_run-url }| ).
      WHEN c_git_status-completed.
        CASE is_check_run-conclusion.
          WHEN c_git_conclusion-neutral.
            ri_html->add_a(
              iv_txt  = zcl_abapgit_html=>icon(
                iv_name = 'circle-solid'
                iv_hint = is_check_run-conclusion )
              iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ is_check_run-url }| ).
          WHEN c_git_conclusion-success.
            ri_html->add_a(
              iv_txt  = zcl_abapgit_html=>icon(
                iv_name  = 'check'
                iv_class = 'success'
                iv_hint  = is_check_run-conclusion )
              iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ is_check_run-url }| ).
          WHEN c_git_conclusion-failure.
            ri_html->add_a(
              iv_txt  = zcl_abapgit_html=>icon(
                iv_name  = 'times-solid'
                iv_class = 'error'
                iv_hint  = is_check_run-conclusion )
              iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ is_check_run-url }| ).
          WHEN OTHERS.
            ri_html->add( |Unexpected value "{ is_check_run-conclusion }" for "conclusion"| ).
        ENDCASE.
      WHEN OTHERS.
        ri_html->add( |Unexpected value "{ is_check_run-status }" for "status"| ).
    ENDCASE.

    lv_msg = is_check_run-name.
    IF lv_msg <> is_check_run-app.
      lv_msg = |{ is_check_run-app } - { is_check_run-name }|.
    ENDIF.

    lv_summary = is_check_run-summary.
    IF lv_summary IS NOT INITIAL.
      REPLACE 'First 50 annotations shown, ' IN lv_summary WITH ''.

      " todo, maybe better to show link only for failures
      lv_summary = ri_html->a(
        iv_txt = lv_summary
        iv_act = |{ c_action-go_abaplint }?key={ iv_key }&checkrun={ is_check_run-id }| ).

      lv_msg = |{ lv_msg }: { lv_summary }|.
    ENDIF.

    ri_html->add( lv_msg ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
