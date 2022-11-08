CLASS zcl_abaplint_abapgit_ext_chkrn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

* GitHub REST API for Checks - Check Run
* https://docs.github.com/en/rest/reference/checks
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_check_run,
        app         TYPE string,
        status      TYPE string,
        conclusion  TYPE string,
        name        TYPE string,
        id          TYPE string,
        url         TYPE string,
        summary     TYPE string,
        count_shown TYPE i,
        count_total TYPE i,
      END OF ty_check_run.

    METHODS constructor
      IMPORTING
        !iv_url    TYPE string
        !iv_commit TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    METHODS get
      RETURNING
        VALUE(rs_check_run) TYPE ty_check_run
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_url TYPE string .
    DATA mv_commit TYPE zif_abapgit_definitions=>ty_sha1 .

ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_chkrn IMPLEMENTATION.


  METHOD constructor.
    mv_url    = iv_url.
    mv_commit = iv_commit.
  ENDMETHOD.


  METHOD get.

    DATA:
      li_json       TYPE REF TO zif_abapgit_ajson,
      lx_error      TYPE REF TO zcx_abapgit_ajson_error,
      lt_check_runs TYPE TABLE OF string,
      lv_check_run  TYPE string,
      lv_msg        TYPE string,
      lv_count      TYPE string,
      lv_app        TYPE string,
      lv_name       TYPE string.

    TRY.
        li_json = zcl_abaplint_abapgit_ext_agent=>get_instance( mv_url )->get_check_runs( mv_commit ).

        lv_msg = li_json->get( '/message' ).

        IF lv_msg <> ''.
          zcx_abapgit_exception=>raise( |Error getting check runs: { lv_msg }| ).
        ENDIF.

        lt_check_runs = li_json->members( '/check_runs' ).

        LOOP AT lt_check_runs INTO lv_check_run.

          lv_app  = li_json->get( |/check_runs/{ lv_check_run }/app/name| ).
          lv_name = li_json->get( |/check_runs/{ lv_check_run }/name| ).

          " Only interested in abaplint run (not builds or abalint/observations)
          IF lv_app = 'abaplint' AND lv_name = 'abaplint'.

            rs_check_run-app         = lv_app.
            rs_check_run-name        = lv_name.
            rs_check_run-id          = li_json->get( |/check_runs/{ lv_check_run }/id| ).
            rs_check_run-status      = li_json->get( |/check_runs/{ lv_check_run }/status| ).
            rs_check_run-conclusion  = li_json->get( |/check_runs/{ lv_check_run }/conclusion| ).
            rs_check_run-url         = li_json->get( |/check_runs/{ lv_check_run }/html_url| ).
            rs_check_run-summary     = li_json->get( |/check_runs/{ lv_check_run }/output/summary| ).
            rs_check_run-count_shown = li_json->get( |/check_runs/{ lv_check_run }/output/annotation_count| ).

            IF rs_check_run-summary CS 'Error'.
              zcx_abapgit_exception=>raise( rs_check_run-summary ).
            ENDIF.

            SPLIT rs_check_run-summary AT space INTO lv_count lv_msg.
            rs_check_run-count_total = lv_count.

            REPLACE ALL OCCURRENCES OF
              cl_abap_char_utilities=>newline && cl_abap_char_utilities=>newline
              IN rs_check_run-summary WITH `, `.

            " Remove link to https://github.com/apps/abaplint/installations/new
            REPLACE REGEX ', \[adjust installations\].*' IN rs_check_run-summary WITH ''.

            EXIT.

          ENDIF.

        ENDLOOP.

      CATCH zcx_abapgit_ajson_error INTO lx_error.
        zcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
