CLASS zcl_abaplint_abapgit_ext_annot DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
* Example:
*     "path": "src/dev/zcl_abapinst_installer.clas.abap",
*     "blob_href": "https://github.com/Marc-Bernard-Tools/ABAP-Installer/blob/
*       95990d129bf6e29701917804c4d2e9f93a701f2f/src/dev/zcl_abapinst_installer.clas.abap",
*     "start_line": 631,
*     "start_column": null,
*     "end_line": 631,
*     "end_column": null,
*     "annotation_level": "failure",
*     "title": "Not a structure, type unknown, FieldChain",
*     "message": "https://rules.abaplint.org/check_syntax",
*     "raw_details": null
      BEGIN OF ty_annotation,
        path             TYPE string,
        blob_href        TYPE string,
        start_line       TYPE i,
        start_column     TYPE i,
        end_line         TYPE i,
        end_column       TYPE i,
        annotation_level TYPE string,
        title            TYPE string,
        message          TYPE string,
        raw_details      TYPE string,
      END OF ty_annotation .
    TYPES ty_annotations TYPE STANDARD TABLE OF ty_annotation WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !iv_url       TYPE string
        !iv_check_run TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS get
      RETURNING
        VALUE(rt_annotations) TYPE ty_annotations
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_url TYPE string .
    DATA mv_check_run TYPE string.

ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_annot IMPLEMENTATION.


  METHOD constructor.
    mv_url       = iv_url.
    mv_check_run = iv_check_run.
  ENDMETHOD.


  METHOD get.

    DATA:
      li_json       TYPE REF TO zif_abapgit_ajson_reader,
      li_json_anno  TYPE REF TO zif_abapgit_ajson_reader,
      lx_error      TYPE REF TO zcx_abapgit_ajson_error,
      lv_msg        TYPE string,
      lt_anno       TYPE TABLE OF string,
      lv_anno       TYPE string,
      ls_annotation TYPE ty_annotation.

    TRY.
        li_json = zcl_abaplint_abapgit_ext_agent=>get_instance( mv_url )->get_annotations( mv_check_run ).

        lv_msg = li_json->get( '/message' ).

        IF lv_msg <> ''.
          zcx_abapgit_exception=>raise( |Error getting annotations: { lv_msg }| ).
        ENDIF.

        lt_anno = li_json->members( '/' ).

        LOOP AT lt_anno INTO lv_anno.

          li_json_anno = li_json->slice( |/{ lv_anno }| ).
          li_json_anno->to_abap( IMPORTING ev_container = ls_annotation ).

          INSERT ls_annotation INTO TABLE rt_annotations.
        ENDLOOP.

      CATCH zcx_abapgit_ajson_error INTO lx_error.
        zcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
