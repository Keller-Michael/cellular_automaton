*&---------------------------------------------------------------------*
*& Report ZCELLULAR_AUTOMATON
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcellular_automaton.

CLASS: lcl_simulation    DEFINITION DEFERRED,
       lcl_playing_field DEFINITION DEFERRED.

TYPES: BEGIN OF attendee,
         pos_x      TYPE i,
         pos_y      TYPE i,
         number     TYPE text10,
         open       TYPE i,     " open-mindedness
         comm       TYPE i,     " communicative competence
         origin     TYPE xfeld, " origin of innovation
         adopted    TYPE xfeld, " innovation transfered
         generation TYPE i,
       END OF attendee.

TYPES attendees TYPE TABLE OF attendee.

TYPES: BEGIN OF protocol_entry,
         date       TYPE sydatum,
         time       TYPE syuzeit,
         generation TYPE i,
         text       TYPE sdydo_text_element,
       END OF protocol_entry.

TYPES protocol_entries TYPE TABLE OF protocol_entry.

DATA gr_simulation TYPE REF TO lcl_simulation.

**********************************************************************

SELECTION-SCREEN: BEGIN OF BLOCK info WITH FRAME TITLE title_01,
                  COMMENT 1(79) text_000,
                  COMMENT /1(79) text_001.
SELECTION-SCREEN: END OF BLOCK info.

SELECTION-SCREEN: BEGIN OF BLOCK params WITH FRAME TITLE title_02,
                  BEGIN OF LINE,
                  COMMENT 1(25) text_002,
                  POSITION 27.
PARAMETERS:       pa_max_x TYPE numc2 DEFAULT 3.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(25) text_003,
                  POSITION 27.
PARAMETERS:       pa_max_y TYPE numc2 DEFAULT 3.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(25) text_004,
                  POSITION 27.
PARAMETERS:       pa_thgrn TYPE numc2 DEFAULT 5.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(25) text_005,
                  POSITION 27.
PARAMETERS:       pa_thorg TYPE numc2 DEFAULT 10.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(25) text_006,
                  POSITION 27.
PARAMETERS:       pa_thred TYPE numc2 DEFAULT 15.
SELECTION-SCREEN: END OF LINE,
                  END OF BLOCK params.

**********************************************************************

CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.

ENDCLASS.


CLASS lcx_error IMPLEMENTATION.
ENDCLASS.

**********************************************************************

CLASS lcl_protocol DEFINITION.

  PUBLIC SECTION.
    METHODS add_entry
      IMPORTING
        iv_generation TYPE i
        iv_text       TYPE sdydo_text_element.

    METHODS get_last_entry
      EXPORTING
        es_entry TYPE protocol_entry.

    METHODS get_entries_per_generation
      IMPORTING
        iv_generation TYPE i
      EXPORTING
        et_entries    TYPE protocol_entries.

  PRIVATE SECTION.
    DATA mt_entries TYPE protocol_entries.

ENDCLASS.

**********************************************************************

CLASS lcl_attendees DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_simulation TYPE REF TO lcl_simulation.

    METHODS get_attendee
      IMPORTING
        iv_pos_x    TYPE i
        iv_pos_y    TYPE i
      EXPORTING
        es_attendee TYPE attendee
      RAISING
        lcx_error.

    METHODS get_all_attendees
      EXPORTING
        et_attendees TYPE attendees.

    METHODS get_neighbourhood
      IMPORTING
        iv_pos_x     TYPE i
        iv_pos_y     TYPE i
      EXPORTING
        et_attendees TYPE attendees
      RAISING
        lcx_error.

    METHODS get_neighbour_above
      IMPORTING
        iv_pos_x           TYPE i
        iv_pos_y           TYPE i
      RETURNING
        VALUE(es_attendee) TYPE attendee
      RAISING
        lcx_error.

    METHODS get_neighbour_below
      IMPORTING
        iv_pos_x           TYPE i
        iv_pos_y           TYPE i
      RETURNING
        VALUE(es_attendee) TYPE attendee
      RAISING
        lcx_error.

    METHODS get_neighbour_to_the_left
      IMPORTING
        iv_pos_x           TYPE i
        iv_pos_y           TYPE i
      RETURNING
        VALUE(es_attendee) TYPE attendee
      RAISING
        lcx_error.

    METHODS get_neighbour_to_the_right
      IMPORTING
        iv_pos_x           TYPE i
        iv_pos_y           TYPE i
      RETURNING
        VALUE(es_attendee) TYPE attendee
      RAISING
        lcx_error.

    METHODS set_attendee_adopted
      IMPORTING
        iv_pos_x      TYPE i
        iv_pos_y      TYPE i
        iv_generation TYPE i.

  PRIVATE SECTION.
    DATA: mt_attendees  TYPE attendees,
          mr_simulation TYPE REF TO lcl_simulation.

ENDCLASS.

**********************************************************************

CLASS lcl_playing_field DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_simulation TYPE REF TO lcl_simulation
        ir_attendees  TYPE REF TO lcl_attendees
      RAISING
        lcx_error.

    METHODS refresh.

    METHODS set_reference
      IMPORTING
        iv_reference TYPE REF TO cl_gui_alv_grid.

    METHODS display
      RAISING
        lcx_error.

  PRIVATE SECTION.
    DATA: mr_simulation    TYPE REF TO lcl_simulation,
          mt_field_catalog TYPE lvc_t_fcat,
          mr_fields        TYPE REF TO data,
          mr_reference     TYPE REF TO cl_gui_alv_grid,
          mr_attendees     TYPE REF TO lcl_attendees.

    METHODS build
      RAISING
        lcx_error.

    METHODS generate_tooltip
      IMPORTING
        iv_pos_x          TYPE i
        iv_pos_y          TYPE i
      RETURNING
        VALUE(ev_tooltip) TYPE text50
      RAISING
        lcx_error.

    METHODS prepare_field_catalog
      RAISING
        lcx_error.

ENDCLASS.

**********************************************************************

CLASS lcl_simulation DEFINITION.

  PUBLIC SECTION.
    DATA: mr_protocol         TYPE REF TO lcl_protocol,
          mv_count_attendees  TYPE i READ-ONLY,
          mv_count_adoption   TYPE i READ-ONLY,
          mv_count_left       TYPE i READ-ONLY,
          mv_count_generation TYPE i READ-ONLY,
          mv_pos_max_x        TYPE i READ-ONLY,
          mv_pos_max_y        TYPE i READ-ONLY,
          mv_start_pos_x      TYPE i READ-ONLY,
          mv_start_pos_y      TYPE i READ-ONLY,
          mv_treshold_green   TYPE numc2 READ-ONLY,
          mv_treshold_orange  TYPE numc2 READ-ONLY,
          mv_treshold_red     TYPE numc2 READ-ONLY.

    METHODS constructor
      IMPORTING
        iv_max_x           TYPE numc2
        iv_max_y           TYPE numc2
        iv_treshold_green  TYPE numc2
        iv_treshold_orange TYPE numc2
        iv_treshold_red    TYPE numc2
      RAISING
        lcx_error.

    METHODS handle_context_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid
      IMPORTING e_object sender.

    METHODS set_playfield_reference
      IMPORTING
        iv_reference TYPE REF TO cl_gui_alv_grid.

    METHODS start_simulation.

    METHODS reset_timer.

    METHODS restart_timer.

  PRIVATE SECTION.
    DATA: mr_playing_field TYPE REF TO lcl_playing_field,
          mr_attendees     TYPE REF TO lcl_attendees,
          mr_timer         TYPE REF TO cl_gui_timer.

    METHODS check_innovation_adoption
      IMPORTING
        is_attendee      TYPE attendee
        it_neighbourhood TYPE attendees
      EXPORTING
        ev_adopted       TYPE xfeld
      RAISING
        lcx_error.

    METHODS create_new_generation
      RAISING
        lcx_error.

    METHODS handle_timer_finished FOR EVENT finished OF cl_gui_timer.
ENDCLASS.

**********************************************************************

CLASS lcl_protocol IMPLEMENTATION.

  METHOD add_entry.
    DATA ls_entry TYPE protocol_entry.

    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.

    ls_entry-date       = sy-datum.
    ls_entry-time       = sy-uzeit.
    ls_entry-generation = iv_generation.
    ls_entry-text       = iv_text.

    APPEND ls_entry TO mt_entries.
  ENDMETHOD.


  METHOD get_last_entry.
    DATA lv_lines TYPE i.

    lv_lines = lines( mt_entries ).

    READ TABLE mt_entries INTO es_entry INDEX lv_lines.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.


  METHOD get_entries_per_generation.
    DATA ls_entry LIKE LINE OF mt_entries.

    LOOP AT mt_entries INTO ls_entry WHERE generation = iv_generation.
      APPEND ls_entry TO et_entries.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_attendees IMPLEMENTATION.

  METHOD constructor.
    DATA: ls_attendee   TYPE attendee,
          lv_x          TYPE numc2,
          lv_y          TYPE numc2,
          lr_random_int TYPE REF TO cl_abap_random_int.

    mr_simulation = ir_simulation.

    lr_random_int = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max = 100 ).

    " generate attendees with random communication and open-mindedness level
    DO mr_simulation->mv_pos_max_y TIMES.
      lv_y = sy-index.
      DO mr_simulation->mv_pos_max_x TIMES.
        CLEAR ls_attendee.
        lv_x = sy-index.
        ls_attendee-pos_x = lv_x.
        ls_attendee-pos_y = lv_y.
        CONCATENATE 'X' lv_x '-' 'Y' lv_y INTO ls_attendee-number.
        ls_attendee-comm = lr_random_int->get_next( ).
        ls_attendee-open = lr_random_int->get_next( ).
        IF lv_x = mr_simulation->mv_start_pos_x AND lv_y = mr_simulation->mv_start_pos_y.
          ls_attendee-origin  = abap_true.
          ls_attendee-adopted = abap_true.
        ENDIF.
        APPEND ls_attendee TO mt_attendees.
      ENDDO.
    ENDDO.
  ENDMETHOD.


  METHOD get_attendee.
    FIELD-SYMBOLS <attendee> TYPE attendee.

    READ TABLE mt_attendees ASSIGNING <attendee>
                            WITH KEY pos_x = iv_pos_x
                                     pos_y = iv_pos_y.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE i499(sy) WITH 'Attendee not found.'.
    ENDIF.

    es_attendee = <attendee>.
  ENDMETHOD.


  METHOD get_all_attendees.
    et_attendees = mt_attendees.
  ENDMETHOD.


  METHOD get_neighbour_above.
    DATA lv_pos_y TYPE i.

    lv_pos_y = iv_pos_y - 1.

    IF lv_pos_y <= 0.
      lv_pos_y = mr_simulation->mv_pos_max_y.
    ENDIF.

    READ TABLE mt_attendees INTO es_attendee
                            WITH KEY pos_x = iv_pos_x
                                     pos_y = lv_pos_y.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Neighbour above not found.'.
    ENDIF.
  ENDMETHOD.


  METHOD get_neighbour_below.
    DATA lv_pos_y TYPE i.

    lv_pos_y = iv_pos_y + 1.

    IF lv_pos_y > mr_simulation->mv_pos_max_y.
      lv_pos_y = 1.
    ENDIF.

    READ TABLE mt_attendees INTO es_attendee
                            WITH KEY pos_x = iv_pos_x
                                     pos_y = lv_pos_y.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Neighbour below not found.'.
    ENDIF.
  ENDMETHOD.


  METHOD get_neighbour_to_the_right.

    DATA lv_pos_x TYPE i.

    lv_pos_x = iv_pos_x + 1.

    IF lv_pos_x > mr_simulation->mv_pos_max_x.
      lv_pos_x = 1.
    ENDIF.

    READ TABLE mt_attendees INTO es_attendee
                            WITH KEY pos_x = lv_pos_x
                                     pos_y = iv_pos_y.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Neighbour to the right not found.'.
    ENDIF.
  ENDMETHOD.


  METHOD get_neighbour_to_the_left.
    DATA lv_pos_x TYPE i.

    lv_pos_x = iv_pos_x - 1.

    IF lv_pos_x <= 0.
      lv_pos_x = mr_simulation->mv_pos_max_x.
    ENDIF.

    READ TABLE mt_attendees INTO es_attendee
                            WITH KEY pos_x = lv_pos_x
                                     pos_y = iv_pos_y.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Neighbour to the left not found.'.
    ENDIF.
  ENDMETHOD.


  METHOD get_neighbourhood.
    DATA: lt_attendees   TYPE attendees,
          ls_attendee    TYPE attendee,
          lv_pos_y_above TYPE i,
          lv_pos_y_below TYPE i,
          lv_pos_y       TYPE i.

    TRY.
        ls_attendee = get_neighbour_above(
                        EXPORTING
                          iv_pos_x    = iv_pos_x
                          iv_pos_y    = iv_pos_y ).

        lv_pos_y_above = ls_attendee-pos_y.
        APPEND ls_attendee TO lt_attendees.

        CLEAR ls_attendee.

        ls_attendee = get_neighbour_below(
                        EXPORTING
                          iv_pos_x    = iv_pos_x
                          iv_pos_y    = iv_pos_y ).

        lv_pos_y_below = ls_attendee-pos_y.
        APPEND ls_attendee TO lt_attendees.

        DO 3 TIMES.
          CASE sy-index.
            WHEN 1.
              lv_pos_y = lv_pos_y_above.
            WHEN 2.
              lv_pos_y = iv_pos_y.
            WHEN 3.
              lv_pos_y = lv_pos_y_below.
          ENDCASE.

          CLEAR ls_attendee.

          ls_attendee = get_neighbour_to_the_left(
                          EXPORTING
                            iv_pos_x    = iv_pos_x
                            iv_pos_y    = lv_pos_y ).

          APPEND ls_attendee TO lt_attendees.

          CLEAR ls_attendee.

          ls_attendee = get_neighbour_to_the_right(
                          EXPORTING
                            iv_pos_x    = iv_pos_x
                            iv_pos_y    = lv_pos_y ).


          APPEND ls_attendee TO lt_attendees.
        ENDDO.

      CATCH lcx_error.
        RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Error identifying neighbourhood .'.
    ENDTRY.

    et_attendees = lt_attendees.
  ENDMETHOD.


  METHOD set_attendee_adopted.
    FIELD-SYMBOLS <attendee> LIKE LINE OF mt_attendees.

    READ TABLE mt_attendees ASSIGNING <attendee>
                            WITH KEY pos_x = iv_pos_x
                                     pos_y = iv_pos_y.

    IF sy-subrc = 0.
      <attendee>-adopted    = abap_true.
      <attendee>-generation = iv_generation.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_playing_field IMPLEMENTATION.

  METHOD build.
    DATA: ls_line       TYPE REF TO data,
          lv_field_name TYPE lvc_fname,
          lv_x_i        TYPE i,
          lv_y_i        TYPE i,
          lv_x_c        TYPE numc2,
          lv_y_c        TYPE numc2.

    FIELD-SYMBOLS: <fields> TYPE STANDARD TABLE,
                   <line>   TYPE any,
                   <field>  TYPE any.

    " create playing field as internal table described by field catalog
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = mt_field_catalog
      IMPORTING
        ep_table        = mr_fields.

    ASSIGN mr_fields->* TO <fields>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE i499(sy) WITH 'Playing field could not be created.'.
    ENDIF.

    CREATE DATA ls_line LIKE LINE OF <fields>.
    ASSIGN ls_line->* TO <line>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE i499(sy) WITH 'Playing field could not be created.'.
    ENDIF.

    " initialize
    DO mr_simulation->mv_pos_max_y TIMES.
      lv_y_i = sy-index.
      lv_y_c = lv_y_i.

      DO mr_simulation->mv_pos_max_x TIMES.
        lv_x_i = sy-index.
        lv_x_c = lv_x_i.

        " set icon
        CONCATENATE 'ICON-' lv_x_c INTO lv_field_name.
        ASSIGN COMPONENT lv_field_name OF STRUCTURE <line> TO <field>.
        IF sy-subrc = 0.
          <field> = generate_tooltip(
            EXPORTING
              iv_pos_x = lv_x_i
              iv_pos_y = lv_y_i ).
        ENDIF.

        CONCATENATE 'NUMBER-' lv_x_c INTO lv_field_name.
        ASSIGN COMPONENT lv_field_name OF STRUCTURE <line> TO <field>.
        IF sy-subrc = 0.
          CONCATENATE 'X' lv_x_c '-' 'Y' lv_y_c INTO <field>.
        ENDIF.
      ENDDO.

      IF ls_line IS NOT INITIAL.
        APPEND <line> TO <fields>.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD constructor.
    mr_simulation = ir_simulation.
    mr_attendees  = ir_attendees.

    TRY.
        me->prepare_field_catalog( ).
        me->build( ).
      CATCH lcx_error.
        RAISE EXCEPTION TYPE lcx_error MESSAGE i499(sy) WITH 'Playing field could not be created.'.
    ENDTRY.
  ENDMETHOD.


  METHOD generate_tooltip.
    DATA: ls_attendee   TYPE attendee,
          lv_op_level   TYPE numc2,
          lv_com_level  TYPE numc2,
          lv_generation TYPE numc3,
          lv_info       TYPE text40,
          lv_icon       TYPE icon_d.

    TRY.
        mr_attendees->get_attendee(
          EXPORTING
            iv_pos_x = iv_pos_x
            iv_pos_y = iv_pos_y
          IMPORTING
            es_attendee = ls_attendee ).
      CATCH lcx_error.
        RETURN.
    ENDTRY.

    lv_op_level  = ls_attendee-open.
    lv_com_level = ls_attendee-comm.

    lv_info = 'OP: &1, COM: &2'.
    SHIFT lv_op_level LEFT DELETING LEADING '0'.
    REPLACE '&1' IN lv_info WITH lv_op_level.

    SHIFT lv_com_level LEFT DELETING LEADING '0'.
    REPLACE '&2' IN lv_info WITH lv_com_level.

    IF ls_attendee-generation <> 0.
      lv_generation = ls_attendee-generation.
      SHIFT lv_generation LEFT DELETING LEADING '0'.
      CONCATENATE lv_info ', GEN: &1' INTO lv_info.
      REPLACE '&1' IN lv_info WITH lv_generation.
    ENDIF.

    IF ls_attendee-origin = abap_true.
      lv_icon = icon_helpassistent_on.
    ELSE.
      IF ls_attendee-adopted = abap_true.
        lv_icon = icon_customer.
      ELSE.
        lv_icon = icon_hr_position.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = lv_icon
        text                  = space
        info                  = lv_info
        add_stdinf            = space
      IMPORTING
        result                = ev_tooltip
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE i499(sy) WITH 'Tooltip not generated.'.
    ENDIF.
  ENDMETHOD.


  METHOD refresh.
    DATA: ls_stable     TYPE lvc_s_stbl,
          lt_attendees  TYPE TABLE OF attendee,
          ls_attendee   LIKE LINE OF lt_attendees,
          ls_line       TYPE REF TO data,
          lv_position   TYPE numc2,
          lv_field_name TYPE lvc_fname,
          ls_cell_color TYPE lvc_s_scol,
          lt_cell_color TYPE lvc_t_scol,
          ls_color      TYPE lvc_s_colo,
          lt_color      TYPE lvc_t_scol.

    FIELD-SYMBOLS: <fields> TYPE STANDARD TABLE,
                   <line>   TYPE any,
                   <field>  TYPE any.

    IF mr_reference IS NOT BOUND.
      RETURN.
    ENDIF.

    mr_attendees->get_all_attendees(
      IMPORTING
        et_attendees = lt_attendees ).

    ASSIGN mr_fields->* TO <fields>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE DATA ls_line LIKE LINE OF <fields>.
    ASSIGN ls_line->* TO <line>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <fields> ASSIGNING <line>. " every row
      CLEAR lt_cell_color.

      DO mr_simulation->mv_pos_max_x TIMES. " every column
        lv_position = sy-index.
        CLEAR ls_attendee.

        CONCATENATE 'NUMBER-' lv_position INTO lv_field_name.
        ASSIGN COMPONENT lv_field_name OF STRUCTURE <line> TO <field>.
        IF sy-subrc = 0.
          READ TABLE lt_attendees INTO ls_attendee
                                  WITH KEY number = <field>.

          IF sy-subrc = 0 AND ls_attendee-adopted = abap_true.
            CONCATENATE 'ICON-' lv_position INTO lv_field_name.
            ASSIGN COMPONENT lv_field_name OF STRUCTURE <line> TO <field>.
            IF sy-subrc = 0.
              TRY.
                  <field> = generate_tooltip(
                    EXPORTING
                      iv_pos_x = ls_attendee-pos_x
                      iv_pos_y = ls_attendee-pos_y ).
                CATCH lcx_error.
                  RETURN.
              ENDTRY.

              " set background color
              ASSIGN COMPONENT 'COLOR' OF STRUCTURE <line> TO <field>.
              IF sy-subrc = 0.
                CLEAR: ls_cell_color,
                       ls_color.

                IF ls_attendee-generation > 0 AND ls_attendee-generation <= mr_simulation->mv_treshold_green.
                  ls_color-col = 5.
                ELSEIF ls_attendee-generation > mr_simulation->mv_treshold_green AND ls_attendee-generation <= mr_simulation->mv_treshold_orange.
                  ls_color-col = 7.
                ELSEIF ls_attendee-generation >= mr_simulation->mv_treshold_red.
                  ls_color-col = 6.
                ENDIF.
                ls_color-int = 1.
                ls_color-inv = 1.
                ls_cell_color-color = ls_color.
                CONCATENATE 'ICON-' lv_position INTO ls_cell_color-fname.
                APPEND ls_cell_color TO lt_cell_color.
                <field> = lt_cell_color.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.

    ls_stable-col = abap_true.
    ls_stable-row = abap_true.

    CALL METHOD mr_reference->refresh_table_display
      EXPORTING
        is_stable      = ls_stable
        i_soft_refresh = abap_false
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " this part is needed to refresh information at top and bottom level
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = '&IC1'.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD prepare_field_catalog.
    DATA: ls_field_catalog TYPE lvc_s_fcat,
          lv_position      TYPE numc2.

    DO mr_simulation->mv_pos_max_x TIMES.
      lv_position = sy-index.

      CLEAR ls_field_catalog.
      CONCATENATE 'ICON-' lv_position INTO ls_field_catalog-fieldname.
      ls_field_catalog-col_pos   = lv_position.
      ls_field_catalog-intlen    = 45.
      ls_field_catalog-inttype   = 'C'.
      ls_field_catalog-outputlen = 4.
      ls_field_catalog-scrtext_s = space.
      ls_field_catalog-scrtext_m = space.
      ls_field_catalog-scrtext_l = space.
      ls_field_catalog-icon      = abap_true.
      ls_field_catalog-col_opt   = abap_true.
      APPEND ls_field_catalog TO mt_field_catalog.

      CLEAR ls_field_catalog.
      CONCATENATE 'NUMBER-' lv_position INTO ls_field_catalog-fieldname.
      ls_field_catalog-col_pos  = lv_position.
      ls_field_catalog-no_out   = abap_true.
      ls_field_catalog-datatype = 'TEXT10'.
      ls_field_catalog-inttype  = 'C'.
      ls_field_catalog-intlen   = 10.
      APPEND ls_field_catalog TO mt_field_catalog.
    ENDDO.

    CLEAR ls_field_catalog.
    ls_field_catalog-col_pos = lv_position + 1.
    ls_field_catalog-fieldname = 'COLOR'.
    ls_field_catalog-ref_field = 'TABCOL'.
    ls_field_catalog-ref_table = 'BAL_S_SHOW_COL'.
    ls_field_catalog-tech      = abap_true.
    ls_field_catalog-no_out    = abap_true.
    APPEND ls_field_catalog TO mt_field_catalog.
  ENDMETHOD.


  METHOD set_reference.
    mr_reference = iv_reference.
  ENDMETHOD.


  METHOD display.
    DATA: ls_layout TYPE lvc_s_layo,
          lt_events TYPE slis_t_event,
          lt_evexit TYPE slis_t_event_exit,
          ls_evexit TYPE slis_event_exit,
          lt_excl   TYPE slis_t_extab,
          lv_cbpfs  TYPE slis_formname,
          ls_line   TYPE REF TO data,
          lv_number TYPE numc5,
          lv_fname  TYPE lvc_fname,
          lv_count  TYPE numc10,
          lv_text   TYPE natxt.

    FIELD-SYMBOLS: <fields> TYPE STANDARD TABLE,
                   <events> TYPE slis_alv_event.

    ls_layout-ctab_fname = 'COLOR'.
    ls_layout-stylefname = 'STYLE'.
    ls_layout-col_opt    = abap_true.
    ls_layout-sel_mode   = 'D'.

    CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
      EXPORTING
        i_list_type     = 4
      IMPORTING
        et_events       = lt_events
      EXCEPTIONS
        list_type_wrong = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE i499(sy) WITH 'Playing field could not be displayed.'.
    ENDIF.

    LOOP AT lt_events ASSIGNING <events>.
      CASE <events>-name.
        WHEN 'CALLER_EXIT'.
          <events>-form = 'ALV_CALLER_EXIT'.

        WHEN 'END_OF_LIST'.
          <events>-form = 'ALV_END_OF_LIST_2'.

        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    ls_evexit-ucomm = '&INFO'.
    ls_evexit-after = 'X'.
    APPEND ls_evexit TO lt_evexit.

    ASSIGN mr_fields->* TO <fields>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE i499(sy) WITH 'Playing field could not be displayed.'.
    ENDIF.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_interface_check           = ' '
        i_bypassing_buffer          = abap_true
*       i_buffer_active             =
        i_callback_program          = sy-repid
*       i_callback_pf_status_set    = ' '
        i_callback_user_command     = 'ALV_USER_COMMAND'
*       i_callback_top_of_page      = ' '
        i_callback_html_top_of_page = 'ALV_TOP_OF_PAGE'
        i_callback_html_end_of_list = 'ALV_END_OF_LIST'
*       i_structure_name            =
*       i_background_id             = ' '
*       i_grid_title                = ' '
*       i_grid_settings             =
        is_layout_lvc               = ls_layout
        it_fieldcat_lvc             = mt_field_catalog
*       it_excluding                = lt_excl
*       it_special_groups_lvc       =
*       it_sort_lvc                 =
*       it_filter_lvc               =
*       it_hyperlink                =
*       is_sel_hide                 =
        i_default                   = abap_true
*       i_save                      = ' '
*       is_variant                  =
        it_events                   = lt_events
        it_event_exit               = lt_evexit
*       is_print_lvc                =
*       is_reprep_id_lvc            =
*       i_screen_start_column       = 0
*       i_screen_start_line         = 0
*       i_screen_end_column         = 0
*       i_screen_end_line           = 0
*       i_html_height_top           =
*       i_html_height_end           =
*       it_alv_graphics             =
*       it_except_qinfo_lvc         =
*       ir_salv_fullscreen_adapter  =
*     IMPORTING
*       e_exit_caused_by_caller     =
*       es_exit_caused_by_user      =
      TABLES
        t_outtab                    = <fields>
      EXCEPTIONS
        program_error               = 1
        OTHERS                      = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE i499(sy) WITH 'Playing field could not be displayed.'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_simulation IMPLEMENTATION.

  METHOD constructor.
    DATA: lv_count_attendees TYPE i,
          lr_random_int      TYPE REF TO cl_abap_random_int.

    mv_pos_max_x = iv_max_x.
    mv_pos_max_y = iv_max_y.

    mv_treshold_green  = iv_treshold_green.
    mv_treshold_orange = iv_treshold_orange.
    mv_treshold_red    = iv_treshold_red.

    CREATE OBJECT mr_protocol.

    mv_count_attendees = iv_max_x * iv_max_y. " attendee with innovation included

    lr_random_int = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max = mv_pos_max_x ).
    mv_start_pos_x = lr_random_int->get_next( ).

    lr_random_int = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max = mv_pos_max_y ).
    mv_start_pos_y = lr_random_int->get_next( ).

    TRY.
        CREATE OBJECT mr_attendees
          EXPORTING
            ir_simulation = me.

        CREATE OBJECT mr_timer.

        SET HANDLER handle_timer_finished FOR mr_timer.

        mr_timer->interval = 1.
        mr_timer->run( ).

        CREATE OBJECT mr_playing_field
          EXPORTING
            ir_simulation = me
            ir_attendees  = mr_attendees.
      CATCH lcx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD check_innovation_adoption.
    DATA: lr_random_int     TYPE REF TO cl_abap_random_int,
          lv_attendee_comm  TYPE i,
          lv_neighbour_comm TYPE i,
          lv_open_minded    TYPE i,
          lv_text           TYPE sdydo_text_element,
          lv_number         TYPE numc3,
          ls_neighbourhood  LIKE LINE OF it_neighbourhood.

    FIELD-SYMBOLS <neighbourhood> LIKE LINE OF it_neighbourhood.

    lr_random_int = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max = 100 ).

    " consider only neighbours who already adopted the innovation
    LOOP AT it_neighbourhood INTO ls_neighbourhood WHERE adopted = abap_true.
      " check if attendee will communicate with neighbour
      lv_attendee_comm = lr_random_int->get_next( ).
      IF lv_attendee_comm > is_attendee-comm.
        CONTINUE.
      ENDIF.

      " check if neighbour will communicate with attendee
      lv_neighbour_comm = lr_random_int->get_next( ).
      IF lv_neighbour_comm > ls_neighbourhood-comm.
        CONTINUE.
      ENDIF.

      " check if innovation is adopted by attendee
      lv_open_minded  = lr_random_int->get_next( ).
      IF lv_open_minded <= is_attendee-open.
        ev_adopted = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF ev_adopted = abap_true.
      lv_text = 'Attendee &1 adopted the innovation from &2 (COM attendee: &3/&4, COM neighbour: &5/&6, OP: &7/&8).'.
      REPLACE '&1' IN lv_text WITH is_attendee-number.
      REPLACE '&2' IN lv_text WITH ls_neighbourhood-number.

      lv_number = lv_attendee_comm.
      SHIFT lv_number LEFT DELETING LEADING '0'.
      REPLACE '&3' IN lv_text WITH lv_number.
      lv_number = is_attendee-comm.
      SHIFT lv_number LEFT DELETING LEADING '0'.
      REPLACE '&4' IN lv_text WITH lv_number.

      lv_number = lv_neighbour_comm.
      SHIFT lv_number LEFT DELETING LEADING '0'.
      REPLACE '&5' IN lv_text WITH lv_number.
      lv_number = ls_neighbourhood-comm.
      SHIFT lv_number LEFT DELETING LEADING '0'.
      REPLACE '&6' IN lv_text WITH lv_number.

      lv_number = lv_open_minded.
      SHIFT lv_number LEFT DELETING LEADING '0'.
      REPLACE '&7' IN lv_text WITH lv_number.
      lv_number = is_attendee-open.
      SHIFT lv_number LEFT DELETING LEADING '0'.
      REPLACE '&8' IN lv_text WITH lv_number.

      mr_protocol->add_entry( iv_generation = mv_count_generation iv_text = lv_text ).
    ENDIF.
  ENDMETHOD.


  METHOD create_new_generation.
    DATA: lv_pos_x     TYPE i,
          lv_pos_y     TYPE i,
          lt_attendees TYPE attendees,
          ls_attendee  TYPE attendee,
          lt_adopted   TYPE attendees.

    FIELD-SYMBOLS <adopted> TYPE attendee.

    mv_count_generation = mv_count_generation + 1.

    DO mv_pos_max_y TIMES. " every row
      lv_pos_y = sy-index.

      DO mv_pos_max_x TIMES. " every column
        lv_pos_x = sy-index.

        CLEAR: lt_attendees,
               ls_attendee.

        mr_attendees->get_attendee(
          EXPORTING
            iv_pos_x    = lv_pos_x
            iv_pos_y    = lv_pos_y
          IMPORTING
            es_attendee = ls_attendee ).

        IF ls_attendee-adopted = abap_true.
          CONTINUE.
        ENDIF.

        mr_attendees->get_neighbourhood(
          EXPORTING
            iv_pos_x = lv_pos_x
            iv_pos_y = lv_pos_y
          IMPORTING
            et_attendees = lt_attendees ).

        me->check_innovation_adoption(
          EXPORTING
            is_attendee      = ls_attendee
            it_neighbourhood = lt_attendees
          IMPORTING
            ev_adopted       = ls_attendee-adopted ).

        IF ls_attendee-adopted = abap_true.
          APPEND ls_attendee TO lt_adopted.
          mv_count_adoption = mv_count_adoption + 1.
        ENDIF.
      ENDDO.
    ENDDO.

    LOOP AT lt_adopted ASSIGNING <adopted>.
      mr_attendees->set_attendee_adopted(
        EXPORTING
          iv_pos_x      = <adopted>-pos_x
          iv_pos_y      = <adopted>-pos_y
          iv_generation = mv_count_generation ).
    ENDLOOP.

    mv_count_left = mv_count_attendees - ( mv_count_adoption + 1 ).

  ENDMETHOD.


  METHOD handle_timer_finished.
    TRY.
        me->create_new_generation( ).
        mr_playing_field->refresh( ).
      CATCH lcx_error.
        RETURN.
    ENDTRY.

    IF mv_count_left <> 0.
      mr_timer->run( ).
    ELSE.
      MESSAGE 'Simulation finished.' TYPE 'I'.
    ENDIF.
  ENDMETHOD.


  METHOD handle_context_menu_request.
    CALL METHOD e_object->add_separator.

    CALL METHOD e_object->add_function
      EXPORTING
        fcode = '&PAUSE'
        text  = 'Pause simulation'
        icon  = icon_system_copy.
*       ftype             =
*       disabled          =
*       hidden            =
*       checked           =
*       accelerator       =
*       insert_at_the_top = SPACE

    CALL METHOD e_object->add_function
      EXPORTING
        fcode = '&CONTINUE'
        text  = 'Continue simulation'
        icon  = icon_system_copy.
*       ftype             =
*       disabled          =
*       hidden            =
*       checked           =
*       accelerator       =
*       insert_at_the_top = SPACE
  ENDMETHOD.


  METHOD set_playfield_reference.
    mr_playing_field->set_reference( iv_reference ).
  ENDMETHOD.


  METHOD start_simulation.
    TRY.
        mr_playing_field->display( ).
      CATCH lcx_error.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD reset_timer.
    mr_timer->cancel( ).
  ENDMETHOD.


  METHOD restart_timer.
    mr_timer->cancel( ).
    mr_timer->run( ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************

INITIALIZATION.
  PERFORM initialize.

START-OF-SELECTION.
  PERFORM start_simulation.

**********************************************************************

FORM start_simulation.

  DATA: lr_error   TYPE REF TO lcx_error,
        lv_message TYPE string.

  TRY.
      CREATE OBJECT gr_simulation
        EXPORTING
          iv_max_x           = pa_max_x
          iv_max_y           = pa_max_y
          iv_treshold_green  = pa_thgrn
          iv_treshold_orange = pa_thorg
          iv_treshold_red    = pa_thred.
    CATCH lcx_error INTO lr_error.
      lr_error->get_text( ).
  ENDTRY.

  IF gr_simulation IS BOUND.
    gr_simulation->start_simulation( ).
  ENDIF.

ENDFORM.


FORM alv_caller_exit USING us_data TYPE slis_data_caller_exit.

  DATA lr_grid TYPE REF TO cl_gui_alv_grid.

  sy-title = 'Simulation "distribution of an innovation" based on cellular automaton'.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     ir_salv_fullscreen_adapter =
    IMPORTING
*     et_excluding               =
*     e_repid                    =
*     e_callback_program         =
*     e_callback_routine         =
      e_grid = lr_grid.
*     et_fieldcat_lvc            =
*     er_trace                   =
*     e_flg_no_html              =
*     es_layout_kkblo            =
*     es_sel_hide                =
*     et_event_exit              =
*     er_form_tol                =
*     er_form_eol                =

  IF lr_grid IS NOT BOUND.
    RETURN.
  ENDIF.

  gr_simulation->set_playfield_reference( lr_grid ).

  SET HANDLER gr_simulation->handle_context_menu_request FOR lr_grid.

ENDFORM.


FORM alv_user_command USING uv_ucomm    TYPE syucomm
                            us_selfield TYPE slis_selfield.

  IF uv_ucomm = '&IC1' OR uv_ucomm = '&CONTINUE'.
    IF uv_ucomm = '&CONTINUE'.
      MESSAGE 'Simulation continued.' TYPE 'S'.
    ENDIF.

    " we need to restart our timer
    IF gr_simulation->mv_count_left > 0.
      gr_simulation->restart_timer( ).
    ENDIF.
  ELSE.
    " timer is automatically deactivated
    MESSAGE 'Simulation paused.' TYPE 'S'.
    gr_simulation->reset_timer( ).
  ENDIF.

ENDFORM.


FORM alv_top_of_page USING ur_doc TYPE REF TO cl_dd_document.

  DATA: lv_count_generation TYPE numc5,
        lv_count_attendees  TYPE numc5,
        lv_count_adoption   TYPE numc5,
        lv_count_left       TYPE numc5,
        lv_text             TYPE sdydo_text_element.

  lv_count_attendees = gr_simulation->mv_count_attendees.
  SHIFT lv_count_attendees LEFT DELETING LEADING '0'.
  CONCATENATE 'Number of attendees: ' lv_count_attendees INTO lv_text SEPARATED BY space.

  CALL METHOD ur_doc->add_text
    EXPORTING
      text         = lv_text
*     text_table   =
*     fix_lines    =
*     sap_style    =
*     sap_color    =
      sap_fontsize = cl_dd_area=>medium
*     sap_fontstyle =
      sap_emphasis = cl_dd_area=>strong.
*     style_class   =
*     a11y_tooltip  =
*   CHANGING
*     document  =

  CALL METHOD ur_doc->new_line.
*   EXPORTING
*     repeat =

  CLEAR lv_text.
  lv_count_adoption = gr_simulation->mv_count_adoption.
  SHIFT lv_count_adoption LEFT DELETING LEADING '0'.
  IF lv_count_adoption = space.
    lv_text = 'Number of adoption: 0'.
  ELSE.
    CONCATENATE 'Number of adoption: ' lv_count_adoption INTO lv_text SEPARATED BY space.
  ENDIF.

  CALL METHOD ur_doc->add_text
    EXPORTING
      text         = lv_text
*     text_table   =
*     fix_lines    =
*     sap_style    =
*     sap_color    =
      sap_fontsize = cl_dd_area=>medium
*     sap_fontstyle =
      sap_emphasis = cl_dd_area=>strong.
*     style_class   =
*     a11y_tooltip  =
*   CHANGING
*     document  =

  CALL METHOD ur_doc->new_line.
*   EXPORTING
*     repeat =

  CLEAR lv_text.
  lv_count_left = gr_simulation->mv_count_left.
  SHIFT lv_count_left LEFT DELETING LEADING '0'.
  IF lv_count_left = space.
    lv_text = 'Left: 0'.
  ELSE.
    CONCATENATE 'Left: ' lv_count_left INTO lv_text SEPARATED BY space.
  ENDIF.

  CALL METHOD ur_doc->add_text
    EXPORTING
      text         = lv_text
*     text_table   =
*     fix_lines    =
*     sap_style    =
*     sap_color    =
      sap_fontsize = cl_dd_area=>medium
*     sap_fontstyle =
      sap_emphasis = cl_dd_area=>strong.
*     style_class   =
*     a11y_tooltip  =
*   CHANGING
*     document  =

  CALL METHOD ur_doc->new_line.
*   EXPORTING
*     repeat =

  CLEAR lv_text.
  lv_count_generation = gr_simulation->mv_count_generation.
  SHIFT lv_count_generation LEFT DELETING LEADING '0'.
  IF lv_count_generation = space.
    lv_text = 'Generation: 0'.
  ELSE.
    CONCATENATE 'Generation: ' lv_count_generation INTO lv_text SEPARATED BY space.
  ENDIF.

  CALL METHOD ur_doc->add_text
    EXPORTING
      text         = lv_text
*     text_table   =
*     fix_lines    =
*     sap_style    =
*     sap_color    =
      sap_fontsize = cl_dd_area=>medium
*     sap_fontstyle =
      sap_emphasis = cl_dd_area=>strong.
*     style_class   =
*     a11y_tooltip  =
*   CHANGING
*     document  =

ENDFORM.


FORM alv_end_of_list USING ur_doc TYPE REF TO cl_dd_document.

  DATA: lv_generation       TYPE i,
        lv_number           TYPE numc5,
        lt_protocol_entries TYPE protocol_entries,
        ls_protocol_entry   LIKE LINE OF lt_protocol_entries,
        lv_text             TYPE sdydo_text_element.

  " protocol per generation below playing field

  lv_generation = gr_simulation->mv_count_generation.
  IF lv_generation = 0.
    RETURN.
  ENDIF.

  gr_simulation->mr_protocol->get_entries_per_generation(
    EXPORTING
      iv_generation = lv_generation
    IMPORTING
      et_entries = lt_protocol_entries ).

  lv_number = lv_generation.
  SHIFT lv_number LEFT DELETING LEADING '0'.
  lv_text = 'Generation &1 reached.'.
  REPLACE '&1' IN lv_text WITH lv_number.

  CALL METHOD ur_doc->add_text
    EXPORTING
      text         = lv_text
*     text_table   =
*     fix_lines    =
*     sap_style    =
*     sap_color    =
*     sap_fontsize =
*     sap_fontstyle =
      sap_emphasis = cl_dd_area=>strong.
*     style_class   =
*     a11y_tooltip  =
*    CHANGING
*     document  =

  CALL METHOD ur_doc->new_line.
*   EXPORTING
*     repeat =

  LOOP AT lt_protocol_entries INTO ls_protocol_entry.
    CALL METHOD ur_doc->add_text
      EXPORTING
        text         = ls_protocol_entry-text
*       text_table   =
*       fix_lines    =
*       sap_style    =
*       sap_color    =
        sap_fontsize = cl_dd_area=>medium.
*       sap_fontstyle =
*       sap_emphasis  =
*       style_class   =
*       a11y_tooltip  =
*     CHANGING
*       document  =

    CALL METHOD ur_doc->new_line.
*      EXPORTING
*        repeat =
  ENDLOOP.

ENDFORM.


FORM initialize.

  sy-title = 'Simulation "distribution of an innovation" based on cellular automaton'.

  title_01 = 'Description'.
  title_02 = 'Parameters'.

  text_000 = 'The report demonstrate the use of cellular automaton to show how an innovation'.
  text_001 = 'is adopted by every member of a crowd.'.
  text_002 = 'Playfield max. X-Axis'.
  text_003 = 'Playfield max. Y-Axis'.
  text_004 = 'Treshold green'.
  text_005 = 'Treshold orange'.
  text_006 = 'Treshold red'.

ENDFORM.
