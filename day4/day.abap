TYPES bingo_cell_t TYPE i.
TYPES bingo_row_t TYPE TABLE OF bingo_cell_t WITH DEFAULT KEY.

TYPES:
  BEGIN OF bingo_field_t,
    rows     TYPE TABLE OF bingo_row_t WITH DEFAULT KEY,
    finished TYPE sap_bool,
  END OF bingo_field_t,
  bingo_field_list_t TYPE TABLE OF bingo_field_t WITH DEFAULT KEY,

  BEGIN OF bingo_line_t,
    unmarked_cell_count TYPE i,
    field               TYPE REF TO bingo_field_t,
  END OF bingo_line_t,
  BEGIN OF bingo_line_entry_t,
    cell TYPE REF TO bingo_cell_t,
    line TYPE REF TO bingo_line_t,
  END OF bingo_line_entry_t,

  BEGIN OF bingo_number_index_line_t,
    value   TYPE i,
    entries TYPE TABLE OF REF TO bingo_line_entry_t WITH DEFAULT KEY,
  END OF bingo_number_index_line_t,
  bingo_number_index_t TYPE TABLE OF bingo_number_index_line_t WITH UNIQUE SORTED KEY value COMPONENTS value.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/4` CHANGING input.

  SPLIT input AT |\n\n| INTO TABLE DATA(bingo_fields_str).
  SPLIT bingo_fields_str[ 1 ] AT `,` INTO TABLE DATA(bingo_numbers_str).
  DATA(bingo_numbers) = VALUE int4_table( FOR number_str IN bingo_numbers_str ( CONV #( number_str ) ) ).
  DELETE bingo_fields_str INDEX 1.

  PERFORM part1.
  PERFORM part2.


FORM load_input USING path TYPE string
             CHANGING input TYPE string.

  DATA input_bin TYPE xstring.
  OPEN DATASET path FOR INPUT IN BINARY MODE.
  READ DATASET path INTO input_bin.
  CLOSE DATASET path.

  DATA(line_break) = CONV xstring( '0A' ).
  DATA(zero) = CONV xstring( '00' ).
  SHIFT input_bin RIGHT DELETING TRAILING line_break IN BYTE MODE.
  SHIFT input_bin LEFT DELETING LEADING zero IN BYTE MODE.

  input = cl_http_utility=>if_http_utility~decode_utf8( input_bin ).

ENDFORM.

FORM read_bingo_fields USING fields_str TYPE string_table
                   CHANGING fields TYPE bingo_field_list_t
                            index TYPE bingo_number_index_t.

  LOOP AT bingo_fields_str INTO DATA(bingo_field_str).

    INSERT INITIAL LINE INTO TABLE fields REFERENCE INTO DATA(bingo_field).
    PERFORM read_bingo_field USING bingo_field_str CHANGING bingo_field->*.
    PERFORM create_bingo_number_index USING bingo_field->* CHANGING index.

  ENDLOOP.

ENDFORM.

FORM read_bingo_field USING field_str TYPE string
                   CHANGING field TYPE bingo_field_t.

  SPLIT field_str AT |\n| INTO TABLE DATA(field_rows_str).


  LOOP AT field_rows_str INTO DATA(field_row_str).

    SPLIT field_row_str AT ` ` INTO TABLE DATA(cells_str).
    DELETE cells_str WHERE table_line IS INITIAL.

    DATA(bingo_row) = VALUE bingo_row_t( FOR cell_str IN cells_str ( CONV bingo_cell_t( cell_str ) ) ).
    INSERT bingo_row INTO TABLE field-rows.

  ENDLOOP.

ENDFORM.
FORM create_bingo_number_index USING bingo_field TYPE bingo_field_t
                            CHANGING index TYPE bingo_number_index_t.

  DATA(row_count) = lines( bingo_field-rows ).
  DATA(column_count) = lines( bingo_field-rows[ 1 ] ).

  DO row_count TIMES.
    DATA(row_id) = sy-index.

    DATA(row_line) = NEW bingo_line_t( unmarked_cell_count = column_count field = REF #( bingo_field ) ).

    DO column_count TIMES.
      DATA(col_id) = sy-index.

      DATA(line_entry) = NEW bingo_line_entry_t( cell = REF #( bingo_field-rows[ row_id ][ col_id ] )
                                                 line = row_line ).
      PERFORM add_bingo_index_entry USING line_entry CHANGING index.

    ENDDO.

  ENDDO.

  DO column_count TIMES.
    col_id = sy-index.

    DATA(col_line) = NEW bingo_line_t( unmarked_cell_count = row_count field = REF #( bingo_field ) ).

    DO row_count TIMES.
      row_id = sy-index.

      line_entry = NEW bingo_line_entry_t( cell = REF #( bingo_field-rows[ row_id ][ col_id ] )
                                                     line = col_line ).
      PERFORM add_bingo_index_entry USING line_entry CHANGING index.

    ENDDO.

  ENDDO.

ENDFORM.
FORM add_bingo_index_entry USING line_entry TYPE REF TO bingo_line_entry_t
                        CHANGING index TYPE bingo_number_index_t.

  DATA(index_line) = REF #( index[ KEY value value = line_entry->cell->* ] OPTIONAL ).
  IF index_line IS NOT BOUND.
    INSERT VALUE #( value = line_entry->cell->* ) INTO TABLE index REFERENCE INTO index_line.
  ENDIF.

  INSERT line_entry INTO TABLE index_line->entries.

ENDFORM.
FORM get_bingo_field_sum USING field TYPE bingo_field_t
                      CHANGING field_sum TYPE i.

  LOOP AT field-rows REFERENCE INTO DATA(row).
    LOOP AT row->* REFERENCE INTO DATA(cell).
      field_sum = field_sum + cell->*.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

FORM part1.

  DATA bingo_index TYPE bingo_number_index_t.
  DATA bingo_fields TYPE TABLE OF bingo_field_t.
  PERFORM read_bingo_fields USING bingo_fields_str CHANGING bingo_fields bingo_index.


  DATA winning_field TYPE REF TO bingo_field_t.
  LOOP AT bingo_numbers INTO DATA(bingo_number).

    DATA(index_entry) = REF #( bingo_index[ KEY value value = bingo_number ] OPTIONAL ).
    IF index_entry IS NOT BOUND.
      CONTINUE.
    ENDIF.

    LOOP AT index_entry->entries INTO DATA(line_entry).

      line_entry->line->unmarked_cell_count = line_entry->line->unmarked_cell_count - 1.
      line_entry->cell->* = 0.

      IF line_entry->line->unmarked_cell_count = 0.
        winning_field = line_entry->line->field.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF winning_field IS BOUND.
      EXIT.
    ENDIF.

  ENDLOOP.


  IF winning_field IS NOT BOUND.
    WRITE / 'Error'.
  ENDIF.

  DATA board_sum TYPE i.
  PERFORM get_bingo_field_sum USING winning_field->* CHANGING board_sum.

  DATA(answer) = board_sum * bingo_number.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA bingo_index TYPE bingo_number_index_t.
  DATA bingo_fields TYPE TABLE OF bingo_field_t.
  PERFORM read_bingo_fields USING bingo_fields_str CHANGING bingo_fields bingo_index.


  DATA last_winning_field TYPE REF TO bingo_field_t.
  DATA(fields_left_count) = lines( bingo_fields ).
  LOOP AT bingo_numbers INTO DATA(bingo_number).

    DATA(index_entry) = REF #( bingo_index[ KEY value value = bingo_number ] OPTIONAL ).
    IF index_entry IS NOT BOUND.
      CONTINUE.
    ENDIF.

    LOOP AT index_entry->entries INTO DATA(line_entry).

      line_entry->line->unmarked_cell_count = line_entry->line->unmarked_cell_count - 1.
      line_entry->cell->* = 0.

      IF line_entry->line->unmarked_cell_count = 0 AND line_entry->line->field->finished = abap_false.
        line_entry->line->field->finished = abap_true.
        fields_left_count = fields_left_count - 1.
        last_winning_field = line_entry->line->field.
      ENDIF.

    ENDLOOP.

    IF fields_left_count = 0.
      EXIT.
    ENDIF.

  ENDLOOP.


  IF last_winning_field IS NOT BOUND.
    WRITE / 'Error'.
  ENDIF.

  DATA board_sum TYPE i.
  PERFORM get_bingo_field_sum USING last_winning_field->* CHANGING board_sum.

  DATA(answer) = board_sum * bingo_number.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.
