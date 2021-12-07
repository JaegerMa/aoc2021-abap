TYPES:
  BEGIN OF point_pair_t,
    x1 TYPE i,
    y1 TYPE i,
    x2 TYPE i,
    y2 TYPE i,
  END OF point_pair_t.


CLASS grid DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t          TYPE i,
      value_ref_t      TYPE REF TO value_t,
      value_ref_list_t TYPE TABLE OF value_ref_t WITH DEFAULT KEY,

      BEGIN OF cell_t,
        col   TYPE i,
        value TYPE value_t,
      END OF cell_t,
      BEGIN OF row_t,
        row   TYPE i,
        cells TYPE SORTED TABLE OF cell_t WITH UNIQUE KEY primary_key COMPONENTS col,
      END OF row_t,

      grid_data_t TYPE SORTED TABLE OF row_t WITH UNIQUE KEY primary_key COMPONENTS row.

    DATA grid_data TYPE grid_data_t.

    METHODS:
      get
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE value_t,
      set
        IMPORTING x     TYPE i
                  y     TYPE i
                  value TYPE value_t,
      get_ref
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE REF TO value_t,
      get_all_refs
        RETURNING VALUE(ret) TYPE REF TO value_ref_list_t.

ENDCLASS.



START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/5` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).
  DATA point_pairs TYPE TABLE OF point_pair_t.
  LOOP AT lines INTO DATA(line).

    SPLIT line AT ` -> ` INTO DATA(point1_str) DATA(point2_str).
    SPLIT point1_str AT `,` INTO DATA(x1str) DATA(y1str).
    SPLIT point2_str AT `,` INTO DATA(x2str) DATA(y2str).


    DATA(point_pair) = VALUE point_pair_t( x1 = CONV i( x1str )
                                           y1 = CONV i( y1str )
                                           x2 = CONV i( x2str )
                                           y2 = CONV i( y2str ) ).
    INSERT point_pair INTO TABLE point_pairs.

  ENDLOOP.


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

FORM part1.

  DATA(grid) = NEW grid( ).

  LOOP AT point_pairs REFERENCE INTO DATA(point_pair).

    IF point_pair->x1 <> point_pair->x2 AND point_pair->y1 <> point_pair->y2.
      CONTINUE.
    ENDIF.

    PERFORM set_range USING point_pair->* grid.

  ENDLOOP.

  DATA(grid_points) = grid->get_all_refs( ).
  DELETE grid_points->* WHERE table_line->* <= 1.


  DATA(answer) = lines( grid_points->* ).
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.


  DATA(grid) = NEW grid( ).

  LOOP AT point_pairs REFERENCE INTO DATA(point_pair).
    PERFORM set_range USING point_pair->* grid.
  ENDLOOP.

  DATA(grid_points) = grid->get_all_refs( ).
  DELETE grid_points->* WHERE table_line->* <= 1.


  DATA(answer) = lines( grid_points->* ).
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.


FORM set_range USING point_pair TYPE point_pair_t
                     grid TYPE REF TO grid.

  DATA(x1) = point_pair-x1.
  DATA(y1) = point_pair-y1.
  DATA(x2) = point_pair-x2.
  DATA(y2) = point_pair-y2.

  DATA point TYPE REF TO grid=>value_t.
  DATA(xshift) = sign( x2 - x1 ).
  DATA(yshift) = sign( y2 - y1 ).

  IF x1 = x2.

    DO abs( y2 - y1 ) + 1 TIMES.

      DATA(y) = y1 + ( ( sy-index - 1 ) * yshift ).
      point = grid->get_ref( x = x1 y = y ).
      point->* = point->* + 1.

    ENDDO.

  ELSEIF y1 = y2.

    DO abs( x2 - x1 ) + 1 TIMES.

      DATA(x) = x1 + ( ( sy-index - 1 ) * xshift ).
      point = grid->get_ref( x = x y = y1 ).
      point->* = point->* + 1.

    ENDDO.

  ELSE.

    DO abs( x2 - x1 ) + 1 TIMES.

      x = x1 + ( ( sy-index - 1 ) * xshift ).
      y = y1 + ( ( sy-index - 1 ) * yshift ).
      point = grid->get_ref( x = x y = y ).
      point->* = point->* + 1.

    ENDDO.

  ENDIF.

ENDFORM.


CLASS grid IMPLEMENTATION.

  METHOD get.

    DATA(ref) = me->get_ref( x = x y = y ).
    ret = ref->*.

  ENDMETHOD.
  METHOD set.

    DATA(ref) = me->get_ref( x = x y = y ).
    ref->* = value.

  ENDMETHOD.

  METHOD get_ref.

    DATA(row) = REF #( me->grid_data[ row = y ] OPTIONAL ).
    IF row IS NOT BOUND.
      INSERT VALUE #( row = y ) INTO TABLE me->grid_data REFERENCE INTO row.
    ENDIF.

    DATA(cell) = REF #( row->cells[ col = x ] OPTIONAL ).
    IF cell IS NOT BOUND.
      INSERT VALUE #( col = x ) INTO TABLE row->cells REFERENCE INTO cell.
    ENDIF.


    ret = REF #( cell->value ).

  ENDMETHOD.

  METHOD get_all_refs.

    DATA(refs) = NEW value_ref_list_t( ).

    LOOP AT me->grid_data REFERENCE INTO DATA(row).
      LOOP AT row->cells REFERENCE INTO DATA(cell).
        INSERT REF #( cell->value ) INTO TABLE refs->*.
      ENDLOOP.
    ENDLOOP.

    ret = refs.

  ENDMETHOD.

ENDCLASS.
