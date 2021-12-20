CLASS grid DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t      TYPE i,
      value_list_t TYPE TABLE OF value_t WITH DEFAULT KEY,

      BEGIN OF point_t,
        x     TYPE i,
        y     TYPE i,
        value TYPE value_t,
        ref   TYPE REF TO value_t,
      END OF point_t,
      point_list_t TYPE TABLE OF point_t WITH DEFAULT KEY,

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
    DATA preset TYPE value_t.

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
      get_point
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE point_t,
      get_all_points
        RETURNING VALUE(ret) TYPE REF TO point_list_t.

ENDCLASS.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/20` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).
  DATA(base_image_size) = strlen( lines[ 3 ] ).

  DATA transform_map TYPE int4_table.
  PERFORM read_transform_map CHANGING transform_map.

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
  PERFORM read_grid USING grid.

  DO 2 TIMES.
    DATA(i) = sy-index - 1.
    PERFORM next_image USING i transform_map CHANGING grid.
  ENDDO.

  DATA(points) = grid->get_all_points( ).
  DATA(active_points) = REDUCE i( INIT count = 0 FOR point IN points->* WHERE ( value = 1 ) NEXT count = count + 1 ).

  DATA(answer) = active_points.
  WRITE / 'Part 1:'.
  WRITE CONV string( active_points ).

ENDFORM.
FORM part2.

  DATA(grid) = NEW grid( ).
  PERFORM read_grid USING grid.

  DO 50 TIMES.
    DATA(i) = sy-index - 1.
    PERFORM next_image USING i transform_map CHANGING grid.
  ENDDO.

  DATA(points) = grid->get_all_points( ).
  DATA(active_points) = REDUCE i( INIT count = 0 FOR point IN points->* WHERE ( value = 1 ) NEXT count = count + 1 ).

  DATA(answer) = active_points.
  WRITE / 'Part 2:'.
  WRITE CONV string( active_points ).

ENDFORM.

FORM read_transform_map CHANGING transform_map TYPE int4_table.

  DATA(transform_string) = lines[ 1 ].
  transform_map = VALUE int4_table( ).
  DO strlen( transform_string ) TIMES.
    DATA(offset) = sy-index - 1.
    INSERT COND i( WHEN transform_string+offset(1) = '#' THEN 1 ELSE 0 ) INTO TABLE transform_map.
  ENDDO.

ENDFORM.
FORM read_grid USING grid TYPE REF TO grid.

  DO lines( lines ) - 2 TIMES.
    DATA(y) = sy-index - 1.

    DATA(line) = lines[ y + 3 ].

    DO strlen( line ) TIMES.
      DATA(x) = sy-index - 1.

      grid->set( x = x y = y value = COND i( WHEN line+x(1) = '#' THEN 1 ELSE 0 ) ).

    ENDDO.

  ENDDO.

ENDFORM.

FORM next_image USING iteration TYPE i
                      transform_map TYPE int4_table
             CHANGING grid TYPE REF TO grid.

  DATA(new_grid) = NEW grid( ).

  IF transform_map[ 1 ] = 1.
    grid->preset = COND #( WHEN iteration MOD 2 = 1 THEN 1 ELSE 0 ).
  ENDIF.

  DATA(image_size) = base_image_size + iteration * 2.
  DO image_size + 2 TIMES.
    "-1 because sy-index is 1-based
    "-iteration because each iteration the image gets bigger by 1 in each direction
    "-1 because for each iteration we have to process the next line outside of the
    "  current image and iteration is 0-based
    DATA(y) = sy-index - 1 - iteration - 1.

    DO image_size + 2 TIMES.
      DATA(x) = sy-index - 1 - iteration - 1.

      DATA(transform_key) = grid->get( x = x - 1 y = y - 1 ) * 256
          + grid->get( x = x y = y - 1 ) * 128
          + grid->get( x = x + 1 y = y - 1 ) * 64
          + grid->get( x = x - 1 y = y ) * 32
          + grid->get( x = x y = y ) * 16
          + grid->get( x = x + 1 y = y ) * 8
          + grid->get( x = x - 1 y = y + 1 ) * 4
          + grid->get( x = x y = y + 1 ) * 2
          + grid->get( x = x + 1 y = y + 1 ) * 1.

      "Tables are still 1-based, therefore it's transform_key + 1
      new_grid->set( x = x y = y value = transform_map[ transform_key + 1 ] ).

    ENDDO.

  ENDDO.

  grid = new_grid.

ENDFORM.



CLASS grid IMPLEMENTATION.

  METHOD get.

    DATA(row) = REF #( me->grid_data[ row = y ] OPTIONAL ).
    IF row IS NOT BOUND.
      ret = me->preset.
      RETURN.
    ENDIF.

    DATA(cell) = REF #( row->cells[ col = x ] OPTIONAL ).
    IF cell IS NOT BOUND.
      ret = me->preset.
      RETURN.
    ENDIF.


    ret = cell->value.

  ENDMETHOD.
  METHOD get_point.

    DATA(value_ref) = me->get_ref( x = x y = y ).
    ret = VALUE #( x = x
                   y = y
                   value = value_ref->*
                   ref = value_ref ).


  ENDMETHOD.
  METHOD get_all_points.

    DATA(points) = NEW point_list_t( ).

    LOOP AT me->grid_data REFERENCE INTO DATA(row).
      LOOP AT row->cells REFERENCE INTO DATA(cell).
        INSERT VALUE #( x = cell->col y = row->row value = cell->value ref = REF #( cell->value ) ) INTO TABLE points->*.
      ENDLOOP.
    ENDLOOP.

    ret = points.

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

ENDCLASS.
