TYPES:
  BEGIN OF basin_t,
    x      TYPE i,
    y      TYPE i,
    height TYPE i,
    size   TYPE i,
  END OF basin_t,
  basin_list_t TYPE TABLE OF basin_t WITH DEFAULT KEY,
  basin_ref_t  TYPE REF TO basin_t.


CLASS grid DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t      TYPE REF TO data,
      value_list_t TYPE TABLE OF value_t WITH DEFAULT KEY,

      BEGIN OF point_t,
        x     TYPE i,
        y     TYPE i,
        value TYPE value_t,
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

    METHODS:
      get
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE value_t,
      get_point
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE point_t,
      get_ref
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE REF TO value_t,
      get_all
        RETURNING VALUE(ret) TYPE REF TO value_list_t,

      set
        IMPORTING x     TYPE i
                  y     TYPE i
                  value TYPE value_t.

ENDCLASS.



START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/9` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

  DATA(grid) = NEW grid( ).
  DATA(height) = lines( lines ).
  DATA(width) = strlen( lines[ 1 ] ).
  LOOP AT lines INTO DATA(line).
    DATA(y) = sy-tabix - 1.

    DO strlen( line ) TIMES.
      DATA(x) = sy-index - 1.
      grid->set( x = x y = y value = NEW i( CONV i( line+x(1) ) ) ).
    ENDDO.

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

  DATA basin_grid TYPE REF TO grid.
  PERFORM create_basin_grid USING grid CHANGING basin_grid.


  DATA(risk_level) = 0.
  DATA(basin_refs) = basin_grid->get_all( ).

  FIELD-SYMBOLS <basin> TYPE basin_t.
  LOOP AT basin_refs->* INTO DATA(basin_ref).
    ASSIGN basin_ref->* TO <basin>.
    risk_level = risk_level + <basin>-height + 1.
  ENDLOOP.


  DATA(answer) = risk_level.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  FIELD-SYMBOLS <i> TYPE i.
  FIELD-SYMBOLS <basin> TYPE basin_t.


  DATA basin_grid TYPE REF TO grid.
  PERFORM create_basin_grid USING grid CHANGING basin_grid.


  DO height TIMES.
    y = sy-index - 1.

    DO width TIMES.
      x = sy-index - 1.

      DATA(point) = grid->get_point( x = x y = y ).
      DATA(way) = VALUE grid=>point_list_t( ( point ) ).
      DO.

        ASSIGN point-value->* TO <i>.
        DATA(value) = <i>.

        IF value = 9.
          EXIT.
        ENDIF.

        DATA(basin) = basin_grid->get( x = point-x y = point-y ).
        IF basin IS BOUND.

          ASSIGN basin->* TO <basin>.
          <basin>-size = <basin>-size + 1.

          LOOP AT way INTO DATA(step).
            basin_grid->set( x = step-x y = step-y value = basin ).
          ENDLOOP.

          EXIT.

        ENDIF.


        DATA(neighbours) = VALUE grid=>point_list_t(
            ( grid->get_point( x = point-x y = point-y - 1 ) )
            ( grid->get_point( x = point-x + 1 y = point-y ) )
            ( grid->get_point( x = point-x y = point-y + 1 ) )
            ( grid->get_point( x = point-x - 1 y = point-y ) )
        ).

        LOOP AT neighbours INTO DATA(neighbour) WHERE value IS BOUND.

          ASSIGN neighbour-value->* TO <i>.
          IF <i> < value.
            point = neighbour.
            INSERT point INTO TABLE way.
            EXIT.
          ENDIF.

        ENDLOOP.

      ENDDO.

    ENDDO.

  ENDDO.


  DATA(basin_refs) = basin_grid->get_all( ).
  DELETE basin_refs->* WHERE table_line IS NOT BOUND.

  DATA basins TYPE basin_list_t.
  LOOP AT basin_refs->* INTO DATA(basin_ref).
    ASSIGN basin_ref->* TO <basin>.
    INSERT <basin> INTO TABLE basins.
  ENDLOOP.

  SORT basins BY size DESCENDING x y.
  DELETE ADJACENT DUPLICATES FROM basins COMPARING x y.


  DATA(answer) = basins[ 1 ]-size * basins[ 2 ]-size * basins[ 3 ]-size.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.


FORM create_basin_grid USING grid TYPE REF TO grid
                CHANGING basin_grid TYPE REF TO grid.

  FIELD-SYMBOLS <i> TYPE i.


  basin_grid = NEW grid( ).

  DO height TIMES.
    DATA(y) = sy-index - 1.

    DO width TIMES.
      DATA(x) = sy-index - 1.


      DATA(point) = grid->get_point( x = x y = y ).

      ASSIGN point-value->* TO <i>.
      DATA(value) = <i>.
      IF value = 9.
        CONTINUE.
      ENDIF.

      DATA(neighbours) = VALUE grid=>point_list_t(
          ( grid->get_point( x = x y = y - 1 ) )
          ( grid->get_point( x = x + 1 y = y ) )
          ( grid->get_point( x = x y = y + 1 ) )
          ( grid->get_point( x = x - 1 y = y ) )
      ).

      DATA(is_basin) = abap_true.
      LOOP AT neighbours INTO DATA(neighbour) WHERE value IS BOUND.

        ASSIGN neighbour-value->* TO <i>.
        IF <i> <= value.
          is_basin = abap_false.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF is_basin = abap_true.
        basin_grid->set( x = x y = y value = NEW basin_t( x = x y = y height = value size = 0 ) ).
      ENDIF.

    ENDDO.


  ENDDO.

ENDFORM.



CLASS grid IMPLEMENTATION.

  METHOD get.

    DATA(ref) = me->get_ref( x = x y = y ).
    ret = ref->*.

  ENDMETHOD.
  METHOD get_point.

    ret = VALUE #( x = x
                   y = y
                   value = me->get( x = x y = y ) ).

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

  METHOD get_all.

    DATA(refs) = NEW value_list_t( ).

    LOOP AT me->grid_data REFERENCE INTO DATA(row).
      LOOP AT row->cells REFERENCE INTO DATA(cell).
        INSERT cell->value INTO TABLE refs->*.
      ENDLOOP.
    ENDLOOP.

    ret = refs.

  ENDMETHOD.

ENDCLASS.
