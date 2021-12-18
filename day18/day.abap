CLASS number DEFINITION.

  PUBLIC SECTION.

    DATA value TYPE i.
    DATA array_path TYPE int4_table.

    METHODS:
      constructor
        IMPORTING value      TYPE i
                  array_path TYPE int4_table,
      get_array_id
        RETURNING VALUE(ret) TYPE i,
      get_depth
        RETURNING VALUE(ret) TYPE i.

ENDCLASS.
CLASS linked_list_node DEFINITION DEFERRED.
CLASS linked_list DEFINITION.

  PUBLIC SECTION.

    TYPES value_t TYPE REF TO number.

    DATA first_node TYPE REF TO linked_list_node.
    DATA last_node TYPE REF TO linked_list_node.
    DATA size TYPE i.

    METHODS:
      push
        IMPORTING value      TYPE value_t
        RETURNING VALUE(ret) TYPE REF TO linked_list_node,
      append_to_node
        IMPORTING value      TYPE value_t
                  node       TYPE REF TO linked_list_node
        RETURNING VALUE(ret) TYPE REF TO linked_list_node,
      prepend_to_node
        IMPORTING value      TYPE value_t
                  node       TYPE REF TO linked_list_node
        RETURNING VALUE(ret) TYPE REF TO linked_list_node,
      remove_node
        IMPORTING node       TYPE REF TO linked_list_node
        RETURNING VALUE(ret) TYPE REF TO linked_list_node.

ENDCLASS.
CLASS linked_list_node DEFINITION.

  PUBLIC SECTION.
    DATA list TYPE REF TO linked_list.
    DATA next_node TYPE REF TO linked_list_node.
    DATA previous_node TYPE REF TO linked_list_node.
    DATA value TYPE linked_list=>value_t.

    METHODS:
      constructor
        IMPORTING value TYPE linked_list=>value_t
                  list  TYPE REF TO linked_list,
      append
        IMPORTING value      TYPE linked_list=>value_t
        RETURNING VALUE(ret) TYPE REF TO linked_list_node,
      prepend
        IMPORTING value      TYPE linked_list=>value_t
        RETURNING VALUE(ret) TYPE REF TO linked_list_node,
      remove
        RETURNING VALUE(ret) TYPE REF TO linked_list_node.


ENDCLASS.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/18` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).
  DATA array_counter TYPE i.

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

  DATA list TYPE REF TO linked_list.
  DATA(first_line) = lines[ 1 ].
  PERFORM read_line USING first_line CHANGING list.

  LOOP AT lines INTO DATA(line) FROM 2.

    DATA new_list TYPE REF TO linked_list.
    PERFORM read_line USING line CHANGING new_list.

    PERFORM merge_lists USING list new_list.
    PERFORM reduce_list USING list.

  ENDLOOP.

  DATA magnitude TYPE i.
  PERFORM get_list_magnitude USING list CHANGING magnitude.

  DATA(answer) = magnitude.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA(max_magnitude) = 0.

  LOOP AT lines INTO DATA(line1).
    DATA(idx) = sy-tabix.
    LOOP AT lines INTO DATA(line2) FROM idx.

      DATA list1 TYPE REF TO linked_list.
      DATA list2 TYPE REF TO linked_list.
      DATA magnitude TYPE i.

      PERFORM read_line USING line1 CHANGING list1.
      PERFORM read_line USING line2 CHANGING list2.
      PERFORM merge_lists USING list1 list2.
      PERFORM reduce_list USING list1.
      PERFORM get_list_magnitude USING list1 CHANGING magnitude.
      max_magnitude = nmax( val1 = max_magnitude val2 = magnitude ).

      PERFORM read_line USING line1 CHANGING list1.
      PERFORM read_line USING line2 CHANGING list2.
      PERFORM merge_lists USING list2 list1.
      PERFORM reduce_list USING list2.
      PERFORM get_list_magnitude USING list2 CHANGING magnitude.
      max_magnitude = nmax( val1 = max_magnitude val2 = magnitude ).

    ENDLOOP.

  ENDLOOP.

  DATA(answer) = max_magnitude.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.


FORM merge_lists USING list TYPE REF TO linked_list
                    new_list TYPE REF TO linked_list.

  array_counter = array_counter + 1.
  DATA(sourrounding_array_id) = array_counter.

  DATA(node) = list->first_node.
  WHILE node IS BOUND.
    INSERT sourrounding_array_id INTO TABLE node->value->array_path.
    node = node->next_node.
  ENDWHILE.

  node = new_list->first_node.
  WHILE node IS BOUND.
    DATA(new_node) = list->push( node->value ).
    INSERT sourrounding_array_id INTO TABLE new_node->value->array_path.
    node = node->next_node.
  ENDWHILE.

ENDFORM.
FORM reduce_list USING list TYPE REF TO linked_list.

  DO.

    DATA(node) = list->first_node.
    WHILE node IS BOUND AND node->next_node IS BOUND.

      DATA(number) = node->value.
      IF number->get_depth( ) = 5.

        DATA(node2) = node->next_node.
        DATA(number2) = node2->value.

        IF node->previous_node IS BOUND.
          DATA(previous_number) = node->previous_node->value.
          previous_number->value = previous_number->value + number->value.
        ENDIF.
        IF node2->next_node IS BOUND.
          DATA(next_number) = node2->next_node->value.
          next_number->value = next_number->value + number2->value.
        ENDIF.


        DATA(new_number) = NEW number( value = 0 array_path = number->array_path ).
        DELETE new_number->array_path INDEX 1.
        node->prepend( new_number ).

        node = node->remove( )->remove( ).

      ELSE.

        node = node->next_node.

      ENDIF.

    ENDWHILE.


    DATA(number_split) = abap_false.

    node = list->first_node.
    WHILE node IS BOUND.

      number = node->value.
      IF number->value >=  10.

        array_counter = array_counter + 1.
        DATA(split_array_id) = array_counter.
        DATA(split_array_path) = VALUE int4_table( ( split_array_id ) ( LINES OF number->array_path ) ).

        node->prepend( NEW number( value = number->value DIV 2 array_path = split_array_path ) ).
        node->prepend( NEW number( value = number->value DIV 2 + number->value MOD 2 array_path = split_array_path ) ).
        node->remove( ).

        number_split = abap_true.
        EXIT.

      ELSE.

        node = node->next_node.

      ENDIF.

    ENDWHILE.

    IF number_split = abap_false.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.
FORM get_list_magnitude USING list TYPE REF TO linked_list
                     CHANGING magnitude TYPE i.

  WHILE list->size > 1.

    DATA(node) = list->first_node.
    WHILE node IS BOUND AND node->next_node IS BOUND.

      DATA(number) = node->value.
      DATA(number2) = node->next_node->value.
      IF number->get_array_id( ) = number2->get_array_id( ).

        DATA(new_number) = NEW number( value = number->value * 3 + number2->value * 2 array_path = number->array_path ).
        DELETE new_number->array_path INDEX 1.
        node->prepend( new_number ).

        node = node->remove( )->remove( ).

      ELSE.

        node = node->next_node.

      ENDIF.

    ENDWHILE.

  ENDWHILE.

  magnitude = list->first_node->value->value.

ENDFORM.


FORM read_line USING line TYPE string
            CHANGING list TYPE REF TO linked_list.

  list = NEW linked_list( ).
  DATA(cursor) = 0.
  DATA array_path TYPE int4_table.
  PERFORM read USING line array_path list CHANGING cursor.

ENDFORM.
FORM read USING line TYPE string
                parent_array_path TYPE int4_table
                list TYPE REF TO linked_list
       CHANGING cursor TYPE i.

  IF line+cursor(1) = '['.

    cursor = cursor + 1.
    array_counter = array_counter + 1.
    DATA(array_path) = VALUE int4_table( ( array_counter ) ( LINES OF parent_array_path ) ).
    DO.
      PERFORM read USING line array_path list CHANGING cursor.

      IF line+cursor(1) = ','.
        cursor = cursor + 1.
      ELSEIF line+cursor(1) = ']'.
        cursor = cursor + 1.
        EXIT.
      ENDIF.
    ENDDO.

  ELSE.

    DATA(number_cursor) = cursor + 1.
    WHILE line+number_cursor(1) CO '0123456789'.
      number_cursor = number_cursor + 1.
    ENDWHILE.

    DATA(number_length) = number_cursor - cursor.
    DATA(number) = CONV i( line+cursor(number_length) ).
    cursor = number_cursor.

    DATA(snailfish_number) = NEW number( value = number array_path = parent_array_path ).
    list->push( snailfish_number ).

  ENDIF.


ENDFORM.



CLASS number IMPLEMENTATION.

  METHOD constructor.

    me->value = value.
    me->array_path = array_path.

  ENDMETHOD.

  METHOD get_array_id.

    ret = VALUE #( me->array_path[ 1 ] DEFAULT -1 ).

  ENDMETHOD.
  METHOD get_depth.

    ret = lines( me->array_path ).

  ENDMETHOD.

ENDCLASS.
CLASS linked_list IMPLEMENTATION.

  METHOD push.

    DATA(node) = NEW linked_list_node( value = value list = me ).

    node->previous_node = me->last_node.
    IF me->last_node IS BOUND.
      me->last_node->next_node = node.
    ENDIF.

    me->last_node = node.
    IF me->first_node IS NOT BOUND.
      me->first_node = node.
    ENDIF.


    me->size = me->size + 1.
    ret = node.

  ENDMETHOD.
  METHOD append_to_node.

    DATA(new_node) = NEW linked_list_node( value = value list = me ).

    IF node->next_node IS BOUND.
      node->next_node->previous_node = new_node.
      new_node->next_node = node->next_node.
    ENDIF.

    node->next_node = new_node.
    new_node->previous_node = node.

    IF me->last_node = node.
      me->last_node = new_node.
    ENDIF.


    me->size = me->size + 1.
    ret = new_node.

  ENDMETHOD.
  METHOD prepend_to_node.

    DATA(new_node) = NEW linked_list_node( value = value list = me ).

    IF node->previous_node IS BOUND.
      node->previous_node->next_node = new_node.
      new_node->previous_node = node->previous_node.
    ENDIF.

    node->previous_node = new_node.
    new_node->next_node = node.

    IF me->first_node = node.
      me->first_node = new_node.
    ENDIF.


    me->size = me->size + 1.
    ret = new_node.

  ENDMETHOD.
  METHOD remove_node.

    IF node->previous_node IS BOUND.
      node->previous_node->next_node = node->next_node.
    ENDIF.
    IF node->next_node IS BOUND.
      node->next_node->previous_node = node->previous_node.
    ENDIF.

    IF me->last_node = node.
      me->last_node = node->previous_node.
    ENDIF.
    IF me->first_node = node.
      me->first_node = node->next_node.
    ENDIF.


    DATA(next_node) = node->next_node.
    node->previous_node = VALUE #( ).
    node->next_node = VALUE #( ).

    me->size = me->size - 1.
    ret = next_node.

  ENDMETHOD.

ENDCLASS.
CLASS linked_list_node IMPLEMENTATION.

  METHOD constructor.

    me->value = value.
    me->list = list.

  ENDMETHOD.

  METHOD append.

    ret = me->list->append_to_node( value = value node = me ).

  ENDMETHOD.
  METHOD prepend.

    ret = me->list->prepend_to_node( value = value node = me ).

  ENDMETHOD.
  METHOD remove.

    ret = me->list->remove_node( node = me ).

  ENDMETHOD.

ENDCLASS.
