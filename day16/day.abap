CLASS bit_reader DEFINITION.

  PUBLIC SECTION.
    DATA bits TYPE int4_table.
    DATA cursor TYPE i.

    METHODS:
      constructor
        IMPORTING bits TYPE int4_table,
      read
        IMPORTING bit_count  TYPE i
        RETURNING VALUE(ret) TYPE int8.

    CLASS-METHODS:
      from_hex_string
        IMPORTING hex_str    TYPE string
        RETURNING VALUE(ret) TYPE REF TO bit_reader.

ENDCLASS.

CLASS packet_reader DEFINITION.

  PUBLIC SECTION.
    TYPES packet_value_t TYPE int8.
    TYPES packet_value_list_t TYPE TABLE OF packet_value_t WITH DEFAULT KEY.

    DATA bit_reader TYPE REF TO bit_reader.
    DATA version_sum TYPE int8.

    METHODS:
      constructor
        IMPORTING bit_reader TYPE REF TO bit_reader,
      read_packet
        RETURNING VALUE(ret) TYPE packet_value_t,
      read_int_packet
        RETURNING VALUE(ret) TYPE packet_value_t,
      read_op_packet
        IMPORTING packet_type TYPE i
        RETURNING VALUE(ret)  TYPE packet_value_t,
      read_op_sub_packets
        RETURNING VALUE(ret) TYPE packet_value_list_t.

ENDCLASS.



START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/16` CHANGING input.

  DATA(bit_reader) = bit_reader=>from_hex_string( input ).
  DATA(packet_reader) = NEW packet_reader( bit_reader ).

  DATA(outer_value) = packet_reader->read_packet( ).

  WRITE / 'Part 1:'.
  WRITE CONV string( packet_reader->version_sum ).
  WRITE / 'Part 2:'.
  WRITE CONV string( outer_value ).



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


CLASS bit_reader IMPLEMENTATION.

  METHOD constructor.

    me->bits = bits.

  ENDMETHOD.
  METHOD read.

    DATA value TYPE int8.
    DO bit_count TIMES.
      value = value * 2.
      value = value + me->bits[ me->cursor + 1 ].
      me->cursor = me->cursor + 1.
    ENDDO.

    ret = value.

  ENDMETHOD.
  METHOD from_hex_string.

    TYPES x1 TYPE x LENGTH 1.

    DATA bits TYPE int4_table.
    DO strlen( hex_str ) TIMES.

      DATA(offset) = sy-index - 1.
      DATA(hex_char) = hex_str+offset(1).
      DATA(hex_value) = CONV i( CONV x1( `0` && hex_char ) ).

      INSERT ( hex_value DIV 8 ) MOD 2 INTO TABLE bits.
      INSERT ( hex_value DIV 4 ) MOD 2 INTO TABLE bits.
      INSERT ( hex_value DIV 2 ) MOD 2 INTO TABLE bits.
      INSERT hex_value MOD 2 INTO TABLE bits.

    ENDDO.

    DATA(bit_reader) = NEW bit_reader( bits ).
    ret = bit_reader.


  ENDMETHOD.

ENDCLASS.
CLASS packet_reader IMPLEMENTATION.

  METHOD constructor.

    me->bit_reader = bit_reader.

  ENDMETHOD.

  METHOD read_packet.

    DATA(version) = bit_reader->read( 3 ).
    DATA(type) = bit_reader->read( 3 ).

    me->version_sum = me->version_sum + version.

    CASE type.
      WHEN 4.
        ret = me->read_int_packet( ).
      WHEN OTHERS.
        ret = me->read_op_packet( CONV #( type ) ).
    ENDCASE.

  ENDMETHOD.

  METHOD read_int_packet.

    DATA packet_value TYPE packet_value_t.
    DO.
      DATA(segment_bit) = bit_reader->read( 1 ).
      DATA(segment_value) = bit_reader->read( 4 ).
      packet_value = packet_value * 2 ** 4 + segment_value.
      IF segment_bit = 0.
        EXIT.
      ENDIF.
    ENDDO.

    ret = packet_value.

  ENDMETHOD.

  METHOD read_op_packet.

    DATA(sub_packet_values) = me->read_op_sub_packets( ).

    CASE packet_type.
      WHEN 0.
        DATA(sum) = CONV packet_value_t( 0 ).
        LOOP AT sub_packet_values INTO DATA(value).
          sum = sum + value.
        ENDLOOP.
        ret = sum.

      WHEN 1.
        DATA(product) = CONV packet_value_t( 1 ).
        LOOP AT sub_packet_values INTO value.
          product = product * value.
        ENDLOOP.
        ret = product.

      WHEN 2.
        DATA(min_value) = sub_packet_values[ 1 ].
        LOOP AT sub_packet_values INTO value FROM 2.
          min_value = nmin( val1 = min_value val2 = value ).
        ENDLOOP.
        ret = min_value.

      WHEN 3.
        DATA(max_value) = sub_packet_values[ 1 ].
        LOOP AT sub_packet_values INTO value FROM 2.
          max_value = nmax( val1 = max_value val2 = value ).
        ENDLOOP.
        ret = max_value.

      WHEN 5.
        ret = COND packet_value_t( WHEN sub_packet_values[ 1 ] > sub_packet_values[ 2 ] THEN 1 ELSE 0 ).
      WHEN 6.
        ret = COND packet_value_t( WHEN sub_packet_values[ 1 ] < sub_packet_values[ 2 ] THEN 1 ELSE 0 ).
      WHEN 7.
        ret = COND packet_value_t( WHEN sub_packet_values[ 1 ] = sub_packet_values[ 2 ] THEN 1 ELSE 0 ).

    ENDCASE.

  ENDMETHOD.
  METHOD read_op_sub_packets.

    DATA packet_values TYPE packet_value_list_t.

    DATA(len_type) = bit_reader->read( 1 ).
    IF len_type = 0.

      DATA(sub_bits_count) = bit_reader->read( 15 ).
      DATA(start_cursor) = bit_reader->cursor.
      WHILE bit_reader->cursor - start_cursor < sub_bits_count.
        INSERT me->read_packet( ) INTO TABLE packet_values.
      ENDWHILE.

    ELSE.

      DATA(sub_packet_count) = bit_reader->read( 11 ).
      DO sub_packet_count TIMES.
        INSERT me->read_packet( ) INTO TABLE packet_values.
      ENDDO.

    ENDIF.

    ret = packet_values.

  ENDMETHOD.

ENDCLASS.
