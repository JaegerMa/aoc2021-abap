START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/7` CHANGING input.


  SPLIT input AT `,` INTO TABLE DATA(input_numbers_str).
  DATA(input_numbers) = VALUE int4_table( FOR input_number_str IN input_numbers_str ( CONV i( input_number_str ) ) ).

  DATA(max_value) = -1.
  LOOP AT input_numbers INTO DATA(number).

    IF max_value = -1 OR number > max_value.
      max_value = number.
    ENDIF.

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

  DATA(min_cost) = -1.

  DO max_value + 1 TIMES.

    DATA(cost) = 0.
    DATA(destination) = sy-index - 1.

    LOOP AT input_numbers INTO DATA(number).
      DATA(distance) = abs( number - destination ).
      cost = cost + distance.
    ENDLOOP.

    IF min_cost = -1 OR cost < min_cost.
      min_cost = cost.
    ENDIF.

  ENDDO.


  DATA(answer) = min_cost.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA(min_cost) = -1.

  DO max_value + 1 TIMES.

    DATA(cost) = 0.
    DATA(destination) = sy-index - 1.

    LOOP AT input_numbers INTO DATA(number).
      DATA(distance) = abs( number - destination ).
      cost = cost + ( distance * ( distance + 1 ) / 2 ).
    ENDLOOP.

    IF min_cost = -1 OR cost < min_cost.
      min_cost = cost.
    ENDIF.

  ENDDO.


  DATA(answer) = min_cost.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.
