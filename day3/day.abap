START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/3` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

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

  DATA(value1_str) = ``.
  DO 12 TIMES.

    DATA(i) = sy-index - 1.
    DATA(ones_ahead) = 0.
    LOOP AT lines INTO DATA(line).
      ones_ahead = ones_ahead + COND i( WHEN line+i(1) = '1' THEN 1 ELSE -1 ).
    ENDLOOP.

    value1_str = value1_str && COND string( WHEN ones_ahead > 0 THEN `1` ELSE `0` ).

  ENDDO.

  DATA(value1) = CONV i( /ui2/cl_number=>base_converter( number = value1_str from = 2 to = 10 ) ).
  DATA(value2x) = CONV hex10( value1 ) BIT-XOR CONV hex10( 2 ** 12 - 1 ).
  DATA(value2) = CONV i( value2x ).


  DATA(answer) = value1 * value2.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.


  DATA(value1_str) = ``.

  TYPES lines_t TYPE TABLE OF char12 WITH EMPTY KEY.

  DATA(lines1) = CORRESPONDING lines_t( lines ).
  DO 12 TIMES.

    DATA(i) = sy-index - 1.
    DATA(ones_ahead) = 0.
    LOOP AT lines1 INTO DATA(line).
      ones_ahead = ones_ahead + COND i( WHEN line+i(1) = '1' THEN 1 ELSE -1 ).
    ENDLOOP.

    DATA(winning_char) = COND char1( WHEN ones_ahead >= 0 THEN `1` ELSE `0` ).
    DELETE lines1 WHERE table_line+i(1) <> winning_char.
    IF lines( lines1 ) = 1.
      EXIT.
    ENDIF.

  ENDDO.

  DATA(lines2) = CORRESPONDING lines_t( lines ).
  DO 12 TIMES.

    i = sy-index - 1.
    ones_ahead = 0.
    LOOP AT lines2 INTO line.
      ones_ahead = ones_ahead + COND i( WHEN line+i(1) = '1' THEN 1 ELSE -1 ).
    ENDLOOP.

    winning_char = COND char1( WHEN ones_ahead < 0 THEN `1` ELSE `0` ).
    DELETE lines2 WHERE table_line+i(1) <> winning_char.
    IF lines( lines2 ) = 1.
      EXIT.
    ENDIF.

  ENDDO.

  DATA(value1) = CONV i( /ui2/cl_number=>base_converter( number = lines1[ 1 ] from = 2 to = 10 ) ).
  DATA(value2) = CONV i( /ui2/cl_number=>base_converter( number = lines2[ 1 ] from = 2 to = 10 ) ).


  DATA(answer) = value1 * value2.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.
